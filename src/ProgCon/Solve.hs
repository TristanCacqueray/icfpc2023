module ProgCon.Solve where

import Control.Monad.Random.Strict
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed qualified as UV
import VectorShuffling.Mutable (shuffle)

import Control.Monad.ST (stToIO)
import Data.List (sortOn)
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import ProgCon.Eval
import ProgCon.Parser (saveSolutionPath)
import ProgCon.Syntax
import Say
import Text.Printf (printf)

solve :: Maybe SolutionDescription -> ProblemDescription -> IO (Maybe SolutionDescription)
solve = geneticSolve

type RandGen a = RandT StdGen IO a

type Grid = Int

-- | Arranging the musicians in a grid, this function returns all the available placements.
allSquarePlacement :: (Grid, Grid) -> [(Grid, Grid)]
allSquarePlacement (width, height) = do
    x <- [0 .. width `div` (2 * radius) - 1]
    y <- [0 .. height `div` (2 * radius) - 1]
    pure (radius + x * 2 * radius, radius + y * 2 * radius)
  where
    radius = 10

toAbsPlacement :: Problem -> (Grid, Grid) -> (Grid, Grid)
toAbsPlacement problem (x, y) = (sx + x, sy + y)
  where
    (sx, sy) = problem.problemStageBottomLeft

geneticSolve :: Maybe SolutionDescription -> ProblemDescription -> IO (Maybe SolutionDescription)
geneticSolve mPrevSolution problemDesc
    | total < musicianCount = do
        -- mapM_ print (allSquarePlacement padding dim)
        sayString $ "Impossible square placement: " <> show dim <> ", for " <> show musicianCount <> " total: " <> show total
        pure Nothing
    | otherwise = runRandGen do
        initialSeeds <- case mPrevSolution of
            Just solution -> do
                newSeeds <- replicateM (seedCount - 1) (randomSolution problem placements)
                pure $ solution : newSeeds
            Nothing -> replicateM seedCount (randomSolution problem placements)
        (newSolution : _) <- go genCount initialSeeds
        pure (Just newSolution)
  where
    genCount = 20
    seedCount = 8
    breedCount = 12
    dim = (problem.problemStageWidth, problem.problemStageHeight)
    placements = toAbsPlacement problem <$> allSquarePlacement dim
    total = length placements
    musicianCount = UV.length problem.problemMusicians
    problem = problemDesc.problem

    go :: Int -> [SolutionDescription] -> RandGen [SolutionDescription]
    go 0 !seeds = pure seeds
    go count !seeds = do
        -- Generate a new population
        population <- concat <$> traverse breedNewSolutions seeds

        -- Order by score
        let populationOrdered = sortOn (\sd -> negate sd.score) population
        let prevScore = case seeds of
                sd : _ -> sd.score
                _ -> minBound
        best <- case populationOrdered of
            sd : _ -> do
                when (sd.score > prevScore) do
                    sayString $ show problemDesc.name <> ": new highscore: " <> show sd.score <> ", saving..."
                    liftIO $ saveSolutionPath sd (solutionPath problemDesc.name)
                pure sd.score
            _ -> pure minBound
        liftIO do
            now <- getCurrentTime
            sayString $ printf "%s %s: gen %2d - %10d" (take 25 $ iso8601Show now) (show problemDesc.name) (genCount - count + 1) best

        -- Repeat the process, keeping only the best seed.
        go (count - 1) (take seedCount populationOrdered)
      where
        breedNewSolutions :: SolutionDescription -> RandGen [SolutionDescription]
        breedNewSolutions sd = do
            newSolutions <- replicateM breedCount (makeNewSeed sd)
            -- Keep the original seed
            pure (sd : newSolutions)

        -- Create a new solution based on the previous one
        makeNewSeed :: SolutionDescription -> RandGen SolutionDescription
        makeNewSeed sd = do
            genPlacements <- GenPlacements <$> MV.clone sd.genPlacements.iov
            doMutate genPlacements
            score <- scoreSolution problem genPlacements
            pure SolutionDescription{score, musicianCount, genPlacements}

        -- Shuffle the musician placement randomly
        doMutate :: GenPlacements -> RandGen ()
        doMutate (GenPlacements iov) = do
            mutationCount <- getRandomR (genCount, MV.length iov `div` 5)
            replicateM_ mutationCount do
                -- Pick a random musician
                musician <- getRandomR (0, musicianCount - 1)
                -- Pick a random new position
                swapPos <- getRandomR (0, total - 1)
                -- Mutate
                MV.swap iov musician swapPos

-- | Create a random solution.
randomSolution :: Problem -> [(Grid, Grid)] -> RandGen SolutionDescription
randomSolution problem xs = do
    iov <- V.thaw (V.fromList xs)
    liftRandT \stdg -> do
        newstdg <- stToIO $ shuffle iov stdg
        pure ((), newstdg)
    let genPlacements = GenPlacements iov
        musicianCount = UV.length problem.problemMusicians
    score <- scoreSolution problem genPlacements
    pure (SolutionDescription{score, musicianCount, genPlacements})

-- | Create the 'Solution' data from a 'GenPlacements'.
toSolution :: Int -> GenPlacements -> IO Solution
toSolution musicianCount (GenPlacements iov) = do
    xs <- UV.convert <$> V.freeze iov
    pure $ Solution $ UV.take musicianCount xs

-- | Compute the score of a 'GenPlacements'.
scoreSolution :: Problem -> GenPlacements -> RandGen Int
scoreSolution problem gs = do
    solution <- liftIO (toSolution (UV.length problem.problemMusicians) gs)
    pure $ scoreHappiness problem solution

-- | Helper to run the MonadRandom.
runRandGen :: RandGen a -> IO a
-- runRandGen action = evalRandT action (mkStdGen 42)
runRandGen action = do
    stdg <- initStdGen
    evalRandT action stdg
