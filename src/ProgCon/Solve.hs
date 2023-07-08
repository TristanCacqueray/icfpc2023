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
import ProgCon.Syntax
import Say
import Text.Printf (printf)

solve :: Maybe (Int, Solution) -> String -> Problem -> IO (Int, Solution)
solve = geneticSolve

type RandGen a = RandT StdGen IO a

{- | All the positions are stored, that way all the mutation happen in-place.
 in 'toSolution' we keep only the one for the active musician.
-}
newtype GenSolution = GenSolution (MV.IOVector (Float, Float))

type SolutionScore = Int

-- | Arranging the musicians in a grid, this function returns all the available placements.
allSquarePlacement :: (Float, Float) -> [(Float, Float)]
allSquarePlacement (width, height) = do
    x <- [0 .. width / (2 * radius) - 1]
    y <- [0 .. height / (2 * radius) - 1]
    pure (radius + x * 2 * radius, radius + y * 2 * radius)
  where
    radius = 10

toAbsPlacement :: Problem -> (Float, Float) -> (Float, Float)
toAbsPlacement problem (x, y) = (sx + x, sy + y)
  where
    (sx, sy) = problem.problemStageBottomLeft

geneticSolve :: Maybe (SolutionScore, Solution) -> String -> Problem -> IO (Int, Solution)
geneticSolve mPrevSolution name problem = runRandGen do
    initialSeeds <- case mPrevSolution of
        Just (prevScore, prevSolution) -> do
            seed <- fromSolution prevSolution problem placements
            pure [(prevScore, seed)]
        Nothing -> replicateM seedCount (randomSolution problem placements)
    ((finalScore, finalSolution) : _) <- go genCount initialSeeds
    solution <- toSolution problem finalSolution
    pure (finalScore, solution)
  where
    genCount = 3
    seedCount = 1
    breedCount = 2
    dim = (problem.problemStageWidth, problem.problemStageHeight)
    placements = toAbsPlacement problem <$> allSquarePlacement dim
    total = length placements
    musicianCount = UV.length problem.problemMusicians

    go :: Int -> [(SolutionScore, GenSolution)] -> RandGen [(SolutionScore, GenSolution)]
    go 0 !seeds = pure seeds
    go count !seeds = do
        -- Generate a new population
        population <- concat <$> traverse breedNewSolutions seeds

        -- Order by score
        let populationOrdered = sortOn (\(score, _) -> negate score) population
        let best = case populationOrdered of
                (score, _) : _ -> score
                _ -> minBound
        liftIO do
            now <- getCurrentTime
            sayString $ printf "%s %s: gen %2d - %10d" (take 25 $ iso8601Show now) name (genCount - count) best

        -- Repeat the process, keeping only the best seed.
        go (count - 1) (take seedCount populationOrdered)
      where
        breedNewSolutions :: (SolutionScore, GenSolution) -> RandGen [(SolutionScore, GenSolution)]
        breedNewSolutions x@(_, s) = do
            newSolutions <- replicateM breedCount (makeNewSeed s)
            -- Keep the original seed
            pure (x : newSolutions)

        -- Create a new solution based on the previous one
        makeNewSeed :: GenSolution -> RandGen (Int, GenSolution)
        makeNewSeed (GenSolution seedPlacements) = do
            newSolution <- GenSolution <$> MV.clone seedPlacements
            doMutate newSolution
            score <- scoreSolution problem newSolution
            pure (score, newSolution)

        -- Shuffle the musician placement randomly
        doMutate :: GenSolution -> RandGen ()
        doMutate (GenSolution iov) = do
            mutationCount <- getRandomR (genCount, MV.length iov `div` 5)
            replicateM_ mutationCount do
                -- Pick a random musician
                musician <- getRandomR (0, musicianCount - 1)
                -- Pick a random new position
                swapPos <- getRandomR (0, total - 1)
                -- Mutate
                MV.swap iov musician swapPos

-- | Create a random solution.
randomSolution :: Problem -> [(Float, Float)] -> RandGen (Int, GenSolution)
randomSolution problem xs = do
    iov <- V.thaw (V.fromList xs)
    liftRandT \stdg -> do
        newstdg <- stToIO $ shuffle iov stdg
        pure ((), newstdg)
    let gs = GenSolution iov
    score <- scoreSolution problem gs
    pure (score, gs)

-- | Create the 'Solution' data from a 'GenSolution'.
toSolution :: Problem -> GenSolution -> RandGen Solution
toSolution problem (GenSolution iov) = do
    xs <- UV.convert <$> V.freeze iov
    pure $ Solution $ UV.take (UV.length problem.problemMusicians) xs

fromSolution :: Solution -> Problem -> [(Float, Float)] -> RandGen GenSolution
fromSolution solution problem xs = do
    iov <- MV.generate (length xs) \pos ->
        if pos < musicianCount
            then solution.solutionPlacements UV.! pos -- re-use previous musician position
            else otherPlacements UV.! (pos - musicianCount)
    pure $ GenSolution iov
  where
    otherPlacements = UV.fromList [pos | pos <- xs, pos `UV.notElem` solution.solutionPlacements]
    musicianCount = UV.length problem.problemMusicians

-- | Compute the score of a 'GenSolution'.
scoreSolution :: Problem -> GenSolution -> RandGen Int
scoreSolution problem gs = do
    solution <- toSolution problem gs
    pure $ scoreHappiness problem solution

-- | Helper to run the MonadRandom.
runRandGen :: RandGen a -> IO a
-- runRandGen action = evalRandT action (mkStdGen 42)
runRandGen action = do
    stdg <- initStdGen
    evalRandT action stdg
