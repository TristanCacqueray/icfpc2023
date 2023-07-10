module ProgCon.Solve (
    solve,
    toSolution,
    fromSolutionDesc,
    maximumPlacements,
    randomSolution,
    runRandGen,
    tryImprove,
    RandGen,
    Improvement(..),
) where

import Control.Monad.Random.Strict
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed qualified as UV
import VectorShuffling.Mutable (shuffle)

import Control.Concurrent
import Control.Monad.ST (stToIO)
import Data.List (sortOn)
import Data.Time.Format
import Data.Time.LocalTime
import ProgCon.Eval
import ProgCon.GUI
import ProgCon.Parser (saveSolutionPath)
import ProgCon.Syntax
import Say
import Text.Printf (printf)

-- import RIO (replicateConcurrently)

solve :: Params -> Maybe ProblemRenderer -> Maybe SolutionDescription -> ProblemDescription -> IO (Maybe SolutionDescription)
solve = geneticSolve

type RandGen a = RandT StdGen IO a

type Grid = Int

-- | Arranging the musicians in a grid, this function returns all the available placements.
allGridPlacement :: (Grid, Grid) -> UV.Vector (Grid, Grid)
allGridPlacement (width, height) = UV.fromList $ go radius radius []
  where
    -- go takes the current (x, y) position, and the list of accumulated position
    go :: Grid -> Grid -> [(Grid, Grid)] -> [(Grid, Grid)]
    go x y !acc =
        let -- store the current pos in the accumulator
            newAcc = (x, y) : acc
         in if
                    | -- there is room to fit another musician on this line, keep the y pos
                      x + nextMusician < width ->
                        go (x + diameter) y newAcc
                    | -- there is room to start another line, reset the x pos
                      y + nextMusician < height ->
                        go radius (y + diameter) newAcc
                    | -- this is the end
                      otherwise ->
                        newAcc

-- | Placement dimension: ( -r- o -r- )
radius, diameter :: Int
radius = 10
diameter = radius * 2

{- | nextMusician is the distance from the current position + a whole new musician
 e.g:     o -r- )( -r- o -r-)|
-}
nextMusician :: Int
nextMusician = radius + diameter

-- | Arranging the musicians in a grid, this function returns all the available placements.
allPackedPlacement :: (Grid, Grid) -> UV.Vector (Grid, Grid)
allPackedPlacement (width, height) = UV.fromList $ go 0 radius radius []
  where
    go
        | -- do the offset per line
          width > height =
            goLine
        | -- do the offset per column
          otherwise =
            goCol

    goLine :: Int -> Grid -> Grid -> [(Grid, Grid)] -> [(Grid, Grid)]
    goLine line x y !acc =
        let newAcc = (x, y) : acc
            -- we alternate the start position every two lines
            newX
                | even line = diameter
                | otherwise = radius
         in if
                    | -- there is room to fit another musician on this line
                      x + nextMusician < width ->
                        goLine line (x + diameter) y newAcc
                    | -- there is room to start another line
                      y + nextMusician < height ->
                        goLine (line + 1) newX (y + newOffset) newAcc
                    | -- this is the end
                      otherwise ->
                        newAcc

    goCol :: Int -> Grid -> Grid -> [(Grid, Grid)] -> [(Grid, Grid)]
    goCol col x y !acc =
        let newAcc = (x, y) : acc
            -- we alternate the start position every two columns
            newY
                | even col = diameter
                | otherwise = radius
         in if
                    | -- there is room to fit another musician on this column
                      y + nextMusician < height ->
                        goCol col x (y + diameter) newAcc
                    | -- there is room to start another column
                      x + nextMusician < width ->
                        goCol (col + 1) (x + newOffset) newY newAcc
                    | -- this is the end
                      otherwise ->
                        newAcc

    newOffset = 19

maximumPlacements :: Problem -> UV.Vector (Grid, Grid)
maximumPlacements problem =
    let best
            | UV.length packed > UV.length grid = packed
            | otherwise = grid
     in UV.map toAbsPlacement best
  where
    dim = (problem.problemStageWidth, problem.problemStageHeight)
    packed = allPackedPlacement dim
    grid = allGridPlacement dim

    toAbsPlacement :: (Grid, Grid) -> (Grid, Grid)
    toAbsPlacement (x, y) = (sx + x, sy + y)
    (sx, sy) = problem.problemStageBottomLeft

data Improvement = Placement | Volume | Both
  deriving (Enum, Bounded, Show)

-- | This function simply try to improve a given solution by applying a single improvement
tryImprove :: ProblemDescription -> SolutionDescription -> Improvement -> RandGen (Maybe SolutionDescription)
tryImprove problemDesc sd improvement = do
    newSolution <- case improvement of
      Placement -> do
        genPlacements <- newPlacements
        pure $ sd{genPlacements}
      Volume -> do
        genVolumes <- newVolumes
        pure $ sd{genVolumes}
      Both -> do
        genPlacements <- newPlacements
        genVolumes <- newVolumes
        pure $ sd{genPlacements, genVolumes}

    score <- scoreSolution problemDesc newSolution.genPlacements newSolution.genVolumes
    pure $ if score > sd.score
      then Just (newSolution{score})
      else Nothing
  where
    musicianCount = UV.length problemDesc.problem.problemMusicians
    newPlacements = do
      -- Copy the previous placements and do one swap
      iov <- MV.clone sd.genPlacements.iov
      musician <- getRandomR (0, musicianCount - 1)
      swapPos <- getRandomR (0, MV.length iov - 1)
      MV.swap iov musician swapPos
      pure $ GenPlacements iov
    newVolumes = do
      -- Copy the previous volumes and do one change
      iov <- MV.clone sd.genVolumes
      musician <- getRandomR (0, musicianCount - 1)
      volume <- getRandomR (0, 10)
      -- TODO: to a relative increase of the current volume?
      MV.write iov musician volume
      pure iov

geneticSolve :: Params -> Maybe ProblemRenderer -> Maybe SolutionDescription -> ProblemDescription -> IO (Maybe SolutionDescription)
geneticSolve params mRenderer mPrevSolution problemDesc
    | total < musicianCount = do
        -- mapM_ print (allSquarePlacement padding dim)
        sayString $ "Impossible square placement: " <> show dim <> ", for " <> show musicianCount <> " total: " <> show total
        pure Nothing
    | otherwise = runRandGen do
        initialSeeds <- case mPrevSolution of
            Just solution -> do
                newSeeds <- parReplicateM (params.seedCount - 1) (randomSolution problemDesc placements)
                pure $ solution : newSeeds
            Nothing -> parReplicateM params.seedCount (randomSolution problemDesc placements)
        (newSolution : _) <- go 0 params.genCount initialSeeds
        pure (Just newSolution)
  where
    dim = (problem.problemStageWidth, problem.problemStageHeight)
    placements = maximumPlacements problem
    total = UV.length placements
    musicianCount = UV.length problem.problemMusicians
    problem = problemDesc.problem

    go :: Int -> Int -> [SolutionDescription] -> RandGen [SolutionDescription]
    go lastHighscoreAge count !seeds
      | lastHighscoreAge > min params.genCount 20 && count <= 0 = pure seeds
      | otherwise = do
        -- Generate a new population
        population <- concat <$> traverse breedNewSolutions seeds

        -- Order by score
        let populationOrdered = sortOn (\sd -> negate sd.score) population
        let prevScore = case seeds of
                sd : _ -> sd.score
                _ -> minBound
        (newHighScore, best) <- case populationOrdered of
            sd : _ -> do
                when (sd.score > prevScore) do
                    sayString $ show problemDesc.name <> ": new highscore: " <> showScore sd.score <> ", saving..."
                    liftIO $ saveSolutionPath sd (solutionPath problemDesc.name)
                    forM_ mRenderer \renderer -> liftIO do
                        solution <- toSolution musicianCount sd.genPlacements sd.genVolumes
                        renderProblem problem solution renderer
                        -- FIX: without this delay, the gloss ui is not refreshing :/
                        liftIO $ threadDelay 1_000_000

                pure (sd.score > prevScore, sd.score)
            _ -> pure (False, minBound)

        let newLastHighscore
              | newHighScore = 0
              | otherwise = lastHighscoreAge + 1

        liftIO do
            now <- getZonedTime
            sayString $ printf "%s %s: gen %4d / %d score %s (since %d)" (formatTime defaultTimeLocale (timeFmt defaultTimeLocale) now) ('#' : show problemDesc.name) (params.genCount - count + 1) params.genCount (showScore best) newLastHighscore

        -- Repeat the process, keeping only the best seed.
        go newLastHighscore (count - 1) (take params.seedCount populationOrdered)
      where
        breedNewSolutions :: SolutionDescription -> RandGen [SolutionDescription]
        breedNewSolutions sd = do
            newSolutions <- parReplicateM params.breedCount (makeNewSeed sd)
            -- Keep the original seed
            pure (sd : newSolutions)

        -- Create a new solution based on the previous one
        makeNewSeed :: SolutionDescription -> RandGen SolutionDescription
        makeNewSeed sd = do
            genPlacements <- GenPlacements <$> MV.clone sd.genPlacements.iov
            doMutate genPlacements
            genVolumes <- MV.clone sd.genVolumes
            doMutateVolume genVolumes
            score <- scoreSolution problemDesc genPlacements genVolumes
            pure SolutionDescription{score, musicianCount, genPlacements, genVolumes}

        doMutateVolume :: MV.IOVector Float -> RandGen ()
        doMutateVolume iov = do
            -- mutate a couple of volumes
            replicateM_ params.volumeCount do
                musician <- getRandomR (0, musicianCount - 1)
                volume <- getRandomR (0, 10)
                MV.write iov musician volume

        -- Shuffle the musician placement randomly
        doMutate :: GenPlacements -> RandGen ()
        doMutate (GenPlacements iov) = do
            replicateM_ params.placementCount do
                -- Pick a random musician
                musician <- getRandomR (0, musicianCount - 1)
                -- Pick a random new position
                swapPos <- getRandomR (0, total - 1)
                -- Mutate
                MV.swap iov musician swapPos

-- | Create a random solution.
randomSolution :: ProblemDescription -> UV.Vector (Grid, Grid) -> RandGen SolutionDescription
randomSolution problemDesc placements = do
    iov <- V.thaw (V.convert placements)
    liftRandT \stdg -> do
        newstdg <- stToIO $ shuffle iov stdg
        pure ((), newstdg)
    let genPlacements = GenPlacements iov
        musicianCount = UV.length problemDesc.problem.problemMusicians
    genVolumes <- MV.replicate musicianCount 1
    score <- scoreSolution problemDesc genPlacements genVolumes
    pure (SolutionDescription{score, musicianCount, genPlacements, genVolumes})

-- | Create the 'Solution' data from a 'GenPlacements'.
toSolution :: Int -> GenPlacements -> MV.IOVector Float -> IO Solution
toSolution musicianCount (GenPlacements iov) ioVolumes = do
    xs <- UV.convert <$> V.freeze iov
    volumes <- UV.convert <$> V.freeze ioVolumes
    pure $ Solution (UV.take musicianCount xs) volumes

fromSolutionDesc :: SolutionDescription -> IO Solution
fromSolutionDesc solutionDesc =
    toSolution solutionDesc.musicianCount solutionDesc.genPlacements solutionDesc.genVolumes

-- | Compute the score of a 'GenPlacements'.
scoreSolution :: ProblemDescription -> GenPlacements -> MV.IOVector Float -> RandGen Int
scoreSolution problemDesc gs vs = do
    solution <- liftIO (toSolution (UV.length problemDesc.problem.problemMusicians) gs vs)
    pure $ scoreHappiness problemDesc solution

-- | Helper to run the MonadRandom.
runRandGen :: RandGen a -> IO a
-- runRandGen action = evalRandT action (mkStdGen 42)
runRandGen action = do
    stdg <- initStdGen
    evalRandT action stdg

parReplicateM :: Int -> RandGen a -> RandGen [a]
parReplicateM count action = do
    -- that doesn't seem to work
    -- liftIO $ replicateConcurrently count (runRandGen action)
    replicateM count action
