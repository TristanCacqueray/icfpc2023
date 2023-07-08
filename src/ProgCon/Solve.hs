module ProgCon.Solve where

import Control.Monad.Random.Strict (RandT, evalRandT, liftRandT, mkStdGen)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed qualified as UV
import System.Random (StdGen)
import VectorShuffling.Mutable (shuffle)

import Control.Monad (replicateM, replicateM_)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Random (MonadRandom (getRandomR))
import Control.Monad.ST (stToIO)
import Data.List (sortOn)
import ProgCon.Eval
import ProgCon.Syntax
import Text.Printf (printf)

solve :: Problem -> IO Solution
solve = geneticSolve

type RandGen a = RandT StdGen IO a

newtype GenSolution = GenSolution (MV.IOVector (Float, Float))

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

geneticSolve :: Problem -> IO Solution
geneticSolve problem = runRand do
    initialSeeds <- replicateM seedCount (randomSolution problem placements)
    (finalSolutions : _) <- go 10 initialSeeds
    toSolution problem (snd finalSolutions)
  where
    seedCount = 10
    breedCount = 10
    dim = (problem.problemStageWidth, problem.problemStageHeight)
    placements = toAbsPlacement problem <$> allSquarePlacement dim
    total = length placements
    count = UV.length problem.problemMusicians

    go :: Int -> [(Int, GenSolution)] -> RandGen [(Int, GenSolution)]
    go !0 !seeds = pure seeds
    go !epoch !seeds = do
        -- Generate a new population
        population <- concat <$> traverse mutate seeds

        -- Order by score
        let populationOrdered = sortOn (\(score, _) -> negate score) population
        let best = case population of
                (x, _) : _ -> x
                _ -> 0
        liftIO $ putStrLn $ printf "epoch %2d: %010d" epoch best

        -- Repeat the process, keeping only the best seed.
        go (epoch - 1) (take seedCount populationOrdered)
      where
        mutate :: (Int, GenSolution) -> RandGen [(Int, GenSolution)]
        mutate x@(_, s) = (x :) <$> replicateM breedCount (makeNewSeed s)

        makeNewSeed :: GenSolution -> RandGen (Int, GenSolution)
        makeNewSeed (GenSolution seedPlacements) = do
            newSolution <- GenSolution <$> MV.clone seedPlacements
            doMutate newSolution
            score <- scoreSolution problem newSolution
            pure (score, newSolution)

        doMutate :: GenSolution -> RandGen ()
        doMutate (GenSolution iov) = do
            mutationCount <- getRandomR (epoch, MV.length iov `div` 5)
            replicateM_ mutationCount do
                musician <- getRandomR (0, count - 1)
                swapPos <- getRandomR (0, total - 1)
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

-- | Compute the score of a 'GenSolution'.
scoreSolution :: Problem -> GenSolution -> RandGen Int
scoreSolution problem gs = do
    solution <- toSolution problem gs
    pure $ scoreHappiness problem solution

-- | Helper to run the MonadRandom.
runRand :: RandGen a -> IO a
runRand action = evalRandT action (mkStdGen 42)