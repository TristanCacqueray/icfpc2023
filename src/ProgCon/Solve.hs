module ProgCon.Solve where

import System.Random (StdGen)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as UV
import VectorShuffling.Immutable (shuffle)
import Control.Monad.Random.Strict (Rand, liftRand, evalRand, mkStdGen)

import ProgCon.Syntax

runRand :: Rand StdGen a -> a
runRand action = evalRand action (mkStdGen 42)

-- | Arranging the musicians in a grid, this function returns all the possible placement
allSquarePlacement :: (Int, Int) -> [(Int, Int)]
allSquarePlacement (width, height) = do
  x <- [0..width `div` (2 * radius) - 1]
  y <- [0..height `div` (2 * radius) - 1]
  pure (radius + x * 2 * radius, radius + y * 2 * radius)
  where
    radius = 10

randomSquarePlacement :: [(Int, Int)] -> Rand StdGen (V.Vector (Int, Int))
randomSquarePlacement xs = liftRand do
  shuffle (V.fromList xs)

data GenSolution = GenSolution {
  solution :: Solution,
  rest :: V.Vector (Int, Int)
  }

toPlacement :: Problem -> (Int, Int) -> (Float, Float)
toPlacement problem (x, y) = (sx + fromIntegral x, sy + fromIntegral y)
  where
    (sx, sy) = problem.problemStageBottomLeft

generateSolution :: Problem -> Rand StdGen GenSolution
generateSolution problem = do
  allPlacements <- randomSquarePlacement (allSquarePlacement dimInt)
  let placements = UV.convert $ V.map (toPlacement problem) $ V.take musiciansCount allPlacements
  let solution = Solution placements
  pure $ GenSolution solution (V.drop musiciansCount allPlacements)
 where
   musiciansCount = UV.length problem.problemMusicians
   dimInt = (round problem.problemStageWidth, round problem.problemStageHeight)
