module ProgCon (main) where

import System.Environment (getArgs)

import Data.Vector.Unboxed qualified as UV

import ProgCon.Parser
import ProgCon.Syntax
import ProgCon.GUI

solve :: Problem -> Solution
solve _ = Solution mempty

usage :: IO FilePath
usage = getArgs >>= \case
  [] -> pure "./problems/problem-10.json"
  (x : _) -> pure x

main :: IO ()
main = do
  fp <- usage
  problem <- loadProblem fp
  putStrLn $ "musicians: "<> show (UV.length problem.problemMusicians)
  putStrLn $ "room: " <> show (problem.problemRoomWidth, problem.problemRoomHeight)
  putStrLn $ "stage: " <> show (problem.problemStageWidth, problem.problemStageHeight)
  putStrLn $ "stagePos: " <> show problem.problemStageBottomLeft

  writeSolution (solve problem)
  renderProblem problem
