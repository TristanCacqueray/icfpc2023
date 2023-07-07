module ProgCon (main) where

import ProgCon.Parser
import ProgCon.Syntax
import ProgCon.GUI

solve :: Problem -> Solution
solve _ = Solution []

main :: IO ()
main = do
  problem <- loadProblem "../problems/problem-1.json"
  putStrLn $ "musicians: "<> show (length problem.problemMusicians)
  putStrLn $ "room: " <> show (problem.problemRoomWidth, problem.problemRoomHeight)
  putStrLn $ "stage: " <> show (problem.problemStageWidth, problem.problemStageHeight)
  putStrLn $ "stagePos: " <> show problem.problemStageBottomLeft

  writeSolution (solve problem)
  renderProblem problem
