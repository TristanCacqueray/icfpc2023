module ProgCon (someFunc) where

import ProgCon.Parser
import ProgCon.Syntax

solve :: Problem -> Solution
solve _ = Solution []

someFunc :: IO ()
someFunc = do
  problem <- loadProblem "../problems/problem-1.json"
  putStrLn $ "musicians: "<> show (length problem.problemMusicians)
  writeSolution (solve problem)
