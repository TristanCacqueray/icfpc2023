module ProgCon (main) where

import System.Environment (getArgs)
import Control.Monad

import Data.Vector.Unboxed qualified as UV

import ProgCon.Eval
import ProgCon.Parser
import ProgCon.Syntax
import ProgCon.GUI
import ProgCon.Solve

mainCheck :: FilePath -> FilePath -> IO Int
mainCheck problemPath solutionPath = do
  problem <- loadJSON @Problem problemPath
  solution <- loadJSON @Solution solutionPath
  pure (scoreHappiness problem solution)

mainSolve :: FilePath -> IO ()
mainSolve problemPath = do
  problem <- loadJSON @Problem problemPath
  solution <- solve problem
  writeSolution solution

mainRender :: FilePath -> IO ()
mainRender problemPath = do
  problem <- loadJSON @Problem problemPath
  putStrLn $ "musicians: "<> show (UV.length problem.problemMusicians)
  putStrLn $ "room: " <> show (problem.problemRoomWidth, problem.problemRoomHeight)
  putStrLn $ "stage: " <> show (problem.problemStageWidth, problem.problemStageHeight)
  putStrLn $ "stagePos: " <> show problem.problemStageBottomLeft
  solution <- solve problem
  renderProblem problem solution

mainTest :: IO ()
mainTest = do
  res <- mainCheck "./problems/problem-spec.json" "./problems/solution-spec.json"
  unless (res == 5343) do
    error $ "Invalid spec score, expected 5343, got: "<> show res

main :: IO ()
main = do
  getArgs >>= \case
    [] -> mainSolve "./problems/problem-10.json"
    ["test"] -> mainTest
    ["render", problemPath] -> mainRender problemPath
    ["solve", fp] -> mainSolve fp
    ["check", problemPath, solutionPath] -> print =<< mainCheck problemPath solutionPath
    _ -> error "usage: check pb solution | solve pb"
