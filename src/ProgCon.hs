module ProgCon (main) where

import Control.Monad
import Data.Aeson qualified as Aeson
import Data.Vector.Unboxed qualified as UV
import Say
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath (takeBaseName)

import Control.Concurrent.Async (mapConcurrently_)
import ProgCon.Eval
import ProgCon.GUI
import ProgCon.Parser
import ProgCon.Solve
import ProgCon.Syntax

mainCheck :: FilePath -> FilePath -> IO Int
mainCheck problemPath solutionPath = do
    problem <- loadJSON @Problem problemPath
    solution <- loadJSON @Solution solutionPath
    pure (scoreHappiness problem solution)

mainSolve :: FilePath -> IO ()
mainSolve problemPath = do
    problem <- loadJSON @Problem problemPath
    (score, solution) <- solve (takeBaseName problemPath) problem
    putStrLn $ "Score: " <> show score
    writeSolution solution

saveSolve :: FilePath -> IO ()
saveSolve problemPath = do
    sayString $ name <> ": starting..."
    prevScore <- do
        hasScore <- doesFileExist scorePath
        if hasScore
            then loadJSON @Int scorePath
            else pure minBound
    problem <- loadJSON @Problem problemPath
    (score, solution) <- solve name problem
    if score > prevScore
        then do
            sayString $ name <> ": new highscore: " <> show score
            Aeson.encodeFile scorePath score
            let resultPath = problemPath <> ".solution.json"
            Aeson.encodeFile resultPath solution
        else do
            sayString $ name <> ": score: " <> show score <> ", prev was: " <> show prevScore
  where
    name = takeBaseName problemPath
    scorePath = problemPath <> ".score"

mainRender :: FilePath -> FilePath -> IO ()
mainRender problemPath solutionPath = do
    problem <- loadJSON @Problem problemPath
    solution <- loadJSON @Solution solutionPath
    putStrLn $ "musicians: " <> show (UV.length problem.problemMusicians)
    putStrLn $ "room: " <> show (problem.problemRoomWidth, problem.problemRoomHeight)
    putStrLn $ "stage: " <> show (problem.problemStageWidth, problem.problemStageHeight)
    putStrLn $ "stagePos: " <> show problem.problemStageBottomLeft
    let score = scoreHappiness problem solution
    putStrLn $ "Score: " <> show score
    renderProblem problem solution

mainTest :: IO ()
mainTest = do
    res <- mainCheck "./problems/problem-spec.json" "./problems/solution-spec.json"
    unless (res == 5343) do
        error $ "Invalid spec score, expected 5343, got: " <> show res

main :: IO ()
main = do
    getArgs >>= \case
        [] -> mainSolve "./problems/problem-10.json"
        ["test"] -> mainTest
        ["render", problemPath, solutionPath] -> mainRender problemPath solutionPath
        ["solve", fp] -> mainSolve fp
        "save" : xs -> mapConcurrently_ saveSolve xs
        ["check", problemPath, solutionPath] -> print =<< mainCheck problemPath solutionPath
        _ -> error "usage: check pb solution | solve pb"
