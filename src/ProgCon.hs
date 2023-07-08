module ProgCon (main) where

import Control.Monad
import Data.Aeson qualified as Aeson
import Data.Vector.Unboxed qualified as UV
import Say
import SimpleCmdArgs
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName)

import Control.Concurrent.Async (mapConcurrently_)
import ProgCon.Eval
--import ProgCon.GUI
import ProgCon.Parser
import ProgCon.Solve
import ProgCon.Syntax
import ProgCon.Submit

mainCheck :: FilePath -> FilePath -> IO ()
mainCheck problemPath solutionPath =
  runCheck problemPath solutionPath >>= print

runCheck ::  FilePath -> FilePath -> IO Int
runCheck problemPath solutionPath = do
    problem <- loadJSON @Problem problemPath
    solution <- loadJSON @Solution solutionPath
    pure (scoreHappiness problem solution)

mainSolve :: FilePath -> IO ()
mainSolve problemPath = do
    problem <- loadJSON @Problem problemPath
    (score, solution) <- solve Nothing desc problem
    putStrLn $ "Score: " <> show score
    writeSolution solution
  where
    desc = ProblemDescription (takeBaseName problemPath) Nothing

saveSolve :: FilePath -> IO ()
saveSolve problemPath = do
    prevScore <- do
        hasScore <- doesFileExist scorePath
        if hasScore
            then loadJSON @Int scorePath
            else pure minBound
    problem <- loadJSON @Problem problemPath
    sayString $ desc.name <> ": starting... musician count: " <> show (UV.length problem.problemMusicians)

    prevSolution <- do
        hasSolution <- doesFileExist solutionPath
        if hasSolution
            then do
                solution <- loadJSON @Solution solutionPath
                sayString $ desc.name <> ": reloading from " <> solutionPath <> " (score: " <> show prevScore
                pure (Just (prevScore, solution))
            else pure Nothing

    (score, solution) <- solve prevSolution desc problem
    if score > prevScore
        then do
            sayString $ desc.name <> ": COMPLETED, new highscore: " <> show score
            Aeson.encodeFile scorePath score
            Aeson.encodeFile solutionPath solution
        else do
            sayString $ desc.name <> ": done, not a highscore: " <> show score <> ", prev was: " <> show prevScore
  where
    scorePath = problemPath <> ".score"
    solutionPath = problemPath <> ".solution.json"
    desc =
        ProblemDescription
            { name = takeBaseName problemPath
            , problemPaths = Just (scorePath, solutionPath)
            }

-- mainRender :: FilePath -> FilePath -> IO ()
-- mainRender problemPath solutionPath = do
--     problem <- loadJSON @Problem problemPath
--     solution <- loadJSON @Solution solutionPath
--     putStrLn $ "musicians: " <> show (UV.length problem.problemMusicians)
--     putStrLn $ "room: " <> show (problem.problemRoomWidth, problem.problemRoomHeight)
--     putStrLn $ "stage: " <> show (problem.problemStageWidth, problem.problemStageHeight)
--     putStrLn $ "stagePos: " <> show problem.problemStageBottomLeft
--     let score = scoreHappiness problem solution
--     putStrLn $ "Score: " <> show score
--     renderProblem problem solution

-- FIXME merge into check
mainTest :: IO ()
mainTest = do
    res <- runCheck "./problems/problem-spec.json" "./problems/solution-spec.json"
    unless (res == 5343) do
        error $ "Invalid spec score, expected 5343, got: " <> show res

main :: IO ()
main =
  simpleCmdArgs Nothing "progcon" "musical concert" $
  subcommands
  [ Subcommand "solve" "solve problem" $
    mainSolve
    <$> strArg "FILE"
  , Subcommand "save" "genetic solve and saving problems" $
    mapConcurrently_ saveSolve
    <$> some (strArg "FILE")
  , Subcommand "check" "check problem solution" $
    mainCheck
    <$> strArg "PROBLEM"
    <*> strArg "SOLUTION"
  -- , Subcommand "render" "show problem" $
  --   mainRender
  --   <$> strArg "PROBLEM"
  --   <*> strArg "SOLUTION"
  , Subcommand "test" "test spec problem solution" $
    pure mainTest
  , Subcommand "submit" "submit problem solution" $
    submitOne
    <$> argumentWith auto "NUM"
  , Subcommand "submit-all" "submit all solutions" $
    pure submitAll
  ]
      --  [] -> mainSolve "./problems/problem-10.json"
--        ["submit", n] -> submitOne (read n)
--        ["submits"] -> submitAll
--        ["test"] -> mainTest
--        ["render", problemPath, solutionPath] -> mainRender problemPath solutionPath
      --  ["solve", fp] -> mainSolve fp
      --  "save" : xs -> mapConcurrently_ saveSolve xs
      --   ["check", problemPath, solutionPath] -> print =<< mainCheck problemPath solutionPath
--        _ -> error "usage: check pb solution | solve pb"
