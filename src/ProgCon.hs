module ProgCon (main) where

import Control.Monad
import Data.Vector.Unboxed qualified as UV
import Say
import SimpleCmdArgs
import System.Directory (doesFileExist)

import Control.Concurrent.Async (mapConcurrently_)
import ProgCon.API
import ProgCon.Eval
import ProgCon.GUI (renderProblem)
import ProgCon.Parser
import ProgCon.Solve
import ProgCon.Syntax
import ProgCon.Submit

mainCheck :: FilePath -> FilePath -> IO ()
mainCheck problemFP solutionFP =
  runCheck problemFP solutionFP >>= print

runCheck ::  FilePath -> FilePath -> IO Int
runCheck problemFP solutionFP = do
    problem <- loadJSON @Problem problemFP
    solutionDesc <- loadSolutionPath solutionFP
    solution <- toSolution solutionDesc.musicianCount solutionDesc.genPlacements
    pure (scoreHappiness problem solution)

loadProblem :: ProblemID -> IO ProblemDescription
loadProblem pid = ProblemDescription pid <$> loadJSON @Problem (problemPath pid)

loadSolution :: ProblemID -> IO (Maybe SolutionDescription)
loadSolution pid = doesFileExist solutionFP >>= \case
  True -> do
    solutionDesc <- loadSolutionPath solutionFP
    sayString $ show pid <> ": reloading from " <> solutionFP <> " (score: " <> show solutionDesc.score
    pure (Just solutionDesc)
  False -> pure Nothing
  where
    solutionFP = solutionPath pid

mainSolve :: ProblemID -> IO ()
mainSolve pid = do
    mPrevSolution <- loadSolution pid
    problemDesc <- loadProblem pid
    let debug msg = sayString $ show problemDesc.name <> ": " <> msg

    debug $ "starting... musician count: " <> show (UV.length problemDesc.problem.problemMusicians)

    let prevScore = case mPrevSolution of
          Nothing -> minBound
          Just prevSolution -> prevSolution.score

    mSolution <- solve mPrevSolution problemDesc
    case mSolution of
      Just solution
        | solution.score > prevScore -> do
            debug $ "COMPLETED, new highscore: " <> show solution.score
            when (solution.score > 0) do
              when (prevScore > minBound) do
                debug $ "score: " ++ show prevScore ++ " -> " ++ show solution.score
              submitOne False pid
        | otherwise ->
            sayString $ show problemDesc.name <> ": done, not a highscore: " <> show solution.score <> ", prev was: " <> show prevScore
      Nothing -> sayString $ show problemDesc.name <> ": couldn't find a solution!"

mainRender :: FilePath -> FilePath -> IO ()
mainRender problemFP solutionFP = do
    problem <- loadJSON @Problem problemFP
    solutionDesc <- loadSolutionPath solutionFP
    solution <- toSolution (UV.length problem.problemMusicians) solutionDesc.genPlacements
    putStrLn $ "musicians: " <> show (UV.length problem.problemMusicians)
    putStrLn $ "room: " <> show (problem.problemRoomWidth, problem.problemRoomHeight)
    putStrLn $ "stage: " <> show (problem.problemStageWidth, problem.problemStageHeight)
    putStrLn $ "stagePos: " <> show problem.problemStageBottomLeft
    let score = scoreHappiness problem solution
    putStrLn $ "Score: " <> show score
    renderProblem problem solution

-- FIXME merge into check
mainTest :: IO ()
mainTest = do
    res <- runCheck "./problems/spec-problem.json" "./problems/spec-solution.json"
    unless (res == 5343) do
        error $ "Invalid spec score, expected 5343, got: " <> show res

main :: IO ()
main =
  simpleCmdArgs Nothing "progcon" "musical concert" $
  subcommands
  [ Subcommand "solve" "solve problem and submit if new highscore" $
    mapConcurrently_ mainSolve
    <$> some intArg
  , Subcommand "submit" "submit problem solution" $
    submitOne False
    <$> intArg
  , Subcommand "score" "compute a solution score" $
    mainCheck
    <$> strArg "PROBLEM"
    <*> strArg "SOLUTION"
  , Subcommand "render" "start GUI to visualize the problem and solution" $
    mainRender
    <$> strArg "PROBLEM"
    <*> strArg "SOLUTION"
  , Subcommand "test" "run unit-test" $
    pure mainTest
  , Subcommand "submit-all" "submit all solutions (this need work to check if existing solution are better)" $
    pure submitAll
  , Subcommand "userboard" "get userboard data" $
    pure userBoard
  ]
  where
    intArg :: Parser ProblemID
    intArg = ProblemID <$> argumentWith auto "NUM"
