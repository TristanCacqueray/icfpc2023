module ProgCon (main) where

import RIO
import Data.Vector.Unboxed qualified as UV
import Say
import SimpleCmdArgs
import System.Directory (doesFileExist)

import ProgCon.API
import ProgCon.Eval
import ProgCon.GUI
import ProgCon.Parser
import ProgCon.Solve
import ProgCon.Syntax
import ProgCon.Submit

mainCheck :: ProblemID -> FilePath -> IO ()
mainCheck pid solutionFP =
  runCheck pid solutionFP >>= print

runCheck ::  ProblemID -> FilePath -> IO Int
runCheck pid solutionFP = do
    problemDesc <- loadProblem pid
    solutionDesc <- loadSolutionPath solutionFP
    solution <- toSolution solutionDesc.musicianCount solutionDesc.genPlacements
    pure (scoreHappiness problemDesc solution)

loadProblem :: ProblemID -> IO ProblemDescription
loadProblem pid = loadProblemPath pid (problemPath pid)

loadSolution :: ProblemID -> IO (Maybe SolutionDescription)
loadSolution pid = doesFileExist solutionFP >>= \case
  True -> do
    solutionDesc <- loadSolutionPath solutionFP
    sayString $ show pid <> ": reloading from " <> solutionFP <> " (score: " <> show solutionDesc.score
    pure (Just solutionDesc)
  False -> pure Nothing
  where
    solutionFP = solutionPath pid

mainSolve :: Bool -> Maybe ProblemRenderer -> ProblemID -> IO ()
mainSolve autoSubmit renderer pid = do
    mPrevSolution <- loadSolution pid
    problemDesc <- loadProblem pid
    let debug msg = sayString $ show problemDesc.name <> ": " <> msg

    debug $ "starting... musician count: " <> show (UV.length problemDesc.problem.problemMusicians)

    let prevScore = case mPrevSolution of
          Nothing -> minBound
          Just prevSolution -> prevSolution.score

    mSolution <- solve renderer mPrevSolution problemDesc
    case mSolution of
      Just solution
        | solution.score > prevScore -> do
            debug $ "COMPLETED, new highscore: " <> show solution.score
            when (solution.score > 0) do
              when (prevScore > minBound) do
                debug $ "score: " ++ show prevScore ++ " -> " ++ show solution.score
              when autoSubmit do
                submitOne False pid
        | otherwise ->
            sayString $ show problemDesc.name <> ": done, not a highscore: " <> show solution.score <> ", prev was: " <> show prevScore
      Nothing -> sayString $ show problemDesc.name <> ": couldn't find a solution!"

mainSolver :: Bool -> Bool -> [ProblemID] -> IO ()
mainSolver autoSubmit withGUI pids
  | withGUI = withRenderer \renderer -> do
       mapM_ (mainSolve autoSubmit (Just renderer)) pids
  | otherwise = mapM_ (mainSolve autoSubmit Nothing) pids

mainRender :: ProblemID -> FilePath -> IO ()
mainRender pid solutionFP = withRenderer \renderer -> do
    problemDesc <- loadProblem pid
    let problem = problemDesc.problem
    solutionDesc <- loadSolutionPath solutionFP
    solution <- toSolution (UV.length problem.problemMusicians) solutionDesc.genPlacements
    putStrLn $ "musicians: " <> show (UV.length problem.problemMusicians)
    putStrLn $ "room: " <> show (problem.problemRoomWidth, problem.problemRoomHeight)
    putStrLn $ "stage: " <> show (problem.problemStageWidth, problem.problemStageHeight)
    putStrLn $ "stagePos: " <> show problem.problemStageBottomLeft
    let score = scoreHappiness problemDesc solution
    putStrLn $ "Score: " <> show score
    renderProblem problemDesc.problem solution renderer

-- FIXME merge into check
mainTest :: IO ()
mainTest = do
    res <- runCheck SpecProblem "./problems/spec-solution.json"
    unless (res == 5343) do
        error $ "Invalid spec score, expected 5343, got: " <> show res

mainPlacements :: ProblemID -> IO ()
mainPlacements pid = withRenderer \renderer -> do
    problemDesc <- loadProblem pid
    let problem = problemDesc.problem
        placements = maximumPlacements problem
        setMaxMusician pd = pd{problem=pd.problem{problemMusicians = UV.generate (UV.length placements) (\pos -> pos `mod` 3)}}
    putStrLn $ "total placements: " <> show (UV.length placements)
    solutionDesc <- runRandGen $ randomSolution (setMaxMusician problemDesc) placements
    solution <- toSolution solutionDesc.musicianCount solutionDesc.genPlacements
    renderProblem problemDesc.problem solution renderer

main :: IO ()
main =
  simpleCmdArgs Nothing "progcon" "musical concert" $
  subcommands
  [ Subcommand "solve" "solve problem and --submit if new highscore" $
    mainSolver
    <$> switchWith 's' "submit" "auto submit when done"
    <*> switchWith 'g' "gui" "render progress"
    <*> some intArg
  , Subcommand "submit" "submit problem solution" $
    submitOne False
    <$> intArg
  , Subcommand "score" "compute a solution score" $
    mainCheck
    <$> intArg
    <*> strArg "SOLUTION"
  , Subcommand "placements" "draw fake placements" $
    mainPlacements
    <$> intArg
  , Subcommand "render" "start GUI to visualize the problem and solution" $
    mainRender
    <$> intArg
    <*> strArg "SOLUTION"
  , Subcommand "test" "run unit-test" $
    pure mainTest
  , Subcommand "submit-all" "submit all solutions (this need work to check if existing solution are better)" $
    pure submitAll
  , Subcommand "userboard" "get userboard data" $
    pure userBoard
  , Subcommand "scoreboard" "get scoreboard data" $
    pure scoreBoard
  ]
  where
    intArg :: Parser ProblemID
    intArg = ProblemID <$> argumentWith auto "NUM"
