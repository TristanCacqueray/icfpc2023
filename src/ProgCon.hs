module ProgCon (main) where

import RIO
import Data.Vector.Unboxed qualified as UV
import Say
import SimpleCmdArgs
import System.Directory (doesFileExist)

import ProgCon.API (scoreBoard, userBoard)
import ProgCon.Eval
import ProgCon.GUI
import ProgCon.Parser
import ProgCon.Solve
import ProgCon.Syntax
import ProgCon.Submit

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
    <*> optional (strArg "SOLUTION")
  , Subcommand "placements" "draw fake placements" $
    mainPlacements
    <$> intArg
  , Subcommand "render" "start GUI to visualize the problem and solution" $
    mainRender
    <$> intArg
    <*> optional (strArg "SOLUTION")
  , Subcommand "test" "run unit-test" $
    pure mainTest
  , Subcommand "submit-all" "submit all solutions (this need work to check if existing solution are better)" $
    pure submitAll
  , Subcommand "userboard" "get userboard data" $
    pure userBoard
  , Subcommand "scoreboard" "get scoreboard data" $
    pure scoreBoard
  , Subcommand "list" "list problem stats" $
    listProblems
    <$> optional (flagWith' True 's' "solved" "list problems with solutions"
                  <|> flagWith' False 'u' "unsolved" "list problems without solutions")
    <*> many intArg
  ]
  where
    intArg :: Parser ProblemID
    intArg = ProblemID <$> argumentWith auto "NUM"

mainCheck :: ProblemID -> Maybe FilePath -> IO ()
mainCheck pid msolutionFP =
  runCheck pid msolutionFP >>= print

runCheck ::  ProblemID -> Maybe FilePath -> IO Int
runCheck pid msolutionFP = do
    problemDesc <- loadProblem pid
    solutionDesc <- getSolutionDesc pid msolutionFP
    solution <- toSolution solutionDesc.musicianCount solutionDesc.genPlacements
    pure (scoreHappiness problemDesc solution)

-- Nothing fails if no existing solution
getSolutionDesc :: ProblemID -> Maybe FilePath -> IO SolutionDescription
getSolutionDesc pid mfp =
  case mfp of
    Just fp -> loadSolutionPath fp
    Nothing -> do
      msd <- loadSolution False pid
      case msd of
        Just sd -> return sd
        Nothing -> error $ show pid ++ "-solution.json missing"

loadProblem :: ProblemID -> IO ProblemDescription
loadProblem pid = loadProblemPath pid (problemPath pid)

loadSolution :: Bool -> ProblemID -> IO (Maybe SolutionDescription)
loadSolution quiet pid = doesFileExist solutionFP >>= \case
  True -> do
    solutionDesc <- loadSolutionPath solutionFP
    unless quiet $
      sayString $ show pid <> ": loading " <> solutionFP <> " (score: " <> show solutionDesc.score <> ")"
    pure (Just solutionDesc)
  False -> pure Nothing
  where
    solutionFP = solutionPath pid

mainSolve :: Bool -> Maybe ProblemRenderer -> ProblemID -> IO ()
mainSolve autoSubmit renderer pid = do
    mPrevSolution <- loadSolution False pid
    problemDesc <- loadProblem pid
    let debug msg = sayString $ show problemDesc.name <> ": " <> msg

    debug $ "starting... (" <> show (UV.length problemDesc.problem.problemMusicians) <> "musicians)"

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

mainRender :: ProblemID -> Maybe FilePath -> IO ()
mainRender pid msolutionFP = withRenderer \renderer -> do
    problemDesc <- loadProblem pid
    let problem = problemDesc.problem
    solutionDesc <- getSolutionDesc pid msolutionFP
    solution <- toSolution (UV.length problem.problemMusicians) solutionDesc.genPlacements
    putStrLn $ "musicians: " <> show (UV.length problem.problemMusicians)
    putStrLn $ "audience: " <> show (length problem.problemAttendees)
    putStrLn $ "pillars: " <> show (length problem.problemPillars)
    putStrLn $ "room: " <> show (problem.problemRoomWidth, problem.problemRoomHeight)
    putStrLn $ "stage: " <> show (problem.problemStageWidth, problem.problemStageHeight)
    putStrLn $ "stagePos: " <> show problem.problemStageBottomLeft
    let score = scoreHappiness problemDesc solution
    putStrLn $ "Score: " <> show score
    renderProblem problemDesc.problem solution renderer

-- FIXME merge into check
mainTest :: IO ()
mainTest = do
    res <- runCheck SpecProblem $ Just "./problems/spec-solution.json"
    unless (res == 5343) do
        error $ "Invalid spec score, expected 5343, got: " <> show res

-- | Create one random musician for every given placements
setMaximumMusician :: UV.Vector (Int, Int) -> ProblemDescription -> ProblemDescription
setMaximumMusician placements problemDesc = problemDesc{problem}
  where
    problem = problemDesc.problem{problemMusicians = UV.generate (UV.length placements) (\pos -> pos `mod` 7)}

mainPlacements :: ProblemID -> IO ()
mainPlacements pid = withRenderer \renderer -> do
    baseProblemDesc <- loadProblem pid
    let placements = maximumPlacements problemDesc.problem
        problemDesc = setMaximumMusician placements baseProblemDesc
    putStrLn $ "total placements: " <> show (UV.length placements)
    solutionDesc <- runRandGen $ randomSolution problemDesc placements
    solution <- toSolution solutionDesc.musicianCount solutionDesc.genPlacements
    renderProblem problemDesc.problem solution renderer

-- FIXME more filtering
listProblems :: Maybe Bool -> [ProblemID] -> IO ()
listProblems msolved pids =
  (if null pids then allProblems else return pids) >>=
  mapM_ showProblem
  where
    showProblem :: ProblemID -> IO ()
    showProblem pid = do
      dispProb <-
        case msolved of
          Just solved ->
            (if solved then isJust else isNothing) <$> loadSolution True pid
          Nothing -> return True
      when dispProb $ do
        problemDesc <- loadProblem pid
        let problem = problemDesc.problem
        putStrLn $
          unwords ['#' : show pid
                  ,"musicians:" <> show (UV.length problem.problemMusicians)
                  ,"pillars:" <> show (length problem.problemPillars)
                  ,"audience:" <> show (length problem.problemAttendees)
                  ]

    -- FIXME read from problems/
    allProblems = return $ map ProblemID [1..90]
