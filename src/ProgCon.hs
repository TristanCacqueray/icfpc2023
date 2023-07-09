module ProgCon (main) where

import RIO
import Data.List.Extra (groupSortOn)
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
import Data.List (sortOn)
import Text.Printf (printf)
import Data.Time (UTCTime, nominalDiffTimeToSeconds)
import RIO.Directory (getModificationTime)
import Data.Time.Clock (getCurrentTime, diffUTCTime)

import Control.Scheduler (Comp(Par), withScheduler_, scheduleWork_)

main :: IO ()
main =
  simpleCmdArgs Nothing "progcon" "musical concert" $
  subcommands
  [ Subcommand "solve" "solve problem and --submit if new highscore" $
    mainSolver
    <$> switchWith 'N' "no-load" "ignore the existing solution"
    <*> switchWith 's' "submit" "auto submit when done"
    <*> switchWith 'g' "gui" "render progress"
    <*> paramsArg
    <*> some intArg
  , Subcommand "driver" "try to find the next problem to solve" $
    pure mainDriver
  , Subcommand "submit" "submit problem solution" $
    submitOne False
    <$> switchWith 'r' "retry" "retry for network issues"
    <*> intArg
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
    <$> switchWith 'g' "groups" "show sizes of instrument groups"
    <*> many intArg
  ]
  where
    intArg :: Parser ProblemID
    intArg = ProblemID <$> argumentWith auto "NUM"
    paramsArg :: Parser Params
    paramsArg =
      Params
        <$> optionalWith auto 's' "seed" "COUNT" "seed count" 10
        <*> optionalWith auto 'b' "breed" "COUNT" "breed count" 10
        <*> optionalWith auto 'c' "gen" "COUNT" "number of gen" 20
        <*> optionalWith auto 'v' "volume" "COUNT" "volume mutation count" 10
        <*> optionalWith auto 'p' "placement" "COUNT" "placement mutation count" 5

mainCheck :: ProblemID -> Maybe FilePath -> IO ()
mainCheck pid msolutionFP =
  checkScore pid msolutionFP >>= putStrLn . showScore

checkScore ::  ProblemID -> Maybe FilePath -> IO Int
checkScore pid msolutionFP = do
    problemDesc <- loadProblem pid
    solutionDesc <- getSolutionDesc True pid msolutionFP
    solution <- toSolution solutionDesc.musicianCount solutionDesc.genPlacements solutionDesc.genVolumes
    let happiness = scoreHappiness problemDesc solution
    when (happiness /= solutionDesc.score) $
      putStr $ showScore solutionDesc.score <> " -> "
    pure happiness

-- Nothing fails if no existing solution
getSolutionDesc :: Bool -> ProblemID -> Maybe FilePath
                -> IO SolutionDescription
getSolutionDesc quiet pid mfp =
  case mfp of
    Just fp -> loadSolutionPath fp
    Nothing -> do
      msd <- loadSolution quiet pid
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
      sayString $ show pid <> ": loading " <> solutionFP <> " (score: " <> showScore solutionDesc.score <> ")"
    pure (Just solutionDesc)
  False -> pure Nothing
  where
    solutionFP = solutionPath pid

mainSolver :: Bool -> Bool -> Bool -> Params -> [ProblemID] -> IO ()
mainSolver ignoreSoln autoSubmit withGUI params pids
  | withGUI = withRenderer \renderer -> do
       mapM_ (mainSolve ignoreSoln autoSubmit (Just renderer) params) pids
  | otherwise = mapM_ (mainSolve ignoreSoln autoSubmit Nothing params) pids

mainSolve :: Bool -> Bool -> Maybe ProblemRenderer -> Params -> ProblemID -> IO ()
mainSolve ignoreSoln autoSubmit renderer params pid = do
    start_time <- getCurrentTime
    mPrevSolution <-
      if ignoreSoln then return Nothing else loadSolution False pid
    problemDesc <- loadProblem pid
    let debug msg = sayString $ "#" <> show problemDesc.name <> ": " <> msg

    debug $ "START (" <>
      show (UV.length problemDesc.problem.problemMusicians) <> " musicians, " <>
      show (UV.length problemDesc.pillars) <> " pillars, " <>
      show (length problemDesc.problem.problemAttendees) <> " attendees) " <>
            "params: " <> show params

    let prevScore = case mPrevSolution of
          Nothing -> minBound
          Just prevSolution -> prevSolution.score

    mSolution <- solve params renderer mPrevSolution problemDesc
    end_time <- getCurrentTime
    debug $ "ran for " <> show @Int (truncate (nominalDiffTimeToSeconds $ diffUTCTime end_time start_time)) <> " seconds"
    case mSolution of
      Just solution
        | solution.score > prevScore -> do
            debug $ "COMPLETED, new highscore: " <> showScore solution.score
            when (solution.score > 0) do
              when (prevScore > minBound) do
                debug $ "score: " ++ showScore prevScore ++ " -> " ++ showScore solution.score
              when autoSubmit do
                submitOne False False pid
        | otherwise ->
            sayString $ show problemDesc.name <> ": done, not a highscore: " <> showScore solution.score <> ", prev was: " <> showScore prevScore
      Nothing -> sayString $ show problemDesc.name <> ": couldn't find a solution!"

mainRender :: ProblemID -> Maybe FilePath -> IO ()
mainRender pid msolutionFP = withRenderer \renderer -> do
    problemDesc <- loadProblem pid
    let problem = problemDesc.problem
    solutionDesc <- getSolutionDesc False pid msolutionFP
    solution <- toSolution (UV.length problem.problemMusicians) solutionDesc.genPlacements solutionDesc.genVolumes
    putStrLn $ "musicians: " <> show (UV.length problem.problemMusicians)
    putStrLn $ "audience: " <> show (length problem.problemAttendees)
    putStrLn $ "pillars: " <> show (length problem.problemPillars)
    putStrLn $ "room: " <> show (problem.problemRoomWidth, problem.problemRoomHeight)
    putStrLn $ "stage: " <> show (problem.problemStageWidth, problem.problemStageHeight)
    putStrLn $ "stagePos: " <> show problem.problemStageBottomLeft
    let score = scoreHappiness problemDesc solution
    putStrLn $ "Score: " <> showScore score
    renderProblem problemDesc.problem solution renderer

-- FIXME merge into check
mainTest :: IO ()
mainTest = do
    res <- checkScore SpecProblem $ Just "./problems/spec-solution.json"
    if res == 3270
      then putStrLn "SUCCESS!"
      else error $ "Invalid spec score, expected 3270, got: " <> show res

-- | Create one random musician for every given placements
setMaximumMusician :: UV.Vector (Int, Int) -> ProblemDescription -> ProblemDescription
setMaximumMusician placements problemDesc = problemDesc{problem}
  where
    problem = problemDesc.problem{problemMusicians = UV.generate (UV.length placements) (`mod` 7)}

mainPlacements :: ProblemID -> IO ()
mainPlacements pid = withRenderer \renderer -> do
    baseProblemDesc <- loadProblem pid
    let placements = maximumPlacements problemDesc.problem
        problemDesc = setMaximumMusician placements baseProblemDesc
    putStrLn $ "total placements: " <> show (UV.length placements)
    solutionDesc <- runRandGen $ randomSolution problemDesc placements
    solution <- fromSolutionDesc solutionDesc
    renderProblem problemDesc.problem solution renderer

mainDriver :: IO ()
mainDriver = withScheduler_ Par \scheduler -> do
  solutions <- sortProblemByScore
  now <- getCurrentTime
  forM_ solutions \(pid, time, solution) -> scheduleWork_ scheduler do
    let
      ageSec :: Integer
      ageSec = truncate (nominalDiffTimeToSeconds $ diffUTCTime now time) `div` 60
    putStrLn (printf "%13s - %3s minutes old - problem %02s" (showScore solution.score) (show ageSec) (show pid))
    problem <- loadProblem pid
    start_time <- getCurrentTime
    runRandGen $ mainImprove problem start_time solution 0

mainImprove :: ProblemDescription -> UTCTime -> SolutionDescription -> Int -> RandGen ()
mainImprove problemDesc start_time solutionDesc idx = do
  mSolution <- tryImprove problemDesc solutionDesc (toEnum (idx `mod` 3))
  (newTime, newSolution) <- case mSolution of
    Nothing -> pure (start_time, solutionDesc)
    Just sd -> liftIO do
      sayString $ show problemDesc.name <> ": new highscore: " <> showScore solutionDesc.score <> " -> " <> showScore sd.score <> ", saving..."
      saveSolutionPath sd (solutionPath problemDesc.name)
      now <- getCurrentTime
      pure (now, sd)

  end_time <- liftIO getCurrentTime
  let elapsed = nominalDiffTimeToSeconds (diffUTCTime end_time start_time)
  when (elapsed < max_time) do
    mainImprove problemDesc newTime newSolution (idx + 1)
 where
   max_time = 30

sortProblemByScore :: IO [(ProblemID, UTCTime, SolutionDescription)]
sortProblemByScore = do
  allSolutions <- traverse (loadSolutionPath . solutionPath) allProblems
  allTimes <- traverse (getModificationTime . solutionPath) allProblems
  pure $ sortOn (\(_pid, _time, s) -> s.score) (zip3 allProblems allTimes allSolutions)

allProblems :: [ProblemID]
allProblems = filter (/= 38) [1..90]

-- FIXME more filtering
listProblems :: Bool -> [ProblemID] -> IO ()
listProblems groups pids =
  mapM_ showProblem (if null pids then allProblems else pids)
  where
    showProblem :: ProblemID -> IO ()
    showProblem pid = do
      mSolutionDesc <- loadSolution True pid
      let score = case mSolutionDesc of
            Nothing -> 0
            Just solutionDesc -> solutionDesc.score
      do
        problemDesc <- loadProblem pid
        let problem = problemDesc.problem
            musicians = problem.problemMusicians
        putStrLn $
          unwords $
          ['#' : show pid
          ,"audience:" <> show (length problem.problemAttendees)
          ,"pillars:" <> show (length problem.problemPillars)
          ,"musicians:" <> show (UV.length musicians)
          ,"score:" <> showScore score
          ]
          ++
          ["instruments:" <>
            let instrmts = (map length . groupSortOn id . UV.toList) musicians
            in if all (==1) instrmts
               then "unique" -- was "solos"
               else show instrmts
          | groups
          ]
