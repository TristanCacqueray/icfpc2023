module ProgCon.Submit where

import Control.Concurrent (threadDelay)
import Data.Aeson
--import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, {-encodeUtf8-})
import Network.HTTP.Query
import Network.HTTP.Simple

import Network.HTTP.Types.Status --(statusCode)
--import System.Environment
-- import System.Time.Extra (sleep)

import Control.Monad (unless)
import Data.Foldable (traverse_)
import ProgCon.Parser (loadSolutionPath)
import ProgCon.Solve (toSolution)
import ProgCon.Syntax
import System.Directory (doesFileExist)

import ProgCon.API (accessAPI)

newtype SubmitID = SubmitID Text
    deriving newtype (FromJSON)
    deriving (Show)

submit :: ProblemID -> Solution -> IO (Maybe SubmitID)
submit pid solution = do
    let obj = object ["problem_id" .= pid,
                      "contents" .= decodeUtf8 (BSL.toStrict $ encode solution)]
        settings =
          setRequestMethod "POST" .
          setRequestBodyLBS (encode obj)
--    error $ show (encode obj)
    accessAPI "submission" [] settings status201

getInfo :: SubmitID -> IO (Maybe Text)
getInfo (SubmitID sid) =
  accessAPI "submission" (makeKey "submission_id" $ T.unpack sid) id status200

waitFor :: SubmitID -> IO ()
waitFor sid = do
    mresp <- getInfo sid
    case mresp of
      Just resp
            | "Processing" `T.isInfixOf` resp -> do
                putStrLn "Processing..."
                threadDelay 5_000_000
                waitFor sid
            | "Success" `T.isInfixOf` resp -> do
                T.putStrLn $ T.take 141 resp
            | otherwise -> do
                T.putStr "!! FAILURE: "
                T.putStrLn $ T.take 207 resp
      Nothing -> putStrLn $ "getInfo failed for " ++ show sid

submitOne :: Bool -> ProblemID -> IO ()
submitOne lenient pid = do
    hasSolution <- doesFileExist solutionFP
    if not hasSolution
        then unless lenient $ error $ "solution file not found: " ++ solutionFP
        else do
            putStrLn $ "Go " <> solutionFP
            solutionDesc <- loadSolutionPath solutionFP
            solution <- toSolution solutionDesc.musicianCount solutionDesc.genPlacements
            submit pid solution >>= \case
                Just sid -> print sid >> waitFor sid
                Nothing -> pure ()
  where
    solutionFP :: FilePath
    solutionFP = solutionPath pid

submitAll :: IO ()
submitAll = traverse_ trySubmit [1 .. 55]
  where
    skip = []
    trySubmit :: Int -> IO ()
    trySubmit pos
        | pos `elem` skip = pure ()
        | otherwise = submitOne True (ProblemID pos)
