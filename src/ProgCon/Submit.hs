module ProgCon.Submit where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (traverse_)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import System.Directory (doesFileExist)
import System.Environment
import System.Time.Extra (sleep)

import ProgCon.API (retryNetwork)
import ProgCon.Parser (loadSolutionPath)
import ProgCon.Solve (toSolution)
import ProgCon.Syntax

newtype SubmitID = SubmitID Text
    deriving newtype (FromJSON)
    deriving (Show)

submit :: ProblemID -> Solution -> IO (Maybe SubmitID)
submit pid solution = do
    let obj = object ["problem_id" .= pid, "contents" .= decodeUtf8 (BSL.toStrict $ encode solution)]
    token <- getEnv "ICFP_TOKEN"
    manager <- newTlsManager
    initialRequest <- parseRequest "https://api.icfpcontest.com/submission"
    let request =
            initialRequest
                { method = "POST"
                , requestBody = RequestBodyLBS $ encode obj
                , requestHeaders =
                    [ ("Content-Type", "application/json")
                    , ("Authorization", "Bearer " <> encodeUtf8 (pack token))
                    ]
                }
    response <- retryNetwork $ httpLbs request manager
    if statusCode (responseStatus response) == 201
        then pure $ decode (responseBody response)
        else do
            putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
            print $ responseBody response
            sleep 1 -- server often fails
            submit pid solution

getInfo :: SubmitID -> IO BS.ByteString
getInfo (SubmitID sid) = do
    token <- getEnv "ICFP_TOKEN"
    manager <- newTlsManager
    initialRequest <- parseRequest $ "https://api.icfpcontest.com/submission?submission_id=" <> unpack sid
    let request =
            initialRequest
                { method = "GET"
                , requestHeaders =
                    [ ("Content-Type", "application/json")
                    , ("Authorization", "Bearer " <> encodeUtf8 (pack token))
                    ]
                }
    response <- retryNetwork $ httpLbs request manager
    pure $ BSL.toStrict $ responseBody response

waitFor :: SubmitID -> IO ()
waitFor sid = do
    resp <- getInfo sid
    if
            | "Processing" `BS.isInfixOf` resp -> do
                putStrLn "Processing..."
                threadDelay 5_000_000
                waitFor sid
            | "Success" `BS.isInfixOf` resp -> do
                BS.putStr $ BS.take 141 resp
                BS.putStr "\n"
            | otherwise -> do
                BS.putStr "!! FAILURE: "
                BS.putStr $ BS.take 207 resp
                BS.putStr "\n"

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
