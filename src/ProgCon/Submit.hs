{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf #-}

module ProgCon.Submit where

import Control.Concurrent (threadDelay)
import Data.Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import System.Environment

import Control.Monad (unless)
import Data.Foldable (traverse_)
import ProgCon.Parser (loadJSON)
import ProgCon.Syntax (Solution)
import System.Directory (doesFileExist)

newtype SubmitID = SubmitID Text
    deriving newtype (FromJSON)
    deriving (Show)

submit :: Int -> Solution -> IO (Maybe SubmitID)
submit problem solution = do
    let obj = object ["problem_id" .= problem, "contents" .= decodeUtf8 (BSL.toStrict $ encode solution)]
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
    response <- httpLbs request manager
    if statusCode (responseStatus response) == 201
        then pure $ decode (responseBody response)
        else do
            putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
            print $ responseBody response
            pure Nothing

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
    response <- httpLbs request manager
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

submitOne :: Bool -> Int -> IO ()
submitOne lenient pos = do
    let fp = solutionPath
    hasSolution <- doesFileExist fp
    if not hasSolution
      then unless lenient $ error $ "solution file not found: " ++ fp
      else do
        putStrLn $ "Go " <> fp
        solution <- loadJSON @Solution fp
        submit pos solution >>= \case
            Just sid -> print sid >> waitFor sid
            Nothing -> pure ()
  where
    solutionPath :: FilePath
    solutionPath = "./problems/problem-" <> show pos <> ".json.solution.json"

submitAll :: IO ()
submitAll = traverse_ trySubmit [1 .. 55]
  where
    skip = []
    trySubmit :: Int -> IO ()
    trySubmit pos
        | pos `elem` skip = pure ()
        | otherwise = submitOne True pos
