module ProgCon.Submit where

import Data.Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
--import Data.Text.Lazy (toStrict)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (statusCode)
import System.Environment

import Control.Monad (when)
import Data.Foldable (traverse_)
import ProgCon.Parser (loadJSON)
import ProgCon.Syntax (Solution)
import System.Directory (doesFileExist)

submit :: Int -> Solution -> IO ()
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
    putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
    print $ responseBody response

submitOne :: Int -> IO ()
submitOne pos = do
    let fp = solutionPath pos
    hasSolution <- doesFileExist fp
    when hasSolution do
        putStrLn $ "Go " <> fp
        solution <- loadJSON @Solution fp
        submit pos solution

solutionPath :: Int -> FilePath
solutionPath pos = "./problems/problem-" <> show pos <> ".json.solution.json"

submits :: IO ()
submits = traverse_ trySubmit [1 .. 55]
  where
    skip = [30, 39, 2, 51]
    trySubmit :: Int -> IO ()
    trySubmit pos
        | pos `elem` skip = pure ()
        | otherwise = submitOne pos
