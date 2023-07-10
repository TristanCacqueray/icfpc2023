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
import SimpleCmd.Git qualified

import ProgCon.API (retryGET, retryPOST)
import ProgCon.Parser (loadSolutionPath)
import ProgCon.Solve (fromSolutionDesc)
import ProgCon.Syntax

newtype SubmitID = SubmitID Text
    deriving newtype (FromJSON)
    deriving (Show)

submit :: Bool -> ProblemID -> Solution -> IO (Maybe SubmitID)
submit resubmit pid solution = do
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
    response <- (if resubmit then retryPOST else id) $ httpLbs request manager
    if statusCode (responseStatus response) == 201
        then pure $ decode (responseBody response)
        else do
            putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
            print $ responseBody response
            return Nothing

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
    response <- retryGET $ httpLbs request manager
    pure $ BSL.toStrict $ responseBody response

waitFor :: ProblemID -> SubmitID -> IO ()
waitFor pid sid = do
    resp <- getInfo sid
    if
            | "Processing" `BS.isInfixOf` resp -> do
                putChar '.'
                threadDelay 5_000_000
                waitFor pid sid
            | "Success" `BS.isInfixOf` resp -> do
                BS.putStr "\n"
                BS.putStr $ BS.take 141 resp
                BS.putStr "\n"
            | otherwise -> do
                BS.putStr "\n"
                BS.putStr "!! FAILURE: "
                BS.putStr $ BS.take 207 resp
                BS.putStr "\n"

submitOne :: Bool -> Bool -> ProblemID -> IO ()
submitOne lenient resubmit pid = do
    hasSolution <- doesFileExist solutionFP
    if not hasSolution
        then unless lenient $ error $ "solution file not found: " ++ solutionFP
        else do
            putStrLn $ "Submitting #" <> show pid
            solutionDesc <- loadSolutionPath solutionFP
            SimpleCmd.Git.git_ "add" [solutionFP]
            solution <- fromSolutionDesc solutionDesc
            submit resubmit pid solution >>= \case
                Just sid -> do
                  print sid >> putStr "Processing" >> waitFor pid sid
                  SimpleCmd.Git.git_ "commit" ["-m", "updated solution for " <> show pid, solutionFP]
                  -- SimpleCmd.Git.git_ "push" ["origin", "HEAD:tristan-driver"]
                Nothing -> pure ()
  where
    solutionFP :: FilePath
    solutionFP = solutionPath pid

submitAll :: IO ()
submitAll = traverse_ trySubmit [1 .. 90]
  where
    skip = []
    trySubmit :: Int -> IO ()
    trySubmit pos
        | pos `elem` skip = pure ()
        | otherwise = submitOne False True (ProblemID pos)
