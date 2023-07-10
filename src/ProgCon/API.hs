module ProgCon.API where

import RIO (when)
import Control.Retry
import Data.Aeson (FromJSON, Object)
import Data.ByteString.Char8 qualified as B8
import Network.HTTP.Client (applyBearerAuth)
import Network.HTTP.Query
import Network.HTTP.Simple
import Network.HTTP.Types
import System.Environment

import ProgCon.Utils (logLn, showScore)

apiServer :: String
apiServer = "https://api.icfpcontest.com"

accessAPI :: FromJSON a => String -> Query -> (Request -> Request) -> Status
          -> IO (Maybe a)
accessAPI method params settings expected = do
  token <- getEnv "ICFP_TOKEN"
  withURLQuery (apiServer +/+ method) params $ \req -> do
    response <-
      retryGET $ httpJSON (applyBearerAuth (B8.pack token) $ settings req)
    if getResponseStatus response == expected
      then return $ Just $ getResponseBody response
      else do
      let status =  getResponseStatus response
      putStrLn $ "status code: " ++ show (statusCode status) ++ " " ++ B8.unpack (statusMessage status)
      --print (getResponseBody response :: Object)
      return Nothing

retryGET :: IO (Response a) -> IO (Response a)
retryGET act =
  recoverAll retrypolicy $ \rs ->
  when (rsIterNumber rs > 0) (logLn "retrying GET") >> act
 where
    retrypolicy = exponentialBackoff 750_000 <> limitRetries 10

retryPOST :: IO (Response a) -> IO (Response a)
retryPOST act =
  recoverAll retrypolicy $ \rs ->
  when (rsIterNumber rs > 0) (logLn "retrying POST") >> act
 where
    retrypolicy = exponentialBackoff 5_000_000 <> limitRetries 3

userBoard:: IO ()
userBoard = do
  mobj <- accessAPI "userboard" [] id status200
  case mobj of
    Nothing -> putStrLn "failed to get userboard"
    Just obj -> do
      case lookupKey "Success" obj >>= lookupKey "problems" of
        Just (scores :: [Maybe Int]) ->
          mapM_ renderScore (zip [(1::Int)..] scores)
        Nothing ->
          case lookupKey "Failure" obj of
            Just err -> putStrLn err
            Nothing -> putStrLn $ "failed to get userboard: " ++ show obj
  where
    renderScore (pos,mscore) =
      putStrLn $ '#' : show pos ++ ": " ++ maybe "null" showScore mscore

scoreBoard :: IO ()
scoreBoard = do
  mobj <- accessAPI "scoreboard" [] id status200
  case mobj of
    Nothing -> putStrLn "failed to get scoreboard"
    Just obj -> do
      case lookupKey "frozen" obj of
        Just True -> putStrLn "frozen"
        _ -> return ()
      case lookupKey "scoreboard" obj of
        Nothing -> putStrLn "no scoreboard data"
        Just (arr :: [Object]) ->
          mapM_ printScore $ zip [1..] arr
  where
    ranking :: Object -> Maybe (Int,String)
    ranking obj = do
      score <- lookupKey "score" obj
      name <- lookupKey "username" obj
      return (score,name)

    printScore (i :: Int, obj) =
      case ranking obj of
        Nothing -> return ()
        Just (score,name) ->
          putStrLn $ show i ++ ". " ++ showScore score ++ " " ++ name
