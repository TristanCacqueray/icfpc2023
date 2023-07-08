module ProgCon.API where

import Data.Aeson (FromJSON)
import Data.ByteString.Char8 qualified as B8
import Network.HTTP.Client (applyBearerAuth)
import Network.HTTP.Query
import Network.HTTP.Simple
import Network.HTTP.Types
import System.Environment

apiServer :: String
apiServer = "https://api.icfpcontest.com"

accessAPI :: FromJSON a => String -> Query -> (Request -> Request) -> Status
          -> IO (Maybe a)
accessAPI method params settings expected = do
  token <- getEnv "ICFP_TOKEN"
  withURLQuery (apiServer +/+ method) params $ \req -> do
    response <-
      httpJSON (applyBearerAuth (B8.pack token) $ settings req)
    if getResponseStatus response == expected
      then return $ Just $ getResponseBody response
      else do
      let status =  getResponseStatus response
      putStrLn $ "status code: " ++ show (statusCode status) ++ " " ++ B8.unpack (statusMessage status)
      --print (getResponseBody response :: Object)
      return Nothing

userBoard:: IO ()
userBoard = do
  mobj <- accessAPI "userboard" [] id status200
  case mobj of
    Nothing -> putStrLn "failed to get userboard"
    Just obj -> do
      -- FIXME combine with problems again
      case lookupKey "Success" obj >>= lookupKey "problems" of
        Just (scores :: [Maybe Int]) ->
          mapM_ renderScore (zip [(1::Int)..] scores)
        Nothing ->
          case lookupKey "Failure" obj of
            Just err -> putStrLn err
            Nothing -> putStrLn $ "failed to get userboard: " ++ show obj
  where
    renderScore (pos,mscore) =
      putStrLn $ '#' : show pos ++ ": " ++ maybe "null" show mscore
