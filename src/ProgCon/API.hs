module ProgCon.API where

import Data.Aeson
import Data.ByteString.Char8 qualified as B8
import Network.HTTP.Client (applyBearerAuth)
import Network.HTTP.Query
import Network.HTTP.Simple
import System.Environment

apiServer :: String
apiServer = "https://api.icfpcontest.com"

userBoard:: IO ()
userBoard = do
  token <- getEnv "ICFP_TOKEN"
  withURLQuery (apiServer +/+ "userboard") [] $ \req -> do
    eobj <- getResponseBody <$> httpJSONEither (applyBearerAuth (B8.pack token) req)
    case eobj of
      Left err -> print err
      Right (obj :: Object) -> do
        case lookupKey "Success" obj of
          Nothing ->
            case lookupKey "Failure" obj of
              Just err -> putStrLn err
              Nothing -> putStrLn $ "failed to get userboard: " ++ show obj
          Just (ps :: Object) ->
            case lookupKey "problems" ps of
              Just (scores :: [Maybe Int]) ->
                mapM_ renderScore (zip [(1::Int)..] scores)
              Nothing -> putStrLn $ "no problems: " ++ show obj
  where
    renderScore (pos,mscore) =
      putStrLn $ '#' : show pos ++ ": " ++ maybe "null" show mscore
