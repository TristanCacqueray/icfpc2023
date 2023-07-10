module ProgCon.Utils where

import Data.Time.Format
import Data.Time.LocalTime
import Fmt

showScore :: Int -> String
showScore s = "" +| commaizeF s |+ ""

formatLogTime :: FormatTime t => t -> String
formatLogTime = formatTime defaultTimeLocale (timeFmt defaultTimeLocale)

logLn :: String -> IO ()
logLn str = do
  now <- getZonedTime
  putStrLn $ formatLogTime now ++ " " ++ str
