-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import qualified OurLib (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  OurLib.someFunc
