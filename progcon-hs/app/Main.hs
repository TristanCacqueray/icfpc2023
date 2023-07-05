-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import qualified ProgCon (someFunc)

main :: IO ()
main = do
  ProgCon.someFunc
