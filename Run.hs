module Main where

import QC

main :: IO ()
main = do
  result <- runTests
  print result
