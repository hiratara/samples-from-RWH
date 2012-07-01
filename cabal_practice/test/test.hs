module Main where
import Test.Framework

-- |
--
tests :: [Test]
tests = []

-- |
--
main :: IO ()
main = flip catch print $ defaultMain tests
