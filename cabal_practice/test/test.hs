module Main where
import Test.Framework

import qualified Cabal.Practice.ServiceTest

-- |
--
tests :: [Test]
tests = [Cabal.Practice.ServiceTest.tests]

-- |
--
main :: IO ()
main = flip catch print $ defaultMain tests
