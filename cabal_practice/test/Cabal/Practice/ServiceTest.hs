{-# LANGUAGE TemplateHaskell #-}
module Cabal.Practice.ServiceTest where
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit

import Cabal.Practice.Service

-- |
--
tests :: Test.Framework.Test
tests = $(testGroupGenerator)

-- |
--
case_1 :: Assertion
case_1 = do 3 @=? add 1 2
