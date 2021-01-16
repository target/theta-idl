{-# LANGUAGE OverloadedStrings #-}

module Test.Theta.Versions where

import           Theta.Versions

import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Versions" [ test_inRange ]


test_inRange :: TestTree
test_inRange = testProperty "inRange" $ \ version range ->
  inRange range version == (version >= lower range && -- inclusive inner
                            version <  upper range)   -- exclusive outer
