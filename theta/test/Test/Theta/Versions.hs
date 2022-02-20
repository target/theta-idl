{-# LANGUAGE OverloadedStrings #-}

module Test.Theta.Versions where

import           Theta.Versions

import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (testCase, (@?=))
import           Test.Tasty.QuickCheck (testProperty)

import           Paths_theta           (version)

tests :: TestTree
tests = testGroup "Versions"
  [ test_inRange
  , test_packageVersion
  ]

test_inRange :: TestTree
test_inRange = testProperty "inRange" $ \ version range ->
  inRange range version == (version >= lower range && -- inclusive inner
                            version <  upper range)   -- exclusive outer

test_packageVersion :: TestTree
test_packageVersion = testCase "packageVersion" $ do
  packageVersion @?= Paths_theta.version
