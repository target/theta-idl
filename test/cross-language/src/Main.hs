module Main where

import           Test.Tasty

import           Distribution.Make (defaultMain)
import           Test.Everything

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Cross-Lanaugage Tests"
  []
