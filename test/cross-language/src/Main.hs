module Main where

import           Test.QuickCheck                 (Gen)
import           Test.Tasty

import           Test.Everything
import           Test.Primitives

import           Theta.Target.Haskell.Conversion (genTheta)
import           Theta.Target.Haskell.HasTheta   (HasTheta (theta))
import           Theta.Value                     (Value, genValue)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Cross-Lanaugage Tests"
  []
