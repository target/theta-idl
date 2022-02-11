{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import qualified Data.Text                       as Text

import           Control.Monad.Except            (runExceptT)

import           Test.QuickCheck                 (Gen, forAll)
import           Test.QuickCheck.Monadic         (monadicIO)
import           Test.Tasty
import           Test.Tasty.QuickCheck           (testProperty)

import           Test.Everything
import           Test.Primitives

import           Theta.Pretty                    (pretty)
import           Theta.Target.Avro.Process       (run)
import           Theta.Target.Haskell.Conversion (genTheta)
import           Theta.Target.Haskell.HasTheta   (HasTheta (theta))
import           Theta.Value                     (Value, genValue)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Cross-Lanaugage Tests"
  [ testProperty "[Primitives] Haskell ⇔ Rust" $
      forAll (genTheta @[Primitives]) $ \ inputs -> handle $ do
        outputs <- run "theta_rust_test" [] inputs
        pure $ outputs == inputs
  ]
  where handle action = monadicIO $ runExceptT action >>= \case
          Left err  -> fail $ Text.unpack $ pretty err
          Right res -> pure res
