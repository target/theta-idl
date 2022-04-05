{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import qualified Data.Text                       as Text

import           Control.Monad                   (when)
import           Control.Monad.Except            (runExceptT)
import           Control.Monad.IO.Class          (liftIO)

import           Data.Int                        (Int32)

import           Text.Printf                     (printf)

import           Test.QuickCheck                 (Arbitrary (arbitrary), Gen,
                                                  forAll, getSize)
import           Test.QuickCheck.Monadic         (monadicIO)
import           Test.Tasty
import           Test.Tasty.QuickCheck           (testProperty)

import           Test.Everything
import           Test.Primitives

import           Theta.Pretty                    (pretty)
import           Theta.Target.Avro.Process       (run)
import           Theta.Target.Haskell.Conversion (genTheta', toTheta)
import           Theta.Target.Haskell.HasTheta   (HasTheta (theta))
import           Theta.Test.Assertions           (assertDiff, assertFirstDiff)
import           Theta.Value                     (Value, genValue)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Cross-Lanaugage Tests"
  [ testProperty "Haskell ⇔ Rust" $
      forAll everything $ \ inputs -> handle $ do
        outputs <- run "cat_everything_rust" [] inputs
        assertFirstDiff outputs inputs

  , testProperty "Haskell ⇔ Python" $
      forAll everything $ \ inputs -> handle $ do
        outputs <- run "cat_everything_python" [] inputs
        assertFirstDiff outputs inputs
  ]
  where handle action = monadicIO $ runExceptT action >>= \case
          Left err  -> fail $ Text.unpack $ pretty err
          Right res -> pure res

        everything = genTheta' @[Everything] [("everything.List", toList <$> arbitrary)]
        toList = toTheta . foldr @[] Cons Nil
