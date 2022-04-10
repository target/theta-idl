{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
module Test.Theta.Target.Avro.Process where


import           Control.Monad                   (forM_)
import           Control.Monad.Except            (MonadIO (liftIO), runExceptT)

import qualified Data.Text                       as Text

import           Test.QuickCheck.Monadic         (monadicIO)
import           Test.Tasty                      (TestTree, testGroup)
import           Test.Tasty.QuickCheck           (forAll, listOf, testProperty)

import qualified Streamly.Prelude                as Streamly

import           Theta.Test.Assertions           (assertDiff)

import           Theta.Pretty                    (Pretty (pretty))
import           Theta.Target.Avro.Process       (run, stream)
import           Theta.Target.Haskell            (loadModule)
import           Theta.Target.Haskell.Conversion (genTheta)


loadModule "test/data/modules" "primitives"

tests :: TestTree
tests = testGroup "Process"
  [ testGroup "run"
    [ testProperty "cat Primitives" $ forAll primitives $ \ input -> handle $ do
        output <- run "cat" [] input
        assertDiff output input
    ]

  , testGroup "stream"
    [ testProperty "cat Primitives" $ forAll (listOf primitives) $ \ inputs -> monadicIO $ do
        outputs <- liftIO $ Streamly.toList $ stream "cat" [] $ Streamly.fromList inputs
        forM_ (outputs `zip` inputs) $ \ (output, input) ->
          assertDiff output input
    ]
  ]
  where primitives = genTheta @Primitives

        handle action = monadicIO $ runExceptT action >>= \case
          Left err  -> fail $ Text.unpack $ pretty err
          Right res -> pure res
