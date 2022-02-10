{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
module Test.Theta.Target.Avro.Process where


import           Control.Monad.Except            (runExceptT)


import qualified Data.Text                       as Text


import           Test.QuickCheck.Monadic         (monadicIO)
import           Test.Tasty                      (TestTree, testGroup)
import           Test.Tasty.QuickCheck           (forAll, testProperty)

import           Test.Assertions                 (assertDiff)



import           Theta.Pretty                    (Pretty (pretty))
import           Theta.Target.Avro.Process       (run)
import           Theta.Target.Haskell            (loadModule)
import           Theta.Target.Haskell.Conversion (genTheta)


loadModule "test/data/modules" "primitives"

tests :: TestTree
tests = testGroup "Process"
  [ testProperty "cat Primitives" $ forAll (genTheta @Primitives) $ \ input -> handle $ do
      output <- run "cat" [] input
      assertDiff output input
  ]
  where handle action = monadicIO $ runExceptT action >>= \case
          Left err  -> fail $ Text.unpack $ pretty err
          Right res -> pure res
