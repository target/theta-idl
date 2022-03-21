{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
module Test.Theta.Error where

import           Test.Tasty
import           Test.Tasty.HUnit     (testCase)

import           Test.Assertions      ((?=))

import           Theta.Error          (ModuleError (UndefinedType),
                                       prettyModuleError)
import           Theta.Metadata       (Metadata (..))
import           Theta.Pretty         (pr)
import           Theta.Target.Haskell (loadModule)
import           Theta.Types          (emptyModule)

loadModule "test/data/modules" "importing_foo"
loadModule "test/data/transitive" "d"

tests :: TestTree
tests = testGroup "Error"
  [ testGroup "prettyModuleError"
    [ testCase "suggest primitive type when language-version is too low" $ do
        prettyModuleError (exampleModule, UndefinedType "example.UUID") ?=
          [pr|
            Error in module ‘example’:
            The type ‘example.UUID’ is not defined.

            Suggestions:
              • UUID (primitive type): requires language-version ≥ 1.1.0
          |]

    , testCase "suggest imported types with exact name match" $ do
        prettyModuleError (theta'importing_foo, UndefinedType "importing_foo.Bar") ?=
          [pr|
            Error in module ‘importing_foo’:
            The type ‘importing_foo.Bar’ is not defined.

            Suggestions:
              • foo.Bar
          |]

    , testCase "note import for suggested type if needed" $ do
        prettyModuleError (theta'd, UndefinedType "b.B") ?=
          [pr|
            Error in module ‘d’:
            The type ‘b.B’ is not defined.

            Suggestions:
              • b.B (import b)
             |]
    ]
  ]
  where exampleModule = emptyModule "example" (Metadata "1.0.0" "1.0.0" "example")
