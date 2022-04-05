{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
module Test.Theta.Error where

import           Control.Monad.Except  (runExceptT)

import qualified Data.Algorithm.Diff   as Diff
import qualified Data.Set              as Set

import           Test.Tasty
import           Test.Tasty.HUnit      (testCase, (@?=))

import           Theta.Test.Assertions ((?=))

import           Theta.Metadata        (Metadata (..))
import           Theta.Pretty          (pr)
import           Theta.Target.Haskell  (loadModule)
import           Theta.Types           (emptyModule)

import           Theta.Error
import           Theta.Import          (getModule)


loadModule "test/data/modules" "importing_foo"
loadModule "test/data/modules" "recursive"
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

    , testCase "local suggestion, namespace prefix optional" $ do
        prettyModuleError (theta'recursive, UndefinedType "foo.MutualA") ?=
          [pr|
            Error in module ‘recursive’:
            The type ‘foo.MutualA’ is not defined.

            Suggestions:
              • MutualA (recursive.MutualA)
          |]

    , testCase "note import for suggested type if needed" $ do
        prettyModuleError (theta'd, UndefinedType "b.B") ?=
          [pr|
            Error in module ‘d’:
            The type ‘b.B’ is not defined.

            Suggestions:
              • D (d.D)
              • b.B (import b)
              • a.A
              • c.C
             |]
    ]

  , testGroup "suggestions"
    [ testGroup "diffParts"
      [ testCase "equal" $
          diffParts 2 "foo.bar.baz" "foo.bar.baz" @?=
            [Diff.Both ["foo", "bar", "baz"] ["foo", "bar", "baz"]]

      , testCase "within 2" $
          diffParts 2 "foos.bar.baz" "foo.bares.bat" @?=
            [Diff.Both ["foos", "bar", "baz"] ["foo", "bares", "bat"]]

      , testCase "removed part" $
          diffParts 2 "foo.bar.baz" "foo.bat" @?=
            [Diff.Both ["foo", "bar"] ["foo", "bat"], Diff.First ["baz"]]

      , testCase "added part" $
          diffParts 2 "foo.bar" "foo.bat.baz" @?=
            [Diff.Both ["foo", "bar"] ["foo", "bat"], Diff.Second ["baz"]]

      , testCase "different part" $
          diffParts 2 "foo.bar.baz" "foo.bats.bat" @?=
            [Diff.Both ["foo"] ["foo"], Diff.First ["bar"], Diff.Second ["bats"], Diff.Both ["baz"] ["bat"]]
      ]

    , testGroup "otherModuleSuggestions"
      [ testCase "one option" $
          otherModuleSuggestions theta'importing_foo "importing_foo.Bar" @?= ["foo.Bar"]

      , testCase "multiple options" $ do
          importingModule <- runExceptT $ getModule "test/data/importing" "importing"
          Set.fromList (otherModuleSuggestions (unsafe importingModule) "foo.Importing") @?=
            [ "importing.Importing"
            , "direct_a.Importing"
            , "direct_b.Importing"
            , "indirect_a.Importing"
            ]
      ]

    , testGroup "similarSpellingSuggestions"
      [ testCase "added namespace part" $
          similarSpellingSuggestions 4 theta'importing_foo "foo.bar.Bar" @?= ["foo.Bar"]

      , testCase "typo in namespace" $
          similarSpellingSuggestions 4 theta'importing_foo "fobs.Bar" @?= ["foo.Bar"]

      , testCase "typo + added part" $
          similarSpellingSuggestions 4 theta'importing_foo "fobs.bar.Bar" @?= ["foo.Bar"]

      , testCase "namespace too different" $
          similarSpellingSuggestions 4 theta'importing_foo "frobles.Bar" @?= []

      , testCase "base name too different" $
          similarSpellingSuggestions 4 theta'importing_foo "foo.BargleBarg" @?= []

      , testCase "base name too different + added namespace part" $
          similarSpellingSuggestions 4 theta'importing_foo "foo.thingy.BargleBarg" @?= []
      ]
    ]
  ]
  where exampleModule = emptyModule "example" (Metadata "1.0.0" "1.0.0" "example")
