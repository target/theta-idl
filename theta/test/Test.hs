module Main where

import           Test.Tasty

import qualified Test.Theta.Import                    as Import
import qualified Test.Theta.Name                      as Name
import qualified Test.Theta.Parser                    as Parser
import qualified Test.Theta.Types                     as Types
import qualified Test.Theta.Versions                  as Versions

import qualified Test.Theta.Target.Avro.Types         as Avro.Types
import qualified Test.Theta.Target.Avro.Values        as Avro.Values

import qualified Test.Theta.Target.Haskell            as Haskell
import qualified Test.Theta.Target.Haskell.Conversion as Haskell.Conversion

import qualified Test.Theta.Target.Kotlin             as Kotlin

import qualified Test.Theta.Target.Python             as Python
import qualified Test.Theta.Target.Python.QuasiQuoter as Python.QuasiQuoter

import qualified Test.Theta.Target.Rust               as Rust

tests :: TestTree
tests = testGroup "Theta"
  [ Import.tests
  , Name.tests
  , Parser.tests
  , Types.tests
  , Versions.tests

  , testGroup "Target"
    [ testGroup "Avro"
      [ Avro.Values.tests
      , Avro.Types.tests
      ]

    , Haskell.tests
    , Haskell.Conversion.tests

    , Kotlin.tests

    , Python.tests
    , Python.QuasiQuoter.tests

    , Rust.tests
    ]
  ]

main :: IO ()
main = defaultMain tests


