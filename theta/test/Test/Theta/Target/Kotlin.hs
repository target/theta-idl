{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
module Test.Theta.Target.Kotlin where

import           Theta.Metadata                  (Metadata (..))
import qualified Theta.Types                     as Theta

import           Theta.Target.Haskell            (loadModule)
import           Theta.Target.Kotlin
import           Theta.Target.Kotlin.QuasiQuoter

import           Test.Tasty
import           Test.Tasty.HUnit

loadModule "test/data/modules" "newtype"
loadModule "test/data/modules" "primitives"
loadModule "test/data/modules" "importing_foo"

tests :: TestTree
tests = testGroup "Kotlin"
  [ test_isValidIdentifier

  , test_toReference
  , test_toRecord
  , test_toVariant
  , test_toModule
  ]

test_isValidIdentifier :: TestTree
test_isValidIdentifier = testGroup "isValidIdentifier"
  [ testCase "valid identifiers" $ do
      isValidIdentifier "foo"         @?= True
      isValidIdentifier "_foo"        @?= True
      isValidIdentifier "foo2"        @?= True
      isValidIdentifier "foo__3blarg" @?= True

  , testCase "invalid identifiers" $ do
      isValidIdentifier ""      @?= False
      isValidIdentifier "1_foo" @?= False
      isValidIdentifier "foo 2" @?= False
      isValidIdentifier "$foo"  @?= False
  ]

test_toReference :: TestTree
test_toReference = testGroup "toReference"
  [ testCase "primitive types" $ do
      toReference Theta.bool'   @?= [kotlin|Boolean|]
      toReference Theta.bytes'  @?= [kotlin|ByteArray|]
      toReference Theta.int'    @?= [kotlin|Int|]
      toReference Theta.long'   @?= [kotlin|Long|]
      toReference Theta.float'  @?= [kotlin|Float|]
      toReference Theta.double' @?= [kotlin|Double|]
      toReference Theta.string' @?= [kotlin|String|]

  , testCase "containers" $ do
      toReference (Theta.array' Theta.int')       @?= [kotlin|Array<Int>|]
      toReference (Theta.array' Theta.string')    @?= [kotlin|Array<String>|]
      toReference (Theta.map' Theta.int')         @?= [kotlin|HashMap<String, Int>|]
      toReference (Theta.map' Theta.string')      @?= [kotlin|HashMap<String, String>|]
      toReference (Theta.optional' Theta.int')    @?= [kotlin|Int?|]
      toReference (Theta.optional' Theta.string') @?= [kotlin|String?|]

  , testCase "nested containers" $ do
      toReference (Theta.array' (Theta.map' Theta.float')) @?=
        [kotlin|Array<HashMap<String, Float>>|]
      toReference (Theta.map' (Theta.array' Theta.float')) @?=
        [kotlin|HashMap<String, Array<Float>>|]

  , testCase "named types" $ do
      let reference = wrap $ Theta.Reference' "base.FooReference"
          record    = wrap $ Theta.Record' "base.FooRecord" []
          variant   = wrap $ Theta.Variant' "base.FooVariant" [Theta.Case "base.Foo" Nothing []]
          newtype_  = wrap $ Theta.Newtype' "base.FooNewtype" record

      toReference reference @?= [kotlin|FooReference|]
      toReference record    @?= [kotlin|FooRecord|]
      toReference variant   @?= [kotlin|FooVariant|]
      toReference newtype_  @?= [kotlin|FooNewtype|]
  ]
  where wrap = Theta.withModule' (Theta.emptyModule "base" metadata)
        metadata = Metadata "1.0.0" "1.0.0" "base"

test_toRecord :: TestTree
test_toRecord = testGroup "toRecord"
  [ testCase "empty record" $ do
      toRecord "base.Empty" [] @?= [kotlin|object Empty|]

  , testCase "simple types" $ do
      let foo = Theta.Field "foo" Nothing Theta.int'
          bar = Theta.Field "bar" Nothing Theta.string'
      toRecord "base.OneField" [foo] @?=
        [kotlin|data class OneField(val foo: Int)|]
      toRecord "base.TwoFields" [foo, bar] @?=
        [kotlin|
           data class TwoFields(
               val foo: Int,
               val bar: String
           )
        |]

  , testCase "references" $ do
      toRecord "test.Foo" [fooField] @?= [kotlin|data class Foo(val foo: Foo)|]
      toRecord "base.Bar" [fooField] @?= [kotlin|data class Bar(val foo: Foo)|]
  ]
  where reference name = Theta.withModule' module_ $ Theta.Reference' name
        module_ = Theta.Module
          { Theta.moduleName = "test_module"
          , Theta.types      = [("test_module.Foo", Theta.Definition "test_module.Foo" Nothing fooRecord)]
          , Theta.imports    = []
          , Theta.metadata   = Metadata "1.0.0" "1.0.0" "test"
          }

        fooRecord = Theta.withModule' module_ (Theta.Record' "test_module.Foo" [fooField])
        fooField  = Theta.Field "foo" Nothing (reference "test_module.Foo")

test_toVariant :: TestTree
test_toVariant = testGroup "toVariant"
  [ testCase "single case" $ do
      let cases = [ Theta.Case "test.Case" Nothing
                   [ Theta.Field "foo" Nothing Theta.int' ] ]
      toVariant "test.Variant" cases @?= [kotlin|
        sealed class Variant {
            data class Case(val foo: Int) : Variant()
        }
      |]

  , testCase "two cases" $ do
      let cases =
            [ Theta.Case "test.One" Nothing [ Theta.Field "foo" Nothing Theta.int']
            , Theta.Case "test.Two" Nothing [ Theta.Field "foo" Nothing Theta.int'
                                            , Theta.Field "bar" Nothing Theta.string']]
      toVariant "test.Variant" cases @?= [kotlin|
        sealed class Variant {
            data class One(val foo: Int) : Variant()

            data class Two(
                val foo: Int,
                val bar: String
            ) : Variant()
        }
      |]
  ]

test_toModule :: TestTree
test_toModule = testGroup "toModule"
  [ testCase "newtype.theta" $ do
      toModule [] theta'newtype @?= [kotlin|
        package newtype

        import java.time.LocalDate
        import java.time.LocalDateTime

        import kotlin.ByteArray

        import kotlin.collections.HashMap



        typealias Newtype = Int

        data class NewtypeRecord(val foo: Newtype)
      |]

  , testCase "importing_foo.theta" $ do
      toModule [] theta'importing_foo @?= [kotlin|
        package importing_foo

        import java.time.LocalDate
        import java.time.LocalDateTime

        import kotlin.ByteArray

        import kotlin.collections.HashMap

        import foo.*


      |]

  , testCase "importing_foo.theta with prefix" $ do
      toModule ["com", "example"] theta'importing_foo @?= [kotlin|
        package com.example.importing_foo

        import java.time.LocalDate
        import java.time.LocalDateTime

        import kotlin.ByteArray

        import kotlin.collections.HashMap

        import com.example.foo.*


      |]
  ]

