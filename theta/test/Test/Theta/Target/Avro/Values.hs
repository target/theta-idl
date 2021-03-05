{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumDecimals           #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
module Test.Theta.Target.Avro.Values where

import           Control.Monad.Except            (runExceptT)

import qualified Data.Avro.Schema                as Schema
import qualified Data.Avro.Types                 as Avro
import qualified Data.HashMap.Strict             as HashMap
import           Data.Semigroup                  ((<>))
import qualified Data.Text                       as Text
import qualified Data.Time.Calendar              as Time
import qualified Data.Time.Clock                 as Time

import           Theta.Metadata                  (Metadata (..))
import qualified Theta.Pretty                    as Theta
import qualified Theta.Target.Avro.Values        as Theta
import qualified Theta.Target.Haskell            as Theta
import qualified Theta.Target.Haskell.Conversion as Theta
import qualified Theta.Target.Haskell.HasTheta   as Theta
import qualified Theta.Types                     as Theta
import qualified Theta.Value                     as Theta

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

Theta.loadModule "test/data/modules" "recursive"

tests :: TestTree
tests = testGroup "Values"
  [ test_toAvro
  , test_fromAvro
  , test_recursive

  , test_date
  , test_datetime
  ]

-- | Testing to/from Avro logic using the realistic example in
-- @replenishment_simulation_input.theta@. This type stresses a lot of
-- Theta functionality, including recursive types.
--
-- This tests that we can go back and forth between Avro and Theta
-- without losing information or having any errors.
test_recursive :: TestTree
test_recursive = testCase "recursive" $
  case Theta.fromAvro schema =<< Theta.toAvro value of
    Left err  -> assertFailure $ Text.unpack $ Theta.pretty err
    Right res -> res @?= value
  where schema = Theta.theta @Recursive
        value = Theta.toTheta (Recurse 10 boxed unboxed)
        boxed = Recurse 10 Nil HashMap.empty
        unboxed = HashMap.empty

test_date :: TestTree
test_date = testGroup "Date functions"
  [ testCase "toDay" $ do
      Theta.toDay 0       @?= Time.fromGregorian 1970 01 01
      Theta.toDay (-1000) @?= Time.fromGregorian 1967 04 07
      Theta.toDay 17938   @?= Time.fromGregorian 2019 02 11
  , testCase "fromDay" $ do
      Theta.fromDay (Time.fromGregorian 1970 01 01) @?= 0
      Theta.fromDay (Time.fromGregorian 1967 04 07) @?= (-1000)
      Theta.fromDay (Time.fromGregorian 2019 02 11) @?= 17938

  , testProperty "toDay ∘ fromDay = id" $ \ n ->
      let day = Time.ModifiedJulianDay n in
      Theta.toDay (Theta.fromDay day) == day
  , testProperty "fromDay ∘ toDay = id" $ \ n ->
      Theta.fromDay (Theta.toDay n) == n
  ]

test_datetime :: TestTree
test_datetime = testGroup "Datetime functions"
  [ testCase "toUTCTime" $ do
      Theta.toUTCTime 0            @?= read "1970-01-01 00:00:00 UTC"
      Theta.toUTCTime (hours (-3)) @?= read "1969-12-31 21:00:00 UTC"
      Theta.toUTCTime (hours 3)    @?= read "1970-01-01 03:00:00 UTC"
  , testCase "fromUTCTime" $ do
      Theta.fromUTCTime (read "1970-01-01 00:00:00 UTC") @?= 0
      Theta.fromUTCTime (read "1969-12-31 21:00:00 UTC") @?= hours (-3)
      Theta.fromUTCTime (read "1970-01-01 03:00:00 UTC") @?= hours 3

  , testProperty "toUTCTime ∘ fromUTCTime = id" $ \ day ->
      forAll (choose (0, 24 * 60 * 60)) $ \ offset ->
      let time = Time.UTCTime (Time.ModifiedJulianDay day) (fromInteger offset) in
      Theta.toUTCTime (Theta.fromUTCTime time) == time
  , testProperty "fromUTCTime ∘ toUTCTime = id" $ \ n ->
      Theta.fromUTCTime (Theta.toUTCTime n) == n
  ]
  where hours n = n * 60 * 60 * 1e6 -- hours to microseconds

test_toAvro :: TestTree
test_toAvro = testGroup "toAvro"
  [ testCase "primitive types" $ check primitives
  , testCase "containers" $ check containers
  , testCase "records" $ check records
  , testCase "variants" $ check variants
  ]
  where check = mapM_ $ \ (value, _, avro) -> do
          converted <- runExceptT $ Theta.toAvro value
          case converted of
            Right converted -> converted @?= avro
            Left err        ->
              assertFailure $ "Encountered error in toAvro:\n" <> show err

test_fromAvro :: TestTree
test_fromAvro = testGroup "fromAvro"
  [ testCase "primitive types" $ check primitives
  , testCase "containers" $ check containers
  , testCase "records" $ check records
  , testCase "variants" $ check variants
  ]
  where check = mapM_ $ \ (theta, type_, avro) -> do
          converted <- runExceptT $ Theta.fromAvro type_ avro
          case converted of
            Right converted -> converted @?= theta
            Left err        ->
              assertFailure $ "Encountered error in fromAvro:\n" <> show err

-- Theta and Avro values that should translate into each other

primitives :: [(Theta.Value, Theta.Type, Avro.Value Schema.Schema)]
primitives =
  [ (Theta.boolean True,  Theta.bool', Avro.Boolean True)
  , (Theta.boolean False, Theta.bool', Avro.Boolean False)

  , (Theta.bytes "",      Theta.bytes', Avro.Bytes "")
  , (Theta.bytes "blarg", Theta.bytes', Avro.Bytes "blarg")

  , (Theta.int (-1),     Theta.int', Avro.Int (-1))
  , (Theta.int 0,        Theta.int', Avro.Int 0)
  , (Theta.int 42,       Theta.int', Avro.Int 42)
  , (Theta.int minBound, Theta.int', Avro.Int minBound)
  , (Theta.int maxBound, Theta.int', Avro.Int maxBound)

  , (Theta.long (-1),     Theta.long', Avro.Long (-1))
  , (Theta.long 0,        Theta.long', Avro.Long 0)
  , (Theta.long 42,       Theta.long', Avro.Long 42)
  , (Theta.long minBound, Theta.long', Avro.Long minBound)
  , (Theta.long maxBound, Theta.long', Avro.Long maxBound)

  , (Theta.float (-1),  Theta.float', Avro.Float (-1))
  , (Theta.float 0,     Theta.float', Avro.Float 0)
  , (Theta.float 42,    Theta.float', Avro.Float 42)
  , (Theta.float pi,    Theta.float', Avro.Float pi)
  , (Theta.float (1/0), Theta.float',  Avro.Float (1/0))

  , (Theta.double (-1),  Theta.double', Avro.Double (-1))
  , (Theta.double 0,     Theta.double', Avro.Double 0)
  , (Theta.double 42,    Theta.double', Avro.Double 42)
  , (Theta.double pi,    Theta.double', Avro.Double pi)
  , (Theta.double (1/0), Theta.double', Avro.Double (1/0))

  , (Theta.string "",      Theta.string', Avro.String "")
  , (Theta.string "blarg", Theta.string', Avro.String "blarg")

  , (Theta.date $ read "2019-02-11", Theta.date', Avro.Int 17938 )

  , (Theta.datetime datetime, Theta.datetime', Avro.Long 1549894992000000)
  ]
  where datetime = read "2019-02-11 14:23:12 UTC"

containers :: [(Theta.Value, Theta.Type, Avro.Value Schema.Schema)]
containers =
  [ -- arrays
    (thetaArray Theta.int' [], Theta.array' Theta.int', Avro.Array [])
  , (thetaArray Theta.int' $ Theta.int <$> [0..10], Theta.array' Theta.int',
     Avro.Array $ Avro.Int <$> [0..10])

    -- maps
  , (thetaMap Theta.int' [], Theta.map' Theta.int', Avro.Map [])
  , (thetaMap Theta.int' [("abc", Theta.int 10), ("def", Theta.int 20)], Theta.map' Theta.int',
     Avro.Map [("abc", Avro.Int 10), ("def", Avro.Int 20)])

    -- normal optional types
  , (optional Theta.int' Nothing, Theta.optional' Theta.int',
     Avro.Union [Schema.Null, Schema.Int] Schema.Null Avro.Null)
  , (optional Theta.int' $ Just $ Theta.int 37, Theta.optional' Theta.int',
     Avro.Union [Schema.Null, Schema.Int] Schema.Int $ Avro.Int 37)

    -- nested optionals (this one is a bit subtle!)
  , (optional (Theta.optional' Theta.int') Nothing, Theta.optional' $ Theta.optional' Theta.int',
     Avro.Union [Schema.Null, nestedUnion] Schema.Null Avro.Null)
  , (optional (Theta.optional' Theta.int') nothing, Theta.optional' $ Theta.optional' Theta.int',
     Avro.Union [Schema.Null, nestedUnion] nestedUnion nullUnion)
  , (optional (Theta.optional' Theta.int') (Just nestedOptional), Theta.optional' $ Theta.optional' Theta.int',
     Avro.Union [Schema.Null, nestedUnion] nestedUnion $
       wrapNested Schema.Int $ Avro.Int 37)
  ]
  where nothing   = Just $ optional Theta.int' Nothing
        nullUnion = wrapNested Schema.Null Avro.Null

        thetaArray t = Theta.Value (Theta.array' t) . Theta.Array
        thetaMap t   = Theta.Value (Theta.map' t) . Theta.Map
        optional t   = Theta.Value (Theta.optional' t) . Theta.Optional

        nestedOptional = optional Theta.int' $ Just $ Theta.int 37
        nestedUnion    = Schema.mkUnion [Schema.Null, Schema.Int]
        wrapNested     = Avro.Union [Schema.Null, Schema.Int]

records :: [(Theta.Value, Theta.Type, Avro.Value Schema.Schema)]
records = [ (emptyValue, emptyType, Avro.Record emptySchema [])
          , (value, type_, Avro.Record schema [ ("foo", Avro.Record emptySchema [])
                                              , ("bar", Avro.Int 42)
                                              ])
          ]
  where wrap = Theta.withModule' $ Theta.Module
                 { moduleName = "foo"
                 , types = [("foo.Empty", Theta.Definition "foo.Empty" Nothing emptyType)]
                 , imports = []
                 , metadata = baseMetadata
                 }

        emptyType  = wrap $ Theta.Record' "foo.Empty" []
        emptyValue = Theta.Value
          { Theta.type_   = emptyType
          , Theta.value   = Theta.Record []
          }
        emptySchema = Schema.Record "foo.Empty" [] Nothing Nothing []

        type_ = wrap $ Theta.Record' "foo.Foo"
                  [ Theta.Field "foo" Nothing emptyType
                  , Theta.Field "bar" Nothing Theta.int'
                  ]
        value = Theta.Value
          { Theta.type_   = type_
          , Theta.value   = Theta.Record [emptyValue , Theta.int 42]
          }
        schema = Schema.Record "foo.Foo" [] Nothing Nothing
          [avroField "foo" emptySchema, avroField "bar" Schema.Int]

        avroField name schema = Schema.Field name [] Nothing Nothing schema Nothing


variants :: [(Theta.Value, Theta.Type, Avro.Value Schema.Schema)]
variants =
  [ (intValue, variantType,
     wrapper "foo.Bar" (Schema.mkUnion [intSchema, stringSchema]) $
       Avro.Union [intSchema, stringSchema] intSchema intRecord)
  , (stringValue, variantType,
     wrapper "foo.Bar" (Schema.mkUnion [intSchema, stringSchema]) $
       Avro.Union [intSchema, stringSchema] stringSchema stringRecord)

    -- nested variant
  , (variantValue, nestedType,
     wrapper "foo.Baz" (Schema.mkUnion [variantSchema, longSchema, floatSchema]) $
      Avro.Union [variantSchema, longSchema, floatSchema] variantSchema variantRecord)
  , (longValue, nestedType,
     wrapper "foo.Baz" (Schema.mkUnion [variantSchema, longSchema, floatSchema]) $
      Avro.Union [variantSchema, longSchema, floatSchema] longSchema longRecord)
  , (floatValue, nestedType,
     wrapper "foo.Baz" (Schema.mkUnion [variantSchema, longSchema, floatSchema]) $
      Avro.Union [variantSchema, longSchema, floatSchema] floatSchema floatRecord)
  ]
  where wrap = Theta.withModule' $ Theta.Module
                 { moduleName = "foo"
                 , types =
                     [ def "foo.Bar" variantType
                     , def "foo.Baz" nestedType
                     ]
                 , imports = []
                 , metadata = baseMetadata
                 }
        def name type_ = (name, Theta.Definition name Nothing type_)

        variantType = wrap $ Theta.Variant' "foo.Bar" [intCase, stringCase]
        intCase     = Theta.Case "foo.Int_" Nothing
          [ Theta.Field "a" Nothing Theta.int'
          , Theta.Field "b" Nothing Theta.int'
          ]
        stringCase  = Theta.Case "foo.String_" Nothing
          [ Theta.Field "a" Nothing Theta.string'
          , Theta.Field "b" Nothing Theta.string'
          ]

        value       = Theta.Value variantType
        intValue    = value $ Theta.Variant "foo.Int_" [Theta.int 1, Theta.int 2]
        stringValue = value $ Theta.Variant "foo.String_"
          [Theta.string "a", Theta.string "b"]

        avroRecord name       = Schema.Record name [] Nothing Nothing
        avroField name schema = Schema.Field name [] Nothing Nothing schema Nothing

        intSchema     = avroRecord "foo.Int_"
          [avroField "a" Schema.Int, avroField "b" Schema.Int]
        stringSchema  = avroRecord "foo.String_"
          [avroField "a" Schema.String, avroField "b" Schema.String]

        intRecord    = Avro.Record intSchema [ ("a", Avro.Int 1)
                                             , ("b", Avro.Int 2)]
        stringRecord = Avro.Record stringSchema [ ("a", Avro.String "a")
                                                , ("b", Avro.String "b")
                                                ]

        wrapper name schema branch = Avro.Record
          (avroRecord name [avroField "constructor" schema]) [("constructor", branch)]

        -- nested variant
        nestedType  = wrap $ Theta.Variant' "foo.Baz" [variantCase, longCase, floatCase]
        variantCase = Theta.Case "foo.Variant_" Nothing
          [ Theta.Field "bar" Nothing variantType ]
        longCase    = Theta.Case "foo.Long_" Nothing
          [ Theta.Field "long" Nothing Theta.long' ]
        floatCase   = Theta.Case "foo.Float_" Nothing
          [ Theta.Field "float" Nothing Theta.float' ]

        nestedValue   = Theta.Value nestedType
        variantValue  =
          nestedValue $ Theta.Variant "foo.Variant_" [intValue]
        longValue     =
          nestedValue $ Theta.Variant "foo.Long_" [Theta.long 1]
        floatValue    =
          nestedValue $ Theta.Variant "foo.Float_" [Theta.float 2]

        variantUnion  = Schema.mkUnion [intSchema, stringSchema]
        variantSchema = avroRecord "foo.Variant_"
          [avroField "bar" $ avroRecord "foo.Bar" [avroField "constructor" variantUnion]]
        longSchema    = avroRecord "foo.Long_" [avroField "long" Schema.Long]
        floatSchema   = avroRecord "foo.Float_" [avroField "float" Schema.Float]

        wrappedRecord = wrapper "foo.Bar" (Schema.mkUnion [intSchema, stringSchema])
          (Avro.Union [intSchema, stringSchema] intSchema intRecord)
        variantRecord = Avro.Record variantSchema [("bar", wrappedRecord)]
        longRecord    = Avro.Record longSchema [("long", Avro.Long 1)]
        floatRecord   = Avro.Record floatSchema [("float", Avro.Float 2)]

-- | Basic metadata for the modules used in this test.
baseMetadata :: Metadata
baseMetadata = Metadata { languageVersion = "1.0.0", avroVersion = "1.0.0", moduleName = "foo" }
