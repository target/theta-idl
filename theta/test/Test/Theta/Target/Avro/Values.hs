{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumDecimals           #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ParallelListComp      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
module Test.Theta.Target.Avro.Values where

import           Control.Monad.Except            (runExceptT)

import qualified Data.Avro.Encoding.FromAvro     as Avro
import qualified Data.Avro.Schema.ReadSchema     as ReadSchema
import qualified Data.ByteString                 as BS
import qualified Data.HashMap.Strict             as HashMap
import           Data.Int                        (Int32, Int64)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Time.Calendar              as Time
import qualified Data.Time.Clock                 as Time
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as Vector

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

primitives :: [(Theta.Value, Theta.Type, Avro.Value)]
primitives =
  [ (Theta.boolean True,  Theta.bool', Avro.Boolean True)
  , (Theta.boolean False, Theta.bool', Avro.Boolean False)

  , (Theta.bytes "",      Theta.bytes', avroBytes "")
  , (Theta.bytes "blarg", Theta.bytes', avroBytes "blarg")

  , (Theta.int (-1),     Theta.int', avroInt (-1))
  , (Theta.int 0,        Theta.int', avroInt 0)
  , (Theta.int 42,       Theta.int', avroInt 42)
  , (Theta.int minBound, Theta.int', avroInt minBound)
  , (Theta.int maxBound, Theta.int', avroInt maxBound)

  , (Theta.long (-1),     Theta.long', avroLong (-1))
  , (Theta.long 0,        Theta.long', avroLong 0)
  , (Theta.long 42,       Theta.long', avroLong 42)
  , (Theta.long minBound, Theta.long', avroLong minBound)
  , (Theta.long maxBound, Theta.long', avroLong maxBound)

  , (Theta.float (-1),  Theta.float', avroFloat (-1))
  , (Theta.float 0,     Theta.float', avroFloat 0)
  , (Theta.float 42,    Theta.float', avroFloat 42)
  , (Theta.float pi,    Theta.float', avroFloat pi)
  , (Theta.float (1/0), Theta.float',  avroFloat (1/0))

  , (Theta.double (-1),  Theta.double', avroDouble (-1))
  , (Theta.double 0,     Theta.double', avroDouble 0)
  , (Theta.double 42,    Theta.double', avroDouble 42)
  , (Theta.double pi,    Theta.double', avroDouble pi)
  , (Theta.double (1/0), Theta.double', avroDouble (1/0))

  , (Theta.string "",      Theta.string', avroString "")
  , (Theta.string "blarg", Theta.string', avroString "blarg")

  , (Theta.date $ read "2019-02-11", Theta.date', avroInt 17938 )

  , (Theta.datetime datetime, Theta.datetime', avroLong 1549894992000000)
  ]
  where datetime = read "2019-02-11 14:23:12 UTC"

containers :: [(Theta.Value, Theta.Type, Avro.Value)]
containers =
  [ -- arrays
    (thetaArray Theta.int' [], Theta.array' Theta.int', Avro.Array [])
  , (thetaArray Theta.int' $ Theta.int <$> [0..10], Theta.array' Theta.int',
     Avro.Array $ avroInt <$> [0..10])

    -- maps
  , (thetaMap Theta.int' [], Theta.map' Theta.int', Avro.Map [])
  , (thetaMap Theta.int' [("abc", Theta.int 10), ("def", Theta.int 20)], Theta.map' Theta.int',
     Avro.Map [("abc", avroInt 10), ("def", avroInt 20)])

    -- normal optional types
  , (optional Theta.int' Nothing, Theta.optional' Theta.int',
     avroUnion [ReadSchema.Null, schemaInt] 0 Avro.Null)
  , (optional Theta.int' $ Just $ Theta.int 37, Theta.optional' Theta.int',
     avroUnion [ReadSchema.Null, schemaInt] 1 $ avroInt 37)

    -- nested optionals (this one is a bit subtle!)
  , (optional (Theta.optional' Theta.int') Nothing, Theta.optional' $ Theta.optional' Theta.int',
     avroUnion [ReadSchema.Null, nestedUnion] 0 Avro.Null)
  , (optional (Theta.optional' Theta.int') nothing, Theta.optional' $ Theta.optional' Theta.int',
     avroUnion [ReadSchema.Null, nestedUnion] 1 nullUnion)
  , (optional (Theta.optional' Theta.int') (Just nestedOptional), Theta.optional' $ Theta.optional' Theta.int',
     avroUnion [ReadSchema.Null, nestedUnion] 1 $
       wrapNested 1 $ avroInt 37)
  ]
  where nothing   = Just $ optional Theta.int' Nothing
        nullUnion = wrapNested 0 Avro.Null

        thetaArray t = Theta.Value (Theta.array' t) . Theta.Array
        thetaMap t   = Theta.Value (Theta.map' t) . Theta.Map
        optional t   = Theta.Value (Theta.optional' t) . Theta.Optional

        nestedOptional = optional Theta.int' $ Just $ Theta.int 37
        nestedUnion    = schemaUnion [ReadSchema.Null, schemaInt]
        wrapNested     = avroUnion [ReadSchema.Null, schemaInt]

records :: [(Theta.Value, Theta.Type, Avro.Value)]
records = [ (emptyValue, emptyType, Avro.Record emptySchema [])
          , (value, type_, Avro.Record schema [ (Avro.Record emptySchema [])
                                              , (avroInt 42)
                                              ])

          -- Test date and datetime handling with logical types (avro-version ≥ 1.1.0)
          , (dates, datesType,
             Avro.Record datesSchema [avroDate 17938, avroDatetime 1549894992000000])
          ]
  where wrap v = Theta.withModule' $ Theta.Module
                   { moduleName = "foo"
                   , types = [("foo.Empty", Theta.Definition "foo.Empty" Nothing emptyType)]
                   , imports = []
                   , metadata = baseMetadata { avroVersion = v }
                   }

        emptyType  = wrap "1.0.0" $ Theta.Record' "foo.Empty" []
        emptyValue = Theta.Value
          { Theta.type_   = emptyType
          , Theta.value   = Theta.Record []
          }
        emptySchema = ReadSchema.Record "foo.Empty" [] Nothing []

        type_ = wrap "1.0.0" $ Theta.Record' "foo.Foo"
                  [ Theta.Field "foo" Nothing emptyType
                  , Theta.Field "bar" Nothing Theta.int'
                  ]
        value = Theta.Value
          { Theta.type_   = type_
          , Theta.value   = Theta.Record [emptyValue , Theta.int 42]
          }
        schema = ReadSchema.Record "foo.Foo" [] Nothing
          [avroField "foo" emptySchema 0, avroField "bar" schemaInt 1]

        avroField name schema i =
          ReadSchema.ReadField name [] Nothing Nothing (ReadSchema.AsIs i) schema Nothing

        dates = Theta.Value
          { Theta.type_ = datesType
          , Theta.value = Theta.Record
            [ Theta.date (Theta.toDay 17938)
            , Theta.datetime (Theta.toUTCTime 1549894992000000)
            ]
          }
        datesType = wrap "1.1.0" $ Theta.Record' "foo.Dates"
          [ Theta.Field "date" Nothing Theta.date'
          , Theta.Field "datetime" Nothing Theta.datetime'
          ]
        datesSchema = ReadSchema.Record "foo.Dates" [] Nothing
          [ avroField "date" schemaDate 0, avroField "datetime" schemaDatetime 1 ]


variants :: [(Theta.Value, Theta.Type, Avro.Value)]
variants =
  [ (intValue, variantType,
     wrapper "foo.Bar" (schemaUnion [intSchema, stringSchema]) $
       avroUnion [intSchema, stringSchema] 0 intRecord)
  , (stringValue, variantType,
     wrapper "foo.Bar" (schemaUnion [intSchema, stringSchema]) $
       avroUnion [intSchema, stringSchema] 1 stringRecord)

    -- nested variant
  , (variantValue, nestedType,
     wrapper "foo.Baz" (schemaUnion [variantSchema, longSchema, floatSchema]) $
      avroUnion [variantSchema, longSchema, floatSchema] 0 variantRecord)
  , (longValue, nestedType,
     wrapper "foo.Baz" (schemaUnion [variantSchema, longSchema, floatSchema]) $
      avroUnion [variantSchema, longSchema, floatSchema] 1 longRecord)
  , (floatValue, nestedType,
     wrapper "foo.Baz" (schemaUnion [variantSchema, longSchema, floatSchema]) $
      avroUnion [variantSchema, longSchema, floatSchema] 2 floatRecord)
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

        avroRecord name fields =
          ReadSchema.Record name [] Nothing [ field i | i <- [0..] | field <- fields ]
        avroField name schema i =
          ReadSchema.ReadField name [] Nothing Nothing (ReadSchema.AsIs i) schema Nothing

        intSchema     = avroRecord "foo.Int_"
          [avroField "a" schemaInt, avroField "b" schemaInt]
        stringSchema  = avroRecord "foo.String_"
          [avroField "a" schemaString, avroField "b" schemaString]

        intRecord    = Avro.Record intSchema [ avroInt 1, avroInt 2 ]
        stringRecord = Avro.Record stringSchema [ avroString "a" , avroString "b" ]

        wrapper name schema branch = Avro.Record
          (avroRecord name [avroField "constructor" schema]) [branch]

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

        variantUnion  = schemaUnion [intSchema, stringSchema]
        variantSchema = avroRecord "foo.Variant_"
          [avroField "bar" $ avroRecord "foo.Bar" [avroField "constructor" variantUnion]]
        longSchema    = avroRecord "foo.Long_" [avroField "long" schemaLong]
        floatSchema   = avroRecord "foo.Float_" [avroField "float" schemaFloat]

        wrappedRecord = wrapper "foo.Bar" (schemaUnion [intSchema, stringSchema])
          (avroUnion [intSchema, stringSchema] 0 intRecord)
        variantRecord = Avro.Record variantSchema [wrappedRecord]
        longRecord    = Avro.Record longSchema [avroLong 1]
        floatRecord   = Avro.Record floatSchema [avroFloat 2]

-- | Basic metadata for the modules used in this test.
baseMetadata :: Metadata
baseMetadata = Metadata { languageVersion = "1.0.0", avroVersion = "1.0.0", moduleName = "foo" }

-- ** Helpers for building Avro values and schemas

avroBytes :: BS.ByteString -> Avro.Value
avroBytes = Avro.Bytes (ReadSchema.Bytes Nothing)

avroInt :: Int32 -> Avro.Value
avroInt = Avro.Int schemaInt

avroDate :: Int32 -> Avro.Value
avroDate = Avro.Int schemaDate

avroLong :: Int64 -> Avro.Value
avroLong = Avro.Long (ReadSchema.Long ReadSchema.ReadLong Nothing)

avroDatetime :: Int64 -> Avro.Value
avroDatetime = Avro.Long schemaDatetime

avroFloat :: Float -> Avro.Value
avroFloat = Avro.Float (ReadSchema.Float ReadSchema.ReadFloat)

avroDouble :: Double -> Avro.Value
avroDouble = Avro.Double (ReadSchema.Double ReadSchema.ReadDouble)

avroString :: Text -> Avro.Value
avroString = Avro.String schemaString

avroUnion :: Vector ReadSchema.ReadSchema -> Int -> Avro.Value -> Avro.Value
avroUnion options i v = Avro.Union (schemaUnion options) i v

schemaUnion :: Vector ReadSchema.ReadSchema -> ReadSchema.ReadSchema
schemaUnion = ReadSchema.Union . Vector.indexed

schemaInt :: ReadSchema.ReadSchema
schemaInt = ReadSchema.Int Nothing

schemaDate :: ReadSchema.ReadSchema
schemaDate = ReadSchema.Int (Just ReadSchema.Date)

schemaLong :: ReadSchema.ReadSchema
schemaLong = ReadSchema.Long ReadSchema.ReadLong Nothing

schemaDatetime :: ReadSchema.ReadSchema
schemaDatetime = ReadSchema.Long ReadSchema.ReadLong (Just ReadSchema.TimestampMicros)

schemaFloat :: ReadSchema.ReadSchema
schemaFloat = ReadSchema.Float ReadSchema.ReadFloat

schemaString :: ReadSchema.ReadSchema
schemaString = ReadSchema.String Nothing
