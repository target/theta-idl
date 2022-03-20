{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Test for the code that generates Haskell types from Theta
-- schemas, including tests for the ToThtea/FromTheta instances.
module Test.Theta.Target.Haskell where

import qualified Data.Avro                       as Avro
import qualified Data.ByteString.Builder         as ByteString
import qualified Data.ByteString.Lazy            as LBS
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as HashMap
import           Data.Int                        (Int32, Int64)
import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.UUID                       as UUID
import qualified Data.Vector                     as Vector

import           Theta.Metadata                  (Metadata (..))
import           Theta.Pretty                    (pretty)
import           Theta.Types

import qualified Theta.Value                     as Theta

import           Theta.Target.Haskell            (loadModule)
import           Theta.Target.Haskell.Conversion
import qualified Theta.Target.Haskell.HasTheta   as HasTheta

import           Test.Tasty
import           Test.Tasty.HUnit

-- * Loading Modules

loadModule "test/data/transitive" "c"
loadModule "test/data/modules" "importing_foo"
loadModule "test/data/modules" "primitives"
loadModule "test/data/modules" "enums"

-- * Tests

tests :: TestTree
tests = testGroup "Haskell"
  [ test_primitives
  , test_containers
  , test_avroEncoding
  , test_avroDecoding
  , test_transitiveImports
  ]

test_primitives :: TestTree
test_primitives = testGroup "primitives"
  [ testCase "toTheta" $ do
      toTheta primitives @?= wrap expected
  , testCase "fromTheta" $ do
      case fromTheta $ wrap expected of
        Right got -> got @?= primitives
        Left err  -> assertFailure $ Text.unpack $ pretty err
  , testCase "fromAvro ∘ toAvro = id" $ do
      Avro.decodeValue (Avro.encodeValue primitives) @?= Right primitives
  ]
  where primitives = Primitives True "blarg" 37 42 1.2 7.4 "foo" today now uuid
        expected = Theta.Record
          [ Theta.boolean True
          , Theta.bytes "blarg"
          , Theta.int 37
          , Theta.long 42
          , Theta.float 1.2
          , Theta.double 7.4
          , Theta.string "foo"
          , Theta.date today
          , Theta.datetime now
          , Theta.uuid uuid
          ]

        today = read "2019-02-11"
        now   = read "2019-02-11 14:23:12 UTC"
        uuid  = fromMaybe (error "Invalid UUID literal.") $
          UUID.fromText "f81d4fae-7dec-11d0-a765-00a0c91e6bf6"

        wrap baseValue = Theta.Value
          { type_ = HasTheta.theta @Primitives
          , value = baseValue
          }

test_containers :: TestTree
test_containers = testGroup "containers"
  [ testCase "toTheta" $ do
      toTheta containers @?= wrap expected
  , testCase "fromTheta" $ do
      case fromTheta $ wrap expected of
        Right got -> got @?= containers
        Left err  -> assertFailure $ Text.unpack $ pretty err
  , testCase "fromAvro ∘ toAvro = id" $ do
      Avro.decodeValue (Avro.encodeValue containers) @?= Right containers
  ]
  where containers = Containers [True] [("foo", True)] (Just True) [("foo", [Nothing])]

        expected = Theta.Record
          [ Theta.Value (array' bool') (Theta.Array [Theta.boolean True])
          , Theta.Value (map' bool') (Theta.Map [("foo", Theta.boolean True)])
          , Theta.Value (optional' bool') (Theta.Optional $ Just $ Theta.boolean True)
          , let optional = optional' bool'
                array    = array' optional

                optionalValue = Theta.Value optional $ Theta.Optional Nothing
            in
            Theta.Value (map' array)
              (Theta.Map [("foo", Theta.Value array $ Theta.Array [optionalValue])])
          ]

        wrap baseValue = Theta.Value
          { type_ = HasTheta.theta @Containers
          , value = baseValue
          }


  -- TODO: add QuickCheck properties
test_avroEncoding :: TestTree
test_avroEncoding = testGroup "avroEncoding"
  [ testGroup "primitive types"
    [ testCase "Bool" $ do
        encodeAvro True @?= encode True
        encodeAvro False @?= encode False

    , testCase "Int" $ do
        encodeAvro (1        :: Int32) @?= encode @Int32 1
        encodeAvro (0        :: Int32) @?= encode @Int32 0
        encodeAvro (-1       :: Int32) @?= encode @Int32 (-1)
        encodeAvro (minBound :: Int32) @?= encode @Int32 minBound
        encodeAvro (maxBound :: Int32) @?= encode @Int32 maxBound

    , testCase "Long" $ do
        encodeAvro (1        :: Int64) @?= encode @Int64 1
        encodeAvro (0        :: Int64) @?= encode @Int64 0
        encodeAvro (-1       :: Int64) @?= encode @Int64 (-1)
        encodeAvro (minBound :: Int64) @?= encode @Int64 minBound
        encodeAvro (maxBound :: Int64) @?= encode @Int64 maxBound

    , testCase "Float" $ do
        encodeAvro (1     :: Float) @?= encode @Float 1
        encodeAvro (0     :: Float) @?= encode @Float 0
        encodeAvro (-1    :: Float) @?= encode @Float (-1)
        encodeAvro (1.2   :: Float) @?= encode @Float 1.2
        encodeAvro (1e-10 :: Float) @?= encode @Float 1e-10

    , testCase "Double" $ do
        encodeAvro (1     :: Double) @?= encode @Double 1
        encodeAvro (0     :: Double) @?= encode @Double 0
        encodeAvro (-1    :: Double) @?= encode @Double (-1)
        encodeAvro (1.2   :: Double) @?= encode @Double 1.2
        encodeAvro (1e-10 :: Double) @?= encode @Double 1e-10

    , testCase "Bytes" $ do
        encodeAvro (""         :: LBS.ByteString) @?= encode @LBS.ByteString ""
        encodeAvro ("abc"      :: LBS.ByteString) @?= encode @LBS.ByteString "abc"
        encodeAvro ("\tλä̃∘̈çs " :: LBS.ByteString) @?= encode @LBS.ByteString "\tλä̃∘̈çs "

    , testCase "String" $ do
        encodeAvro (""         :: Text) @?= encode @Text ""
        encodeAvro ("abc"      :: Text) @?= encode @Text "abc"
        encodeAvro ("\tλä̃∘̈çs " :: Text) @?= encode @Text "\tλä̃∘̈çs "
    ]

  , testGroup "containers"
    [ testCase "Array" $ do
        encodeAvro ([]      :: [Int32]) @?= encode @[Int32] []
        encodeAvro ([1]     :: [Int32]) @?= encode @[Int32] [1]
        encodeAvro ([1..10] :: [Int32]) @?= encode @[Int32] [1..10]

    , testCase "Map" $ do
        encodeAvro ([]      :: HashMap Text Int32) @?=
          encode @(HashMap Text Int) []
        encodeAvro ([("a", 1)] :: HashMap Text Int32) @?=
          encode @(HashMap Text Int) [("a", 1)]

        let keys = Text.singleton <$> ['a'..]
        encodeAvro (HashMap.fromList $ zip keys [1 :: Int32 ..10]) @?=
          encode (HashMap.fromList @Text @Int $ zip keys [1..10])

    , testCase "Optional" $ do
        encodeAvro (Nothing :: Maybe Int32) @?= encode (Nothing :: Maybe Int32)

        let encoded = ByteString.toLazyByteString $ encodeInt 1 <> encodeInt 42
        encodeAvro (Just 42 :: Maybe Int32) @?= encoded
    ]
  ]
  where encode :: (Avro.HasAvroSchema a, Avro.ToAvro a) => a -> LBS.ByteString
        encode = Avro.encodeValue

test_avroDecoding :: TestTree
test_avroDecoding = testGroup "avroDecoding"
  [ testGroup "primitive types"
    [ testCase "Bool" $ do
        check True
        check False

    , testCase "Int" $ do
        check (1        :: Int32)
        check (0        :: Int32)
        check (-1       :: Int32)
        check (minBound :: Int32)
        check (maxBound :: Int32)

    , testCase "Long" $ do
        check (1        :: Int64)
        check (0        :: Int64)
        check (-1       :: Int64)
        check (minBound :: Int64)
        check (maxBound :: Int64)

    , testCase "Float" $ do
        check (1     :: Float)
        check (0     :: Float)
        check (-1    :: Float)
        check (1.2   :: Float)
        check (1e-10 :: Float)

    , testCase "Double" $ do
        check (1     :: Double)
        check (0     :: Double)
        check (-1    :: Double)
        check (1.2   :: Double)
        check (1e-10 :: Double)

    , testCase "Bytes" $ do
        check (""         :: LBS.ByteString)
        check ("abc"      :: LBS.ByteString)
        check ("\tλä̃∘̈çs " :: LBS.ByteString)

    , testCase "String" $ do
        check (""         :: Text)
        check ("abc"      :: Text)
        check ("\tλä̃∘̈çs " :: Text)
    ]

  , testGroup "containers"
    [ testCase "Array" $ do
        check ([]      :: [Int32])
        check ([1]     :: [Int32])
        check ([1..10] :: [Int32])

    , testCase "Map" $ do
        check ([]         :: HashMap Text Int32)
        check ([("a", 1)] :: HashMap Text Int32)

        let keys = Text.singleton <$> ['a'..]
        check (HashMap.fromList $ zip keys [1 :: Int32 ..10])

    , testCase "Optional" $ do
        check (Nothing :: Maybe Int32)
        check (Just 42 :: Maybe Int32)
    ]
  ]
  where check value = do
          result <- handle $ decodeAvro $ encodeAvro value
          result @?= value

        handle = \case
          Left err  -> assertFailure $ Text.unpack $ pretty err
          Right res -> pure res

test_transitiveImports :: TestTree
test_transitiveImports = testCase "transitiveImports" $ do
  types theta'a @?= types a
  types theta'b @?= types b
  types theta'c @?= types c
  where a = _Module "a" [def "a.A" $ withModule' a (Newtype' "a.A" string')] []
        b = _Module "b" [def "b.B" $ withModule' b bRecord] [a]
        c = _Module "c" [def "c.C" $ withModule' c cRecord] [b]

        def name type_ = (name, Definition name Nothing type_)

        bRecord = Record' "b.B" [ Field "a" Nothing (withModule' b (Reference' "a.A")) ]
        cRecord = Record' "c.C" [ Field "a" Nothing (withModule' c (Reference' "a.A"))
                                , Field "b" Nothing (withModule' b (Reference' "b.B")) ]

        _Module name types imports = Module
          { moduleName = name
          , types
          , imports
          , metadata = Metadata "1.0.0" "1.0.0" name
          }

-- ** Helpers

wrapNothing :: Type -> Theta.Value
wrapNothing (optional' -> type_) = Theta.Value { type_, value = Theta.Optional Nothing }

wrapArray :: Type -> Vector.Vector Theta.Value -> Theta.Value
wrapArray (array' -> type_) vs = Theta.Value { type_ , value = Theta.Array vs }

wrapMap :: Type -> HashMap Text Theta.Value -> Theta.Value
wrapMap (map' -> type_) vs = Theta.Value { type_ , value = Theta.Map vs }
