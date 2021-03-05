{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Test that the logic that converts directly between Haskell types
-- and Avro matches the logic that converts between Haskell,
-- @Theta.Value@ and Avro.
module Test.Theta.Target.Haskell.Conversion where

import           Control.Monad.State             (evalStateT)

import qualified Data.Avro.Decode                as Avro
import qualified Data.Avro.Encode                as Avro
import qualified Data.Binary.Get                 as Get
import qualified Data.ByteString.Builder         as Builder
import qualified Data.ByteString.Lazy            as LBS
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as HashMap
import           Data.Int                        (Int32, Int64)
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import qualified Data.Text                       as Text

import qualified Theta.Pretty                    as Theta

import qualified Theta.Target.Avro.Types         as Avro.Types
import           Theta.Target.Avro.Values        (fromAvro, toAvro, toDay,
                                                  toUTCTime)

import           Theta.Target.Haskell.Conversion
import qualified Theta.Target.Haskell.HasTheta   as HasTheta

import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Haskell.Conversion"
  [ testGroup "avroEncoding = encodeAvro ∘ toAvro ∘ toTheta"
    [ testGroup "primitive types"
      [ testProperty "Bool"     $ checkAvroEncoding   @Bool
      , testProperty "Bytes"    $ checkAvroEncoding . LBS.pack
      , testProperty "Int"      $ checkAvroEncoding   @Int32
      , testProperty "Long"     $ checkAvroEncoding   @Int64
      , testProperty "Float"    $ checkAvroEncoding   @Float
      , testProperty "Double"   $ checkAvroEncoding   @Double
      , testProperty "String"   $ checkAvroEncoding . Text.pack
      , testProperty "Date"     $ checkAvroEncoding . toDay
      , testProperty "Datetime" $ checkAvroEncoding . toUTCTime
      ]

    , testGroup "containers"
      [ testProperty "Array"    $ checkAvroEncoding @[Int32]
      , testProperty "Map"      $ checkAvroEncoding @(HashMap Text Int32) . toMap
      , testProperty "Optional" $ checkAvroEncoding @(Maybe Int32)
      ]
    ]

  , testGroup "avroDecoding = fromTheta ∘ fromAvro ∘ decodeAvro"
    [ testGroup "primitive types"
      [ testProperty "Bool"     $ checkAvroDecoding   @Bool
      , testProperty "Bytes"    $ checkAvroDecoding . LBS.pack
      , testProperty "Int"      $ checkAvroDecoding   @Int32
      , testProperty "Long"     $ checkAvroDecoding   @Int64
      , testProperty "Float"    $ checkAvroDecoding   @Float
      , testProperty "Double"   $ checkAvroDecoding   @Double
      , testProperty "String"   $ checkAvroDecoding . Text.pack
      , testProperty "Date"     $ checkAvroDecoding . toDay
      , testProperty "Datetime" $ checkAvroDecoding . toUTCTime
      ]

    , testGroup "containers"
      [ testProperty "Array"    $ checkAvroDecoding @[Int32]
      , testProperty "Map"      $ checkAvroDecoding @(HashMap Text Int32) . toMap
      , testProperty "Optional" $ checkAvroDecoding @(Maybe Int32)
      ]
    ]
  ]
  where toMap = HashMap.fromList . map (\ (k, v) -> (Text.pack k, v))

-- * Functions for building properties

checkAvroEncoding :: forall a. ToTheta a => a -> Bool
checkAvroEncoding a = Avro.encodeAvro avro == Builder.toLazyByteString (avroEncoding a)
  where avro = case toAvro $ toTheta a of
          Left err  -> error $ Text.unpack $ Theta.pretty err
          Right res -> res

checkAvroDecoding :: forall a. (Eq a, FromTheta a, ToTheta a) => a -> Bool
checkAvroDecoding a = from == decoded
  where encoded = Builder.toLazyByteString (avroEncoding a)

        decoded = case Get.runGetOrFail @a avroDecoding encoded of
          Left (_, _, err)  -> error err
          Right (_, _, res) -> res

        from = case fromTheta =<< fromAvro (HasTheta.theta @a) avro of
          Left err  -> error $ Text.unpack $ Theta.pretty err
          Right res -> res

        avro = case Avro.decodeAvro schema encoded of
          Left err  -> error err
          Right res -> res
        schema = case toSchema $ HasTheta.theta @a of
          Left err  -> error $ Text.unpack $ Theta.pretty err
          Right res -> res

        toSchema type_ = evalStateT (Avro.Types.typeToAvro type_) Set.empty
