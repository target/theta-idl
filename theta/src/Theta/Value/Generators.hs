{-# LANGUAGE ViewPatterns #-}
module Theta.Value.Generators where

import           Control.Monad        (replicateM)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.List.NonEmpty   as NonEmpty
import qualified Data.Set             as HashSet
import qualified Data.Text            as Text
import qualified Data.Time            as Time
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vector

import           Test.QuickCheck

import           Theta.Types
import           Theta.Value

-- | Return a random generator that produces Theta values that match
-- the given schema.
--
-- Warning: The generator does not handle recursive types well: it
-- will generate prohibitively large values or even loop forever given
-- a recursive schema.
genValue :: Type -> Gen Value
genValue t = case baseType t of
  Bool'          -> boolean <$> arbitrary
  Bytes'         -> bytes . LBS.pack <$> arbitrary
  Int'           -> int <$> arbitrary
  Long'          -> long <$> arbitrary
  Float'         -> float <$> arbitrary
  Double'        -> double <$> arbitrary
  String'        -> string . Text.pack <$> arbitrary
  Date'          -> date <$> genDate
  Datetime'      -> datetime <$> genDatetime

  Array' item    -> Value t <$> genArray item
  Map' item      -> Value t <$> genMap item
  Optional' item -> Value t <$> genOptional item

  Reference' name -> case lookupName name (module_ t) of
    Right t' -> genValue t'
    Left err -> error err -- should not happen for validly constructed
                          -- Theta types

  Enum' _ symbols  ->
    Value t . Enum <$> elements (NonEmpty.toList symbols)
  Record' _ fields ->
    Value t . Record <$> genFields fields
  Variant' _ cases -> do
    case_ <- elements (NonEmpty.toList cases)
    Value t . Variant (caseName case_) <$> genFields (caseParameters case_)

  Newtype' _ t' -> Value t . value <$> genValue t'

-- | Given a list of fields, generate a vector of values with an
-- element corresponding to each field's type.
genFields :: Fields Type -> Gen (Vector Value)
genFields (fields -> f) = Vector.fromList <$> mapM genValue (fieldType <$> f)

-- | Generate a random date.
genDate :: Gen Time.Day
genDate = Time.ModifiedJulianDay <$> arbitrary

-- | Generate a random timestamp.
genDatetime :: Gen Time.UTCTime
genDatetime = Time.UTCTime <$> genDate <*> (fromInteger <$> arbitrary)

-- | Generate an array with items of the given type.
--
-- The number of elements depends on the generator's size
-- parameter. See 'listOf' for details.
genArray :: Type -> Gen BaseValue
genArray item = Array . Vector.fromList <$> listOf (genValue item)

-- | Generate a map with unique keys and random values. The size of
-- the map scales with the size parameter.
genMap :: Type -> Gen BaseValue
genMap item = do
  keys   <- HashSet.toList . HashSet.fromList <$> listOf (Text.pack <$> arbitrary)
  values <- replicateM (length keys) (genValue item)
  pure $ Map $ HashMap.fromList $ keys `zip` values

-- | Generate an Optional with the given type. Generates nulls with a
-- 5% probability.
genOptional :: Type -> Gen BaseValue
genOptional item = frequency
  [ (1, pure $ Optional Nothing)
  , (19, Optional . Just <$> genValue item)
  ]
