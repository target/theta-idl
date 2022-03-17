{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Theta supports a number of "primitive" types that have built-in
-- encodings and behaviors.
--
-- Based on [Avro's primitive
-- types](https://avro.apache.org/docs/current/spec.html#schema_primitive):
--
--   * Bool
--   * Bytes
--   * Int
--   * Long
--   * Float
--   * Double
--   * String
--
-- Based on [Avro's logical
-- types](https://avro.apache.org/docs/current/spec.html#Logical+Types):
--
--   * Date
--   * Datetime
module Theta.Primitive where

import           Data.Text      (Text)
import qualified Data.Text      as Text

import           Theta.Hash     (Hash)
import           Theta.Metadata (Version)
import           Theta.Name     (Name, hashName)
import           Theta.Pretty   (Pretty, pretty)

-- | Theta's primitive types.
--
-- A primitive type has its own encoding and behavior—can be
-- fundamentally different from user-defined types.
data Primitive = Bool
               | Bytes
               -- ^ A variable-length byte array—can store binary
               -- blobs
               | Int
               -- ^ 32-bit signed integers
               | Long
               -- ^ 64-bit signed integers
               | Float
               -- ^ 32-bit floating-point numbers
               | Double
               -- ^ 64-bit floating-point numbers
               | String
               -- ^ Unicode-aware strings
               | Date
               -- ^ An absolute date (eg @2020-01-10@) with no time
               -- attached
               | Datetime
               -- ^ An absolute timestamp in UTC (no locale/timezone)
 deriving stock (Eq, Show, Ord, Enum, Bounded)

instance Pretty Primitive where pretty = primitiveKeyword

-- | Returns a canonical name for each primitive type.
--
-- Primitive types have definitions in the @theta.primitive@ module,
-- so that's the canonical namespace. @Int@ become
-- @theta.primitive.Int@, @String@ becomes
-- @theta.primitive.String@... etc.
primitiveName :: Primitive -> Name
primitiveName Bool     = "theta.primitive.Bool"
primitiveName Bytes    = "theta.primitive.Bytes"
primitiveName Int      = "theta.primitive.Int"
primitiveName Long     = "theta.primitive.Long"
primitiveName Float    = "theta.primitive.Float"
primitiveName Double   = "theta.primitive.Double"
primitiveName String   = "theta.primitive.String"
primitiveName Date     = "theta.primitive.Date"
primitiveName Datetime = "theta.primitive.Datetime"

-- | The canonical keyword for each primitive type.
--
-- Currently this is automatically derived from the 'Show' instance
-- for 'Primitive', but this is not guaranteed to hold in the future.
primitiveKeyword :: Primitive -> Text
primitiveKeyword = Text.pack . show

-- | Return a canonical hash for each primitive type.
hashPrimitive :: Primitive -> Hash
hashPrimitive = hashName . primitiveName

-- | The earliest @language-version@ that supports the given primitive
-- type.
definedIn :: Primitive -> Version
definedIn Bool     = "1.0.0"
definedIn Bytes    = "1.0.0"
definedIn Int      = "1.0.0"
definedIn Long     = "1.0.0"
definedIn Float    = "1.0.0"
definedIn Double   = "1.0.0"
definedIn String   = "1.0.0"
definedIn Date     = "1.0.0"
definedIn Datetime = "1.0.0"

-- | Every single primitive type supported by Theta.
primitives :: [Primitive]
primitives = [minBound..maxBound]
