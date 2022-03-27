{-# LANGUAGE DeriveLift         #-}
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

import           Data.Text                  (Text)
import qualified Data.Text                  as Text

import           Language.Haskell.TH.Syntax (Lift)

import           Theta.Hash                 (Hash)
import           Theta.Metadata             (Version)
import           Theta.Name                 (Name (Name), hashName)
import           Theta.Pretty               (Pretty, pretty)

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
               -- ^ An absolute date (eg @2020-01-10@) with no
               -- timezone/locale specified.
               | Datetime
               -- ^ An absolute timestamp in UTC.
               | UUID
               -- ^ A universally unique identifier (UUID), conforming
               -- to [RFC 4122](https://www.ietf.org/rfc/rfc4122.txt)
               --
               -- Example: @f81d4fae-7dec-11d0-a765-00a0c91e6bf6@
               | Time
               -- ^ The time of day, starting at midnight.
               | LocalDatetime
               -- ^ An absolute timestamp in whatever timezone is
               -- considered local. (No timezone/locale is specified.)
 deriving stock (Eq, Show, Ord, Enum, Bounded, Lift)

instance Pretty Primitive where pretty = primitiveKeyword

-- | Returns a canonical name for each primitive type.
--
-- Primitive types have @theta.primitive@ as their canonical
-- namespace. @Int@ become @theta.primitive.Int@, @String@ becomes
-- @theta.primitive.String@... etc.
--
-- Currently these names are not defined in Theta itself, but that
-- will probably change in the future.
primitiveName :: Primitive -> Name
primitiveName = Name "theta.primitive" . primitiveKeyword

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
definedIn Bool          = "1.0.0"
definedIn Bytes         = "1.0.0"
definedIn Int           = "1.0.0"
definedIn Long          = "1.0.0"
definedIn Float         = "1.0.0"
definedIn Double        = "1.0.0"
definedIn String        = "1.0.0"
definedIn Date          = "1.0.0"
definedIn Datetime      = "1.0.0"
definedIn UUID          = "1.1.0"
definedIn Time          = "1.1.0"
definedIn LocalDatetime = "1.1.0"

-- | Every single primitive type supported by Theta.
primitives :: [Primitive]
primitives = [minBound..maxBound]
