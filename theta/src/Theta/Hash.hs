{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskellQuotes      #-}
-- | Theta types carry a unique, structural hash for quick equality
-- comparisons.
--
-- Two equivalent types will always have the same hash. A reference to
-- a type will have the same hash as the type itself.
--
-- The hashing implementation may change in different versions of
-- Theta, so you should not rely on hashes being stable across
-- releases of the @theta@ package.
module Theta.Hash where

import           Data.Binary                (Binary, encode)
import qualified Data.Binary                as Binary
import qualified Data.ByteString.Lazy       as LBS
import           Data.Digest.Pure.MD5       (MD5Digest, md5)
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as Text

import           Instances.TH.Lift          ()

import           Language.Haskell.TH.Syntax (Lift (liftTyped))

-- | The output of our hashing function.
--
-- Currently uses MD5 under the hood, but this might change in the
-- future—don't rely on the hashing implementation directly.
newtype Hash = Hash MD5Digest
  deriving newtype (Binary, Show, Eq, Ord)

instance Semigroup Hash where
  Hash a <> Hash b = toHash $ encode a <> encode b

instance Lift Hash where
  liftTyped (Hash hash) = [|| Binary.decode encoded ||]
    where encoded = Binary.encode hash

-- | The hashing function we're using. Designed to be easy to
-- change—just change this function + the type underneath 'Hash'.
toHash :: LBS.ByteString -> Hash
toHash = Hash . md5

-- | Calculate a hash for the given text. This doesn't do any extra
-- processing, so differences in whitespace/etc will produce different
-- hashes.
hashText :: Text -> Hash
hashText = toHash . LBS.fromStrict . Text.encodeUtf8

-- | Calculate a canonical hash for a list of hashes.
--
-- This lets us consistently hash multiple hashable values in a given
-- order.
hashList :: [Hash] -> Hash
hashList = foldr (<>) (hashText "")
