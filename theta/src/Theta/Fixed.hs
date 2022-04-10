{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Theta.Fixed
  ( FixedBytes
  , toByteString

  , fixedBytes
  , fixedBytes'
  , size
  , sizeOf
  )
where

import qualified Data.ByteString.Lazy as LBS
import           Data.Data            (Proxy (Proxy))
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as Text
import           Data.TreeDiff        (Expr (App), ToExpr (toExpr))

import           GHC.TypeLits         (KnownNat, Nat, natVal)

import           Text.Printf          (printf)

import           Theta.Pretty         (Pretty (pretty))

-- | A bytestring with a size specified at the type level.
--
-- The @size@ should always be the same as the length of the
-- underlying bytestring, otherwise the behavior of the type is
-- unspecified.
newtype FixedBytes (size :: Nat) = FixedBytes { toByteString :: LBS.ByteString }
  deriving stock (Eq)

instance KnownNat size => Show (FixedBytes size) where
  show fixed = printf "FixedBytes @%d %s" (sizeOf fixed) (show $ toByteString fixed)

instance Pretty (FixedBytes size) where
  pretty (FixedBytes bytes) = Text.pack $ show bytes

instance KnownNat size => ToExpr (FixedBytes size) where
  toExpr t = App "FixedBytes" [toExpr $ show t]

-- | Wrap a bytestring into a 'FixedBytes' with a set size.
--
-- Returns 'Nothing' if the length of the bytestring does not match
-- the statically expected size.
--
-- When the @size@ type variable cannot be inferred, you can specify
-- it with type applications:
--
-- >>> fixedBytes @3 "abc"
-- Just (FixedBytes {toByteString = "abc"})
--
-- >>> fixedBytes @4 "abc"
-- Nothing
--
fixedBytes :: forall size. KnownNat size => LBS.ByteString -> Maybe (FixedBytes size)
fixedBytes bytes = [FixedBytes bytes | LBS.length bytes == size]
  where size = fromIntegral $ natVal (Proxy @size)

-- | Wrap a bytestring into a 'FixedBytes' with a set size.
--
-- Errors at runtime if the bytestring's length does not match the
-- expected size.
--
-- When the @size@ type variable cannot be inferred, you can specify
-- it with type applications:
--
-- >>> fixedBytes' @3 "abc"
-- FixedBytes {toByteString = "abc"}
--
-- >>> fixedBytes' @4 "abc"
-- fixedBytes expected 4 bytes but got a bytestring of length 3.
--
fixedBytes' :: forall size. KnownNat size => LBS.ByteString -> FixedBytes size
fixedBytes' bytes = fromMaybe failure $ fixedBytes bytes
  where failure = error $ printf
          "fixedBytes expected %d bytes but got a bytestring of length %d" expected got
        expected = natVal (Proxy @size)
        got = LBS.length bytes

-- | The runtime size that corresponds to a type-level size.
--
-- The @size@ type variable will always be ambiguous, so this is meant
-- to be used with visible type applications:
--
-- >>> size @10
-- 10
--
size :: forall size. KnownNat size => Word
size = fromIntegral $ natVal (Proxy @size)

-- | The statically specified size of the given 'FixedBytes' value.
--
-- >>> sizeOf (fixedBytes' @3 "abc")
-- 3
--
sizeOf :: forall size. KnownNat size => FixedBytes size -> Word
sizeOf _ = size @size
