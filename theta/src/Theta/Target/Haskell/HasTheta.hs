{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | This module defines a class which maps Haskell types to Theta
-- types.
--
-- Haskell types generated with Template Haskell use this class to
-- point to the Theta type that created them.
module Theta.Target.Haskell.HasTheta where

import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  (HashMap)
import           Data.Int             (Int32, Int64)
import           Data.Text            (Text)
import           Data.Time            (Day, LocalTime, TimeOfDay, UTCTime)
import           Data.UUID            (UUID)

import           GHC.TypeLits         (KnownNat)

import           Theta.Fixed          (FixedBytes)
import qualified Theta.Fixed          as Fixed
import qualified Theta.Types          as Theta

-- | A class for Haskell types that correspond to a Theta type. Types
-- generated via Template Haskell will automatically have an instance
-- of this class pointing to the schema that created them.
class HasTheta a where
  -- | The Theta type that corresponds to a Haskell type. Because the
  -- Haskell type is ambiguous, this method has to be used with a
  -- visible type application, with the TypeApplications language extension
  -- (e.g. `theta @MyExample`)
  theta :: Theta.Type

-- * Instances for primitive types

instance HasTheta Bool where
  theta = Theta.bool'

instance HasTheta ByteString where
  theta = Theta.bytes'

instance HasTheta Int32 where
  theta = Theta.int'

instance HasTheta Int64 where
  theta = Theta.long'

instance HasTheta Float where
  theta = Theta.float'

instance HasTheta Double where
  theta = Theta.double'

instance HasTheta Text where
  theta = Theta.string'

instance HasTheta Day where
  theta = Theta.date'

instance HasTheta UTCTime where
  theta = Theta.datetime'

instance HasTheta UUID where
  theta = Theta.uuid'

instance HasTheta TimeOfDay where
  theta = Theta.time'

instance HasTheta LocalTime where
  theta = Theta.localDatetime'

instance KnownNat size => HasTheta (FixedBytes size) where
  theta = Theta.fixed' $ Fixed.size @size

instance HasTheta a => HasTheta [a] where
  theta = Theta.array' $ theta @a

instance HasTheta a => HasTheta (HashMap Text a) where
  theta = Theta.map' $ theta @a

instance HasTheta a => HasTheta (Maybe a) where
  theta = Theta.optional' $ theta @a
