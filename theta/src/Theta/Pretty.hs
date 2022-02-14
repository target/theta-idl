-- | This module defines a 'Pretty' class that translates types to
-- user-friendly 'Text' representations.
--
-- Any user-facing text (like error messages) should use this class to
-- display values in a consistent way.
module Theta.Pretty
  ( Pretty(..)
  , ShowPretty(..)
  , showPretty
  , p
  )
where

import           Data.Text                 (Text)
import qualified Data.Text                 as Text

import           Data.String.Interpolate   (__i)

import           Language.Haskell.TH.Quote (QuasiQuoter)

class Pretty a where
  pretty :: a -> Text

-- | A quasiquoter for interpolating text for 'Pretty' instances.
--
-- For now this just uses '__i' from @string-interpolate@, but it
-- might change to something Theta-specific in the future.
p :: QuasiQuoter
p = __i

-- | The same as 'pretty' but returns a 'String'.
showPretty :: Pretty a => a -> String
showPretty = Text.unpack . pretty

-- | A newtype that has a Show instance using 'showPretty' on the
-- underlying type.
--
-- This is primarily designed for @DerivingVia@:
--
-- @
-- data MyType = MyType {...}
--   deriving Show via ShowPretty
-- @
newtype ShowPretty a = ShowPretty a

instance Pretty a => Show (ShowPretty a) where
  show (ShowPretty x) = showPretty x
