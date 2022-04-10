{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
-- | This module defines a 'Pretty' class that translates types to
-- user-friendly 'Text' representations.
--
-- Any user-facing text (like error messages) should use this class to
-- display values in a consistent way.
module Theta.Pretty
  ( Pretty(..)

  , ShowPretty(..)
  , showPretty

  , prettyList

  , p
  , pr
  )
where

import           Data.Text                 (Text)
import qualified Data.Text                 as Text

import           Data.String.Interpolate   (__i)

import           Language.Haskell.TH.Quote (QuasiQuoter)

class Pretty a where
  pretty :: a -> Text

instance Pretty Text where pretty x = x

deriving via ShowPretty Word instance Pretty Word

-- | A quasiquoter for interpolating text for 'Pretty' instances.
--
-- For now this just uses '__i' from @string-interpolate@, but it
-- might change to something Theta-specific in the future.
p :: QuasiQuoter
p = __i

-- | Version of 'p' with a name that doesn't conflict with Template
-- Haskell.
pr :: QuasiQuoter
pr = p

-- | The same as 'pretty' but returns a 'String'.
showPretty :: Pretty a => a -> String
showPretty = Text.unpack . pretty

-- | Render a list of items as a bulleted list, with one item per
-- line.
--
-- Example: @showPretty ["abc", "def", "ghi"]@
--
-- @
--   • abc
--   • def
--   • ghi
-- @
prettyList :: Pretty a => [a] -> Text
prettyList = Text.intercalate "\n" . map (bullet . pretty)
  where bullet = ("  • " <>)

-- | A newtype with:
--
--   * a Show instance using 'showPretty' on the underlying type
--
--   * a Pretty instance using 'show' on the underlying type
--
-- You can use this with @DerivingVia@ to get a 'Show' instance for a
-- type which already has a 'Pretty' instance or vice-versa.
--
-- @
-- data MyType = MyType {...}
--   deriving Show via ShowPretty MyType
-- @
--
-- @
-- data MyType = MyType {...}
--   deriving Pretty via ShowPretty MyType
-- @
--
-- __Note__: doing this for /both/ 'Pretty' /and/ 'Show' on the same
-- type will lead to an infinite loop.
newtype ShowPretty a = ShowPretty a

instance Pretty a => Show (ShowPretty a) where
  show (ShowPretty x) = showPretty x

instance Show a => Pretty (ShowPretty a) where
  pretty (ShowPretty x) = Text.pack (show x)
