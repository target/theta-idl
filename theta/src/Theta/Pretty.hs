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
  , indentBy

  , pr
  , p
  )
where

import           Data.Text                 (Text)
import qualified Data.Text                 as Text

import           Language.Haskell.TH.Quote (QuasiQuoter)

import qualified Data.Char                 as Char
import qualified PyF

class Pretty a where
  pretty :: a -> Text

instance Pretty Text where pretty x = x

deriving via ShowPretty Word instance Pretty Word

-- | A quasiquoter for interpolating text, good for writing 'Pretty'
-- instances/etc.
--
-- At the moment there is no 'Pretty'-specific support, but that is
-- likely to change in the future.
--
-- Leading whitespace is trimmed from each line of the quoted
-- string. The first line is ignored if it is all whitespace.
--
-- __Example__
--
-- @
-- let name = "com.example.Name"
--     moduleName = "com.example"
-- in [pr|
--      {pretty name} is not defined in the module {pretty moduleName}.
--    |]
-- @
--
-- would produce:
--
-- @
-- "com.example.Name is not defined in the module com.example.\n"
-- @
pr :: QuasiQuoter
pr = PyF.fmtTrim

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
prettyList = Text.intercalate "\n" . map (("• " <>) . pretty)

-- | Indent every line in the given string using the specified number
-- of spaces.
indentBy :: Int -> Text -> Text
indentBy level str = case Text.lines str of
  []    -> ""
  lines -> foldr1 (\ a b -> a <> "\n" <> b) $ indent <$> lines
  where indent line
          | Text.all Char.isSpace line = line
          | otherwise                  = Text.replicate level " " <> line

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
