{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- | Helper functions for testing: assertions/etc with human-readable
-- diff outputs.
module Theta.Test.Assertions where

import           Control.Arrow                 (Arrow (first))
import           Control.Monad                 (when)

import qualified Data.Algorithm.Diff           as Diff
import qualified Data.Algorithm.DiffOutput     as Diff
import           Data.Foldable                 (Foldable (toList))
import           Data.Functor                  ((<&>))
import qualified Data.List                     as List
import           Data.Maybe                    (isJust)
import           Data.Sequence                 ((|>))
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Data.Time                     (LocalTime, TimeOfDay)
import           Data.TreeDiff                 (Edit (..), EditExpr (..),
                                                Expr (App), GToExpr,
                                                ToExpr (..), exprDiff,
                                                genericToExpr,
                                                ppEditExprCompact, ppExpr)
import qualified Data.TreeDiff.OMap            as OMap
import           Data.TreeDiff.Pretty          (Pretty (..), ppEditExpr)

import           GHC.Generics                  (Generic, Rep)

import           Prettyprinter                 hiding (Pretty)
import           Prettyprinter.Render.Terminal (AnsiStyle,
                                                Color (Green, Red, White),
                                                color, colorDull)

import           Text.Printf                   (printf)


import qualified Test.Tasty.HUnit              as HUnit

import           Theta.Target.LanguageQuoter   (Interpolable, toText)

-- * Human-Readable Diffs

-- ** Algebraic Data Types

-- | Compare two values, failing with a human-readable diff if they
-- differ.
--
-- This provides a better error message when used in QuickCheck than
-- failing normally.
--
-- This should work for (most?) types with 'Generic' instances.
assertDiff :: (Eq a, Generic a, GToExpr (Rep a), MonadFail m) => a -> a -> m ()
assertDiff a b = when (a /= b) $
  let diff = exprDiff (genericToExpr a) (genericToExpr b) in
  fail $ show $ prettyEditExpr diff

-- | Compare two values, failing with a __compact__ human-readable
-- diff if they differ.
assertCompactDiff :: (Eq a, Generic a, GToExpr (Rep a), MonadFail m) => a -> a -> m ()
assertCompactDiff a b = when (a /= b) $
  let diff = exprDiff (genericToExpr a) (genericToExpr b) in
  fail $ show $ prettyEditExprCompact diff

-- | Compare two values, failing with a human-readable diff if they
-- differ. The human-readable diff will include only the first part of
-- the two values that differs, with a trail of "breadcrumbs" to
-- specify the location.
--
-- Useful for comparing two large values that differ in small parts.
assertFirstDiff :: (Eq a, Generic a, GToExpr (Rep a), MonadFail m) => a -> a -> m ()
assertFirstDiff a b = when (a /= b) $ do
  let diff = exprDiff (genericToExpr a) (genericToExpr b)
  fail $ case firstDifference diff of
    Just (breadcrumbs, expr) ->
      printf "%s\n\n%s" (show breadcrumbs) (show $ prettyEditExpr expr)
    Nothing -> "<no difference>"


-- | Compare two algebraic data types, generating a human-readable
-- diff if they differ. This is the HUnit-specific version of
-- 'assertDiff'.
--
-- This should work for (most?) types with 'Generic' instances.
(?=:) :: (Eq a, Generic a, GToExpr (Rep a)) => a -> a -> HUnit.Assertion
a ?=: b = when (a /= b) $
  let diff = exprDiff (genericToExpr a) (genericToExpr b) in
  HUnit.assertFailure $ show $ prettyEditExpr diff

-- *** Diff Formatting

-- | Find the first part of an 'Edit EditExpr' that's different: an
-- insert, delete or swap but not a copy. This helps debug /really
-- large/ diffs where finding the difference in the formatted output
-- is difficult.
--
-- Return the part that is different along with a list of
-- human-readable breadcrumbs (constructor names, field names, list
-- indices) for locating the difference.
--
-- Returns 'Nothing' if the diff is entirely unchanged.
firstDifference :: Edit EditExpr -> Maybe ([String], Edit EditExpr)
firstDifference = fmap (first toList) . go []
  where go breadcrumbs edit = case edit of
          Ins{}  -> Just (breadcrumbs, edit)
          Del{}  -> Just (breadcrumbs, edit)
          Swp{}  -> Just (breadcrumbs, edit)

          Cpy ee -> case ee of
            EditApp constructor args   -> goValues (breadcrumbs |> constructor) args
            EditRec constructor fields -> goFields (breadcrumbs |> constructor) fields
            EditLst exps               -> goValues breadcrumbs exps
            EditExp{}                  -> Nothing

        goValues breadcrumbs args = do
          i                   <- List.findIndex isJust differences
          (breadcrumbs', exp) <- differences !! i
          pure ((breadcrumbs |> show i) <> breadcrumbs', exp)
          where differences = go breadcrumbs <$> args

        goFields breadcrumbs (OMap.toList -> fields) = do
          i <- List.findIndex isJust differences
          (breadcrumbs', exp) <- differences !! i
          pure (breadcrumbs <> breadcrumbs', exp)
            where differences = fields <&> \ (fieldName, fieldExp) ->
                    go (breadcrumbs |> fieldName) fieldExp


-- | The tree-diff library is designed to work with different
-- pretty-printing libraries. It does not have @prettyprinter@ support
-- built-in, so I'm adding it here.
prettyprinter :: Pretty (Doc AnsiStyle)
prettyprinter = Pretty
  { ppCon    = pretty
  , ppApp    = \ f xs -> group $ nest 2 $ vsep (f : xs)
  , ppRec    = record
  , ppLst    = group' lbracket rbracket
  , ppCpy    = annotate $ colorDull White
  , ppIns    = \ d -> annotate (color Green) $ "+" <> d
  , ppDel    = \ d -> annotate (color Red) $ "-" <> d
  , ppEdits  = sep
  , ppEllip  = "..."
  , ppParens = parens
  }
  where record constructor fields =
          group' (constructor <+> lbrace) rbrace (field <$> fields)
        field (fieldName, value) =
          pretty fieldName <+> fillSep [equals, value]

        group' l r xs =
          group $ vcat [nest 2 $ vcat [l, vsep (punctuate comma xs)], r]

-- | Pretty print an 'Expr' with the @prettyprinter@ package.
prettyExpr :: Expr -> Doc AnsiStyle
prettyExpr = ppExpr prettyprinter

-- | Pretty-print an 'Edit EditExpr' with the @prettyprinter@ package.
prettyEditExpr :: Edit EditExpr -> Doc AnsiStyle
prettyEditExpr = ppEditExpr prettyprinter

-- | Pretty-print an 'Edit EditExpr' with the @prettyprinter@ package,
-- in a more compact form.
prettyEditExprCompact :: Edit EditExpr -> Doc AnsiStyle
prettyEditExprCompact = ppEditExprCompact prettyprinter

-- ** Code (LanguageQuoter)

-- | Calculate a line-based diff between two values.
--
-- The same action (add/remove/change) on multiple subsequent lines
-- gets grouped into a single action covering all those lines.
diff :: Interpolable a => a -> a -> [Diff.Diff [Text]]
diff (toText -> a) (toText -> b) =
  Diff.getGroupedDiff (Text.lines $ Text.strip a) (Text.lines $ Text.strip b)

-- | Return a human-readable line-based diff between two values.
--
-- This is great for error messages and unit test failures.
humanDiff :: Interpolable a => a -> a -> Text
humanDiff a b = Text.pack $ Diff.ppDiff (toString <$> diff a b)
  where toString (Diff.First a)  = Diff.First  (Text.unpack <$> a)
        toString (Diff.Second b) = Diff.Second (Text.unpack <$> b)
        toString (Diff.Both a b) = Diff.Both   (Text.unpack <$> a) (Text.unpack <$> b)

-- | Assert that the two values are equal when converted to 'Text'
-- with leading/trailing whitespace trimmed.
--
-- If the assertion fails, include a human-readable diff in the error
-- message (see 'humanDiff').
(?=) :: (Interpolable a) => a -> a -> HUnit.Assertion
a ?= b = when (normalize a /= normalize b) $ do
  HUnit.assertFailure (Text.unpack $ humanDiff a b)
  where normalize = Text.strip . toText

-- ** Orphan Instances

instance ToExpr TimeOfDay where
  toExpr t = App "TimeOfDay" [toExpr $ show t]

instance ToExpr LocalTime where
  toExpr t = App "LocalTime" [toExpr $ show t]
