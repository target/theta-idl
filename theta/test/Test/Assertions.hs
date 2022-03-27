{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- | Helper functions for testing: assertions/etc with human-readable
-- diff outputs.
module Test.Assertions where

import           Control.Monad                 (when)

import qualified Data.Algorithm.Diff           as Diff
import qualified Data.Algorithm.DiffOutput     as Diff
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Data.TreeDiff                 (Edit, EditExpr, Expr (App), GToExpr,
                                                ToExpr (..), exprDiff,
                                                genericToExpr,
                                                ppEditExprCompact, ppExpr)
import           Data.TreeDiff.Pretty          (Pretty (..), ppEditExpr)

import           GHC.Generics                  (Generic, Rep)

import           Prettyprinter                 hiding (Pretty)
import           Prettyprinter.Render.Terminal (AnsiStyle,
                                                Color (Green, Red, White),
                                                color, colorDull)

import qualified Test.Tasty.HUnit              as HUnit

import           Data.Time                     (TimeOfDay)
import           Theta.Target.LanguageQuoter   (Interpolable, toText)

-- * Human-Readable Diffs

-- ** Algebraic Data Types

-- | Compare two values, failing with a human-readable diff if they
-- differ.
--
-- This should work for (most?) types with 'Generic' instances.
assertDiff :: (Eq a, Generic a, GToExpr (Rep a), MonadFail m) => a -> a -> m ()
assertDiff a b = when (a /= b) $
  let diff = exprDiff (genericToExpr a) (genericToExpr b) in
  fail $ show $ prettyEditExpr diff

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
