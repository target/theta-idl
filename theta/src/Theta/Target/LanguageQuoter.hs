{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
-- | This module contains a quasiquoter that lets us do string
-- interpolation in a way that works well for generating code in
-- different programming languages (like Python and Kotlin).
--
-- Each quasiquoter lets us interpolate text values (@$foo@ will
-- interpolate the Haskell variable @foo@), with a few simple rules to
-- manage indentation intelligently:
--
--  * leading and trailing blank lines are ignored completely
--    (including for applying the rest of these rules)
--
--  * leading indentation is ignored—you can indent the internals of a
--    @[python| |]@ block as part of your code layout without
--    affecting the semantics of the generated Python code
--
--  * interpolating a multi-line snippet in a variable will indent
--    every line to the same level
--
-- This lets us define a multi-line method, include it in a class body
-- and have all the indentation work out correctly:
--
-- @
-- class' :: Python
-- class' = [python|
--   class Fooable:
--     $method
--   |]
--  where method = [python|
--      def foo():
--        print("foo")
--        print ("bar")
--     |]
-- @
--
-- This produces the following Python snippet:
--
-- @
-- class Fooable:
--   def foo():
--     print("foo")
--     print("bar")
-- @
module Theta.Target.LanguageQuoter where

import           Data.Char                 (isSpace)
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Void                 (Void)

import           Language.Haskell.TH       (Name, mkName)
import           Language.Haskell.TH.Lib   (appTypeE, conT, integerL, litE,
                                            stringL, varE)
import           Language.Haskell.TH.Quote (QuasiQuoter (..))

import           Text.Megaparsec           (Parsec, anySingleBut, eof,
                                            errorBundlePretty, many,
                                            notFollowedBy, parse, some, try,
                                            (<?>), (<|>))
import           Text.Megaparsec.Char      (alphaNumChar, char, letterChar,
                                            string)

-- | The quasiquoter can use types that are isomorphic to 'Text'. This
-- is intended for language-specific types:
--
-- @
-- newtype Python = Python { fromPython :: Text }
--
-- instance Interpolable Python where
--   toText = fromPython
--   fromText = Python
-- @
class Interpolable a where
  toText   :: a -> Text
  fromText :: Text -> a

-- | Generate a quoter. This needs a specific 'Interpolable' type
-- specified, which you can do with @VisibleTypeApplication@:
--
-- @
-- python :: QuasiQuoter
-- python = quoter ''Python
-- @
quoter :: Name -> QuasiQuoter
quoter typeName = QuasiQuoter
  { quoteExp  = interpolate . adjustIndentation . stripBlanks . lines
  , quotePat  = unsupported
  , quoteType = unsupported
  , quoteDec  = unsupported
  }
  where unsupported =
          error "This quasiquoter can only be used in an expression context."

        interpolate = combine . map (interpolateLine . parseLine)

        from = appTypeE [e| fromText |] [t| $(conT typeName) |]

        combine []    = [e| $(from) (Text.pack "") |]
        combine lines =
          [e| $(from) $ Text.pack $(foldr1 (\ a b -> [e| $(a) <> "\n" <> $(b) |]) lines) |]

        interpolateLine = \case
          Line level tokens                 ->
            [e| indentBy $(litE $ integerL level) $(go tokens) |]
          where go []                     = [e| "" |]
                go (Variable name : rest) = [e| $(var name) <> $(go rest) |]
                go (Literal text : rest)  = [e| $(litE $ stringL text) <> $(go rest) |]

        var name = [e| Text.unpack (toText $(varE name)) |]

        adjustIndentation lines = drop indentationLevel <$> lines
          where indentationLevel = minimum $ length . takeWhile isSpace <$> ignoreBlanks lines
                ignoreBlanks = filter (not . all isSpace)

        stripBlanks [] = []
        stripBlanks (x : xs)
          | all isSpace x = stripLastLine xs
          | otherwise     = stripLastLine (x : xs)

        stripLastLine [] = []
        stripLastLine [x]
          | all isSpace x = []
          | otherwise     = [x]
        stripLastLine (x : xs) = x : stripLastLine xs

-- | A line fed to the quasiquoter is a mix of literal text and
-- interpolated variables (@$variableName@). Leading spaces are
-- stripped and counted.
data Line = Line Integer [Token] deriving (Show, Eq)

-- | A token is part of a 'Line'—either literal text or an
-- interpolated variable.
data Token = Literal String
           | Variable Name deriving (Show, Eq)

-- | Parses a string (which should be a single line) into a 'Line'—a
-- series of 'Token's with the initial whitespace trimmed off and
-- counted.
--
-- If there is a parse error of some kind—like an invalid name being
-- interpolated—the function will fail with an exception.
parseLine :: String -> Line
parseLine str = case parse (line <* eof) "[python| |]" str of
  Right res -> res
  Left  err -> error $ errorBundlePretty err
  where line :: Parsec Void String Line
        line = do
          indentationLevel <- fromIntegral . length <$> many (char ' ')
          tokens <- many token
          pure $ Line indentationLevel tokens

        token = try variable <|> literal

        variable = do
          char '$' <* notFollowedBy (char '$')
          first <- letterChar
          rest  <- many (alphaNumChar <|> char '_')
          pure $ Variable $ mkName $ first : rest

        literal = Literal <$> some (escapedDollar <|> (anySingleBut '$' <?> ""))
        escapedDollar = ('$' <$ string "$$") <?> "variable name (escape ‘$’ with ‘$$’)"

-- | Indent every line in the given string using the specified number
-- of spaces.
indentBy :: Int -> String -> String
indentBy level str = case lines str of
  []    -> ""
  lines -> foldr1 (\ a b -> a <> "\n" <> b) $ indent <$> lines
  where indent line
          | all isSpace line = line
          | otherwise        = replicate level ' ' <> line
