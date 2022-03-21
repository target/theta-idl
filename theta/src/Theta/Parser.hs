{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}

module Theta.Parser where

import           Control.Monad              (void, when)
import           Control.Monad.Reader       (ReaderT)
import qualified Control.Monad.Reader       as Reader
import           Control.Monad.Trans        (lift)

import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NonEmpty
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Versions              (semver')
import           Data.Void                  (Void)

import           GHC.Exts                   (fromList)

import           Prelude                    hiding (map)

import           Text.Megaparsec            (MonadParsec (eof, lookAhead, notFollowedBy, takeWhileP, try),
                                             Parsec, anySingle, anySingleBut,
                                             between, choice, many, manyTill,
                                             option, optional, sepBy, sepBy1,
                                             skipMany, some, (<|>))
import           Text.Megaparsec.Char       (alphaNumChar, char, letterChar,
                                             space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

import           Theta.Metadata             (Metadata, Version (..))
import qualified Theta.Metadata             as Metadata
import qualified Theta.Name                 as Name
import           Theta.Pretty               (p)
import           Theta.Primitive            (definedIn, primitiveKeyword,
                                             primitives)
import           Theta.Types                (BaseType (..), BaseType' (..),
                                             Case (..), Definition (..),
                                             Doc (Doc), EnumSymbol (EnumSymbol),
                                             Field (..), FieldName (FieldName),
                                             Fields, HasDoc, Statement (..),
                                             setDoc, wrapFields)

-- | Parsers for the Theta syntax outside the metadata section take
-- the 'Metadata' for the module as an input. This allows parsing
-- behavior to change based on the language-version set for the
-- module.
--
-- To run a parser likes this, you should first parse the metadata
-- section then pass that to the rest with `runReaderT`:
--
-- @
-- metadata <- parse metadataSection
-- body     <- parse (runReaderT moduleBody metadata)
-- ...
-- @
--
-- This design allows us to check the versions and raise an
-- appropriate error before parsing the rest of the module.
--
-- @
-- metadata <- parse metadataSection
-- checkVersions metadata
-- body     <- parse (runReaderT moduleBody metadata)
-- @
type Parser = ReaderT Metadata (Parsec Void Text)

-- * Language Version

-- | Return the language-version set for the current module in the
-- module's metadata section.
version :: Parser Version
version = Reader.asks Metadata.languageVersion

-- | Check whether the version of the module is at least the specified
-- minimum version.
--
-- If the version of the module is too low, raises a parse error with
-- an error message explaining that the named feature is not
-- supported.
requireVersion :: Version
               -- ^ The minimum version (inclusive) that supports the
               -- named feature.
               -> Text
               -- ^ The name of the feature used for the user-facing
               -- error message.
               -> Parser ()
requireVersion minVersion feature = do
  v <- version
  when (v < minVersion) $ fail $ versionError feature minVersion v

-- | Returns a human-readable error message when a feature is not
-- supported by the language-version of the module being parsed.
versionError :: Text
             -- ^ The name of the feature that isn't supported.
             -> Version
             -- ^ The minimum version the feature requries.
             -> Version
             -- ^ The version of the current module.
             -> String
versionError feature minVersion v = [p|
    Support for #{feature} requires language-version ≥ #{minVersion}
    Current module being parsed has language-version = #{v}
  |]

-- * Metadata

-- | One of the entries we expect in a metadata section.
data MetadataEntry = LanguageVersion Version
                   -- ^ "language-version: 1.2.0"
                   | AvroVersion Version
                   -- ^ "avro-version: 1.2.0"

-- | The metadata section defines parsers for the top part of a Theta
-- module which specifies the version of the language and encoding the
-- schema relies on.
--
-- The metadata section currently requires two entries ("avro-version"
-- and "language-version") in any order, followed by "---":
--
-- @
-- language-version: 1.2.0
-- avro-version: 1.3.2
-- ---
-- @
metadataSection :: Name.ModuleName -> Parsec Void Text Metadata
metadataSection moduleName = do
  void whitespace
  first    <- languageVersion <|> avroVersion
  case first of
    LanguageVersion language -> do
      AvroVersion avro <- avroVersion
      pure $ Metadata.Metadata language avro moduleName
    AvroVersion avro         -> do
      LanguageVersion language <- languageVersion
      pure $ Metadata.Metadata language avro moduleName
  where avroVersion     =
          symbol "avro-version" *> symbol ":" *> (AvroVersion <$> version)
        languageVersion =
          symbol "language-version" *> symbol ":" *> (LanguageVersion <$> version)

        version = (Version <$> semver') <* whitespace

        symbol     = L.symbol whitespace
        whitespace =
          L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

-- * Lexing

-- | This parser ignores whitespace and comments.
--
-- Theta currently supports classic C-style comment syntax:
--
--  * @//@ for line comments
--  * @/* */@ for block comments
--
-- Theta also supports documentation comments using
-- Javadocs/Doxygen-style syntax:
--
--  * @///@ for line documentation comments
--  * @/** */@ for block documentation comments
--
-- Documentation comments are not ignored by the parser, and a
-- documentation comment not immediately beofre a type definition,
-- variant case or record field will result in a parse error.
whitespace :: Parser ()
whitespace = L.space space1 (try lineComment) (try blockComment)
  where lineComment  = do
          string "//"
          notFollowedBy (char '/')
          void $ takeWhileP (Just "character") (/= '\n')
        blockComment = do
          string "/*"
          notFollowedBy (char '*')
          void $ manyTill anySingle (string "*/")

-- | Wraps a parser into a parser that ignores trailing whitespace and
-- comments.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

-- | Parses a symbol with exactly the given text.
--
-- @symbol "foo"@ would parse exactly @"foo"@.
symbol :: Text -> Parser Text
symbol = L.symbol whitespace

-- | Parses the given keyword.
--
-- A keyword is a specific alphanumeric string that cannot be followed
-- by alphanumeric characters or underscores.
--
-- @keyword "foo"@ would parse @"foo"@ but not @"foobar"@.
keyword :: Text -> Parser Text
keyword word = lexeme $ string word <* notFollowedBy (alphaNumChar <|> char '_')

reservedWords :: [(Version, [Text])]
reservedWords = [ ("1.0.0", v1_0_0)
                , ("1.1.0", v1_1_0)
                ]
  where v1_0_0 = [ "import"
                 , "type"
                 , "alias"

                 -- primitive types
                 , "Bool"
                 , "Bytes"
                 , "Date"
                 , "Datetime"
                 , "Int"
                 , "Long"
                 , "Float"
                 , "Double"
                 , "String"
                 ]

        v1_1_0 = [ "enum" ]

identifier :: Parser Text
identifier = do
  let first = letterChar <|> char '_'
      rest  = many $ alphaNumChar <|> char '_'
  ident <- Text.pack <$> lexeme ((:) <$> first <*> rest)

  v <- version
  let reserved = concatMap snd $ takeWhile (\ (v', _) -> v' <= v) reservedWords

  when (ident `elem` reserved) $
    fail $ "Keyword " <> Text.unpack ident <> " cannot be an identifier."

  pure ident

-- | Parse a name.
--
-- Names in a module can always have a namespace specified explicitly.
--
-- If the namespace is not specified explicitly, it is inferred as the
-- name of the current module.
name :: Parser Name.Name
name = do
  components <- identifier `sepBy1` symbol "."
  moduleName <- case components of
    []  -> error "sepBy1 returned an empty list!"
    [_] -> Reader.asks Metadata.moduleName
    xs  -> pure $ fromList $ init xs
  pure $ Name.Name { Name.moduleName, Name.name = last components }

-- | The name of a module which is one or more identifiers separated
-- by dots (.).
moduleIdentifier :: Parser Name.ModuleName
moduleIdentifier = do
  components <- identifier `sepBy1` symbol "."
  pure $ Name.ModuleName
    { Name.baseName  = last components
    , Name.namespace = init components
    }
    -- init and last are safe here because sepBy1 never returns an
    -- empty list

-- | Parse a documentation comment. This will only be used in front of
-- a type definition, variant case or record field.
--
-- Docs have syntax inspired by Javadocs/Doxygen:
--
-- @
-- /** Anything in this block comment is a doc
--     Leading whitespace is ignored.
--  */
--
-- /** Leading * in block comments are also ignored
--  *  as far as documentation is concerned.
--  */
--
-- /// Any consecutive comment lines starting with ///
-- /// make up a single doc comment
-- ///    leading whitespace is ignored, but linebreaks
-- ///    are preserved
-- @
withDoc :: HasDoc a => Parser a -> Parser a
withDoc parser = do
  d <- optional docComment
  setDoc d <$> parser
  where docComment = Doc . Text.strip <$> lexeme (blockDoc <|> lineDocs)

        blockDoc = do
          string "/**"
          lines <- blockLine `sepBy` char '\n'
          string "*/"
          pure $ Text.strip $ Text.unlines lines

        blockLine = do
          optional (try leadingStar)
          chars <- manyTill (anySingleBut '\n') $ lookAhead endLine
          pure $ Text.strip $ Text.pack chars
        endLine = string "\n" <|> string "*/"
        leadingStar =
          skipMany space1 *> char '*' *> notFollowedBy (char '/')

        lineDocs = Text.unlines <$> some (Text.strip <$> lineDoc)
        lineDoc = lexeme $
          string "///" *> takeWhileP (Just "character") (/= '\n')

-- * Types

-- | Parse a keyword for a primitive type.
primitive :: Parser BaseType'
primitive = do
  v <- version
  choice [wrap t <$ try (keyword $ primitiveKeyword t) | t <- supportedAt v]
  where wrap = BaseType' . Primitive'

        supportedAt v = [t | t <- primitives, definedIn t <= v]

-- | Parses a reference to some other named type.
reference :: Parser BaseType'
reference = BaseType' . Reference' <$> name

-- ** Containers

-- | Parses an array type which is any "atomic" type inside square
-- braces ([]).
--
-- @
-- [Int]
-- [[Foo]]
-- @
array :: Parser BaseType'
array = BaseType' . Array' <$> between (symbol "[") (symbol "]") signature'

-- | Parse a map type which is any "atomic" type inside curly braces
-- ({}).
map :: Parser BaseType'
map = BaseType' . Map' <$> between (symbol "{") (symbol "}") signature'

-- | Parses types that are optional (nullable). Nullable types are
-- denoted by a trailing "?", like @Int?@ or @[String]?@.
optional_ :: Parser BaseType'
optional_ = BaseType' . Optional' <$> atom <* symbol "?"

-- ** Composite Types

atom :: Parser BaseType'
atom = array
   <|> map
   <|> try primitive
   <|> reference

-- | Parses types that can be used in signatures. This includes
-- everything except for complex, structured declarations like records
-- and variants.
signature' :: Parser BaseType'
signature' = try optional_ <|> atom

-- | Parses a field definition:
--
-- @
-- user_id : Long
-- @
--
-- Fields can optionally have a leading documentation comment:
--
-- @
-- /// Unique user ID
-- user_id : Long
-- @
field :: Parser (Field BaseType')
field = withDoc $ do
  fieldName <- FieldName <$> identifier
  symbol ":"
  fieldType <- signature'
  pure $ Field { fieldName, fieldDoc = Nothing, fieldType }

recordBody :: Parser (Fields BaseType')
recordBody = wrapFields <$> fields
  where fields = between (symbol "{") (symbol "}") $ field `sepBy` symbol ","

-- | Each branch of a variant defines its own record.
--
-- @
-- type Foo = Bar { a : Int }
--          | Baz { a : String, b : Long }
--          | Qux {} // no arguments
-- @
--
-- We can also define a constructor with no fields by omitting the
-- braces:
--
-- @
-- type NodeColor = Red {} // explicit empty record
--                | Black  // implicit syntax
-- @
--
-- There is an exception for variants with a single constructor because
--
-- @
-- type Foo = Bar
-- @
--
-- has two possible interpretations:
--
--  1. a newtype @Foo@ over an existing type @Bar@
--
--  2. a variant @Foo@ with a single constructor @Bar@
--
-- We go with option 1 (defining a newtype) because that maintains
-- backwards compatibility and seems more natural. If you actually
-- want a variant with a single field and no arguments, use the
-- explicit syntax:
--
-- @
-- type Foo = Bar {}
-- @
variantBody :: Parser (NonEmpty (Case BaseType'))
variantBody = try cases <|> singleCase
  where singleCase = do
          case' <- withDoc $ do
            caseName <- name
            caseParameters <- recordBody
            pure $ Case { caseName, caseDoc = Nothing, caseParameters }
          pure $ case' :| []

        cases = do
          let case' = withDoc $ do
                caseName <- name
                caseParameters <- option [] recordBody
                pure $ Case { caseName, caseDoc = Nothing, caseParameters }

          firstCase <- case'
          rest      <- some (symbol "|" *> case')
          pure $ firstCase :| rest

-- | Parses a type definition:
--
-- @
-- type TCIN = String
-- @
--
-- @
-- type TransferOrder = {
--   item : TCIN,
--   quantity : Int
-- }
-- @
--
-- @
-- type OrderLevel = OTL { level : Int }
--                 | OpOTL { level : Int, point : Int }
-- @
--
-- Type definitions can optionally have documentation attached:
--
-- @
-- /// A TCIN is an item identifier at both Target stores
-- /// and Target.com.
-- type TCIN = String
-- @
definition :: Parser (Definition BaseType')
definition = do
  void $ symbol "type"
  definitionName <- name

  void $ symbol "="

  let variant  = Variant' definitionName <$> variantBody
      record   = Record'  definitionName <$> recordBody
      newtype_ = Newtype' definitionName <$> signature'
  type_ <- try variant <|> try record <|> newtype_

  pure Definition
    { definitionName
    , definitionDoc  = Nothing
    , definitionType = BaseType' type_
    }

-- | Parse an enum definition:
--
-- @
-- enum Suit = Spades | Hearts | Clubs | Diamonds
-- @
--
-- Just like type definitions, enums can have documentation attached:
--
-- @
-- /// Cards can have one of four suits
-- enum Suit = Spades | Hearts | Clubs | Diamonds
-- @
--
-- Enums are supported with language-version ≥ 1.1.0
enumDefinition :: Parser (Definition BaseType')
enumDefinition = do
  void $ symbol "enum"
  requireVersion "1.1.0" "enum"
  definitionName <- name
  void $ symbol "="
  symbols <- enumSymbol `sepBy1` symbol "|"
  let enum = Enum' definitionName $ NonEmpty.fromList symbols
  pure $ Definition { definitionDoc = Nothing
                    , definitionName
                    , definitionType = BaseType' enum
                    }
  where enumSymbol = EnumSymbol . Text.pack <$> lexeme ((:) <$> first <*> rest)
        first = letterChar <|> char '_'
        rest  = many $ alphaNumChar <|> char '_'

-- | Parses a type alias.
--
-- @
-- alias ReferenceId = String
--
-- alias DemandForecast = {[Probabiliy]}
-- @
--
-- Unlike a type definition, an alias does not create a new, distinct
-- type—it just lets us refer to an existing type with a new name. In
-- Haskell, the difference is that a type definition compiles to a
-- newtype or data declaration while an alias compiles to a type
-- synonym.
--
-- Aliases work for primitive types, containers (arrays, maps,
-- optionals) and names of types (including other aliases). They do
-- not work for records and variants directly, so the following will
-- not parse:
--
-- @
-- alias Foo = { a : Int, b : Int }
--
-- alias Bar = A {} | B {}
-- @
alias :: Parser (Definition BaseType')
alias = do
  void $ symbol "alias"
  definitionName <- name
  void $ symbol "="
  definitionType <- signature'
  pure Definition { definitionDoc = Nothing, definitionName, definitionType }

statement :: Parser Statement
statement = definitionStatement <|> importStatement
  where definitionStatement =
          DefinitionStatement <$> withDoc (definition <|> alias <|> enumDefinition)
        importStatement =
          ImportStatement <$> import_

-- * Modules and Imports

-- | Parses an import statement:
--
-- @
-- import com.target.foo
-- @
import_ :: Parser Name.ModuleName
import_ = try (symbol "import") *> moduleIdentifier

-- | Parses an entire module, including the metadata section, and
-- returns every Theta statement (import or definition) in the
-- module's body.
moduleBody :: Name.ModuleName -> Parser [Statement]
moduleBody moduleName =
  lift (metadataSection moduleName) *> symbol "---" *> body <* eof
  where body = statement `sepBy` whitespace
