{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Compile Theta schemas to Python classes that can
-- serialize/deserialize to Avro.
module Theta.Target.Python
  ( Python (..)
  , python

  , toModule

  , toReference
  , toRecord
  , toVariant
  )
where

import           Control.Monad.Except            (MonadError)

import qualified Data.Aeson                      as Aeson
import qualified Data.Avro                       as Avro
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.Foldable                   as Foldable
import           Data.List.NonEmpty              (NonEmpty ((:|)))
import qualified Data.List.NonEmpty              as NonEmpty
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Text

import qualified Theta.Error                     as Theta
import qualified Theta.Name                      as Name
import           Theta.Pretty                    (p)
import qualified Theta.Pretty                    as Theta
import qualified Theta.Types                     as Theta

import           Theta.Target.Avro.Types         (toSchema)
import           Theta.Target.Python.QuasiQuoter (Python (..), python)

-- * Python Types

-- | Compile a Theta module to a Python module.
toModule :: MonadError Theta.Error m
         => Theta.Module
         -> Maybe Python
         -- ^ The package path to use when importing other
         -- Theta-generated modules.
         -> m Python
toModule Theta.Module {..} packagePath = do
  definitions <- mapM (toDefinition moduleName) types
  let definitionLines = toDefinitions $ Foldable.toList definitions
  pure [python|
    from abc import ABC
    from dataclasses import dataclass
    from datetime import date, datetime
    import json
    from typing import Any, ClassVar, Dict, Iterator, List, Mapping, Optional

    from theta import avro, container

    $importLines

    $definitionLines
  |]
  where importLines =
          toLines [ qualifiedImport moduleName packagePath
                  | Theta.Module { Theta.moduleName } <- imports
                  ]

        qualifiedImport moduleName packagePath = case packagePath of
          Nothing   -> [python|import $qualified|]
          Just path ->
            let prefixed = path <> "." <> qualified in
              [python|import $prefixed as $qualified|]
          where qualified = Python $ Name.renderModuleName moduleName

-- | Return a Python snippet that /refers/ to the given Theta type.
--
-- For primitive types, this returns the equivalent Python type.
--
-- Containers use the equivalent Python container with the type
-- parameter converted the same way.
--
-- Named types (references, records, variants and newtypes) are
-- referred to by name, ignoring namespaces.
toReference :: Name.ModuleName -> Theta.Type -> Python
toReference currentModule Theta.Type { Theta.baseType } = case baseType of

  -- primitive types
  Theta.Bool'           -> [python|bool|]
  Theta.Bytes'          -> [python|bytes|]
  Theta.Int'            -> [python|int|]
  Theta.Long'           -> [python|int|]
  Theta.Float'          -> [python|float|]
  Theta.Double'         -> [python|float|]
  Theta.String'         -> [python|str|]
  Theta.Date'           -> [python|date|]
  Theta.Datetime'       -> [python|datetime|]

  -- containers
  Theta.Array' a        ->
    let items = toReference currentModule a
    in [python|List[$items]|]
  Theta.Map' a          ->
    let values = toReference currentModule a
    in [python|Mapping[str, $values]|]
  Theta.Optional' a     ->
    let type_ = toReference currentModule a
    in [python|Optional[$type_]|]

  -- named types
  Theta.Reference' name -> toIdentifier currentModule name
  Theta.Record' name _  -> toIdentifier currentModule name
  Theta.Variant' name _ -> toIdentifier currentModule name
  Theta.Newtype' name _ -> toIdentifier currentModule name

-- | Return a Python snippet that defines a type with the given name
-- corresponding to the given Theta type.
--
-- For primitive types, containers and newtypes, this produces a type
-- synonym in Python:
--
-- @
-- Quantity = int
-- @
--
-- For records and variants, this produces Python class definitions
-- corresponding to the Theta type.
toDefinition :: MonadError Theta.Error m
             => Name.ModuleName
             -> Theta.Definition Theta.Type
             -> m Python
toDefinition currentModule definition =
  case Theta.baseType $ Theta.definitionType definition of
    -- structured types
    Theta.Record' name fields   -> do
      schema <- toSchema definition
      toRecord currentModule schema name fields
    Theta.Variant' name cases   -> do
      schema <- toSchema definition
      toVariant currentModule schema name cases
    Theta.Newtype' _ underlying ->
      -- treat newtypes the same as aliases
      toDefinition currentModule $ definition { Theta.definitionType = underlying }

    -- everything else becomes a type alias
    _ -> let reference  =
               toReference currentModule $ Theta.definitionType definition
             identifier =
               toIdentifier currentModule $ Theta.definitionName definition
         in pure [python|$identifier = '$reference'|]

-- | Compile a Theta record to a Python dataclass.
toRecord :: MonadError Theta.Error m
         => Name.ModuleName
         -- ^ The name of the module that we're generating Python code
         -- for.
         -> Avro.Schema
         -- ^ The Avro schema for this type.
         -> Name.Name
         -> Theta.Fields Theta.Type
         -> m Python
toRecord currentModule schema recordName Theta.Fields { Theta.fields } = do
  let avroSchema = schemaLiteral schema

  fieldEncodings <- case fields of
    [] -> pure [python|pass|]
    _  -> toLines <$> mapM avroEncoding fields
  let decode = decodingFunction currentModule . Theta.fieldType
  fieldDecodings <- toList <$> mapM decode fields

  pure [python|
    @dataclass
    class $name:
        avro_schema: ClassVar[Dict[str, Any]] = $avroSchema

        $fieldDeclarations

        def encode_avro(self, encoder: avro.Encoder):
            $fieldEncodings

        def to_avro(self, out):
            self.encode_avro(avro.Encoder(out))

        @staticmethod
        def decode_avro(decoder: avro.Decoder):
            return $name($fieldDecodings)

        @staticmethod
        def from_avro(in_):
            return $name.decode_avro(avro.Decoder(in_))

        @staticmethod
        def write_container(objects: List['$name'], out,
                            codec: str="deflate", sync_marker: Optional[bytes]=None):
            encoder = avro.Encoder(out)
            container.encode_container(encoder, objects, codec, sync_marker, $name)

        @staticmethod
        def read_container(in_) -> Iterator['$name']:
            decoder = avro.Decoder(in_)
            return container.decode_container(decoder, $name)
  |]
  where name = toIdentifier currentModule recordName
        fieldDeclarations = toLines $ fieldDeclaration currentModule <$> fields

-- | Declare a field and its type in the style used by dataclasses.
--
-- @
-- foo: int
-- @
fieldDeclaration :: Name.ModuleName -> Theta.Field Theta.Type -> Python
fieldDeclaration currentModule Theta.Field { Theta.fieldName, Theta.fieldType } =
  [python|$name: '$reference'|]
  where name      = toFieldName fieldName
        reference = toReference currentModule fieldType

-- | Compile a Theta variant to a set of Python classes.
toVariant :: MonadError Theta.Error m
          => Name.ModuleName
          -- ^ The name of the module that we're generating Python code
          -- for.
          -> Avro.Schema
          -- ^ The Avro schema for this variant.
          -> Name.Name
          -> NonEmpty (Theta.Case Theta.Type)
          -> m Python
toVariant currentModule schema variantName cases = do
  let avroSchema = schemaLiteral schema

  caseClasses <- toDefinitions <$> mapM toClass (zip [0..] $ NonEmpty.toList cases)

  pure [python|
    class $variantIdentifier(ABC):
        avro_schema: ClassVar[Dict[str, Any]] = $avroSchema

        @staticmethod
        def decode_avro(decoder: avro.Decoder):
            tag = decoder.integral()

            $caseBranches
            else:
                raise Exception(f"Invalid tag for variant: {tag}.")

        @staticmethod
        def from_avro(in_):
            return $variantIdentifier.decode_avro(avro.Decoder(in_))

        @staticmethod
        def write_container(objects: List['$variantIdentifier'], out,
                            codec: str="deflate", sync_marker: Optional[bytes]=None):
            encoder = avro.Encoder(out)
            container.encode_container(encoder, objects, codec, sync_marker, $variantIdentifier)

        @staticmethod
        def read_container(in_) -> Iterator['$variantIdentifier']:
            decoder = avro.Decoder(in_)
            return container.decode_container(decoder, $variantIdentifier)

    $caseClasses
  |]
  where variantIdentifier = toIdentifier currentModule variantName

        toClass (n, Theta.Case { Theta.caseName, Theta.caseParameters }) = do
          fieldEncodings <- case fields of
            [] -> pure [python|pass|]
            _  -> toLines <$> mapM avroEncoding fields

          let decode = decodingFunction currentModule . Theta.fieldType
          fieldDecodings <- toList <$> mapM decode fields

          pure [python|
             @dataclass
             class $caseIdentifier($variantIdentifier):
                 $fieldDeclarations

                 @staticmethod
                 def decode_avro(decoder: avro.Decoder):
                     return $caseIdentifier($fieldDecodings)

                 def encode_avro(self, encoder: avro.Encoder):
                     # Tag
                     encoder.integral($tag)

                     # Record
                     $fieldEncodings

                 def to_avro(self, out):
                     self.encode_avro(avro.Encoder(out))
          |]
          where caseIdentifier = toIdentifier currentModule caseName

                fieldDeclarations =
                  toLines $ fieldDeclaration currentModule <$> fields
                fields = Theta.fields caseParameters

                tag = Python $ Text.pack $ show n

        caseBranches = go cases
          where go (case_ :| cases) =
                  toLines $ ifCase case_ : (elifCase <$> zip [1..] cases)

                ifCase Theta.Case { Theta.caseName } =
                  let identifier = toIdentifier currentModule caseName in
                    [python|
                      if tag == 0:
                          return $identifier.decode_avro(decoder)
                    |]

                elifCase (n, Theta.Case { Theta.caseName }) =
                  let tag = Python $ Text.pack $ show n
                      identifier = toIdentifier currentModule caseName
                  in
                    [python|
                      elif tag == $tag:
                          return $identifier.decode_avro(decoder)
                    |]


-- * Errors

-- | Errors specific to the Python 3 code generation.
data PythonError = NonExistentType Name.Name
                 -- ^ The type with the given name does not exist.
  deriving (Show)

instance Theta.Pretty PythonError where
  pretty = \case
    NonExistentType name ->
      [p| The name ‘#{Theta.pretty name}’ is not defined. |]

-- | Throw a Python 3 error.
throw :: MonadError Theta.Error m => PythonError -> m a
throw = Theta.throw "Python 3"

-- * Avro-Specific Definitions

-- ** Schemas

-- | Returns a Python expression that evaluates to a loaded JSON
-- object representing the given Avro schema.
--
-- @
-- json.loads('''{...}''')
-- @
schemaLiteral :: Avro.Schema -> Python
schemaLiteral schema = [python|json.loads('''$avroLiteral''')|]
  where avroLiteral = Python $ Text.replace "\\" "\\\\" avroJson
        avroJson    = Text.decodeUtf8 $ LBS.toStrict $ Aeson.encode schema

-- ** Encoding

-- | Return an expression that encodes a field with the given name and
-- Theta type to Avro.
--
-- This assumes the Python identifier @encoder@ is in scope and has
-- the right type.
avroEncoding :: MonadError Theta.Error m => Theta.Field Theta.Type -> m Python
avroEncoding (Theta.Field (toFieldName -> name) _ type_) = do
  encoding <- encodingFunction type_
  pure [python|($encoding)(self.$name)|]

-- | Return a Python function that encodes the given type. This will
-- either be a lambda or an expression (ie @encoder.bool@). Since this
-- might be a lambda, it could require extra parentheses in some
-- contexts.
--
-- This assumes the Python identifier @encoder@ is in scope and has
-- the right type.
encodingFunction :: MonadError Theta.Error m => Theta.Type ->  m Python
encodingFunction Theta.Type { Theta.baseType, Theta.module_ } = case baseType of

  -- Primitive Types
  Theta.Bool'     -> pure [python|encoder.bool|]
  Theta.Bytes'    -> pure [python|encoder.bytes|]
  Theta.Int'      -> pure [python|encoder.integral|]
  Theta.Long'     -> pure [python|encoder.integral|]
  Theta.Float'    -> pure [python|encoder.float|]
  Theta.Double'   -> pure [python|encoder.double|]
  Theta.String'   -> pure [python|encoder.string|]
  Theta.Date'     -> pure [python|encoder.date|]
  Theta.Datetime' -> pure [python|encoder.datetime|]

  -- Containers
  Theta.Array' type_    -> do
    elementFunction <- encodingFunction type_
    pure [python|
      lambda array: encoder.array(array, $elementFunction)
    |]
  Theta.Map' type_      -> do
    elementFunction <- encodingFunction type_
    pure [python|
      lambda map: encoder.map(map, $elementFunction)
    |]
  Theta.Optional' type_ -> do
    elementFunction <- encodingFunction type_
    pure [python|
      lambda optional: encoder.optional(optional, $elementFunction)
    |]

  -- Named Types
  Theta.Record' _ _  ->
    pure [python|
      lambda record: record.encode_avro(encoder)
    |]
  Theta.Variant' _ _ ->
    pure [python|
      lambda record: record.encode_avro(encoder)
    |]

  -- References and Newtypes
  Theta.Newtype' _ type_ -> encodingFunction type_
  Theta.Reference' name  -> case Theta.lookupName name module_ of
    Left _      -> throw $ NonExistentType name
    Right type_ -> encodingFunction type_

-- ** Decoding

-- | Return a Python function that decodes a value of the given
-- type.
--
-- This assumes the Python identifier @decoder@ is in scope and has
-- the right type.
decodingFunction :: MonadError Theta.Error m
                 => Name.ModuleName
                 -> Theta.Type
                 -> m Python
decodingFunction currentModule Theta.Type { Theta.baseType, Theta.module_ } =
  case baseType of
    -- Primitive Types
    Theta.Bool'     -> pure [python|decoder.bool()|]
    Theta.Bytes'    -> pure [python|decoder.bytes()|]
    Theta.Int'      -> pure [python|decoder.integral()|]
    Theta.Long'     -> pure [python|decoder.integral()|]
    Theta.Float'    -> pure [python|decoder.float()|]
    Theta.Double'   -> pure [python|decoder.double()|]
    Theta.String'   -> pure [python|decoder.string()|]
    Theta.Date'     -> pure [python|decoder.date()|]
    Theta.Datetime' -> pure [python|decoder.datetime()|]

    -- Containers
    Theta.Array' type_ -> do
      elementFunction <- decodingFunction currentModule type_
      pure [python|decoder.array(lambda: $elementFunction)|]
    Theta.Map' type_ -> do
      elementFunction <- decodingFunction currentModule type_
      pure [python|decoder.map(lambda: $elementFunction)|]
    Theta.Optional' type_ -> do
      elementFunction <- decodingFunction currentModule type_
      pure [python|decoder.optional(lambda: $elementFunction)|]

    -- Named Types
    Theta.Record' (toIdentifier currentModule -> name) _ ->
      pure [python|$name.decode_avro(decoder)|]
    Theta.Variant' (toIdentifier currentModule -> name) _ ->
      pure [python|$name.decode_avro(decoder)|]

    -- References and Newtypes
    Theta.Newtype' _ type_ -> decodingFunction currentModule type_
    Theta.Reference' name  -> case Theta.lookupName name module_ of
      Left _      -> throw $ NonExistentType name
      Right type_ -> decodingFunction currentModule type_

-- * Python Syntax

-- | Compile a list of 'Python' snippets into a single Python
-- expression made up of a comma-separated list with no trailing
-- comma.
--
-- This function is great for arguments to functions/methods, tuples
-- and lists.
--
-- @
-- toList [[python|x|], [python|x + 1|], [python|x + 2|]]
-- @
--
-- produces:
--
-- @
-- x, x + 1, x + 2
-- @
toList :: [Python] -> Python
toList []     = [python||]
toList [x]    = [python|$x|]
toList (x:xs) = let rest = toList xs in [python|$x, $rest|]

-- | Compile a list of 'Python' snippets into a single Python block
-- with one snippet per line.
--
-- @
-- toLines [[python|self.foo = foo|], [python|self.bar = bar|]]
-- @
--
-- produces:
--
-- @
-- self.foo = foo
-- self.bar = bar
-- @
toLines :: [Python] -> Python
toLines []     = [python||]
toLines (l:ls) = let rest = toLines ls in [python|
  $l
  $rest
  |]

-- | Compile a list of 'Python' snippets into a block of code with
-- multiple definitions. Each definition is separated by blank lines.
--
-- This is useful for combining multiple class or method definitions
-- into a single snippet.
--
-- @
-- f1 = [python|
--  def foo(x):
--    return x
--  |]
-- f2 = [python|
--  def bar(x):
--    return x
--
-- toList [f1, f2]
-- @
--
-- compiles to:
--
-- @
-- def foo(x):
--   return x
--
-- def bar(x):
--   return x
-- @
toDefinitions :: [Python] -> Python
toDefinitions []     = [python||]
toDefinitions [l]    = [python|$l|]
toDefinitions (l:ls) = let rest = toDefinitions ls in [python|
  $l

  $rest
  |]

-- | Convert a 'Name.Name' to a 'Python' identifier.
--
-- If the name is from the /current/ module, it is turned into an
-- unqualified name. If the name is /imported/, it is rendered as
-- /fully qualified/.
--
-- Currently this ignores the Theta namespace and transcribes the name
-- directly to Python.
--
-- @
-- > toIdentifier "example" "example.Foo"
-- Python "Foo"
-- > toIdentifier "example" "example.foo.Foo"
-- Python "example.foo.Foo"
-- @
toIdentifier :: Name.ModuleName
             -- ^ What module we're generating code for, so that we
             -- know whether to use qualified names or not.
             -> Name.Name
             -- ^ The name we're converting to Python.
             -> Python
toIdentifier currentModule name
  | Name.moduleName name == currentModule =
    Python $ Name.name name
  | otherwise =
    Python $ Name.render name

-- | Convert a 'Theta.FieldName' to the corresponding 'Python' field
-- name.
toFieldName :: Theta.FieldName -> Python
toFieldName (Theta.FieldName name) = Python name
