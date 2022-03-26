{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}

module Theta.Target.Rust
  ( Rust(..)
  , rust

  , toEnum
  , toFile
  , toModule
  , toDefinition
  , toReference
  , toRecord
  , toVariant
  , toNewtype

  , refersTo
  )
where

import           Prelude                       hiding (toEnum)

import           Control.Monad.State           (evalState, get, modify)

import           Data.List.NonEmpty            (NonEmpty)
import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import qualified Data.Text                     as Text
import           Data.Tree                     (Tree (..))

import           Text.Printf                   (printf)

import           Theta.Name                    (Name)
import qualified Theta.Name                    as Name
import qualified Theta.Types                   as Theta

import           Theta.Target.Haskell          (disambiguateConstructors)

import qualified Theta.Primitive               as Theta
import           Theta.Target.Rust.QuasiQuoter (Rust (..), rust)

-- | Given a set of modules, generate a self-contained Rust file with
-- definitions for every single Theta type as well as the imports and
-- settings needed to build the module.
--
-- You can pipe this into a @foo.rs@ file and include it in your Rust
-- build with no additional changes.
toFile :: [Theta.Module] -> Rust
toFile modules = definitionLines $ attrs : (go [] <$> hierarchy)
  where attrs = [rust|
          #![allow(non_snake_case)]
          #![allow(unused_imports)]
          |]

        -- Create a nested module hierarchy based on the namespaces of
        -- all the modules we are working with
        go soFar (Node part children) = [rust|
          pub mod $baseName {
              $supers
              $definitions
          }
          |]
          where definitions = content <> "\n\n" <> definitionLines subModules

                subModules = go (soFar <> [part]) <$> children

                -- Ensure all of our top-level modules are in scope
                supers = statements [ [rust|use super::$root|] | root <- moduleRoots]
                moduleRoots = map Rust $ Set.toList $ Set.fromList
                  [ Name.moduleRoot $ Theta.moduleName m | m <- modules ]

                content = maybe "" toModule (Map.lookup name moduleMap)

                name     = Name.fromModuleParts $ soFar <> [part]
                baseName = Rust $ Name.baseName name

        hierarchy = Name.moduleHierarchy $ Theta.moduleName <$> modules
        moduleMap = Map.fromList [(Theta.moduleName m, m) | m <- modules]

-- | Compile an entire Theta module to a Rust module. This generates
-- definitions for every type in a Theta module as well as all the
-- needed imports.
toModule :: Theta.Module -> Rust
toModule Theta.Module { Theta.types } = definitionLines $ imports : typeDefinitions
  where typeDefinitions = toDefinition <$> Map.elems types

        imports = [rust|
          use chrono::{Date, DateTime, NaiveTime, Utc};
          use std::collections::HashMap;
          use theta::avro::{FromAvro, ToAvro};
          use nom::{IResult, Err, error::{context, ErrorKind}};
          use uuid::{Uuid};
        |]

-- | Return a Rust snippet that defines a type for the given Theta
-- type definition.
--
-- Primitive types and containers get compiled to aliases in Rust:
--
-- @
-- type Foo = i32;
-- @
--
-- Records, variants and newtypes get compiled as documented in their
-- respective functions:
--
--  * 'toRecord'
--  * 'toVariant'
--  * 'toNewtype'
toDefinition :: Theta.Definition Theta.Type -> Rust
toDefinition Theta.Definition {..} = case Theta.baseType definitionType of
  -- structured types
  Theta.Enum' name symbols  -> toEnum name symbols
  Theta.Record' name fields -> toRecord name fields
  Theta.Variant' name cases -> toVariant name cases
  Theta.Newtype' name type_ -> toNewtype name type_

  -- everything else (aliases)
  _ -> let name = ident definitionName
           reference = toReference definitionType
       in [rust|pub type $name = $reference;|]

-- | Return a Rust snippet that /refers/ to the given Theta type.
--
-- For primitive types, this returns the equivalent Rust
-- type. Containers use the corresponding Rust container with its type
-- parameter converted the same way.
--
-- Records, variants, newtypes and references are referred to by name,
-- ignoring namespaces.
toReference :: Theta.Type -> Rust
toReference type_@Theta.Type { Theta.baseType } = case baseType of
  -- special cases for primitive types
  Theta.Primitive' Theta.Bytes    -> [rust|Vec<u8>|]
  Theta.Primitive' Theta.Date     -> [rust|Date<Utc>|]
  Theta.Primitive' Theta.Datetime -> [rust|DateTime<Utc>|]

  -- containers
  Theta.Array' a    -> let ref = toReference a in [rust|Vec<$ref>|]
  Theta.Map' a      -> let ref = toReference a in [rust|HashMap<String, $ref>|]
  Theta.Optional' a -> let ref = toReference a in [rust|Option<$ref>|]

  -- all other types are referenced directly by name
  _                 -> path $ typeIdentifier type_

-- | Return a list of identifiers that corresponds to the "base" Rust
-- type for a Theta type.
--
-- For most types, this is the name of the type: @i32@ is @["i32"]@.
--
-- For types with generics, this is the name *without* generics. Both
-- @Vec<u8>@ and @Vec<i32>@ become @["Vec"]@.
--
-- For names in Theta, this is the /fully qualified/ name of the
-- type. @com.example.Foo@ becomes @["com", "example", "Foo"]@.
typeIdentifier :: Theta.Type -> [Rust]
typeIdentifier Theta.Type { Theta.baseType } = case baseType of
  -- primitive types
  Theta.Primitive' p -> case p of
    Theta.Bool     -> ["bool"]
    Theta.Bytes    -> ["Vec"]
    Theta.Int      -> ["i32"]
    Theta.Long     -> ["i64"]
    Theta.Float    -> ["f32"]
    Theta.Double   -> ["f64"]
    Theta.String   -> ["String"]
    Theta.Date     -> ["Date"]
    Theta.Datetime -> ["DateTime"]
    Theta.UUID     -> ["Uuid"]
    Theta.Time     -> ["NaiveTime"]

  -- containers
  Theta.Array' _        -> ["Vec"]
  Theta.Map' _          -> ["HashMap"]
  Theta.Optional' _     -> ["Option"]
  Theta.Reference' name -> qualifiedIdent name

  -- structured types
  Theta.Enum' name _    -> qualifiedIdent name
  Theta.Record' name _  -> qualifiedIdent name
  Theta.Variant' name _ -> qualifiedIdent name
  Theta.Newtype' name _ -> qualifiedIdent name

-- | Compile a Theta enum to a Rust enum.
--
-- Theta enums can be encoded as Rust enums with the same name
-- disambiguation rules as Haskell. (See 'disambiguateConstructors'
-- for details.)
--
-- @
-- type Foo = Bar | baz | _Baz
-- @
--
-- gives us the Rust type:
--
-- @
-- #[derive(Clone, Debug, PartialEq)]
-- enum Foo {
--   Bar,
--   Baz,
--   Baz_,
-- }
-- @
--
-- along with several traits implementations:
--
-- @
-- impl ToAvro for Foo {
--   fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
--     match self {
--       Foo::Bar  => 0i64.to_avro_buffer(buffer),
--       Foo::Baz  => 1i64.to_avro_buffer(buffer),
--       Foo::Baz_ => 2i64.to_avro_buffer(buffer),
--     }
--   }
-- }
-- @
--
-- @
-- impl FromAvro for Foo {
--   fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
--     context("test.Foo", |input| {
--       let (input, tag) = i64::from_avro(input)?;
--       match tag {
--         0 => Ok((input, Foo::Bar)),
--         1 => Ok((input, Foo::Baz)),
--         2 => Ok((input, Foo::Baz_)),
--         _ => Err(Err::Error((input, ErrorKind::Tag))),
--       }
--     })(input)
--   }
-- }
-- @
toEnum :: Name -> NonEmpty Theta.EnumSymbol -> Rust
toEnum name symbols = [rust|
  #[derive($defaultDerives)]
  pub enum $typeName {
      $cases
  }

  $implToAvro

  $implFromAvro
  |]
  where typeName = ident name

        cases = commaLines (fst <$> constructors)

        implToAvro = toAvro typeName [rust|
          match self {
            $toAvroBranches
          }
        |]
        toAvroBranches = commaLines $ toAvroBranch <$> constructors
        toAvroBranch (constructor, i_) = [rust|
          $typeName::$constructor => $i.to_avro_buffer(buffer)
        |]
          where i = Rust $ Text.pack $ printf "%di64" i_

        implFromAvro = fromAvro typeName $ wrapContext name [rust|
          let (input, tag) = i64::from_avro(input)?;
          match tag {
            $fromAvroBranches
            _ => Err(Err::Error((input, ErrorKind::Tag))),
          }
        |]
        fromAvroBranches = commaLines $ fromAvroBranch <$> constructors
        fromAvroBranch (constructor, i_) = [rust|
          $i => Ok((input, $typeName::$constructor))
        |]
          where i = Rust $ Text.pack $ printf "%d" i_

        constructors = [ (Rust constructor, i)
                       | constructor <- disambiguateConstructors name symbols
                       | i <- [0::Int ..]
                       ]

-- | Compile a Theta record to a Rust struct.
--
-- This is pretty much a 1:1 translation—life is easy since Rust
-- supports algebraic data types. The following Theta record:
--
-- @
-- type Foo = { bar : Int
--            , baz : [Double]
--            }
-- @
--
-- gets compiled to the following Rust struct:
--
-- @
-- #[derive(Clone, Debug, PartialEq)]
-- pub struct Foo {
--   pub foo: i32,
--   pub baz: Vec<f64>,
-- }
-- @
--
-- Records also have trait implementation to go to/from Avro:
--
-- @
-- impl ToAvro for Foo {
--   fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
--     self.foo.to_avro_buffer(buffer);
--     self.baz.to_avro_buffer(buffer);
--   }
-- }
--
-- impl FromAvro for Foo {
--   fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
--     context("Foo", |input| {
--       let (input, foo) = i32::from_avro(input)?;
--       let (input, baz) = Vec::<f64>::from_avro(input)?;
--       Ok((input, Foo { foo, baz }))
--     })(input)
--   }
-- }
-- @
--
-- In order to handle recursive types, a field will be boxed if its
-- type refers to this type directly or indirectly. (See 'refersTo'
-- for more details.) This means the field will be wrapped in a @Box@,
-- which will also need to happen in the @FromAvro@ implementation:
--
-- @
-- type Recursive = Recursive { recurse: Recursive }
--
-- impl ToAvro for Recursive { ... }
--
-- impl FromAvro for Recursive {
--   fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
--     context("Recursive", |input| {
--       let (input, recursive) = Recursive::from_avro(input)?;
--       Ok((input, Recursive { recursive: Box::new(recursive) }))
--     })(input)
--   }
-- }
-- @
toRecord :: Name -> Theta.Fields Theta.Type -> Rust
toRecord name Theta.Fields { Theta.fields } = [rust|
  #[derive($defaultDerives)]
  pub struct $typeName {
      $structFields
  }

  $implToAvro

  $implFromAvro
  |]
  where typeName     = ident name
        structFields = commaLines $ toField name Pub <$> fields

        implToAvro = toAvro typeName $ statements $
          callToAvro . fieldAccess . Theta.fieldName <$> fields
        fieldAccess (fieldIdent -> fieldName) = [rust|self.$fieldName|]

        implFromAvro = fromAvro typeName $ wrapContext name $
          statements [ callFromAvro (fieldIdent fieldName) fieldType |
            Theta.Field { Theta.fieldName, Theta.fieldType } <- fields ]
          <> "\n" <> [rust|Ok((input, $struct))|]
        struct = structExpr name [typeName] fields

-- | Compile a Theta variant to a Rust enum.
--
-- This is pretty much a 1:1 translation—life is easy since Rust
-- supports algebraic data types. The following Theta variant:
--
-- @
-- type Foo = Bar { a : Int, b : [Double] }
--          | Baz { a : Int }
-- @
--
-- gets compiled to the following Rust enum:
--
-- @
-- #[derive(Clone, Debug, PartialEq)]
-- pub enum Foo {
--   Bar {
--     a: i32,
--     b: Vec<f64>,
--   },
--   Baz {
--     a: i32,
--   },
-- }
-- @
--
-- In order to handle recursive types, a field will be boxed if its
-- type refers to this type directly or indirectly. (See 'isRecursive'
-- for more details.)
--
-- Each type also implements several traits.
--
-- @ToAvro@:
--
-- @
-- impl ToAvro for Foo {
--   fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
--     match self {
--       Foo::Bar { a, b } => {
--         0i64.to_avro_buffer(buffer);
--         a.to_avro_buffer(buffer);
--         b.to_avro_buffer(buffer);
--       },
--       Foo::Baz { a } => {
--         1i64.to_avro_buffer(buffer);
--         a.to_avro_buffer(buffer);
--       }
--     }
--   }
-- }
-- @
--
-- @FromAvro@:
--
-- @
-- impl FromAvro for Foo {
--   fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
--     let (input, tag) = i64::from_avro(input)?;
--     match tag {
--       0 => {
--         let (input, a) = i32::from_avro(input)?;
--         let (input, b) = Vec::from_avro(input)?;
--         Ok((input, Foo::Bar { a, b }))
--       },
--       1 => {
--         let (input, a) = i32::from_avro(input)?;
--         Ok((input, Foo::Baz { a }))
--       },
--       _ => Err(Err::Error((input, ErrorKind::Tag))),
--     }
--   }
-- }
-- @
--
-- Just like structs, recursive fields in variants need to be boxed in
-- the @FromAvro@ implementation:
--
-- @
-- impl FromAvro for Recursive {
--   ...
--      Ok((input, Recursive::A { recursive: Box::new(recursive) }))
--   ...
-- }
-- @
toVariant :: Name -> NonEmpty (Theta.Case Theta.Type) -> Rust
toVariant name (NonEmpty.toList -> cases) = [rust|
  #[derive($defaultDerives)]
  pub enum $typeName {
      $variantCases
  }

  $implToAvro

  $implFromAvro
  |]
  where typeName = ident name

        variantCases = commaLines $ toCase <$> cases
        toCase Theta.Case { Theta.caseName, Theta.caseParameters } = [rust|
        $name {
          $fields
        }
        |]
          where name   = ident caseName
                fields = commaLines [ toField caseName Inherited type_
                                    | type_ <- Theta.fields caseParameters ]

        implToAvro = toAvro typeName [rust|$match;|]
          where match = toAvroMatch name cases

        implFromAvro = fromAvro typeName $ wrapContext name $
          parseTag <> fromAvroMatch name cases
          where parseTag = "let (input, tag) = i64::from_avro(input)?;\n"

-- | The default set of derives we use for every generated public type
-- definition.
--
-- Currently: @Clone, Debug, PartialEq@
defaultDerives :: Rust
defaultDerives = commaList ["Clone", "Debug", "PartialEq"]

-- | Builds the big @match@ expression inside a variant's
-- @to_avro_buffer@ implementation.
--
-- @
-- match self {
--   Foo::Bar { a, b } => {
--     0i64.to_avro_buffer(buffer);
--     a.to_avro_buffer(buffer);
--     b.to_avro_buffer(buffer);
--   },
--   Foo::Baz { a } => {
--     1i64.to_avro_buffer(buffer);
--     a.to_avro_buffer(buffer);
--   }
-- }
-- @
toAvroMatch :: Name -> [Theta.Case Theta.Type] -> Rust
toAvroMatch name cases = [rust|
  match self {
    $branches
  }
  |]
  where branches = commaLines $ branch <$> zip [0..] cases
        branch (i, Theta.Case { Theta.caseName, Theta.caseParameters }) = [rust|
          $constructor { $fields } => {
              $caseBlock
          }
          |]
          where constructor = path [ident name, ident caseName]
                fields      = commaList $
                  fieldIdent . Theta.fieldName <$> Theta.fields caseParameters

                caseBlock = [rust|
                  $tag.to_avro_buffer(buffer);
                  $fieldsToAvro
                  |]
                tag = Rust $ Text.pack (show i) <> "i64"
                fieldsToAvro = statements $ toAvro <$> Theta.fields caseParameters
                toAvro (fieldIdent . Theta.fieldName -> field) =
                  [rust|$field.to_avro_buffer(buffer)|]

-- | Builds the big @match@ expression inside a variant's @from_avro@
-- implementation.
--
-- @
-- match tag {
--   0 => {
--     let (input, a) = i32::from_avro(input)?;
--     let (input, b) = Vec::from_avro(input)?;
--     Ok((input, Foo::Bar { a, b }))
--   },
--   1 => {
--     let (input, a) = i32::from_avro(input)?;
--     Ok((input, Foo::Baz { a }))
--   },
--   _ => Err(Err::Error((input, ErrorKind::Tag))),
-- }
-- @
fromAvroMatch :: Name -> [Theta.Case Theta.Type] -> Rust
fromAvroMatch name cases = [rust|
  match tag {
    $branches
    _ => Err(Err::Error((input, ErrorKind::Tag))),
  }
  |]
  where branches = commaLines $ branch <$> zip [0..] cases
        branch (i, Theta.Case { Theta.caseName, Theta.caseParameters }) = [rust|
          $tag => {
              $fromAvros
              $output
          }
        |]
          where tag = Rust $ Text.pack $ show i

                fields    = Theta.fields caseParameters
                fromAvros = statements
                  [ callFromAvro (fieldIdent fieldName) fieldType
                  | Theta.Field { Theta.fieldName, Theta.fieldType } <- fields ]

                output       = [rust|Ok((input, $outputStruct))|]
                outputStruct = structExpr name [ident name, ident caseName] fields

-- | Convert a Theta newtype to a Rust struct. The struct will have a
-- single value with no field name.
--
-- The following Theta newtype:
--
-- @
-- type Foo = Int
-- @
--
-- gets compiled to the following Rust enum:
--
-- @
-- #[derive(Clone, Debug, PartialEq)]
-- pub struct Foo(i32)
-- @
toNewtype :: Name -> Theta.Type -> Rust
toNewtype name type_ = [rust|
  #[derive($derives)]
  pub struct $typeName(pub $typeReference);

  $implToAvro

  $implFromAvro
  |]
  where typeName      = ident name
        typeReference = toReference type_

        derives
          | copyable  = "Copy, " <> defaultDerives
          | otherwise = defaultDerives

        copyable = Theta.isPrimitive underlying
                && underlying /= Theta.string'
                && underlying /= Theta.bytes'
        underlying = Theta.underlyingType type_

        implToAvro = toAvro typeName [rust|self.0.to_avro_buffer(buffer);|]

        implFromAvro = fromAvro typeName $ wrapContext name $ parse <> ";\n" <> output
        parse  = callFromAvro "value" type_
        output = [rust|Ok((input, $typeName(value)))|]

-- | Wrap a parser body (ie body of a @from_avro@ method) in a
-- @context@ for better error messages.
--
-- This assumes the parser body uses the variable name @input@ for the
-- parser's input.
--
-- @
-- context("Name", |input| {
--   <parser body>
-- })(input)
-- @
wrapContext :: Name -> Rust -> Rust
wrapContext (Rust . Name.render -> name) body = [rust|
  context("$name", |input| {
      $body
  })(input)
  |]

-- | An @impl@ of the @ToAvro@ trait for the specified type, with the
-- given body in the @to_avro_buffer@ function.
--
-- @
-- impl ToAvro for Type {
--   fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
--     <body>
--   }
-- }
-- @
toAvro :: Rust -> Rust -> Rust
toAvro typeName body = [rust|
  impl ToAvro for $typeName {
      fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
          $body
      }
  }
  |]

-- | Call the @to_avro_buffer@ method on the given base expression.
--
-- @
-- base.to_avro_buffer(buffer);
-- @
callToAvro :: Rust -> Rust
callToAvro base = [rust|$base.to_avro_buffer(buffer)|]

-- | An @impl@ of the @FromAvro@ trait for the specified types, with
-- the given body in the @from_avro@ method.
fromAvro :: Rust -> Rust -> Rust
fromAvro typeName body = [rust|
  impl FromAvro for $typeName {
      fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
          $body
      }
  }
  |]

-- | Call the @from_avro@ method on the field with the given name and
-- type.
--
-- @
-- let (input, foo) = i32::from_avro(input)?
-- @
--
-- To handle name conflicts, a field called @input@ will be turned
-- into @__input__@:
--
-- @
-- let (input, __input__) = i32::from_avro(input)?
-- @
--
-- This same convention is used in @fieldExpr@, so the final
-- expression will match:
--
-- @
-- Foo { bar, input: __input__ }
-- @
callFromAvro :: Rust -> Theta.Type -> Rust
callFromAvro name fieldType = [rust|
  let (input, $result) = $fromAvro(input)?
  |]
  where result   = disambiguate name
        fromAvro = path $ typeIdentifier fieldType <> ["from_avro"]

        disambiguate "input" = "__input__"
        disambiguate ident   = ident

-- | Corresponds to either a @pub@ visibility annotation or no
-- annotation in Rust.
data Visibility = Pub | Inherited deriving (Show, Eq)

-- | Convert a 'Theta.Field' to a corresponding 'Rust.StructField',
-- which can be used to build struct and enum definitions.
--
-- A Theta struct field @foo: Int@ will turn into the following Rust:
--
-- @
--   pub foo: i32,
-- @
--
-- If a field has a type that refers to its enclosing struct or
-- enum—directly or indirectly—the field is boxed:
--
-- @
--   pub foo: Box<Foo>,
-- @
toField :: Name
           -- ^ The name of the type that immediately contains this
           -- field. This is used to check for recursive types and box
           -- fields as appropriate.
        -> Visibility
           -- ^ The visibility of the field. Should be @"pub"@ for
           -- structs and @""@ for variants.
        -> Theta.Field Theta.Type
        -> Rust
toField containing visibility Theta.Field { Theta.fieldName, Theta.fieldType } =
   case visibility of
    Pub       -> [rust|pub $name: $rustType|]
    Inherited -> [rust|$name: $rustType|]
  where name = fieldIdent fieldName
        rustType
          | fieldType `refersTo` containing = [rust|Box<$reference>|]
          | otherwise                       = reference
        reference = toReference fieldType

-- | Convert a 'Theta.Name' to a Rust identifier.
--
-- This ignores namespaces. If you need all the components of a fully
-- qualified name in Rust, use `qualifiedIdent`.
--
-- @
-- > ident "com.example.Foo"
-- "Foo"
-- @
ident :: Name -> Rust
ident = Rust . Name.name

-- | Return the /fully qualified/ version of a Theta 'Name'.
--
-- @
-- > qualifiedIdent "com.example.Foo"
-- ["com", "example", "Foo"]
-- @
qualifiedIdent :: Name -> [Rust]
qualifiedIdent = map Rust . Name.parts

-- | Create a Rust identifier corresponding to the given field's name.
fieldIdent :: Theta.FieldName -> Rust
fieldIdent (Theta.FieldName name) = Rust name

-- | Wrap a series of identifiers into an absolute qualified path.
--
-- @
-- path ["foo", "bar", "baz"]
-- @
--
-- compiles to
--
-- @
-- foo::bar::baz
-- @
path :: [Rust] -> Rust
path = Rust . Text.intercalate "::" . map fromRust

-- | Generate a literal struct expression for the given type.
--
-- @
-- Foo { foo, bar }
-- @
--
-- This assumes that variables with the same name as each of the
-- struct's fields are in scope.
--
-- To handle name conflicts in the generated @from_avro@ method,
-- there's a special case if the field is named @input@:
--
-- @
-- Foo { foo, input: __input__ }
-- @
--
-- This conflict needs to be handled the same way in any function
-- calling `structExpr`.
--
-- Recursive fields are also a special case. Since recursive fields
-- need to be boxed, they need to be wrapped in a @Box::new@:
--
-- @
-- Recursive { recursive: Box::new(recursive) }
-- @
structExpr :: Name
           -- ^ The name of the Theta type that contains all these
           -- fields (used to check if any fields are recursive).
           -> [Rust]
           -- ^ Components of the struct's Rust name.
           -> [Theta.Field Theta.Type]
           -- ^ The fields themselves.
           -> Rust
structExpr containing names fields = [rust|$fullName { $rustFields }|]
  where fullName = path names
        rustFields = commaList $ field <$> fields

        field Theta.Field { Theta.fieldName, Theta.fieldType }
          | fieldType `refersTo` containing = case fieldIdentifier of
              "input" -> [rust|input: Box::new(__input__)|]
              _       -> [rust|$fieldIdentifier: Box::new($fieldIdentifier)|]
          | otherwise                       = case fieldIdentifier of
              "input" -> [rust|input: __input__|]
              _       -> [rust|$fieldIdentifier: $fieldIdentifier|]

          where fieldIdentifier = fieldIdent fieldName

-- | A comma-separated list of the given Rust fragments.
--
-- @
-- commaList ["a", "b"]
-- @
--
-- produces
--
-- @
-- a, b
-- @
commaList :: [Rust] -> Rust
commaList = Rust . Text.intercalate ", " . map fromRust

-- | Given a list of 'Rust' statements, produces a single Rust
-- fragement with each statement on a line, followed by a
-- semicolon. Note that there is no trailing newline added.
--
-- Produces an empty code fragment (no trailing semicolon) if the list
-- is empty.
--
-- @
-- statements ["let x = 10", "x"]
-- @
--
-- produces
--
-- @
-- let x = 10;
-- x;
-- @
--
-- Note that this does not wrap the block in { and }!
statements :: [Rust] -> Rust
statements []    = ""
statements lines = Rust $
  Text.intercalate ";\n" (fromRust <$> lines) <> ";"

-- | A list of lines, each of which ends with a comma—like field
-- declarations in a struct definition. Matching common Rust
-- conventions, this includes a trailing comma (but no newline) unless
-- the list is empty.
--
-- @
-- commaLines [[rust|x: i32|], [rust|y: i32|]]
-- @
--
-- produces
--
-- @
-- x: i32,
-- y: i32,
-- @
commaLines :: [Rust] -> Rust
commaLines []    = ""
commaLines lines = Rust $
  Text.intercalate ",\n" (fromRust <$> lines) <> ","

-- | Join together definitions on separate lines, with one line of
-- whitespace between each.
--
-- @
-- definitionLines ["some_definition_a", "some_definition_b"]
-- @
--
-- produces
--
-- @
-- some_definition_a
--
-- some_definition_b
-- @
definitionLines :: [Rust] -> Rust
definitionLines = Rust . Text.intercalate "\n\n" . map fromRust

-- | Returns whether the given `Theta.Type` refers to a type with the
-- given name anywhere in its transitive closure.
--
-- A type refers to a name if any of the following is true:
--
--  1. It is a reference to the given name.
--
--  2. It is a structure type with the given name.
--
--  3. It is a reference to, newtype of a type that itself refers to
--  the given name.
--
--  4. It is an option, containing a type that refers to the given
--  name.
--
--  5. It is a structured type that contains any field whose type
--  refers to the given name.
--
-- Maps and arrays do not count as referring to types becuase they are
-- already on the heap and do not need to be boxed.
--
-- Note how cases 3–5 are recursive, traversing down the entire type.
refersTo :: Theta.Type -> Name -> Bool
refersTo t@Theta.Type { Theta.module_ } name =
  evalState (go t) Set.empty
  where go t = case Theta.baseType t of
          -- references and newtypes
          Theta.Reference' name'
            | name' == name -> pure True
            | otherwise     -> case Theta.lookupName name' module_ of
                Right type_ -> go type_
                Left _err   -> pure False
          Theta.Newtype' _ type_ -> go type_

          -- containers
          Theta.Array' _        -> pure False -- already on heap
          Theta.Map' _          -> pure False -- already on heap
          Theta.Optional' type_ -> go type_

          -- structured types
          Theta.Enum' name' _ -> whenUnseen name' $ pure (name == name')

          Theta.Record' name' fields
            | name' == name -> pure True
            | otherwise     -> whenUnseen name' $ fieldsReference fields

          Theta.Variant' name' cases
            | name' == name           -> pure True
            | name `elem` names cases -> pure True
            | otherwise               -> whenUnseen name' $
              or <$> mapM fieldsReference (Theta.caseParameters <$> cases)

          -- primitive types can never refer to another type
          Theta.Primitive' _ -> pure False

        names cases = Theta.caseName <$> cases

        fieldsReference Theta.Fields { Theta.fields } =
          or <$> mapM go (Theta.fieldType <$> fields)

        whenUnseen name doThis = do
          seen <- get
          if Set.member name seen
            then pure False
            else modify (Set.insert name) *> doThis
