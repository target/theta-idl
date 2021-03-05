{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

module Theta.Target.Rust where

import           Data.Foldable               (toList)
import qualified Data.List                   as List
import           Data.List.NonEmpty          (NonEmpty)
import qualified Data.List.NonEmpty          as NonEmpty
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import qualified Data.Text                   as Text
import           Data.Tree                   (Tree (..))

import qualified Language.Rust.Data.Ident    as Rust
import qualified Language.Rust.Data.Position as Rust
import qualified Language.Rust.Quote         as Rust
import qualified Language.Rust.Syntax        as Rust

import           Theta.Name                  (Name)
import qualified Theta.Name                  as Name
import qualified Theta.Types                 as Theta

-- | Given a set of modules, generate a self-contained Rust file with
-- definitions for every single Theta type as well as the imports and
-- settings needed to build the module.
--
-- You can pipe this into a @foo.rs@ file and include it in your Rust
-- build with no additional changes.
toFile :: [Theta.Module] -> Rust.SourceFile ()
toFile modules = noSpan $ Rust.SourceFile Nothing attrs $ go [] <$> hierarchy
  where attrs = [ noSpan [Rust.attr| #![allow(non_snake_case)] |]
                , noSpan [Rust.attr| #![allow(unused_imports)] |]
                ]

        -- Create a nested module hierarchy based on the namespaces of
        -- all the modules we are working with
        go soFar (Node part children) = Rust.Mod [] Rust.PublicV baseName
          (Just $ supers <> content <> subModules) ()
          where subModules = go (soFar <> [part]) <$> children

                -- Ensure all of our top-level modules are in scope
                supers =
                  [ use $ path ["super", partIdent root] | root <- moduleRoots ]
                moduleRoots = toList $ Set.fromList
                  [ Name.moduleRoot $ Theta.moduleName m | m <- modules ]
                use path = Rust.Use [] Rust.InheritedV useTree ()
                  where useTree = Rust.UseTreeSimple path Nothing ()

                content = case Map.lookup name moduleMap of
                  Just module_ -> toModule module_
                  Nothing      -> []
                name    = Name.fromModuleParts $ soFar <> [part]
                baseName = partIdent $ Name.baseName name

                partIdent = Rust.mkIdent . Text.unpack


        hierarchy = Name.moduleHierarchy $ Theta.moduleName <$> modules
        moduleMap = Map.fromList [(Theta.moduleName m, m) | m <- modules]

-- | Compile an entire Theta module to a Rust module. This generates
-- definitions for every type in a Theta module, but does not generate
-- imports needed to bring external types into scope. To get fully
-- compilable code, you need to at least add:
--
-- @
-- use chrono;
-- use std::collections::HashMap;
-- @
toModule :: Theta.Module -> [Rust.Item ()]
toModule Theta.Module { Theta.types } = imports <> typeDefinitions
  where typeDefinitions = toDefinition =<< toList types

        imports =
          [ noSpan [Rust.item| use chrono::{Date, DateTime, Utc}; |]
          , noSpan [Rust.item| use std::collections::HashMap; |]
          , noSpan [Rust.item| use theta::avro::{FromAvro, ToAvro}; |]
          , noSpan [Rust.item| use nom::{IResult, Err, error::{context, ErrorKind}}; |]
          ]

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
toDefinition :: Theta.Definition Theta.Type -> [Rust.Item ()]
toDefinition Theta.Definition {..} = case Theta.baseType definitionType of
  -- structured types
  Theta.Record' name fields -> toRecord name fields
  Theta.Variant' name cases -> toVariant name cases
  Theta.Newtype' name type_ -> toNewtype name type_

  -- everything else (aliases)
  _ ->
    [Rust.TyAlias [] Rust.PublicV (ident definitionName)
       (toReference definitionType) noGenerics ()]

-- | Return a Rust snippet that /refers/ to the given Theta type.
--
-- For primitive types, this returns the equivalent Rust
-- type. Containers use the corresponding Rust container with its type
-- parameter converted the same way.
--
-- Records, variants, newtypes and references are referred to by name,
-- ignoring namespaces.
toReference :: Theta.Type -> Rust.Ty ()
toReference type_@Theta.Type { Theta.baseType } = case baseType of
  -- primitive types
  Theta.Bytes'    -> noSpan [Rust.ty| Vec<u8> |]
  Theta.Date'     -> noSpan [Rust.ty| Date<Utc> |]
  Theta.Datetime' -> noSpan [Rust.ty| DateTime<Utc> |]

  -- containers
  Theta.Array' a    -> generic "Vec"     [toReference a]
  Theta.Map' a      -> generic "HashMap" [noSpan [Rust.ty| String |], toReference a]
  Theta.Optional' a -> generic "Option"  [toReference a]

  -- all other types are referenced directly by name
  _   -> Rust.PathTy Nothing (path $ typeIdentifier type_) ()

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
typeIdentifier :: Theta.Type -> [Rust.Ident]
typeIdentifier Theta.Type { Theta.baseType } = case baseType of
  -- primitive types
  Theta.Bool'           -> ["bool"]
  Theta.Bytes'          -> ["Vec"]
  Theta.Int'            -> ["i32"]
  Theta.Long'           -> ["i64"]
  Theta.Float'          -> ["f32"]
  Theta.Double'         -> ["f64"]
  Theta.String'         -> ["String"]
  Theta.Date'           -> ["Date"]
  Theta.Datetime'       -> ["DateTime"]

  -- containers
  Theta.Array' _        -> ["Vec"]
  Theta.Map' _          -> ["HashMap"]
  Theta.Optional' _     -> ["Option"]
  Theta.Reference' name -> qualifiedIdent name

  -- structured types
  Theta.Record' name _  -> qualifiedIdent name
  Theta.Variant' name _ -> qualifiedIdent name
  Theta.Newtype' name _ -> qualifiedIdent name

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
toRecord :: Name -> Theta.Fields Theta.Type -> [Rust.Item ()]
toRecord name Theta.Fields { Theta.fields } =
  [ struct
  , toAvro typeName toAvroBody
  , fromAvro typeName $ wrapContext name fromAvroBody
  ]
  where typeName = ident name

        struct       =
          Rust.StructItem [derives] Rust.PublicV typeName structFields noGenerics ()
        structFields = Rust.StructD (toField name Rust.PublicV <$> fields) ()
        derives      = derive ["Clone", "Debug", "PartialEq"]

        -- statements implementing to_avro_buffer:
        -- self.foo.to_avro_buffer(buffer);
        -- self.baz.to_avro_buffer(buffer);
        toAvroBody = callToAvro . fieldAccess . Theta.fieldName <$> fields

        fromAvroBody = [ callFromAvro (toFieldName fieldName) fieldType |
                         Theta.Field { Theta.fieldName, Theta.fieldType } <- fields ]
                    <> [output]

        -- final expression:
        -- Ok((input, Foo { foo, baz }))
        output = Rust.NoSemi ok ()
        ok     = Rust.Call [] (noSpan [Rust.expr| Ok |]) [pair] ()
        pair   =
          Rust.TupExpr [] [ noSpan [Rust.expr| input |]
                          , structExpr name [typeName] fields ] ()

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
toVariant :: Name -> NonEmpty (Theta.Case Theta.Type) -> [Rust.Item ()]
toVariant name (NonEmpty.toList -> cases) =
  [ enum
  , toAvro typeName [Rust.Semi (toAvroMatch name cases) ()]
  , fromAvro typeName $ wrapContext name
      [parseTag, Rust.NoSemi (fromAvroMatch name cases) ()]
  ]
  where enum = Rust.Enum [derives] Rust.PublicV typeName variantCases noGenerics ()
        typeName = ident name
        derives = derive ["Clone", "Debug", "PartialEq"]

        variantCases = toCase <$> cases
        toCase Theta.Case { Theta.caseName, Theta.caseParameters } =
          Rust.Variant (ident caseName) [] fields Nothing ()
          where fields = Rust.StructD (go <$> Theta.fields caseParameters) ()
                go     = toField name Rust.InheritedV

        parseTag = noSpan [Rust.stmt|
          let (input, tag) = i64::from_avro(input)?;
        |]

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
toAvroMatch :: Name -> [Theta.Case Theta.Type] -> Rust.Expr ()
toAvroMatch name cases =
  match (noSpan [Rust.expr| self |]) $ branch <$> zip [0..] cases
  where branch (i, Theta.Case { Theta.caseName, Theta.caseParameters }) =
          ( casePat caseName caseParameters
          , Rust.BlockExpr [] (caseBlock i caseParameters) ()
          )

        -- a pattern for each case
        -- Bar { a, b }
        casePat :: Name -> Theta.Fields Theta.Type -> Rust.Pat ()
        casePat (ident -> caseName) Theta.Fields { Theta.fields } =
          Rust.StructP (path [ident name, caseName]) (field <$> fields) False ()
        field Theta.Field { Theta.fieldName } =
          Rust.FieldPat Nothing (fieldPat $ toFieldName fieldName) ()

        -- a block calling to_avro_buffer on the tag and each field
        caseBlock :: Integer -> Theta.Fields Theta.Type -> Rust.Block ()
        caseBlock tag Theta.Fields { Theta.fields } =
          Rust.Block (tagToAvro : (toAvro <$> fields)) Rust.Normal ()
          where tagToAvro = Rust.Semi (toAvroBuffer tagLit) ()
                tagLit    = Rust.Lit [] (Rust.Int Rust.Dec tag Rust.I64 ()) ()

                toAvro field = Rust.Semi (call field) ()
                call field = methodCall (expr field) "to_avro_buffer"
                               [noSpan [Rust.expr| buffer |]]
                expr Theta.Field { Theta.fieldName } = pathExpr [toFieldName fieldName]

        toAvroBuffer :: Rust.Expr () -> Rust.Expr ()
        toAvroBuffer expr =
          methodCall expr "to_avro_buffer" [noSpan [Rust.expr| buffer |]]

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
fromAvroMatch :: Name -> [Theta.Case Theta.Type] -> Rust.Expr ()
fromAvroMatch name cases =
  match (noSpan [Rust.expr| tag |]) $ (branch <$> zip [0..] cases) <> [invalid]
  where branch (i, Theta.Case { Theta.caseName, Theta.caseParameters }) =
          (tagPat i, Rust.BlockExpr [] (caseBlock caseName caseParameters) ())

        tagPat :: Integer -> Rust.Pat ()
        tagPat i =
          Rust.LitP (Rust.Lit [] (Rust.Int Rust.Dec i Rust.Unsuffixed ()) ()) ()

        caseBlock :: Name -> Theta.Fields Theta.Type -> Rust.Block ()
        caseBlock caseName Theta.Fields { Theta.fields } =
          Rust.Block body Rust.Normal ()
          where body = [ callFromAvro (toFieldName fieldName) fieldType |
                         Theta.Field { Theta.fieldName, Theta.fieldType } <- fields ]
                    <> [output]

                -- final expression
                -- Ok((input, Foo::Bar { a, b }))
                output = Rust.NoSemi ok ()
                ok     = Rust.Call [] (noSpan [Rust.expr| Ok |]) [pair] ()
                pair   = Rust.TupExpr [] [noSpan [Rust.expr| input |], result] ()
                result =
                  structExpr name [ident name, ident caseName] fields

        -- fall-through error case
        -- _ => { panic!("Invalid tag when decoding variant.") }
        invalid :: (Rust.Pat (), Rust.Expr ())
        invalid =
          ( noSpan [Rust.pat| _ |]
          , noSpan [Rust.expr| Err(Err::Error((input, ErrorKind::Tag))) |] )

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
toNewtype :: Name -> Theta.Type -> [Rust.Item ()]
toNewtype name type_ =
  [ struct
  , toAvro (ident name) [noSpan [Rust.stmt| self.0.to_avro_buffer(buffer); |]]
  , fromAvro (ident name) $ wrapContext name [parse, output]
  ]
  where struct  =
          Rust.StructItem [derives] Rust.PublicV (ident name) tuple noGenerics ()
        tuple   = (Rust.TupleD [Rust.StructField Nothing Rust.PublicV ref [] ()] ())
        ref     = toReference type_

        derives
          | copyable  = derive ["Copy", "Clone", "Debug", "PartialEq"]
          | otherwise = derive ["Clone", "Debug", "PartialEq"]

        copyable = Theta.isPrimitive underlying
                && underlying /= Theta.string'
                && underlying /= Theta.bytes'
        underlying = Theta.underlyingType type_

        parse  = callFromAvro "value" type_
        output = Rust.NoSemi ok ()
        ok     = Rust.Call [] (noSpan [Rust.expr| Ok |]) [pair] ()
        pair   = Rust.TupExpr [] [noSpan [Rust.expr| input |], value] ()

        value =
          Rust.Call [] (pathExpr [(ident name)]) [noSpan [Rust.expr| value |]] ()

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
wrapContext :: Name -> [Rust.Stmt ()] -> [Rust.Stmt ()]
wrapContext (Text.unpack . Name.render -> name) body = [Rust.NoSemi call ()]
  where call = Rust.Call [] withContext [noSpan [Rust.expr| input |]] ()

        withContext = Rust.Call [] (noSpan [Rust.expr| context |]) [lit, lambda] ()
        lit         = Rust.Lit [] (Rust.Str name Rust.Cooked Rust.Unsuffixed ()) ()

        lambda = Rust.Closure [] Rust.Movable Rust.Ref args bodyBlock ()
        args = Rust.FnDecl [Rust.Arg input (Rust.Infer ()) ()] Nothing False ()
        input = Just $ noSpan [Rust.pat| input |]
        bodyBlock = Rust.BlockExpr [] (Rust.Block body Rust.Normal ()) ()

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
toAvro :: Rust.Ident -> [Rust.Stmt ()] -> Rust.Item ()
toAvro typeName body = impl "ToAvro" typeName [toAvroFunction]
  where toAvroFunction = method "to_avro_buffer" [self, buffer] Nothing body
        self = Rust.SelfRegion Nothing Rust.Immutable ()
        buffer = arg [Rust.pat| buffer |] [Rust.ty| &mut Vec<u8> |]

-- | Call the @to_avro_buffer@ method on the given base expression.
--
-- @
-- base.to_avro_buffer(buffer);
-- @
callToAvro :: Rust.Expr a -> Rust.Stmt ()
callToAvro (noSpan -> base) = Rust.Semi call ()
  where call = Rust.MethodCall [] base "to_avro_buffer" Nothing args ()
        args = [noSpan [Rust.expr| buffer |]]

-- | An @impl@ of the @FromAvro@ trait for the specified types, with
-- the given body in the @from_avro@ method.
fromAvro :: Rust.Ident -> [Rust.Stmt ()] -> Rust.Item ()
fromAvro typeName body = impl "FromAvro" typeName [fromAvroFunction]
  where fromAvroFunction = method "from_avro" [input] (Just iresult) body

        input   = arg [Rust.pat| input |] [Rust.ty| &[u8] |]
        iresult = noSpan [Rust.ty| IResult<&[u8], Self> |]

-- | Call the @from_avro@ method on the field with the given name and
-- type.
--
-- @
-- let (input, foo) = i32::from_avro(input)?;
-- @
--
-- To handle name conflicts, a field called @input@ will be turned
-- into @__input__@:
--
-- @
-- let (input, __input__) = i32::from_avro(input)?;
-- @
--
-- This same convention is used in @fieldExpr@, so the final
-- expression will match:
--
-- @
-- Foo { bar, input: __input__ }
-- @
callFromAvro :: Rust.Ident -> Theta.Type -> Rust.Stmt ()
callFromAvro name fieldType =
  Rust.Local tuplePat Nothing (Just callFromType) [] ()
  where tuplePat = Rust.TupleP pat Nothing ()
        pat = [noSpan [Rust.pat| input |], fieldPat $ disambiguate name]

        callFromType = Rust.Try [] call ()
        call         = Rust.Call [] fromAvro [noSpan [Rust.expr| input |]] ()
        fromAvro     = pathExpr $ typeIdentifier fieldType <> ["from_avro"]

        disambiguate "input" = "__input__"
        disambiguate ident   = ident

-- | Access a field on @self@ with the given Theta name.
--
-- @
-- self.foo
-- @
fieldAccess :: Theta.FieldName -> Rust.Expr ()
fieldAccess (toFieldName -> fieldName) =
  Rust.FieldAccess [] (noSpan [Rust.expr| self |]) fieldName  ()

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
        -> Rust.Visibility ()
           -- ^ The visibility of the field. Should be 'Rust.PublicV'
           -- for structs and 'Rust.InheritedV' for variants.
        -> Theta.Field Theta.Type
        -> Rust.StructField ()
toField containing fieldVisibility Theta.Field { Theta.fieldName, Theta.fieldType } =
  Rust.StructField (Just $ toFieldName fieldName) fieldVisibility rustType [] ()
  where rustType
          | fieldType `refersTo` containing = box $ toReference fieldType
          | otherwise                       = toReference fieldType

        box type_ = generic "Box" [type_]

-- | Convert a 'Theta.Name' to a Rust identifier.
--
-- This ignores namespaces. If you need all the components of a fully
-- qualified name in Rust, use `qualifiedIdent`.
--
-- @
-- > ident "com.example.Foo"
-- "Foo"
-- @
ident :: Name -> Rust.Ident
ident = Rust.mkIdent . Text.unpack . Name.name

-- | Return the /fully qualified/ version of a Theta 'Name'.
--
-- @
-- > qualifiedIdent "com.example.Foo"
-- ["com", "example", "Foo"]
-- @
qualifiedIdent :: Name -> [Rust.Ident]
qualifiedIdent = map (Rust.mkIdent . Text.unpack) . Name.parts

-- | Convert a 'Theta.FieldName' to a Rust field name.
toFieldName :: Theta.FieldName -> Rust.Ident
toFieldName (Theta.FieldName name) = Rust.mkIdent $ Text.unpack name

-- | Create a generic type with the given name and type parameters.
--
-- @generic "HashMap" [[Rust.ty| String |], [Rust.ty| i32 |]]@
--
-- produces:
--
-- @ HashMap<String, i32> @
generic :: Rust.Ident -> [Rust.Ty ()] -> Rust.Ty ()
generic container args =
  Rust.PathTy Nothing (Rust.Path False [fullType] ()) ()
  where fullType   = Rust.PathSegment container (Just parameters) ()
        parameters = Rust.AngleBracketed [] (map (() <$) args) [] ()

-- | A 'Rust.Generics' value that corresponds to having no type
-- parameters.
--
-- Theta-generated types—structs, enums and newtypes—do not have any
-- type parameters. This definition lets us generate types like this
-- in a slightly less verbose way.
noGenerics :: Rust.Generics ()
noGenerics = Rust.Generics [] [] (Rust.WhereClause [] ()) ()

-- | Generate a @#[derive(A, B...)]@ attribute AST fragment.
derive :: [String] -> Rust.Attribute ()
derive traits = Rust.Attribute Rust.Outer (path ["derive"]) tree ()
  where tree = Rust.Tree $ Rust.Delimited
          { Rust.span  = span
          , Rust.delim = Rust.Paren
          , Rust.tts   = Rust.Stream $ List.intersperse comma $ toTree <$> traits
          }
        comma = Rust.Tree $ Rust.Token span Rust.Comma

        toTree trait =
          Rust.Tree $ Rust.Token span $ Rust.IdentTok (Rust.mkIdent trait)

        span = Rust.Span Rust.NoPosition Rust.NoPosition

-- | Given a name of a trait, a name of a type and the items inside
-- the @impl@, generate a full @impl@ block:
--
-- @
-- impl Trait for Type {
--   <body>
-- }
-- @
impl :: Rust.Ident -> Rust.Ident -> [Rust.ImplItem ()] -> Rust.Item ()
impl traitName typeName body =
  Rust.Impl [] Rust.InheritedV Rust.Final Rust.Normal Rust.Positive
    generics (Just traitRef) typePath body ()
  where generics = Rust.Generics [] [] (Rust.WhereClause [] ()) ()
        traitRef = Rust.TraitRef $ path [traitName]
        typePath = Rust.PathTy Nothing (path [typeName]) ()

-- | Given the name of the method, its arguments, its return type and
-- its body, generate a method definition that can be used inside an
-- @impl@ block.
--
-- @
-- fn methodName(arg: Type) -> ReturnType {
--   <body statements>
-- }
-- @
method :: Rust.Ident
       -> [Rust.Arg ()]
       -> Maybe (Rust.Ty ())
       -> [Rust.Stmt ()]
       -> Rust.ImplItem ()
method methodName args return body =
  Rust.MethodI [] Rust.InheritedV Rust.Final methodName generics sig bodyBlock ()
  where generics = Rust.Generics [] [] (Rust.WhereClause [] ()) ()

        sig = Rust.MethodSig Rust.Normal Rust.NotConst Rust.Rust decl
        decl = Rust.FnDecl args return False ()

        bodyBlock = Rust.Block body Rust.Normal ()

-- | Generate a method call expression.
--
-- @
-- (a + b).blarg(arg1, arg2)
-- @
methodCall :: Rust.Expr () -> Rust.Ident -> [Rust.Expr ()] -> Rust.Expr ()
methodCall expr method args = Rust.MethodCall [] expr method Nothing args ()

-- | Create a "normal" argument to a function (a name with a type
-- signature).
--
-- @
-- x: Foo
-- @
arg :: Rust.Pat a -> Rust.Ty a -> Rust.Arg ()
arg pat ty = Rust.Arg (Just $ noSpan pat) (noSpan ty) ()

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
path :: [Rust.Ident] -> Rust.Path ()
path names = Rust.Path False (segment <$> names) ()
  where segment name = Rust.PathSegment name Nothing ()

-- | Just like 'path', but wrapping the result into an expression.
pathExpr :: [Rust.Ident] -> Rust.Expr ()
pathExpr names = Rust.PathExpr [] Nothing (path names) ()

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
           -> [Rust.Ident]
           -- ^ The names of the fields in Rust, which should be in
           -- scope in the generated code.
           -> [Theta.Field Theta.Type]
           -- ^ The fields themselves.
           -> Rust.Expr ()
structExpr containing names fields =
  Rust.Struct [] (path names) (field <$> fields) Nothing ()
  where field Theta.Field { Theta.fieldName, Theta.fieldType }
          | fieldType `refersTo` containing = case fieldIdentifier of
              "input" ->
                Rust.Field "input" (Just $ noSpan [Rust.expr| Box::new(__input__) |]) ()
              _       ->
                Rust.Field fieldIdentifier (Just $ box fieldIdentifier) ()
          | otherwise                       = case fieldIdentifier of
              "input" ->
                Rust.Field "input" (Just $ noSpan [Rust.expr| __input__ |]) ()
              _       ->
                Rust.Field fieldIdentifier Nothing ()

          where fieldIdentifier = toFieldName fieldName
                box identifier =
                  Rust.Call [] (pathExpr ["Box", "new"]) [pathExpr [identifier]] ()

-- | A pattern matching a field in a struct. @x@ in:
--
-- @
-- Foo { x, y }
-- @
fieldPat :: Rust.Ident -> Rust.Pat ()
fieldPat name = Rust.IdentP (Rust.ByValue Rust.Immutable) name Nothing ()

-- | A @match@ expression with the given patterns and branch bodies.
--
-- @
-- match expr {
--   <pat> => <body>,
--   <pat> => <body>,
-- }
-- @
match :: Rust.Expr () -> [(Rust.Pat (), Rust.Expr ())] -> Rust.Expr ()
match expr branches = Rust.Match [] expr arms ()
  where arms = [Rust.Arm [] [pat] Nothing body () | (pat, body) <- branches]

-- | Remove the span annotations from a Rust AST fragment.
--
-- We do not want to keep track of spans for code that we are
-- generating and pretty printing, but the @language-rust@
-- quasiquoters return AST fragments with spans, so we need some to
-- get rid of them to make our types match up.
--
-- Under the hood this is just @(() <$)@, but I wanted a more readable
-- and domain-specific name for this function.
noSpan :: (Functor f) => f a -> f ()
noSpan ast = () <$ ast -- eta-expanded for inlineability

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
refersTo Theta.Type { Theta.baseType, Theta.module_ } name = case baseType of
  -- references and newtypes
  Theta.Reference' name'
    | name' == name -> True
    | otherwise     -> case Theta.lookupName name' module_ of
        Right type_ -> refersTo type_ name
        Left _err   -> False
  Theta.Newtype' _ type_ -> refersTo type_ name

  -- containers
  Theta.Array' _        -> False -- already on heap
  Theta.Map' _          -> False -- already on heap
  Theta.Optional' type_ -> refersTo type_ name

  -- structured types
  Theta.Record' name' fields
    | name' == name -> True
    | otherwise     -> fieldsReference fields
  Theta.Variant' name' cases
    | name' == name -> True
    | otherwise     -> any fieldsReference $ Theta.caseParameters <$> cases

  -- primitive types
  -- (listed explicitly to raise a warning if we add new kinds of
  -- types to Theta)
  Theta.Bool'     -> False
  Theta.Bytes'    -> False
  Theta.Int'      -> False
  Theta.Long'     -> False
  Theta.Float'    -> False
  Theta.Double'   -> False
  Theta.String'   -> False
  Theta.Date'     -> False
  Theta.Datetime' -> False

  where fieldsReference Theta.Fields { Theta.fields } =
          any (`refersTo` name) $ Theta.fieldType <$> fields
