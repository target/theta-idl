{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ViewPatterns      #-}
module Theta.Target.Kotlin
  ( Kotlin (..)
  , toModule

  , toRecord
  , toVariant
  , toReference

  , isValidIdentifier
  ) where

import           Prelude                         hiding (toEnum)

import qualified Data.Char                       as Char
import           Data.Foldable                   (toList)
import           Data.List.NonEmpty              (NonEmpty)
import qualified Data.List.NonEmpty              as NonEmpty
import qualified Data.Text                       as Text

import           Theta.Name                      (ModuleName, Name)
import qualified Theta.Name                      as Name
import qualified Theta.Types                     as Theta

import qualified Theta.Primitive                 as Theta
import           Theta.Target.Kotlin.QuasiQuoter (Kotlin (..), kotlin)

-- | Compile a Theta module to a single Kotlin file.
--
-- This will have an @import@ for corresponding Kotlin packages for
-- any Theta modules imported by this module.
toModule :: [Kotlin]
         -- ^ An optional prefix you can use to namespace generated
         -- Kotlin files. Each entry in the list should be a valid
         -- Kotlin identifier.
         --
         -- The prefix @["com", "example", "foo"]@ for a module @bar@
         -- would result in the following package declaration:
         --
         -- @
         -- package com.example.foo.bar
         -- @
         -> Theta.Module
         -- ^ The module to compile to Kotlin.
         -> Kotlin
toModule prefix Theta.Module {..} =
  [kotlin|
    package $package

    import java.time.LocalDate
    import java.time.LocalDateTime
    import java.time.LocalTime
    import java.time.OffsetDateTime

    import java.util.UUID

    import kotlin.ByteArray

    import kotlin.collections.HashMap

    $importLines

    $definitions
  |]
  where definitions =
          toDefinitions $ map toDefinition $ toList types
        package = toQualifiedImport prefix moduleName

        importLines =
          toLines [[kotlin|import $identifier.*|]
                  | Theta.Module { Theta.moduleName } <- imports
                  , let identifier = toQualifiedImport prefix moduleName
                  ]

-- | Return a Kotlin snippet that defines a type with the given name
-- corresponding to the given Theta type.
--
-- For primitive types, containers and newtypes, this produces a type
-- alias:
--
-- @
-- typealias Quantity = int
-- @
--
-- For records and variants, this produces Kotlin class definitions
-- corresponding to the Theta type.
toDefinition :: Theta.Definition Theta.Type
             -> Kotlin
toDefinition Theta.Definition { Theta.definitionName, Theta.definitionType }  =
  case Theta.baseType definitionType of
    -- structured types
    Theta.Enum' name symbols       -> toEnum name symbols
    Theta.Record' name fields      -> toRecord name fields
    Theta.Variant' name cases      -> toVariant name cases
    Theta.Newtype' name underlying ->
      let identifier = toIdentifier name
          type_ = toReference underlying
      in [kotlin|typealias $identifier = $type_|]

    -- everything else becomes a type alias
    _ ->
      let reference = toReference definitionType
          identifier = toIdentifier definitionName
      in [kotlin|typealias $identifier = $reference|]

-- | Compile a Theta record to a Kotlin data class.
--
--   If the record has no fields, it is compiled to an object instead.
toRecord :: Name
         -> Theta.Fields Theta.Type
         -> Kotlin
toRecord (toIdentifier -> name) Theta.Fields { Theta.fields } =
  case toDeclaration <$> fields of
    []            ->
      [kotlin|object $name|]
    [declaration] ->
      [kotlin|data class $name($declaration)|]
    declarations  ->
      let declarationLines = toLinesList declarations in
      [kotlin|
         data class $name(
             $declarationLines
         )
      |]
  where toDeclaration Theta.Field {..} =
          let name  = toFieldName fieldName
              type_ = toReference fieldType
          in [kotlin|val $name: $type_|]

-- | Compile a Theta variant to a Kotlin sealed class, with data
-- classes for each case in the variant.
toVariant :: Name
          -> NonEmpty (Theta.Case Theta.Type)
          -> Kotlin
toVariant (toIdentifier -> name) (NonEmpty.toList -> cases) =
  [kotlin|
    sealed class $name {
        $caseClasses
    }
  |]
  where caseClasses = toDefinitions $ toClass <$> cases

        toClass Theta.Case {..} = [kotlin|$caseClass : $name()|]
          where caseClass = toRecord caseName caseParameters

-- | Compile a Theta enum to a Kotlin enum class.
--
-- Since Kotlin's identifiers have the same syntax restrictions as
-- Avro enum symbols, the symbol names are used directly in the enum
-- class definition.
toEnum :: Name -> NonEmpty Theta.EnumSymbol -> Kotlin
toEnum (toIdentifier -> name) (NonEmpty.toList -> symbols) =
  [kotlin|
    enum class $name {
      $kotlinSymbols
    }
   |]
  where kotlinSymbols =
          toLinesList [ Kotlin symbol | Theta.EnumSymbol symbol <- symbols ]

-- | Return a Kotlin snippet that /refers/ to the given Theta type.
--
-- For primitive types, this returns the equivalent Kotlin type.
--
-- Containers use the equivalent Kotlin container with the type
-- parameter converted the same way.
--
-- Named types (references, records, variants and newtypes) are
-- referred to by name, ignoring namespaces.
toReference :: Theta.Type -> Kotlin
toReference Theta.Type { Theta.baseType } = case baseType of

  -- primitive types
  Theta.Primitive' t -> case t of
    Theta.Bool          -> "Boolean"
    Theta.Bytes         -> "ByteArray"
    Theta.Int           -> "Int"
    Theta.Long          -> "Long"
    Theta.Float         -> "Float"
    Theta.Double        -> "Double"
    Theta.String        -> "String"
    Theta.Date          -> "LocalDate"
    Theta.Datetime      -> "OffsetDateTime"
    Theta.UUID          -> "UUID"
    Theta.Time          -> "LocalTime"
    Theta.LocalDatetime -> "LocalDateTime"

  -- containers
  Theta.Array' a        -> let items = toReference a in [kotlin|Array<$items>|]
  Theta.Map' a          -> let values = toReference a in
                             [kotlin|HashMap<String, $values>|]
  Theta.Optional' a     -> let type_ = toReference a in [kotlin|$type_?|]

  -- named types
  Theta.Enum' name _    -> toIdentifier name
  Theta.Record' name _  -> toIdentifier name
  Theta.Variant' name _ -> toIdentifier name
  Theta.Newtype' name _ -> toIdentifier name

  Theta.Reference' name -> toIdentifier name

-- | Compile a list of 'Kotlin' snippets into a single Kotlin block
-- with one snippet per line.
--
-- @
-- toLines [[kotlin|self.foo = foo|], [kotlin|self.bar = bar|]]
-- @
--
-- produces:
--
-- @
-- self.foo = foo
-- self.bar = bar
-- @
toLines :: [Kotlin] -> Kotlin
toLines []     = [kotlin||]
toLines (l:ls) = let rest = toLines ls in [kotlin|
  $l
  $rest
  |]

-- | Compile a list of 'Kotlin' snippets into a comma-separated list
-- with each element on one line and no trailing comma.
--
-- @
-- toLines [[kotlin|val foo: Int|], [kotlin|val bar: Int|]]
-- @
--
-- produces:
--
-- @
-- val foo: Int,
-- val bar: Int
-- @
toLinesList :: [Kotlin] -> Kotlin
toLinesList []     = [kotlin||]
toLinesList [line] = [kotlin|$line|]
toLinesList (l:ls) = let rest = toLinesList ls in [kotlin|
  $l,
  $rest
  |]

-- | Compile a list of 'Kotlin' snippets into a block of code with
-- multiple definitions. Each definition is separated by blank lines.
--
-- This is useful for combining multiple class or method definitions
-- into a single snippet.
--
-- @
-- f1 = [kotlin|
--  def foo(x):
--    return x
--  |]
-- f2 = [kotlin|
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
toDefinitions :: [Kotlin] -> Kotlin
toDefinitions []     = [kotlin||]
toDefinitions [l]    = [kotlin|$l|]
toDefinitions (l:ls) = let rest = toDefinitions ls in [kotlin|
  $l

  $rest
  |]

-- | Convert a 'Theta.Name' to a 'Kotlin' identifier.
--
-- Currently this ignores the Theta namespace and transcribes the name
-- directly to Kotlin.
toIdentifier :: Name -> Kotlin
toIdentifier = Kotlin . Name.name

-- | Returns whether the given 'Kotlin' snippet is a syntactically
-- valid identifier in Kotlin.
--
-- Identifiers can contain letters, numbers and underscores, but can't
-- /start/ with a number.
isValidIdentifier :: Kotlin -> Bool
isValidIdentifier (Kotlin identifier) = case Text.uncons identifier of
  Nothing            -> False
  Just (first, rest) -> (Char.isAlpha first || first == '_')
                     && Text.all (\ x -> Char.isAlphaNum x || x == '_') rest

-- | Convert a 'Theta.FieldName' to the corresponding 'Kotlin' field
-- name.
toFieldName :: Theta.FieldName -> Kotlin
toFieldName (Theta.FieldName name) = Kotlin name

-- | Generate a fully qualified reference (e.g. `com.target.foo`) to
-- the given module.
toQualifiedImport :: [Kotlin]
                  -- ^ An optional extra prefix that will go /before/
                  -- the module's namespace in the generated
                  -- file. Each entry in the list should be a valid
                  -- Kotlin identifier.
                  --
                  -- The prefix @["com", "example"]@ for a module
                  -- called @"foo.bar"@ will result in:
                  --
                  -- @
                  -- com.example.foo.bar
                  -- @
                  -> ModuleName
                  -> Kotlin
toQualifiedImport prefix moduleName =
  Kotlin $ Text.intercalate "." $ (fromKotlin <$> prefix)
                               <> Name.moduleParts moduleName
