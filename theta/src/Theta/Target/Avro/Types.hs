{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ViewPatterns          #-}

-- | This module lets us compile Theta records and variants to fully
-- self-contained Avro schemas.
--
-- Most Theta types map to Avro directly—Theta was explicitly designed
-- to work well with Avro. The main difference is that Theta has
-- variants (tagged unions) while Avro only supports normal
-- unions. @["int", "int"]@ is not a valid Avro union, but we /can/
-- define a Theta variant with two cases each of which has a single
-- @Int@ value.
--
-- To support this, each case in a Theta variant is encoded as a
-- distinct record in Avro. The record wrapper acts as the tag for the
-- tagged union. The downside with this approach is that case names
-- become globally scoped—we can't compile two variants to Avro if
-- they share a case name (unless the cases also have identical
-- fields).
--
-- This also means that all Theta schemas can be compiled to Avro
-- schemas, but not all Avro schemas correspond to Theta types. In
-- particular, Avro schemas with unions that do not correspond to
-- Theta variants or optional types have no analog in Theta.
module Theta.Target.Avro.Types
  ( toSchema
  , typeToAvro
  )
where

import           Control.DeepSeq            (liftRnf)
import           Control.Monad.Except
import           Control.Monad.State.Strict

import           Data.Avro                  (Schema, TypeName (..))
import qualified Data.Avro                  as Avro
import qualified Data.Avro.Schema.Schema    as Avro
import           Data.List                  (sort)
import           Data.List.NonEmpty         (NonEmpty (..))
import qualified Data.List.NonEmpty         as NonEmpty
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.String.Interpolate    (__i)
import qualified Data.Vector                as Vector

import qualified Theta.Error                as Theta
import           Theta.Metadata             (Version)
import qualified Theta.Metadata             as Metadata
import           Theta.Name                 (Name)
import qualified Theta.Name                 as Name
import           Theta.Target.Avro.Error
import           Theta.Types
import qualified Theta.Versions             as Versions

-- | Transform a compatible Theta type into a full Avro schema. This
-- will only work with Theta records and variants.
--
-- The resulting Avro schema is /self-contained/. This means that any
-- referenced type (record, variant... etc) is included in the schema
-- directly the first time it is used and referenced by name from then
-- on out.
--
-- Most encodings are direct—Theta booleans become Avro booleans,
-- strings become strings...etc.
--
-- Optional values are encoded as Avro unions with null. @Int?@ in
-- Theta translates to @["null", "int"]@ in Avro.
--
-- Variants encoded as records in order to express /tagged/ unions and
-- to allow variants as the top-level definition in an Avro schema. A
-- Theta variant is encoded as an Avro record with a single field
-- called @constructor@ that has a union for every case in the
-- variant. Each case is encoded as a record itself.
--
-- Simple example:
--
-- @
-- type Foo = Bar { a : Int }
--          | Baz { a : String }
-- @
--
-- turns into the following Avro:
--
-- @
-- {
--   "name" : "Foo",
--   "type" : "record",
--   "fields" : [
--     {
--       "name" : "constructor",
--       "type" : [
--         {
--           "name" : "Bar",
--           "type" : "record",
--           "fields" : [ { "name" : "a", "type" : "string" } ]
--         },
--         {
--           "name" : "Baz",
--           "type" : "record",
--           "fields" : [ { "name" : "a", "type" : "int" } ]
--         }
--       ]
--     }
--   ]
-- }
-- @
toSchema :: (MonadError Theta.Error m)
         => Definition Type
         -> m Schema
toSchema Definition { definitionType, definitionDoc } =
  annotateVersion <$> toSchema' definitionType
  where
    annotateVersion = \case
      -- verbose to avoid partial pattern-match warning
      Avro.Record {..} -> Avro.Record
        { Avro.name    = name
        , Avro.aliases = aliases
        , Avro.doc     = annotation doc
        , Avro.fields  = fields
        }
      _ -> error "Top-level Avro schema has to be a record.\n\
                 \This is probably a bug in Theta."

    annotation Nothing    = Just message
    annotation (Just doc) = Just $ message <> "\\n" <> doc

    message = [__i|
      Generated with Theta #{Versions.packageVersion'}
      Type hash: #{hash definitionType}
    |]

    toSchema' Type { baseType = Record' name fields } =
      evalStateT (record name avroVersion definitionDoc fields) Set.empty
    toSchema' Type { baseType = Variant' name cases } =
      evalStateT (variant name avroVersion definitionDoc cases) Set.empty
    toSchema' Type { baseType = Reference' name, module_ } =
      case lookupDefinition name module_ of
        Right type_ -> toSchema type_
        Left _      -> throw $ NonExistentType name
    toSchema' invalid                          =
      throw $ InvalidExport invalid

    avroVersion = Metadata.avroVersion $ metadata $ module_ definitionType
{-# SPECIALIZE toSchema :: Definition Type -> Either Theta.Error Schema #-}

-- | Build an Avro schema for a Theta variant given the variant's name
-- and its cases.
--
-- Variants are encoded in Avro as unions with a record for each
-- case. Each record has the same name as the corresponding branch of
-- the Theta variant.
--
-- For example, the following variant:
--
-- @
-- data Foo = Bar { a : String }
--          | Baz { a : Int }
-- @
--
-- will be encoded as the following Avro record:
--
-- @
-- {
--   "name" : "Foo",
--   "type" : "record",
--   "fields" : [
--     {
--       "name" : "constructor",
--       "type" : [
--         {
--           "name" : "Bar",
--           "type" : "record",
--           "fields" : [ { "name" : "a", "type" : "string" } ]
--         },
--         {
--           "name" : "Baz",
--           "type" : "record",
--           "fields" : [ { "name" : "a", "type" : "int" } ]
--         }
--       ]
--     }
--   ]
-- }
-- @
variant :: (MonadError Theta.Error m, MonadState (Set Name) m)
        => Name
        -> Version
        -- ^ The avro-version set for the module where this variant
        -- was deifned.
        -> Maybe Doc
        -> (NonEmpty (Case Type))
        -> m Schema
variant variantName avroVersion doc variantCases = do
  included <- get
  if variantName `Set.member` included
    then throw $ DuplicateName variantName
    else do
    modify $ Set.insert variantName
    let caseToRecord Case { caseName, caseParameters, caseDoc }
          | caseName `Set.member` included = pure $ namedType caseName
          | otherwise                      =
            record caseName avroVersion caseDoc caseParameters
    types <- force <$> traverse caseToRecord variantCases

    let field = Avro.Field
          { Avro.fldName    = "constructor"
          , Avro.fldAliases = []
          , Avro.fldDoc     = Nothing
          , Avro.fldOrder   = Nothing
          , Avro.fldType    = Avro.mkUnion types
          , Avro.fldDefault = Nothing
          }

    pure $ Avro.Record
      { Avro.name      = nameToAvro variantName
      , Avro.aliases   = []
      , Avro.doc       = getText <$> doc
      , Avro.fields    = [field]
      }
  where force xs = liftRnf (`seq` ()) xs `seq` xs
{-# SPECIALIZE variant :: Name -> Version -> Maybe Doc -> NonEmpty (Case Type) -> StateT (Set Name) (Either Theta.Error) Schema #-}

-- | Build an Avro schema for a Theta enum. A Theta enum is encoded as
-- an avro enum with the same symbols in the same order.
enum :: (MonadError Theta.Error m, MonadState (Set Name.Name) m)
     => Name.Name
     -- ^ The name of the enum.
     -> Maybe Doc
     -> NonEmpty EnumSymbol
     -- ^ The symbols for the enum in order. This should not have
     -- duplicates but, if it does, duplicates will be discarded in
     -- the generated Avro type.
     -> m Schema
enum enumName doc (NonEmpty.toList -> symbols) = do
  included <- get
  let alreadyDefined = enumName `Set.member` included
  when alreadyDefined $ throw $ DuplicateName enumName

  pure $ Avro.Enum
    { Avro.name    = nameToAvro enumName
    , Avro.aliases = []
    , Avro.doc     = getText <$> doc
    , Avro.symbols = avroSymbols
    }
  where avroSymbols = Vector.fromList $ enumSymbol <$> symbols

-- | Build an Avro schema for a Theta record. A Theta record is turned
-- into an Avro record with the same name and fields.
record :: (MonadError Theta.Error m, MonadState (Set Name.Name) m)
       => Name.Name
       -- ^ The name of the record. The Avro record will have the same
       -- name, including the namespace.
       -> Version
       -- ^ The avro-version set in the module where the record was
       -- defined.
       -> Maybe Doc
       -> Fields Type
       -- ^ The fields in the record. The Avro record definitions will
       -- have the same fields in the same order.
       -> m Schema
record recordName avroVersion doc Fields { fields } = do
  included <- get
  let alreadyDefined  = recordName `Set.member` included
      duplicateFields = not $ unique $ fieldName <$> fields

  if | duplicateFields -> throw $ DuplicateFieldName recordName
     | alreadyDefined  -> throw $ DuplicateName recordName
     | otherwise       -> mkType

  where
    unique (sort -> ls) = and $ zipWith (/=) ls (drop 1 ls)

    mkType = do
      modify $ Set.insert recordName
      !fields <- force <$> traverse fieldToAvro fields
      pure $ Avro.Record
        { Avro.name      = nameToAvro recordName
        , Avro.aliases   = []
        , Avro.doc       = getText <$> doc
        , Avro.fields    = fields
        }

    fieldToAvro Field { fieldName, fieldType, fieldDoc } = do
      !type_ <- typeToAvro avroVersion fieldType
      pure $ Avro.Field
        { Avro.fldName    = textName fieldName
        , Avro.fldAliases = []
        , Avro.fldDoc     = getText <$> fieldDoc
        , Avro.fldOrder   = Nothing
        , Avro.fldType    = type_
        , Avro.fldDefault = Nothing
        }

    force xs = liftRnf (`seq` ()) xs `seq` xs
{-# SPECIALIZE record :: Name -> Version -> Maybe Doc -> Fields Type -> StateT (Set Name) (Either Theta.Error) Schema #-}

-- | Turn a Theta 'Name.Name' to the corresponding Avro name.
nameToAvro :: Name.Name -> TypeName
nameToAvro Name.Name { Name.name, Name.moduleName } =
  TN name $ Name.namespace moduleName <> [Name.baseName moduleName]

-- | Generate a reference /in Avro/ that calls back to a type that has
-- already been defined in the schema.
namedType :: Name.Name -> Avro.Schema
namedType = Avro.NamedType . nameToAvro

-- | Transforms any Theta expression into a fragment of an Avro
-- schema. This schema might not stand alone—for example, it could
-- just be a reference to a named or built-in Avro type.
--
-- Newtypes are encoded exactly the same as their underlying
-- types. (This means distinction between different newtypes is not
-- preserved in Avro!)
typeToAvro :: (MonadError Theta.Error m, MonadState (Set Name) m)
           => Version
              -- ^ The Avro version of the *context* of this type.
              --
              -- For user-defined types (records, variants and
              -- newtypes) this is the avro-version set in the module
              -- where they are defined.
              --
              -- For primitive types, this is the avro-version of the
              -- module where they are referenced, either directly or
              -- through an alias.
              --
              -- This approach lets us maintain backwards
              -- compatibility in a controllable way for Avro
              -- encoding—for example, whether or not to use logical
              -- Avro types for Date and Datetime.
           -> Type
           -> m Schema
typeToAvro contextAvroVersion Type { baseType, module_ } = case baseType of
  Bool'   -> pure $! Avro.Boolean
  Bytes'  -> pure $! Avro.Bytes Nothing
  Int'    -> pure $! Avro.Int Nothing
  Long'   -> pure $! Avro.Long Nothing
  Float'  -> pure $! Avro.Float
  Double' -> pure $! Avro.Double
  String' -> pure $! Avro.String Nothing

  Date'
    | contextAvroVersion < "1.1.0" -> pure $! Avro.Int Nothing
    | otherwise                    -> pure $! Avro.Int (Just Avro.Date)
  Datetime'
    | contextAvroVersion < "1.1.0" -> pure $! Avro.Long Nothing
    | otherwise                    -> pure $! Avro.Long (Just Avro.TimestampMicros)

  Array' item     -> Avro.Array <$!> typeToAvro contextAvroVersion item
  Map' value      -> Avro.Map   <$!> typeToAvro contextAvroVersion value
  Optional' type_ -> nullUnion  <$!> typeToAvro contextAvroVersion type_

  Enum' name symbols  -> enum name (getDoc name) symbols
  Record' name fields -> record name definitionAvroVersion (getDoc name) fields
  Variant' name cases -> variant name definitionAvroVersion (getDoc name) cases

  Newtype' _ type_    -> typeToAvro definitionAvroVersion type_
  Reference' typeName -> do
    included <- get
    if typeName `Set.member` included
      then pure $! namedType typeName
      else case lookupName typeName module_ of
             Left _    -> throw $ NonExistentType typeName
             Right res -> typeToAvro contextAvroVersion res
  where nullUnion type_ = Avro.mkUnion $ Avro.Null :| [type_]

        getDoc name = case lookupDefinition name module_ of
          Right Definition { definitionDoc } -> definitionDoc
          Left _                             -> Nothing

        -- avro-version where this type was defined, as opposed to
        -- avro-version where it was used (contextAvroVersion)
        definitionAvroVersion = Metadata.avroVersion $ metadata module_
{-# SPECIALIZE typeToAvro :: Version -> Type -> StateT (Set Name) (Either Theta.Error) Schema #-}
