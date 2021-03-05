{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Errors specific to working with Avro:
--
--    * converting Theta types to Avro schemas
--    * converting to and from 'Theta.Value' and 'Avro.Value'
module Theta.Target.Avro.Error where

import           Control.Monad.Except     (MonadError)

import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.Avro.Schema         as Schema
import qualified Data.Avro.Types          as Avro
import qualified Data.ByteString.Lazy     as LBS
import           Data.Foldable            (toList)
import           Data.Hashable            (Hashable)
import           Data.HashSet             (HashSet)
import qualified Data.HashSet             as HashSet
import           Data.List.NonEmpty       (NonEmpty)
import           Data.Semigroup           ((<>))
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text

import qualified Theta.Error              as Theta
import           Theta.Name               (Name (..))
import           Theta.Pretty             (Pretty (pretty), p)
import           Theta.Types

-- | Throw an Avro-specific error, annotating it as coming from the
-- @"Avro"@ target.
throw :: (MonadError Theta.Error m) => AvroError -> m a
throw = Theta.throw "Avro"

-- | Errors specific to compiling to an Avro schema or converting
-- to/from Avro values.
--
-- The majority of these errors (duplicate/non-existing names) can
-- only be raised in two situations:
--
--  * the module was not validated before being exported to Avro
--
--  * the module disambiguates names with namespaces, which the Avro
--    target doesn't currently support
--
-- The only error that might be thrown on a validated module with
-- unique names is 'InvalidExport'.
data AvroError = InvalidExport Type
                 -- ^ The type we're compiling to a schema is not a
                 -- record or variant. We can't export other types as
                 -- a top-level schema in Avro.
                 --
                 -- Aliases and newtypes over records and variants are
                 -- also valid because aliases and types are compiled
                 -- to their underlying types in Avro.
               | NonExistentType Name
                 -- ^ The type referenced by the given name does not
                 -- exist.
               | InvalidName Schema.TypeName
                 -- ^ The given Avro name is not a valid name for
                 -- Theta.
               | DuplicateName Name
                 -- ^ The type with the given name was defined
                 -- multiple times.
               | DuplicateFieldName Name
                 -- ^ The record with the given name has a duplicate
                 -- field.
               | MissingField Name FieldName
                 -- ^ The Avro record does not have a field that is
                 -- expected by the Theta type we are converting to.
               | InvalidVariant Reason Name
                 -- ^ The Avro encoding of the variant with the given
                 -- name was not valid for the specified reason.
               | TypeMismatch Type (Avro.Value Schema.Schema)
                 -- ^ Mismatch between the expected Theta.Type and the
                 -- Avro object being converted.
               | FieldNameMismatch Name (HashSet FieldName) (HashSet FieldName)
                 -- ^ The fields of an Avro record are not the same as
                 -- the fields of the expected Theta record.
               deriving (Show)

-- | The reason why a variant encoding is invalid.
data Reason = ConstructorField (Avro.Value Schema.Schema)
              -- ^ Expected the "constructor" field to be a union but
              -- got something else.
            | InvalidRecord (Avro.Value Schema.Schema)
              -- ^ Expected a record with a single "constructor" field
              -- but got something else.
            | ExtraCase (NonEmpty (Case Type)) Schema.TypeName
              -- ^ The Avro object corresponds to a case that is not
              -- in the Theta variant.
            | DifferentFields (HashSet FieldName) (HashSet FieldName)
              -- ^ The Avro object encoding a case has a different set
              -- of keys than expected by the Theta type.
            | InvalidBranch (Avro.Value Schema.Schema)
              -- ^ The Avro object encoding a case of a variant was
              -- not a record.
            deriving (Show)

instance Pretty AvroError where
  pretty = \case
    InvalidExport type_ ->
      [p|
        Cannot export the chosen type as a top-level Avro schema:

        #{prettyType type_}

        Only records and variants can be exported as stand-alone Avro schemas.
           |]
    NonExistentType name ->
      [p| The name ‘#{name}’ is not defined. |]
    InvalidName name ->
      [p| The name ‘#{name}’ is not a valid Theta name. |]
    DuplicateName name ->
      [p| The name ‘#{pretty name}’ has multiple definitions. |]
    DuplicateFieldName name ->
      [p| The record ‘#{pretty name}’ has multiple fields with the same name. |]
    MissingField record field ->
      [p| The record ‘#{pretty record}’ is missing the expected field ‘#{pretty field}’. |]
    InvalidVariant reason variant ->
      [p|
        The variant ‘#{pretty variant}’ was not encoded correctly in Avro:

        #{prettyReason reason}
           |]
    TypeMismatch expected got ->
      [p|
        Type mismatch.
        Expected a:
          #{prettyType expected}
        But got the following Avro object:
          #{prettyAvro got}
           |]
    FieldNameMismatch record theta avro ->
      let header  =
            [p| "The Avro record ‘#{pretty record}’ has a different set of fields than expected." |]
          missing = prettyDifference "Missing" theta avro
          extra   = prettyDifference "Extra" avro theta
      in header <> missing <> extra

-- | Render a human-readable description of why a varaint was not
-- encoded correctly.
prettyReason :: Reason -> Text
prettyReason = \case
  ConstructorField avro ->
    [p|
      Expected the “constructor” field to be a union but got:
      #{prettyAvro avro}
      |]
  InvalidRecord avro ->
    [p|
      Expected a record with a single “constructor” field but got:
      #{prettyAvro avro}
      |]
  ExtraCase cases name ->
    [p|
      The case #{Schema.renderFullname name} is not in the variant. The possible cases are:

      #{prettyCaseList cases}
      |]
  DifferentFields theta avro ->
    let header  = "The Avro object has a different set of fields than expected from the Theta type."
        missing = prettyDifference "Missing" theta avro
        extra   = prettyDifference "Extra" avro theta
    in header <> missing <> extra
  InvalidBranch avro ->
    [p|
      Expected a variant encoded as an Avro record but got:
      #{prettyAvro avro}
      |]
  where prettyCaseList = Text.intercalate "\n" . map (prettyList . caseName) . toList

-- | Render a value as an entry in a bulleted list.
prettyList :: Pretty a => a -> Text
prettyList name = "  • " <> pretty name

-- | Compare two lists of names and render the difference between them
-- (ie the names in the first not present in the second) as a
-- human-readable bulleted list.
--
-- The first argument is used to format the header message (“Missing
-- fields in Avro” vs “Extra fields in Avro”).
prettyDifference :: (Eq name, Hashable name, Pretty name)
                 => Text
                 -> HashSet name
                 -> HashSet name
                 -> Text
prettyDifference header a b = case HashSet.toList (HashSet.difference a b) of
  []         -> ""
  difference ->
      [p|

        #{header} fields in Avro:
        #{Text.intercalate "\n" $ map prettyList difference}
        |]

-- | Render an Avro value as formatted, human-readable JSON.
prettyAvro :: Avro.Value Schema.Schema -> Text
prettyAvro avro = Text.decodeUtf8 $ LBS.toStrict $ Aeson.encodePretty avro
