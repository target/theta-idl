{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumDecimals           #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ParallelListComp      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Conversions between Theta values and Avro. Theta schemas can be
-- compiled to Avro (see 'Theta.Target.Avro.Types'), and we can
-- convert to/from Theta values and Avro objects encoded according to
-- the Theta schema.
--
-- Avro supports two over-the-wire formats: an Avro-specific binary
-- format and JSON (with special rules for encoding binary data and
-- unions). If you need to convert Theta values to/from JSON you can
-- do it via Avro as long as you are cognizant of how Avro is
-- expressed in JSON (see 'Data.Avro.JSON' for more details:
-- <http://hackage.haskell.org/package/avro-0.3.5.1/docs/Data-Avro-JSON.html>).
--
-- Note that while all Theta schemas can be compiled to Avro schemas,
-- not all Avro schemas correspond to Theta types. In particular, the
-- encoding for variants is significantly different from Avro's
-- unions. This means there are Avro objects that cannot be validly
-- converted with /any/ Theta schema.
module Theta.Target.Avro.Values where

import           Control.Monad              (when)
import           Control.Monad.Except       (MonadError)
import           Control.Monad.Identity     (Identity (..))
import           Control.Monad.State.Strict (evalStateT)

import qualified Data.Avro.Schema           as Schema
import qualified Data.Avro.Types            as Avro
import qualified Data.ByteString.Lazy       as LBS
import           Data.HashMap.Strict        (HashMap, (!))
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.HashSet               as HashSet
import           Data.Int                   (Int32, Int64)
import           Data.List.NonEmpty         (NonEmpty)
import qualified Data.List.NonEmpty         as NonEmpty
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Time                  as Time
import           Data.Time.Clock.POSIX      as Time
import           Data.Vector                (Vector)
import qualified Data.Vector                as Vector

import           GHC.Exts                   (fromList)

import           Theta.Error                (Error)
import qualified Theta.Error                as Error
import           Theta.Name                 (Name)
import qualified Theta.Name                 as Name
import           Theta.Target.Avro.Error
import           Theta.Target.Avro.Types
import qualified Theta.Types                as Theta
import           Theta.Value

-- | Convert a Theta 'Value' to an Avro object which can be directly
-- serialized to JSON or the Avro binary format.
--
-- The resulting Avro object will have a schema that corresponds to
-- the Theta type of the value, as compiled by 'toSchema'.
toAvro :: MonadError Error m => Value -> m (Avro.Value Schema.Schema)
toAvro value@Value { type_ } = do
  !schema <- evalStateT (typeToAvro type_) Set.empty

  let env      = runIdentity . Schema.buildTypeEnvironment die schema
      die name = error $ show name <> " not defined in generated Avro schema."

  pure $! go env schema value
  where go env !schema v@(!Value { value }) = case (schema, value) of
          -- named types
          (Schema.NamedType name, _)  -> go env (env name) v

          -- primitive types
          (Schema.Boolean, Boolean b) -> Avro.Boolean b
          (Schema.Bytes, Bytes bs)    -> Avro.Bytes $ LBS.toStrict bs
          (Schema.Int, Int i)         -> Avro.Int i
          (Schema.Long, Long l)       -> Avro.Long l
          (Schema.Float, Float f)     -> Avro.Float f
          (Schema.Double, Double d)   -> Avro.Double d
          (Schema.String, String t)   -> Avro.String t
          (Schema.Int, Date d)        -> Avro.Int $ fromDay d
          (Schema.Long, Datetime t)   -> Avro.Long $ fromUTCTime t

          -- containers
          (Schema.Array t, Array vs)                    ->
            Avro.Array . forceVector $ Vector.map (go env t) vs
          (Schema.Map t, Map vs)                        ->
            Avro.Map $ go env t <$> vs
          (Schema.Union options@(Vector.toList -> [Schema.Null, t]), Optional a) ->
            let union = Avro.Union options in
            case a of
              Just a  -> union t $! go env t a
              Nothing -> union Schema.Null Avro.Null

          -- records
          (t@Schema.Record{}, Record values) ->
            Avro.Record t $! toHashMap values
              where toHashMap values = HashMap.fromList
                      [ convert name value
                      | name  <- Schema.fldName <$> Schema.fields t
                      | value <- Vector.toList values
                      ]

                    convert name value =
                      case HashMap.lookup name fieldTypes of
                        Just type_ -> (name, go env type_ value)
                        Nothing    -> error $ "No field named "
                                           <> Text.unpack name
                                           <> " in Avro record schema."

                    fieldTypes = HashMap.fromList [ (fldName, fldType)
                                                  | Schema.Field {..} <- Schema.fields t ]

          -- variants
          (wrapper@Schema.Record{}, Variant branch values) ->
            Avro.Record wrapper [("constructor", union)]
            where
              unionSchema = case Schema.fields wrapper of
                [Schema.Field { Schema.fldName = "constructor", Schema.fldType }] ->
                  fldType
                _ -> error "Invalid encoding of variant object in Avro schema."

              chosenRecord = case HashMap.lookup branch caseMap of
                Just case_ -> case_
                Nothing    -> error $ "Invalid type_ in a Value for a variant.\n"
                                     <> "Missing branch " <> show branch

              caseMap =
                HashMap.fromList [ (Error.unsafe $ nameFromAvro $ Schema.name c, c)
                                 | c <- Vector.toList $ Schema.options unionSchema ]

              record = Avro.Record chosenRecord $! HashMap.fromList
                [ convert name value
                | name <- Schema.fldName <$> Schema.fields chosenRecord
                | value <- Vector.toList values
                ]

              union = Avro.Union (Schema.options unionSchema) chosenRecord record

              convert name value =
                case HashMap.lookup name fieldTypes of
                  Just type_ -> (name, go env type_ value)
                  Nothing    -> error $ "No field named "
                                     <> Text.unpack name
                                     <> " in Avro record schema for variant."

              fieldTypes =
                HashMap.fromList [ (fldName, fldType)
                                 | Schema.Field {..} <- Schema.fields chosenRecord ]

          -- fallback—hitting this means there was a bug in parsing or
          -- processing the Theta Type or Value
          (schema, value) ->
            error $ "Mismatch between the type_ and value of a Value. \
                     \This is a bug in the Theta implementation.\n"
                 <> show schema <> "\n" <> show value

        forceVector v = Vector.foldl' (const (`seq` ())) () `seq` v
        -- TODO: get rid of forceVector once we upgrade to a version
        -- of vector that has an NFData1 instance (> 0.12.1, I expect)
{-# SPECIALIZE toAvro :: Value -> Either Error (Avro.Value Schema.Schema) #-}

-- | Convert from an Avro object to a Theta 'Value', verifying against
-- the given Theta type.
--
-- Will throw an exception if the Avro object does not match up with
-- the given Theta schema. (This will be changed to more explicit
-- error handling soon...)
--
-- The Avro value needs to have a schema that matches the result of
-- calling `toSchema` on the Theta type being decoded.
fromAvro :: forall m. MonadError Error m
         => Theta.Type
         -- ^ The expected type that the Avro value has to match.
         -> Avro.Value Schema.Schema
         -- ^ The Avro object to verify against the schema and
         -- convert.
         -> m Value
fromAvro type_@Theta.Type { baseType, module_ } avro = do
  value <- value'
  pure $ Value { type_, value }
  where value' = case (baseType, avro) of
          -- references
          (Theta.Reference' name, _)        -> case Theta.lookupName name module_ of
            Left _  -> throw $ NonExistentType name
            Right t -> recurse t avro

          -- primitive types
          (Theta.Bool', Avro.Boolean b)     -> pure $ Boolean b
          (Theta.Int', Avro.Int i)          -> pure $ Int i
          (Theta.Long', Avro.Long l)        -> pure $ Long l
          (Theta.Float', Avro.Float f)      -> pure $ Float f
          (Theta.Double', Avro.Double d)    -> pure $ Double d
          (Theta.Bytes', Avro.Bytes bs)     -> pure $ Bytes $ LBS.fromStrict bs
          (Theta.String', Avro.String t)    -> pure $ String t

          (Theta.Date', Avro.Int i)         -> pure $ Date $ toDay i
          (Theta.Datetime', Avro.Long i)    -> pure $ Datetime $ toUTCTime i

          -- containers
          (Theta.Array' t, Avro.Array xs)   ->
            Array <$> Vector.mapM (fromAvro t) xs
          (Theta.Map' t, Avro.Map map)      -> Map <$> mapM (fromAvro t) map

          -- optional types
          (Theta.Optional' t,
           Avro.Union [Schema.Null, _] _ v) -> case v of
            Avro.Null -> pure $ Optional Nothing
            _         -> Optional . Just <$> fromAvro t v

          -- constructed types
          (Theta.Record' name fields,
           Avro.Record _ avroFields)        ->
            Record <$> convertFields name fields avroFields

                -- TODO: add a check that the alternatives in the Avro
                -- union match the cases of the Theta variant
          (Theta.Variant' name cases,
           record@Avro.Record{})            -> convertVariant name cases record
          (Theta.Newtype' _ t, v)           -> recurse t v

          (_, got)                          -> throw $ TypeMismatch type_ got

        convertFields :: Name
                      -> Theta.Fields Theta.Type
                      -> HashMap Text (Avro.Value Schema.Schema)
                      -> m (Vector Value)
        convertFields recordName thetaFields avroFields
          | thetaNames /= avroNames =
            throw $ FieldNameMismatch recordName thetaNames avroNames
          | otherwise               =
            Vector.fromList <$> mapM matchField (Theta.fields thetaFields)
          where -- match a field with its corresponding Avro value in
                -- the Avro record and convert the Avro value to a
                -- Theta value
                matchField :: Theta.Field Theta.Type -> m Value
                matchField Theta.Field { Theta.fieldName, Theta.fieldType } =
                  case HashMap.lookup (Theta.textName fieldName) avroFields of
                    Nothing    -> throw $ MissingField recordName fieldName
                    Just field -> fromAvro fieldType field

                thetaNames = HashSet.fromList $ Theta.fieldNames thetaFields
                avroNames  = HashSet.fromList $
                  Theta.FieldName <$> HashMap.keys avroFields

        -- a variant is a union wrapped in a record with a single
        -- field called "constructor"
        convertVariant :: Name
                       -> NonEmpty (Theta.Case Theta.Type)
                       -> Avro.Value Schema.Schema
                       -> m BaseValue
        convertVariant variantName cases avro@(Avro.Record _ avroFields) =
          case HashMap.toList avroFields of
            [("constructor", branch)] -> case branch of
              Avro.Union _ _ record -> convertBranch variantName cases record
              invalid               ->
                throw $ InvalidVariant (ConstructorField invalid) variantName
            _                         ->
              throw $ InvalidVariant (InvalidRecord avro) variantName
        convertVariant variantName _ invalidValue =
          throw $ InvalidVariant (InvalidRecord invalidValue) variantName

        -- convert a branch of a variant based on its Avro type
        --
        -- every branch in an Avro encoding of a Theta variant has to
        -- be a record, so we error out if it isn't
        convertBranch :: Name
                      -> NonEmpty (Theta.Case Theta.Type)
                      -> Avro.Value Schema.Schema
                      -> m BaseValue
        convertBranch variantName cases (Avro.Record schema avroFields) = do
          caseName <- nameFromAvro $ Schema.name schema
          case HashMap.lookup caseName $ caseMap cases of
            Just case_ -> do
              let parameters = Theta.caseParameters case_
                  fields     = fieldMap parameters
                  values     =
                    [ (fields ! name, avroValues ! name)
                    | name <- Theta.fieldNames parameters ]
                  keys       = HashSet.fromMap . fmap (const ())
                  thetaKeys  = keys fields
                  avroKeys   = keys avroValues

              when (thetaKeys /= avroKeys) $
                throw $ InvalidVariant (DifferentFields thetaKeys avroKeys) variantName
              Variant caseName . Vector.fromList <$> traverse (uncurry go) values
            Nothing    ->
              throw $ InvalidVariant (ExtraCase cases (Schema.name schema)) variantName
          where go field avro = fromAvro (Theta.fieldType field) avro

                                -- TODO: is there a better way to do this
                                -- map transformation?
                avroValues :: HashMap Theta.FieldName (Avro.Value Schema.Schema)
                avroValues = HashMap.fromList $ toValue <$> HashMap.toList avroFields
                toValue (text, avro) = (Theta.FieldName text, avro)

        convertBranch variantName _ invalidValue =
          throw $ InvalidVariant (InvalidBranch invalidValue) variantName

        caseMap :: NonEmpty (Theta.Case Theta.Type) -> HashMap Name (Theta.Case Theta.Type)
        caseMap cases = HashMap.fromList $ NonEmpty.toList $ toKV <$> cases
          where toKV case_ = (Theta.caseName case_, case_)

        fieldMap :: Theta.Fields Theta.Type
                 -> HashMap Theta.FieldName (Theta.Field Theta.Type)
        fieldMap Theta.Fields { Theta.fields } =
          HashMap.fromList [(Theta.fieldName field, field) | field <- fields]

        recurse t avro = do Value { value } <- fromAvro t avro
                            pure value
{-# SPECIALIZE fromAvro :: Theta.Type -> Avro.Value Schema.Schema -> Either Error Value #-}

-- | Convert from an Avro-style date to a Haskell 'Day'.
--
-- The Avro date format stores the number of days from the Unix epoch
-- (1970-1-1) as a 32-bit integer. This is the same as the logical
-- @date@ type Avro offers.
toDay :: Int32 -> Time.Day
toDay (fromIntegral -> n) = Time.ModifiedJulianDay $ n + offset
  where offset = Time.diffDays (Time.fromGregorian 1970 1 1) (Time.ModifiedJulianDay 0)
{-# INLINE toDay #-}

-- | Convert from a Haskell 'Day' to an Avro-style date.
--
-- The Avro date format stores the number of days from the Unix epoch
-- (1970-1-1) as a 32-bit integer. This is the same as the logical
-- @date@ type Avro offers.
fromDay :: Time.Day -> Int32
fromDay day = fromIntegral $ Time.toModifiedJulianDay day - offset
  where offset = Time.diffDays (Time.fromGregorian 1970 1 1) (Time.ModifiedJulianDay 0)
{-# INLINE fromDay #-}

-- | Convert from an Avro-style microsecond timestamp to a Haskell
-- 'Time.UTCTime'.
--
-- The Avro format for timestamps is stored as the number of
-- microseconds from the Unix epoch (1970-1-1) as a 64-bit
-- integer. This is the same as the logical @timestamp-micros@ type
-- Avro offers.
toUTCTime :: Int64 -> Time.UTCTime
toUTCTime n = Time.posixSecondsToUTCTime $ fromIntegral n / 1e6
{-# INLINE toUTCTime #-}

-- | Convert from a Haskell 'Time.UTCTime' to an Avro-style
-- microsecond timestamp.
--
-- The Avro format for timestamps is stored as the number of
-- microseconds from the Unix epoch (1970-1-1) as a 64-bit
-- integer. This is the same as the logical @timestamp-micros@ type
-- Avro offers.
fromUTCTime :: Time.UTCTime -> Int64
fromUTCTime time = floor (Time.utcTimeToPOSIXSeconds time * 1e6)
{-# INLINE fromUTCTime #-}


-- | Convert an Avro name to the corresponding Theta name.
--
-- Will error if the Avro name has no namespace.
nameFromAvro :: MonadError Error m => Schema.TypeName -> m Name
nameFromAvro invalid@(Schema.TN _ [])   = throw $ InvalidName invalid
nameFromAvro (Schema.TN name namespace) = pure $ Name.Name
  { Name.name
  , Name.moduleName = fromList namespace
  }
