{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-pattern-binds #-}
-- for some reason, my TH code produces false-positive "unused
-- binds" and "unused matches" warnings

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Facilities for generating Haskell types from Theta schemas using
-- Template Haskell.
--
-- The simplest way to use this module is by enabling Template Haskell
-- and pointing 'loadModule' at a Theta module. Here is how you would
-- load a module called "protocols" located at
-- "../specs/theta/protocols.theta":
--
-- @
-- loadModule "../specs/theta" "protocols"
-- @
--
-- A call to 'loadModule' will create a Haskell type for every
-- definition in the Theta schema, with several typeclass instances:
--
--  * Show
--  * Eq
--  * Generic
--  * HasTheta
--  * ToTheta
--  * FromTheta
--  * HasAvroSchema
--  * ToAvro
--  * FromAvro
--
-- Given `ToTheta` and `FromTheta` instances, we can convert to and
-- from any other formats supported by Theta. We can go to/from Theta
-- with @toTheta@ and @fromTheta@ and then we can go to/from Avro with
-- @toAvro@ and @fromAvro@. (Note that the "from" direction requires
-- some extra error handling.)
--
-- Currently, loading multiple Theta modules which define conflicting
-- types will cause an error (even if the conflicting definitions are
-- exactly the same). Instead, write a new Theta module that imports
-- all the modules you need and then load that with 'loadModule'.
module Theta.Target.Haskell where

import           Control.Monad.Except
import           Control.Monad.State             (evalStateT)

import qualified Data.Avro                       as Avro
import qualified Data.Avro.Encoding.ToAvro       as Avro
import qualified Data.Avro.Encoding.FromAvro     as Avro
import qualified Data.Avro.Internal.Get          as Avro
import           Data.Avro.HasAvroSchema         (HasAvroSchema (..))
import           Data.ByteString.Lazy            (ByteString)
import           Data.Foldable                   (toList, traverse_)
import           Data.HashMap.Strict             (HashMap)
import           Data.Int                        (Int32, Int64)
import           Data.List                       (foldl')
import           Data.List.NonEmpty              (NonEmpty)
import qualified Data.List.NonEmpty              as NonEmpty
import qualified Data.Map                        as Map
import qualified Data.Set                        as Set
import           Data.Tagged                     (Tagged (..))
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Time.Clock                 (UTCTime)
import           Data.Time.Calendar              (Day)
import           Data.Vector                     ((!))
import qualified Data.Vector                     as Vector
import           Data.Versions                   (SemVer(..), VUnit(..))

import qualified GHC.Exts                        as Exts
import           GHC.Generics                    (Generic)

import           Language.Haskell.TH             as TH
import           Language.Haskell.TH.Syntax

import           Theta.Error                     (Error)
import qualified Theta.Import                    as Import
import           Theta.Metadata                  (Metadata (..), Version(..))
import qualified Theta.Name                      as Name
import qualified Theta.Pretty                    as Theta
import qualified Theta.Types                     as Theta
import qualified Theta.Value                     as Theta

import           Theta.Target.Avro.Types         (typeToAvro)
import qualified Theta.Target.Avro.Values        as Theta

import qualified Theta.Target.Haskell.Conversion as Conversion
import           Theta.Target.Haskell.HasTheta   (HasTheta (..))

-- * Generating Haskell types

-- | Generate Haskell types from a Theta module located at the given
-- path. This will generate a type for every definition in the given
-- Theta module as well as all the definitions it imports from other
-- modules.
--
-- Each generated type has several typecalss instances:
--
--  * Show
--  * Eq
--  * Generic
--  * HasTheta
--  * ToTheta
--  * FromTheta
--  * HasAvroSchema
--  * ToAvro
--  * FromAvro
--
-- The 'Name.Name' argument has an 'IsString' instance so you can call
-- 'loadModule' at the top-level of a Haskell module with two string
-- literals as arguments:
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell   #-}
-- @
--
-- @
-- loadModule "specs/theta" "com.example.foo"
-- @
loadModule :: Import.LoadPath
           -- ^ The path to the Theta root directory. We will retrieve
           -- the specified module and all of its imports starting
           -- from this path.
           -> Name.ModuleName
           -- ^ The fully qualified name of the module written out
           -- with dots. Example: @"com.target.foo"@ for a module
           -- named @"foo"@ in the @"com.target"@ namespace.
           -> Q [Dec]
loadModule loadPath moduleName = do
  (module_, moduleFiles) <- runTheta $ do
    (definition, path) <- Import.getModuleDefinition loadPath moduleName
    (module_, paths)   <- Import.resolveModule loadPath moduleName definition
    pure (module_, path : paths)

  -- tells GHC it needs to recompile the module if any of the loaded
  -- schema files change
  traverse_ qAddDependentFile moduleFiles

  generateDefinitions module_

  where runTheta :: ExceptT Error IO a -> Q a
        runTheta theta = runIO $ runExceptT theta >>= \case
          Left err  -> fail $ Text.unpack $ Theta.pretty err
          Right res -> return res

-- ** Modules

-- | Generate Haskell types for the given Theta module if the types
-- are not in scope. This generates several things:
--
--  * a top-level identifier for the module (called @theta'foo@ for a
--    module @foo@, ignoring namespaces [for now])
--
--  * a type definition for every type in the module
--
--  * typeclass instances for each record, variant and newtype: Eq,
--    Show, HasTheta, ToTheta, FromTheta
--
-- This will not generate types for a module if the top-level module
-- identifier already exists in the current module—this will happen
-- either if the module was already generated or if it was imported
-- from a different Haskell module. If we call 'generateDefinitions'
-- on a module called @foo@ and @theta'foo@ is in scope, no types or
-- definitions will be generated.
generateDefinitions :: Theta.Module -> Q [Dec]
generateDefinitions module_ = concat <$> traverse generate modules
  where modules = Theta.transitiveImports [module_]

        generate module_ = do
          let moduleExp    = generateModule module_
              topLevelName = globalModuleName module_

          lookupValueName (nameBase topLevelName) >>= \case
            Just _  -> pure []
            Nothing -> do
              moduleSignature  <- sigD topLevelName [t| Theta.Module |]
              moduleDefinition <- valD (varP topLevelName) (normalB moduleExp) []

              definitions <-
                traverse (generateDefinition topLevelName) (Theta.types module_)

              pure $ moduleSignature : moduleDefinition : concat definitions

-- | The name of the global definition storing the given Theta module.
--
-- This name is the fully qualified name of the module with @.@
-- replaced by @'@ and prefixed with @theta'@. The module
-- @com.example.foo@ would have the Haskell global name
-- @theta'com'example'foo@.
globalModuleName :: Theta.Module -> Name
globalModuleName module_ = mkName $ "theta'" <> Text.unpack fullName
  where fullName = Text.intercalate "'" $ Exts.toList $ Theta.moduleName module_

-- | Generate an expression that evaluates to the given
-- 'Theta.Module'. This will include a literal 'Map' with all the
-- bindings in the module.
--
-- @
-- HashMap.fromList [("TypeName", <type expression>), …]
-- @
generateModule :: Theta.Module -> Q Exp
generateModule (Theta.Module name (Map.toList -> bindings) imports metadata) =
  [e| Theta.Module { Theta.moduleName = $(generateModuleName name)
                   , Theta.types      = Map.fromList $(moduleExps)
                   , Theta.imports    = $(listE $ varE . globalModuleName <$> imports)
                   , Theta.metadata   = $(generateMetadata metadata)
                   }
    |]
  where moduleExps = listE $ binding <$> bindings
        binding (name, Theta.Definition {..}) =
          [e| ($(generateName name), Theta.Definition
                { Theta.definitionName = $(generateName definitionName)
                , Theta.definitionDoc  = $(generateDoc definitionDoc)
                , Theta.definitionType = $(generateThetaExp definitionType)
                })
            |]

-- | Generate an expression that evaluates to the given
-- 'Theta.Metadata' value.
--
--  @
--  Metadata
--    { languageVersion = fromString "1.0.0"
--    , avroVersion     = fromString "1.0.0"
--    , moduleName      = "module_name"
--    }
--  @
generateMetadata :: Metadata -> Q Exp
generateMetadata Metadata { languageVersion, avroVersion, moduleName } =
  [e|
   Metadata
    { languageVersion = Version $(generateVersion languageVersion)
    , avroVersion     = Version $(generateVersion avroVersion)
    , moduleName      = $(generateModuleName moduleName)
    }
    |]
  where generateVersion (Version (SemVer { _svMajor, _svMinor, _svPatch
                                        , _svPreRel, _svMeta })) =
          [e| SemVer { _svMajor  = $(litE $ integerL $ fromIntegral _svMajor)
                     , _svMinor  = $(litE $ integerL $ fromIntegral _svMinor)
                     , _svPatch  = $(litE $ integerL $ fromIntegral _svPatch)
                     , _svPreRel = $(listE $ units <$> _svPreRel)
                     , _svMeta   = $(meta _svMeta)
                     }
            |]

        units = listE . toList . NonEmpty.map unit

        unit (Digits word) = [e| Digits $(litE $ integerL $ fromIntegral word) |]
        unit (Str text)    = [e| Text $(litE $ stringL $ Text.unpack text) |]

        meta = \case
          Just text -> litE $ stringL $ Text.unpack text
          Nothing   -> [e| Nothing |]

-- ** Type Definitions

-- | Generate a Haskell type for a definition.
--
-- This will generate a defintion *only* for the specified type, not
-- for any of the types it references.
--
-- This generates new Haskell types for records, variants and
-- newtypes. Any other bindings become type synonyms:
--
-- @
-- type Foo = ByteString
-- @
--
-- See the documentation for 'generateRecord', 'generateVariant' and
-- 'generateNewtype' for details on what those Haskell types look
-- like.
generateDefinition :: Name
                   -- ^ The name of the generated module expression
                   -- for the module this definition comes from (ie
                   -- @theta'foo@ for a module named "foo").
                   -> Theta.Definition Theta.Type
                   -> Q [Dec]
generateDefinition moduleName Theta.Definition {..} =
  case Theta.baseType definitionType of
    -- records, variants and newtypes need to be defined explicitly
    Theta.Record' name fields -> withInstances =<< generateRecord name fields
    Theta.Variant' name cases -> withInstances =<< generateVariant name cases
    Theta.Newtype' name type_ -> withInstances =<< generateNewtype name type_

    -- any other type becomes a type synonym
    _                         -> typeSynonym definitionName $ generateType definitionType
  where -- HasTheta, ToTheta and FromTheta instance declarations
        withInstances :: Dec -> Q [Dec]
        withInstances definition = do
          hasTheta  <- hasThetaInstance moduleName definitionName
          toTheta   <- toThetaInstance moduleName definitionType
          fromTheta <- fromThetaInstance definitionType

          hasAvro   <- hasAvroInstance definitionName
          toAvro    <- toAvroInstance definitionName
          fromAvro  <- fromAvroInstance definitionName

          pure $ definition : (hasTheta <> toTheta <> fromTheta <>
                               hasAvro <> toAvro <> fromAvro)

-- *** Structured Types

-- $ Faclilities for defining Haskell types for records,
-- variants... etc.

-- | Generates a record with the specified fields, using the given
-- name for both the type and the data constructor.
--
-- The name of the Theta record is used as both the type name /and/
-- the constructor name in Haskell.
--
-- Namespaces are currently ignored in both the record name and the
-- field names.
--
-- The following Theta record:
--
-- @
-- type com.example.Foo = { a : String, b : Int }
-- @
--
-- generates the following Haskell type:
--
-- @
-- data Foo = Foo { a :: Text, b :: Int32 }
-- @
generateRecord :: Name.Name -> Theta.Fields Theta.Type -> Q Dec
generateRecord name Theta.Fields { Theta.fields } =
  dataD (pure []) (toName name) [] Nothing [recordFields name pairs] [defaultClasses]
  where pairs = [ (fieldName, fieldType)
                | Theta.Field { Theta.fieldName, Theta.fieldType } <- fields ]

-- | Generate a record constructor with field names.
--
-- Namespaces are ignored in both the constructor name and the field
-- names.
--
-- The following Theta record:
--
-- @
-- type com.example.Foo = { a : String, b : Int }
-- @
--
-- would result in the following constructor:
--
-- @
-- Foo { a :: Text, b :: Int }
-- @
recordFields :: Name.Name -> [(Theta.FieldName, Theta.Type)] -> Q Con
recordFields (toName -> name) fields =
  recC name $ generateField <$> fields
  where generateField (toFieldName -> fieldName, type_) =
          varBangType fieldName $ parameter type_

-- | Generate a data type with multiple constructors to deal with
-- variants.
--
-- The Theta expression has names for each field, but the names are
-- left out in Haskell to avoid creating partial accessor functions.
--
-- Namespaces are currently ignored for the variant name and
-- constructors.
--
-- The following Theta variant:
--
-- @
-- type com.example.Foo = Baz { a : Int }
--                      | Qux { a : String, b : Long }
-- @
--
-- generates the following Haskell type:
--
-- @
-- data Foo = Baz { a :: Int32 }
--          | Qux { a :: Text, b :: Int64 }
-- @
--
-- One potential problem is having the same field name in multiple
-- cases with *different types* (using the same name with the *same*
-- type is not a problem):
--
-- @
-- type Foo = Bar { a : Int, b : Int }
--          | Baz { a : String }
-- @
--
-- Fields like this will be disambiguated with the name of their
-- constructor, separated by a @'@:
--
-- @
-- data Foo = Bar { a'Bar :: Int32, b :: Int 32 }
--          | Baz { a'Baz :: Text }
-- @
--
-- This is awkward, but I couldn't find a better solution within
-- Haskell.
generateVariant :: Name.Name -> (NonEmpty (Theta.Case Theta.Type)) -> Q Dec
generateVariant (toName -> name) (toList -> cases) =
  dataD (pure []) name [] Nothing (generateCase <$> disambiguate cases) [defaultClasses]
  where generateCase (caseName, fields) = recordFields caseName fields

        disambiguate :: [Theta.Case Theta.Type]
                     -> [(Name.Name, [(Theta.FieldName, Theta.Type)])]
        disambiguate cases =
          [ (caseName, rename caseName <$> Theta.fields caseParameters)
          | Theta.Case { caseName, caseParameters } <- cases ]
          where rename (Name.Name _ caseName) (Theta.Field {..})
                  | Set.member fieldName duplicated = (renamed, fieldType)
                  | otherwise                       = (fieldName, fieldType)
                  where renamed =
                          Theta.FieldName $ Theta.textName fieldName <> "'" <> caseName

                -- The use of Theta.prettyType here is pretty shady,
                -- but it *is* valid because the string created by
                -- pretty type is unique for every possible Theta type
                -- (unless we have an invalid schema where two types
                -- share the same fully qualified name).
                --
                -- Using prettyType in the set was a lesser evil than
                -- giving Theta.Type a brittle and arbitrary Ord
                -- instance.
                duplicated = Map.keysSet $ Map.filter (\ s -> length s > 1) fields
                fields = Map.fromListWith Set.union
                  [ (fieldName, Set.singleton $ Theta.prettyType fieldType)
                  | Theta.Case { Theta.caseParameters } <- cases
                  , Theta.Field {..} <- Theta.fields caseParameters ]

-- | Generates a Haskell @newtype@, using the given name for both the
-- type and the constructor.
--
-- The namespace of the type name is currently ignored.
--
-- The following Theta definition:
--
-- @
-- type Blarg = String
-- @
--
-- results in the following Haskell type:
--
-- @
-- newtype Blarg = Blarg Text
-- @
generateNewtype :: Name.Name -> Theta.Type -> Q Dec
generateNewtype (toName -> name) type_ =
  newtypeD (pure []) name [] Nothing (normalC name [param type_]) [defaultClasses]
  where param = bangType (bang noSourceUnpackedness noSourceStrictness) . generateType

-- | This is the default set of classes that types for records,
-- variants and newtypes derive.
defaultClasses :: Q DerivClause
defaultClasses = derivClause Nothing [[t| Eq |], [t| Show |], [t| Generic |]]

-- | Produce a parameter with the given type that is strict by
-- default.
parameter :: Theta.Type -> BangTypeQ
parameter type_ = bangType strict $ generateType type_
  where strict = bang noSourceUnpackedness sourceStrict

-- ** Typeclass Instances

-- | An expression that looks up the Theta type with the given name
-- from the specified top-level Theta module.
--
-- Generates the following for a type @com.example.Foo@ from a module
-- @com.example.Bar@ (module namespacing not handled yet):
--
-- @
-- let name = Name { namespace = ["com", "example"], name = "Foo" } in
-- case Theta.lookupName name theta'bar of
--   Right res -> res
--   Left err  -> error $ "Template Haskell bug: " <> err
-- @
thetaType :: Name -> Name.Name -> Q Exp
thetaType moduleName definitionName =
  [e| let name = $(generateName definitionName) in
      case Theta.lookupName name $(varE moduleName) of
        Right res -> res
        Left err  -> error $ "Template Haskell bug: " <> err
    |]

-- *** HasTheta

-- | Generate a 'HasTheta' instance for a Theta type with the given
-- name.
--
-- Given a Theta type @com.example.Foo@ from a module called @bar@,
-- generates the following instance:
--
-- @
-- instance HasTheta Foo where
--   module_ _ = theta'bar
--   theta _   =
--     let name = Name { namespace = ["com", "example"], name = "Foo" } in
--     case Theta.lookupName name theta'bar of
--       Right res -> res
--       Left err  -> error $ "Tempalte Haskell bug: " <> err
-- @
hasThetaInstance :: Name
                    -- ^ The top-level Haskell name for the module (ie
                    -- @theta'bar@).
                 -> Name.Name
                    -- ^ The fully qualified name of the Theta type.
                 -> Q [Dec]
hasThetaInstance moduleName definitionName =
  [d| instance HasTheta $(conT $ toName definitionName) where
        theta = $(thetaType moduleName definitionName)
    |]

-- *** Avro Instances

-- | Generate a 'HasAvroSchema' instance that calls 'typeToAvro' under the hood.
--
-- The result of 'typeToAvro' might error out, which would mean the TH
-- code has a bug.
hasAvroInstance :: Name.Name -> Q [Dec]
hasAvroInstance (toName -> type_) =
  [d| instance HasAvroSchema $(conT type_) where
        schema = Tagged $ case evalStateT toAvro Set.empty of
          Left err  -> error $ Text.unpack $ Theta.pretty err
          Right res -> res
          where toAvro                = typeToAvro definitionAvroVersion thetaType
                thetaType             = theta @ $(conT type_)
                definitionAvroVersion = avroVersion $ Theta.metadata $ Theta.module_ thetaType
    |]

-- | Generate a 'Avro.ToAvro' instance for the type. This might
-- generate an error, which would mean this TH code has a bug.
--
-- @
-- instance Avro.ToAvro Foo where
--   toAvro = Theta.toAvro . toTheta
-- @
toAvroInstance :: Name.Name -> Q [Dec]
toAvroInstance (toName -> type_) =
  [d| instance Avro.ToAvro $(conT type_) where
        toAvro _ value = Conversion.avroEncoding value
    |]

-- | Generate a 'Avro.FromAvro' instance for the type.
--
-- @
-- instance Avro.FromAvro Foo where
--   fromAvro avro =
--     case Conversion.fromTheta =<< Theta.fromAvro schema avro of
--       Left err  -> Avro.Error $ Text.unpack $ Theta.pretty err
--       Right res -> Avro.Success res
--     where schema = theta @Foo
-- @
fromAvroInstance :: Name.Name -> Q [Dec]
fromAvroInstance (toName -> type_) =
  [d| instance Avro.FromAvro $(conT type_) where
        fromAvro avro =
          case Conversion.fromTheta =<< Theta.fromAvro schema avro of
            Left err  -> Left $ Text.unpack $ Theta.pretty err
            Right res -> Right res
          where schema = theta @ $(conT type_)
    |]

-- *** ToTheta

-- | Generate a 'Conversion.ToTheta' instance for a Haskell
-- type. These instances only make sense for structured types—records,
-- variants and newtypes. Calling 'toThetaInstance' on some other
-- Theta type generates no declarations.
--
-- The instance for a newtype uses the 'ToTheta' instance of its
-- underlying type:
--
-- @
-- instance ToTheta Foo where
--   toTheta (Foo x) = toTheta x
--
--   avroEncoding (Foo x) = avroEncoding x
-- @
--
-- Refer to the documentation of 'recordToTheta' and 'variantToTheta'
-- for what their respective instances look like.
toThetaInstance :: Name
                   -- ^ The top-level haskell name for the module (ie
                   -- @theta'bar@).
                -> Theta.Type
                   -- ^ The Theta type to generate the
                   -- 'Conversion.ToTheta' instance for.
                -> Q [Dec]
toThetaInstance moduleName type_ = case Theta.baseType type_ of
  Theta.Record' name fields -> recordToTheta moduleName name fields
  Theta.Variant' name cases -> variantToTheta moduleName name cases
  Theta.Newtype' name _     ->
    [d| instance Conversion.ToTheta $(conT $ toName name) where
          toTheta $(conP (toName name) [ [p| x |] ]) = Conversion.toTheta x

          avroEncoding $(conP (toName name) [ [p| x |] ]) = Conversion.avroEncoding x
      |]
            -- primitive types and containers do not need custom instances
  _                          -> [d| |]

-- | A capturable name that we use to define 'toTheta' functions.
argName :: Name
argName = mkName "value"

-- | Generates a 'Conversion.ToTheta' instance for a record, relying
-- on the 'Conversion.ToTheta' instances of each field type.
--
-- Given the following Theta type from a module @com.example.bar@:
--
-- @
-- type com.example.Foo = { a : Int, b : String }
-- @
--
-- we get the following 'Conversion.ToTheta' instance:
--
-- @
-- instance ToTheta Foo where
--   toTheta value = Theta.Value
--     { Theta.value   = record
--     , Theta.module_ = theta'bar
--     , Theta.type_   =
--         let name = Name { namespace = ["com", "example"], name = "Foo" } in
--         case Theta.lookupName name theta'bar of
--           Right res -> res
--           Left err  -> error $ "Template Haskell bug: " <> err
--     }
--    where record = Theta.Record $ Vector.fromList
--                     [ let Foo { a = x } = value in toTheta x
--                     , let Foo { b = x } = value in toTheta x
--                     ]
--
--    avroEncoding value = mconcat encodedFields
--      where encodedFields = [ let Foo { a = x } = value in avroEncoding x
--                            , let Foo { b = x } = value in avroEncoding x
--                            ]
-- @
recordToTheta :: Name
                 -- ^ The top-level haskell name for the module (ie
                 -- @theta'bar@).
              -> Name.Name
                 -- ^ The name of the Theta record.
              -> Theta.Fields Theta.Type
                 -- ^ The fields of the Theta record.
              -> Q [Dec]
recordToTheta moduleName name Theta.Fields { Theta.fields } =
  [d| instance Conversion.ToTheta $(conT $ toName name) where
        toTheta $(varP argName) = Theta.Value
          { Theta.value   = record
          , Theta.type_   = $(thetaType moduleName name)
          }
          where record = Theta.Record $ Vector.fromList $(listE $ field <$> fields)

        avroEncoding $(varP argName) = mconcat encodedFields
          where encodedFields = $(listE $ encodeField <$> fields)
    |]
  where -- an expression for a single field:
        -- ("a", let Foo { a = x } = value in toTheta x)
        field :: Theta.Field Theta.Type -> Q Exp
        field Theta.Field { Theta.fieldName } =
          [e| let $(patternMatch fieldName) = $(varE argName) in Conversion.toTheta x |]

        -- an expression encoding the value of a single field to binary:
        -- let Foo { a = x } = value in avroEncoding x
        encodeField :: Theta.Field Theta.Type -> Q Exp
        encodeField Theta.Field { Theta.fieldName } =
          [e| let $(patternMatch fieldName) = $(varE argName) in
                Conversion.avroEncoding x |]

        patternMatch fieldName =
          recP (toName name) [fieldPat (toFieldName fieldName) [p| x |]]

-- | Generates a 'Conversion.ToTheta' instance for a variant, relying
-- on the 'ToTheta' of each field type.
--
-- Given the following Theta variant from a module @com.example.bar@:
--
-- @
-- type com.example.Foo = Baz { a : Int }
--                      | Qux { a : String, b : Long }
-- @
--
-- we get the folllowing 'Conversion.ToTheta' instance:
--
-- @
-- instance ToTheta Foo where
--   toTheta value = Theta.Value
--     { Theta.value   = variant
--     , Theta.module_ = theta'bar
--     , Theta.type_   =
--         let name = Name { namespace = ["com", "example"], name = "Foo" } in
--         case Theta.lookupName name theta'bar of
--           Right res -> res
--           Left err  -> error $ "Template Haskell bug: " <> err
--     }
--     where variant = case value of
--       Baz x   ->
--         Theta.Variant "Baz" $ Vector.fromList [toTheta x]
--       Qux x y ->
--         Theta.Variant "Qux" $ Vector.fromList [toTheta x, toTheta y]
--
--   avroEncoding value = case value of
--     Baz x   -> encodeInt 0 <> mconcat [avroEncoding x]
--     Qux x y -> encodeInt 1 <> mconcat [avroEncoding x, avroEncoding y]
-- @
variantToTheta :: Name -> Name.Name -> NonEmpty (Theta.Case Theta.Type) -> Q [Dec]
variantToTheta moduleName name cases =
  [d| instance Conversion.ToTheta $(conT $ toName name) where
        toTheta $(varP argName) = Theta.Value
          { Theta.value   = variant
          , Theta.type_   = $(thetaType moduleName name)
          }
         where variant = $(caseE (varE argName) $ caseToTheta <$> toList cases)

        avroEncoding $(varP argName) =
          $(caseE (varE argName) $ encodeCase <$> [0..] `zip` toList cases)
    |]
   where caseToTheta case_@Theta.Case { Theta.caseName } =
           caseMatch case_ $ \ parameters ->
             let wrapped = listE $ [[e| Conversion.toTheta $(arg)|] | arg <- parameters]
             in
             [e| Theta.Variant $(generateName caseName) (Vector.fromList $(wrapped)) |]

         encodeCase (index, case_) = caseMatch case_ $ \ parameters ->
           let i        = litE $ integerL index
               wrap exp = [e| Conversion.avroEncoding $(exp) |]
           in
           [e| Conversion.encodeInt $(i) <> mconcat $(listE $ wrap <$> parameters) |]

         caseMatch Theta.Case { Theta.caseName, Theta.caseParameters } body = do
           parameterVars <- replicateM (length $ Theta.fields caseParameters) (newName "x")

           let casePattern = conP constructor (varP <$> parameterVars)
               constructor = mkName $ Text.unpack $ Name.name caseName

           match casePattern (normalB $ body $ varE <$> parameterVars) []

-- *** FromTheta

-- | Generate a 'Conversion.ToTheta' instance for a Haskell
-- type. These instances only make sense for structrued types—records,
-- variants and newtypes. Calling 'fromThetaInstance' on other Theta
-- types generates no declarations.
--
-- The instance for a newtype uses the 'FromTheta' instance of its
-- underlying type:
--
-- @
-- instance FromTheta Foo where
--   fromTheta' value@Value { type_ } =
--     Foo <$> withContext type_ (fromTheta' value)
--
--   avroDecoding = Foo <$> avroDecoding
-- @
--
-- Refer to the documentation of 'recordFromTheta' and
-- 'variantFromTheta' for what their respective instances look like.
fromThetaInstance :: Theta.Type
                     -- ^ The schema for whose Haskell representation
                     -- we're generating the instance.
                  -> Q [Dec]
fromThetaInstance type_ = case Theta.baseType type_ of
  Theta.Record' name fields -> recordFromTheta name fields
  Theta.Variant' name cases -> variantFromTheta name cases
  Theta.Newtype' name _     ->
    [d| instance Conversion.FromTheta $(conT $ toName name) where
          fromTheta' value@Theta.Value { Theta.type_ } =
            $(conE $ toName name) <$>
              Conversion.withContext type_ (Conversion.fromTheta' value)

          avroDecoding = $(conE $ toName name) <$> Conversion.avroDecoding
      |]

  _                         -> [d| |]

-- | Generates a 'Conversion.FromTheta' instance for a record, relying
-- on the 'Conversion.FromTheta' instances for each field type.
--
-- Given the following Theta type:
--
-- @
-- type com.example.Foo = { a : Int, b : String }
-- @
--
-- we get the following 'Conversion.FromTheta' instance:
--
-- @
-- instance FromTheta Foo where
--   fromTheta' v@Theta.Value { Theta.type_, Theta.value } = case value of
--     Theta.Record fields -> do
--       checkSchema @Foo v
--       withContext type_ $ do
--         x <- fromTheta' $ fields ! 0
--         y <- fromTheta' $ fields ! 1
--         pure $ Foo { a = x, b = y }
--     _                   -> mismatch (theta @Foo) type_
--
--   avroDecoding = do
--     x <- avroDecoding
--     y <- avroDecoding
--     pure $ Foo { a = x, b = y }
-- @
recordFromTheta :: Name.Name -> Theta.Fields Theta.Type -> Q [Dec]
recordFromTheta name Theta.Fields { Theta.fields } = do
  -- names for the do-notation variables
  names <- replicateM (length fields) (newName "x")
  [d| instance Conversion.FromTheta $(conT $ toName name) where
        fromTheta' v@Theta.Value { Theta.type_, Theta.value } = case value of
          Theta.Record $(varP $ mkName "fields") -> do
            $(apCheckSchema) v
            Conversion.withContext type_ $(extractFields names)
          _                   -> Conversion.mismatch $(apTheta) type_

        avroDecoding = $(doE $ (fieldDecoding <$> names)
                            <> [noBindS [e| pure $(record names) |]])
    |]
  where extractFields :: [Name] -> Q Exp
        extractFields names = doE $ (field <$> zip names [0..])
                                 <> [noBindS [e| pure $(record names) |]]

        field :: (Name, Integer) -> Q Stmt
        field (bindingName, index) = bindS (varP bindingName)
          [e| Conversion.fromTheta' $ $(varE fieldMap) ! $(litE $ integerL index) |]

        fieldDecoding :: Name -> Q Stmt
        fieldDecoding bindingName =
          bindS (varP bindingName) [e| Conversion.avroDecoding |]

        record :: [Name] -> Q Exp
        record names = recConE (toName name) $ binding <$> zip names fields

        binding :: (Name, Theta.Field Theta.Type) -> Q (Name, Exp)
        binding (bindingName, Theta.Field { Theta.fieldName }) =
          (toFieldName fieldName,) <$> (varE bindingName)

        fieldMap = mkName "fields"

        -- Apply `checkSchema` to _ and then name, since the first
        -- quantified type is `m`
        apCheckSchema = appTypeE [e| Conversion.checkSchema @_ |]
                        (conT $ toName name)
        apTheta       = appTypeE [e| theta |] (conT $ toName name)

-- | Generate a 'Conversion.FromTheta' instance for a variant, relying
-- on the 'Conversion.FromTheta' instances for each field type.
--
-- Given the following Theta variant:
--
-- @
-- type com.example.Foo = Baz { a : Int }
--                      | Qux { a : String, b : Long }
-- @
--
-- we get the following 'Conversion.FromTheta' instance:
--
-- @
-- instance FromTheta Foo where
--   fromTheta' v@Theta.Value { Theta.type_, Theta.value } = case value of
--     Theta.Variant caseName parameters -> do
--       checkSchema @Foo v
--       withContext type_ $ case caseName of
--         "baz"   -> pure Baz <*> fromTheta' (parameters ! 0)
--         "qux"   -> pure Qux <*> fromTheta' (parameters ! 0)
--                             <*> fromTheta' (parameters ! 1)
--         invalid -> error $ "Invalid case name '" <> Text.unpack invalid <> "'!"
--     _                             ->
--       mismatch (theta @Foo) type_
--
--   avroDecoding = do
--     tag <- Avro.getLong
--     case tag of
--       0       -> pure Baz <*> avroDecoding
--       1       -> pure Qux <*> avroDecoding <*> avroDecoding
--       invalid ->
--         fail "Invalid case tag. Expected [0..1] but got " <> show invalid <> "!"
-- @
variantFromTheta :: Name.Name -> NonEmpty (Theta.Case Theta.Type) -> Q [Dec]
variantFromTheta name (toList -> cases) =
  [d| instance Conversion.FromTheta $(conT $ toName name) where
        fromTheta' v@Theta.Value { Theta.type_, Theta.value } = case value of
          Theta.Variant $(varP caseName) $(varP parameters) -> do
            $(apCheckSchema) v
            Conversion.withContext type_ $(chooseCase)
          _                             ->
            Conversion.mismatch $(apTheta) type_

        avroDecoding = do
          tag <- Avro.getLong
          $(decodingCase)
    |]
  where
    chooseCase = caseE (varE caseName) $ (matchCase <$> cases) <> [baseCase]
      where
        matchCase Theta.Case { Theta.caseName, Theta.caseParameters } =
          match caseString (normalB matchBody) []
          where
            caseString = litP $ stringL $ Text.unpack $ Name.render caseName
            matchBody =
              foldl' apply [e| pure $(conE $ toName caseName) |] $ getValue <$> indices
            indices = [i | i <- [0..] | _ <- Theta.fields caseParameters]
            getValue index =
              let i = litE $ integerL index in
              [e| Conversion.fromTheta' $ ($(varE parameters) ! $(i)) |]

        baseCase = match (varP invalid) (normalB errorCall) []
        errorCall = [e| error $ "Invalid case name '"
                             <> Text.unpack (Name.render $(varE invalid)) <> "'!"
                      |]

    apCheckSchema = appTypeE [e| Conversion.checkSchema @_ |] (conT $ toName name)
    apTheta = appTypeE [e| theta |] (conT $ toName name)

    decodingCase = caseE [e| tag |] $ (decodeTag <$> [0..] `zip` cases) <> [baseCase]
      where
        decodeTag (tag, case_) =
          match (litP $ integerL tag) (normalB $ matchBody case_) []
        matchBody (Theta.Case name _ Theta.Fields { Theta.fields }) =
          foldl' apply [e| pure $(conE $ toName name) |] $
            [e| Conversion.avroDecoding |] <$ fields

        baseCase =
          match (varP invalid) (normalB [e| fail $ $(litE $ stringL message)
                                                <> show $(varE invalid) <> "!" |]) []
        message = "Invalid case tag. Expected [0.." <> show maxTag <> "but got"
        maxTag = length cases - 1

    apply exp arg = infixE (Just exp) [e| (<*>) |] (Just arg)

    invalid    = mkName "invalid"
    caseName   = mkName "caseName"
    parameters = mkName "_parameters"

-- ** Types

-- | Generate a Haskell expression that evaluates to the given
-- 'Theta.Type' value. This lets us transfer compile-time 'Theta.Type'
-- values to runtime.
generateThetaExp :: Theta.Type -> Q Exp
generateThetaExp type_ = case Theta.baseType type_ of
  Theta.Bool'               -> [e| Theta.bool' |]
  Theta.Bytes'              -> [e| Theta.bytes' |]
  Theta.Int'                -> [e| Theta.int' |]
  Theta.Long'               -> [e| Theta.long' |]
  Theta.Float'              -> [e| Theta.float' |]
  Theta.Double'             -> [e| Theta.double' |]
  Theta.String'             -> [e| Theta.string' |]
  Theta.Date'               -> [e| Theta.date' |]
  Theta.Datetime'           -> [e| Theta.datetime' |]

  Theta.Array' type_        -> [e| Theta.array' $(generateThetaExp type_) |]
  Theta.Map' type_          -> [e| Theta.map' $(generateThetaExp type_) |]
  Theta.Optional' type_     -> [e| Theta.optional' $(generateThetaExp type_) |]

  Theta.Record' name fields -> wrap $ recordExp name fields
  Theta.Variant' name cases -> wrap $ variantExp name cases

  Theta.Reference' name     -> wrap [e| Theta.Reference' $(generateName name) |]
  Theta.Newtype' name type_ -> wrap $ newtypeExp name type_
  where wrap exp =
          [e| Theta.withModule' $(varE $ globalModuleName $ Theta.module_ type_) $(exp) |]

        recordExp name Theta.Fields { Theta.fields } =
          [e| Theta.Record' $(generateName name)
                            (Theta.wrapFields $(listE $ fieldExp <$> fields)) |]
        fieldExp Theta.Field {..} =
          [e| Theta.Field $(generateFieldName fieldName)
                          $(generateDoc fieldDoc)
                          $(generateThetaExp fieldType) |]

        variantExp name cases =
          [e| Theta.Variant' $(generateName name) (NonEmpty.fromList $(casesExp cases)) |]
        casesExp = listE . map caseExp . NonEmpty.toList
        caseExp (Theta.Case name doc Theta.Fields { Theta.fields }) =
          [e| Theta.Case $(generateName name)
                         $(generateDoc doc)
                         (Theta.wrapFields $(listE $ fieldExp <$> fields)) |]

        newtypeExp name type_ =
          [e| Theta.Newtype' $(generateName name) $(generateThetaExp type_) |]

-- | Generate the Haskell type signature for a Theta type. This is how
-- the type is referenced in other signatures.
--
-- Atomic types become their corresponding Haskell type (@Int@ becomes
-- 'Int32', @Bytes@ becomes 'ByteString'... etc).
--
-- Arrays become lists and maps become 'HashMap's.
--
-- Named types (references, records, variants and newtypes) become a
-- reference to their name.
generateType :: Theta.Type -> Q Type
generateType type_ = case Theta.baseType type_ of
  Theta.Bool'           -> [t| Bool |]
  Theta.Bytes'          -> [t| ByteString |]
  Theta.Int'            -> [t| Int32 |]
  Theta.Long'           -> [t| Int64 |]
  Theta.Float'          -> [t| Float |]
  Theta.Double'         -> [t| Double |]
  Theta.String'         -> [t| Text |]
  Theta.Date'           -> [t| Day |]
  Theta.Datetime'       -> [t| UTCTime |]

  Theta.Array' items    -> [t| [$(generateType items)] |]
  Theta.Map' values     -> [t| HashMap Text $(generateType values) |]
  Theta.Optional' type_ -> [t| Maybe $(generateType type_) |]

  Theta.Record' name _  -> toType name
  Theta.Variant' name _ -> toType name

  Theta.Reference' name -> toType name
  Theta.Newtype' name _ -> toType name

-- ** Names

-- TODO: how to support namespaces here?
-- | Wrap a 'Name.Name' into a Template Haskell identifier.
toName :: Name.Name -> Name
toName (Name.Name { Name.name }) = mkName $ Text.unpack name

-- | Wrap a 'Theta.FieldName' into a Template Haskell identifier.
toFieldName :: Theta.FieldName -> Name
toFieldName = mkName . Text.unpack . Theta.textName

-- | Wrap a 'Name.Name' into a Template Haskell type that references
-- the same name.
toType :: Name.Name -> Q Type
toType = conT . toName

-- | Define a type synonym with the given name and Haskell type. This
-- can be used to deal with aliases from Theta (ie @type Foo = Bool@).
typeSynonym :: Name.Name -> Q Type -> Q [Dec]
typeSynonym (toName -> name) type_ = pure <$> tySynD name [] type_

-- | Generate an expression that evaluates to the given 'Name.Name'.
generateName :: Name.Name -> Q Exp
generateName (Name.Name { Name.name, Name.moduleName }) =
  [e| Name.Name { Name.name       = $(textExp name)
                , Name.moduleName = $(generateModuleName moduleName)
                }
    |]
  where textExp = litE . stringL . Text.unpack

-- | Generate an expression that evaluates to the given
-- 'Name.ModuleName'.
generateModuleName :: Name.ModuleName -> Q Exp
generateModuleName Name.ModuleName { Name.baseName, Name.namespace } =
  [e| Name.ModuleName { Name.baseName = $(textExp baseName)
                      , Name.namespace = $(listE $ textExp <$> namespace)
                      }
    |]
  where textExp = litE . stringL . Text.unpack

-- | Generate an expression that evaluates to the given
-- 'Theta.FieldName'.
--
-- The name @"foo"@ generates the following Haskell expression:
--
-- @
-- Theta.FieldName "foo"
-- @
generateFieldName :: Theta.FieldName -> Q Exp
generateFieldName (Theta.FieldName name) =
  [e| Theta.FieldName $(litE $ stringL $ Text.unpack name) |]

-- | Generate an expression that evaluates to the given 'Theta.Doc'.
--
-- The doc @"doc"@ generates:
--
-- @
-- Theta.Doc "doc"
-- @
generateDoc :: Maybe Theta.Doc -> Q Exp
generateDoc (Just doc) =
  [e| Just $ Theta.Doc $(litE $ stringL $ Text.unpack $ Theta.getText doc) |]
generateDoc Nothing = [e| Nothing |]
