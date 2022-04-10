{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-pattern-binds #-}
-- for some reason, my TH code produces false-positive "unused
-- binds" and "unused matches" warnings

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ParallelListComp    #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
module Theta.Target.Haskell
  ( loadModule
  , disambiguateConstructors
  )
where

import           Control.Monad.Except
import           Control.Monad.State             (evalState, evalStateT, get,
                                                  put)

import qualified Data.Avro                       as Avro
import qualified Data.Avro.Encoding.FromAvro     as Avro
import qualified Data.Avro.Encoding.ToAvro       as Avro
import           Data.Avro.HasAvroSchema         (HasAvroSchema (..))
import qualified Data.Avro.Internal.Get          as Avro
import           Data.ByteString.Lazy            (ByteString)
import qualified Data.Char                       as Char
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
import           Data.Time                       (Day, LocalTime, TimeOfDay,
                                                  UTCTime)
import           Data.UUID                       (UUID)
import           Data.Vector                     ((!))
import qualified Data.Vector                     as Vector

import qualified GHC.Exts                        as Exts
import           GHC.Generics                    (Generic)

import qualified Language.Haskell.TH             as TH
import qualified Language.Haskell.TH.Syntax      as TH

import           Theta.Error                     (Error)
import           Theta.Fixed                     (FixedBytes)
import qualified Theta.Import                    as Import
import           Theta.Metadata                  (Metadata (..))
import qualified Theta.Name                      as Name
import qualified Theta.Pretty                    as Theta
import qualified Theta.Primitive                 as Primitive
import qualified Theta.Types                     as Theta
import qualified Theta.Value                     as Theta

import           Theta.Target.Avro.Types         (typeToAvro)
import qualified Theta.Target.Avro.Values        as Theta

import qualified Theta.Target.Haskell.Conversion as Conversion
import           Theta.Target.Haskell.HasTheta   (HasTheta (..))

-- * Generating Haskell types

-- | Generate Haskell types from the specified Theta module. This will
-- generate a type for every definition in the given Theta module as
-- well as all the definitions it imports from other modules.
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
-- Both the load path and the module name have 'IsString' instances,
-- so the most common pattern is calling 'loadModule' with two strings:
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE TemplateHaskell   #-}
-- @
--
-- @
-- loadModule "specs/theta:other-specs/theta" "com.example.foo"
-- @
loadModule :: Import.LoadPath
           -- ^ The Theta load path—one or more root directories that
           -- Theta will search for the module with the given name.
           --
           -- This takes the same kind of load path and syntax used by
           -- the @THETA_LOAD_PATH@ environment variable: one or more
           -- file paths separated by @:@.
           --
           -- Paths can be relative or absolute; when compiling with
           -- GHC, relative paths will be resolved relative to your
           -- project's root directory (ie where your .cabal file is
           -- located).
           --
           -- Example:
           --
           -- @
           -- "specs/theta:other-specs/theta"
           -- @
           -> Name.ModuleName
           -- ^ The fully qualified name of the module written out
           -- with dots. Example: @"com.target.foo"@ for a module
           -- named @"foo"@ in the @"com.target"@ namespace.
           -> TH.Q [TH.Dec]
loadModule loadPath moduleName = do
  (module_, moduleFiles) <- runTheta $ do
    (definition, path) <- Import.getModuleDefinition loadPath moduleName
    (module_, paths)   <- Import.resolveModule loadPath definition
    pure (module_, path : paths)

  -- tells GHC it needs to recompile the module if any of the loaded
  -- schema files change
  traverse_ TH.qAddDependentFile moduleFiles

  generateDefinitions module_

  where runTheta :: ExceptT Error IO a -> TH.Q a
        runTheta theta = TH.runIO $ runExceptT theta >>= \case
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
generateDefinitions :: Theta.Module -> TH.Q [TH.Dec]
generateDefinitions module_ = concat <$> traverse generate modules
  where modules = Theta.transitiveImports [module_]

        generate module_ = do
          let topLevelName = globalModuleName module_

          TH.lookupValueName (TH.nameBase topLevelName) >>= \case
            Just _  -> pure []
            Nothing -> do
              moduleSignature  <- TH.sigD topLevelName [t| Theta.Module |]
              moduleDefinition <-
                TH.valD (TH.varP topLevelName) (TH.normalB $ generateModule module_) []

              definitions <-
                traverse (generateDefinition topLevelName) (Theta.types module_)

              pure $ moduleSignature : moduleDefinition : concat definitions

-- | The name of the global definition storing the given Theta module.
--
-- This name is the fully qualified name of the module with @.@
-- replaced by @'@ and prefixed with @theta'@. The module
-- @com.example.foo@ would have the Haskell global name
-- @theta'com'example'foo@.
globalModuleName :: Theta.Module -> TH.Name
globalModuleName module_ = TH.mkName $ "theta'" <> Text.unpack fullName
  where fullName = Text.intercalate "'" $ Exts.toList $ Theta.moduleName module_

-- | Generate an expression that evaluates to the given
-- 'Theta.Module'. This will include a literal 'Map' with all the
-- bindings in the module.
--
-- @
-- HashMap.fromList [("TypeName", <type expression>), …]
-- @
generateModule :: Theta.Module -> TH.Q TH.Exp
generateModule (Theta.Module name (Map.toList -> bindings) imports metadata) =
  [e| Theta.Module { Theta.moduleName = name
                   , Theta.types      = Map.fromList $(moduleExps)
                   , Theta.imports    = $(TH.listE $ TH.varE . globalModuleName <$> imports)
                   , Theta.metadata   = metadata
                   }
    |]
  where moduleExps = TH.listE $ binding <$> bindings
        binding (name, Theta.Definition {..}) =
          [e| (name, Theta.Definition
                { Theta.definitionName = definitionName
                , Theta.definitionDoc  = definitionDoc
                , Theta.definitionType = $(generateThetaExp definitionType)
                })
            |]

-- | Generate a Haskell expression that evaluates to the given
-- 'Theta.Type' value. This lets us transfer compile-time 'Theta.Type'
-- values to runtime.
generateThetaExp :: Theta.Type -> TH.Q TH.Exp
generateThetaExp type_ = case Theta.baseType type_ of
  Theta.Primitive' p -> case p of
    Primitive.Bool          -> [e| Theta.bool' |]
    Primitive.Bytes         -> [e| Theta.bytes' |]
    Primitive.Int           -> [e| Theta.int' |]
    Primitive.Long          -> [e| Theta.long' |]
    Primitive.Float         -> [e| Theta.float' |]
    Primitive.Double        -> [e| Theta.double' |]
    Primitive.String        -> [e| Theta.string' |]
    Primitive.Date          -> [e| Theta.date' |]
    Primitive.Datetime      -> [e| Theta.datetime' |]
    Primitive.UUID          -> [e| Theta.uuid' |]
    Primitive.Time          -> [e| Theta.time' |]
    Primitive.LocalDatetime -> [e| Theta.localDatetime' |]

  Theta.Fixed' size         -> [e| Theta.fixed' size |]

  Theta.Array' type_        -> [e| Theta.array' $(generateThetaExp type_) |]
  Theta.Map' type_          -> [e| Theta.map' $(generateThetaExp type_) |]
  Theta.Optional' type_     -> [e| Theta.optional' $(generateThetaExp type_) |]

  Theta.Enum' name symbols  -> wrap $ enumExp name symbols
  Theta.Record' name fields -> wrap $ recordExp name fields
  Theta.Variant' name cases -> wrap $ variantExp name cases

  Theta.Reference' name     -> wrap [e| Theta.Reference' name |]
  Theta.Newtype' name type_ -> wrap $ newtypeExp name type_
  where wrap exp = [e| Theta.withModule' $(TH.varE globalModule) $(exp) |]
        globalModule = globalModuleName $ Theta.module_ type_

        enumExp name symbols = [e| Theta.Enum' name symbols |]

        recordExp name Theta.Fields { Theta.fields } =
          [e| Theta.Record' name (Theta.wrapFields $(TH.listE $ fieldExp <$> fields)) |]
        fieldExp Theta.Field {..} =
          [e| Theta.Field fieldName fieldDoc $(generateThetaExp fieldType) |]

        variantExp name cases =
          [e| Theta.Variant' name (NonEmpty.fromList $(casesExp cases)) |]
        casesExp = TH.listE . map caseExp . NonEmpty.toList
        caseExp (Theta.Case name doc Theta.Fields { Theta.fields }) =
          [e| Theta.Case name doc (Theta.wrapFields $(TH.listE $ fieldExp <$> fields)) |]

        newtypeExp name type_ = [e| Theta.Newtype' name $(generateThetaExp type_) |]


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
generateDefinition :: TH.Name
                   -- ^ The name of the generated module expression
                   -- for the module this definition comes from (ie
                   -- @theta'foo@ for a module named "foo").
                   -> Theta.Definition Theta.Type
                   -> TH.Q [TH.Dec]
generateDefinition moduleName Theta.Definition {..} =
  case Theta.baseType definitionType of
    -- named types need to be defined explicitly
    Theta.Enum' name symbols  -> withInstances =<< generateEnum name symbols
    Theta.Record' name fields -> withInstances =<< generateRecord name fields
    Theta.Variant' name cases -> withInstances =<< generateVariant name cases
    Theta.Newtype' name type_ -> withInstances =<< generateNewtype name type_

    -- any other type becomes a type synonym
    _                         -> typeSynonym definitionName $ generateType definitionType
  where -- HasTheta, ToTheta and FromTheta instance declarations
        withInstances :: TH.Dec -> TH.Q [TH.Dec]
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

-- | Generates a Haskell type for an enum. The type will have a
-- constructor for each enum symbol.
--
-- The constructor name for a symbol is the symbol's name with the
-- first letter capitalized and any leading underscores dropped. If
-- these rules result in duplicate names, each subsequent constructor
-- will have an extra _ added to the end.
--
-- The following Theta enum:
--
-- @
-- enum com.example.Foo = Bar | baz | _Baz
-- @
--
-- will produce the following Haskell type:
--
-- @
-- data Foo = Bar | Baz | Baz_
-- @
generateEnum :: Name.Name -> NonEmpty Theta.EnumSymbol -> TH.Q TH.Dec
generateEnum name symbols =
  TH.dataD (pure []) (toName name) [] Nothing constructors [defaultClasses]
  where constructors = [ TH.normalC name [] | name <- enumConstructors name symbols ]

-- | Return a list of disambiguated constructor names for the given
-- set of enum symbols.
--
-- The constructor name for a symbol is that symbol with the leading
-- letter capitalized and any leading underscores stripped. If this
-- change results in multiple symbols mapping to the same name, each
-- subsequent name gets an additional trailing underscore.
--
-- @
-- enum Foo = Bar | baz | _Baz
-- @
--
-- will produce the constructors
--
-- @
-- Bar | Baz | Baz_
-- @
disambiguateConstructors :: Name.Name -> NonEmpty Theta.EnumSymbol -> [Text]
disambiguateConstructors enumName (NonEmpty.toList -> symbols) = names
  where names = evalState (mapM disambiguate symbols) Set.empty

        disambiguate (Theta.EnumSymbol (normalize -> symbol)) = do
          seen <- get
          if Set.member symbol seen
            then disambiguate $ Theta.EnumSymbol (symbol <> "_")
            else symbol <$ put (Set.insert symbol seen)

        normalize = capitalize . Text.dropWhile (== '_')
        capitalize text = case Text.uncons text of
          Just (c, cs) -> Text.cons (Char.toUpper c) cs
          Nothing      -> error $ "Empty enum symbol in " <> show enumName

-- | Return disambiguated contructor names (as 'Name') for the given
-- enum symbols.
--
-- See 'disambiguateConstructors' for the rules used to convert and
-- disambiguate constructor names.
enumConstructors :: Name.Name -> NonEmpty Theta.EnumSymbol -> [TH.Name]
enumConstructors enumName symbols =
  TH.mkName . Text.unpack <$> disambiguateConstructors enumName symbols

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
generateRecord :: Name.Name -> Theta.Fields Theta.Type -> TH.Q TH.Dec
generateRecord name Theta.Fields { Theta.fields } =
  TH.dataD (pure []) (toName name) [] Nothing [recordFields name pairs] [defaultClasses]
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
recordFields :: Name.Name -> [(Theta.FieldName, Theta.Type)] -> TH.Q TH.Con
recordFields (toName -> name) fields =
  TH.recC name $ generateField <$> fields
  where generateField (toFieldName -> fieldName, type_) =
          TH.varBangType fieldName $ parameter type_

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
generateVariant :: Name.Name -> NonEmpty (Theta.Case Theta.Type) -> TH.Q TH.Dec
generateVariant (toName -> name) (toList -> cases) =
  TH.dataD (pure []) name [] Nothing (generateCase <$> disambiguate cases) [defaultClasses]
  where generateCase (caseName, fields) = recordFields caseName fields

        disambiguate :: [Theta.Case Theta.Type]
                     -> [(Name.Name, [(Theta.FieldName, Theta.Type)])]
        disambiguate cases =
          [ (caseName, rename caseName <$> Theta.fields caseParameters)
          | Theta.Case { caseName, caseParameters } <- cases ]
          where rename (Name.Name _ caseName) Theta.Field {..}
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
generateNewtype :: Name.Name -> Theta.Type -> TH.Q TH.Dec
generateNewtype (toName -> name) type_ =
  TH.newtypeD (pure []) name [] Nothing (TH.normalC name [param type_]) [defaultClasses]
  where param = TH.bangType (TH.bang TH.noSourceUnpackedness TH.noSourceStrictness) . generateType

-- | This is the default set of classes that types for records,
-- variants and newtypes derive.
defaultClasses :: TH.Q TH.DerivClause
defaultClasses = TH.derivClause Nothing [[t| Eq |], [t| Show |], [t| Generic |]]

-- | Produce a parameter with the given type that is strict by
-- default.
parameter :: Theta.Type -> TH.BangTypeQ
parameter type_ = TH.bangType strict $ generateType type_
  where strict = TH.bang TH.noSourceUnpackedness TH.sourceStrict

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
thetaType :: TH.Name -> Name.Name -> TH.Q TH.Exp
thetaType moduleName definitionName =
  [e| case Theta.lookupName definitionName $(TH.varE moduleName) of
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
--   module_ _ = theta'com'example
--   theta _   =
--     let name = Name { namespace = ["com", "example"], name = "Foo" } in
--     case Theta.lookupName name theta'com'example of
--       Right res -> res
--       Left err  -> error $ "Tempalte Haskell bug: " <> err
-- @
hasThetaInstance :: TH.Name
                    -- ^ The top-level Haskell name for the module (ie
                    -- @theta'com'example@).
                 -> Name.Name
                    -- ^ The fully qualified name of the Theta type.
                 -> TH.Q [TH.Dec]
hasThetaInstance moduleName definitionName =
  [d| instance HasTheta $(TH.conT $ toName definitionName) where
        theta = $(thetaType moduleName definitionName)
    |]

-- *** Avro Instances

-- | Generate a 'HasAvroSchema' instance that calls 'typeToAvro' under the hood.
--
-- The result of 'typeToAvro' might error out, which would mean the TH
-- code has a bug.
hasAvroInstance :: Name.Name -> TH.Q [TH.Dec]
hasAvroInstance (toName -> type_) =
  [d| instance HasAvroSchema $(TH.conT type_) where
        schema = Tagged $ case evalStateT toAvro Set.empty of
          Left err  -> error $ Text.unpack $ Theta.pretty err
          Right res -> res
          where toAvro                = typeToAvro definitionAvroVersion $(thetaType)
                definitionAvroVersion = avroVersion $ Theta.metadata $ Theta.module_ $(thetaType)
    |]
 where thetaType = TH.appTypeE [e|theta|] (TH.conT type_)

-- | Generate a 'Avro.ToAvro' instance for the type. This might
-- generate an error, which would mean this TH code has a bug.
--
-- @
-- instance Avro.ToAvro Foo where
--   toAvro = Theta.toAvro . toTheta
-- @
toAvroInstance :: Name.Name -> TH.Q [TH.Dec]
toAvroInstance (toName -> type_) =
  [d| instance Avro.ToAvro $(TH.conT type_) where
        toAvro _ = Conversion.avroEncoding
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
fromAvroInstance :: Name.Name -> TH.Q [TH.Dec]
fromAvroInstance (toName -> type_) =
  [d| instance Avro.FromAvro $(TH.conT type_) where
        fromAvro avro =
          case Conversion.fromTheta =<< Theta.fromAvro $(schema) avro of
            Left err  -> Left $ Text.unpack $ Theta.pretty err
            Right res -> Right res
    |]
 where schema = TH.appTypeE [e|theta|] (TH.conT type_)

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
toThetaInstance :: TH.Name
                   -- ^ The top-level haskell name for the module (ie
                   -- @theta'com'example@).
                -> Theta.Type
                   -- ^ The Theta type to generate the
                   -- 'Conversion.ToTheta' instance for.
                -> TH.Q [TH.Dec]
toThetaInstance moduleName type_ = case Theta.baseType type_ of
  Theta.Enum' name symbols  -> enumToTheta moduleName name symbols
  Theta.Record' name fields -> recordToTheta moduleName name fields
  Theta.Variant' name cases -> variantToTheta moduleName name cases
  Theta.Newtype' name _     ->
    [d| instance Conversion.ToTheta $(TH.conT $ toName name) where
          toTheta $(TH.conP (toName name) [ [p| x |] ]) = Conversion.toTheta x

          avroEncoding $(TH.conP (toName name) [ [p| x |] ]) = Conversion.avroEncoding x
      |]
            -- primitive types and containers do not need custom instances
  _                          -> [d| |]

-- | A capturable name that we use to define 'toTheta' functions.
argName :: TH.Name
argName = TH.mkName "value"

-- | Generate a 'Conversion.ToTheta' instance for an enum.
--
-- Given:
--
-- @
-- enum com.example.Foo = Bar | baz | _Baz
-- @
--
-- we get:
--
-- @
-- instance ToTheta Foo where
--   toTheta value = Theta.Value
--     { Theta.value   = enumValue
--     , Theta.type_   = let name = Name { namespace = ["com", "example"], name = "Foo" } in
--         case Theta.lookupName name theta'com'example of
--           Right res -> res
--           Left err  -> error $ "Template Haskell bug: " <> err
--     }
--     where enumValue = case value of
--       Bar  -> Theta.Enum (Theta.EnumSymbol "Bar")
--       Baz  -> Theta.Enum (Theta.EnumSymbol "baz")
--       Baz_ -> Theta.Enum (Theta.EnumSymbol "_Baz")
--
--   avroEncoding value = case value of
--     Bar  -> encodeInt 0
--     Baz  -> encodeInt 1
--     Baz_ -> encodeInt 2
-- @
enumToTheta :: TH.Name -> Name.Name -> NonEmpty Theta.EnumSymbol -> TH.Q [TH.Dec]
enumToTheta moduleName enumName symbols =
  [d| instance Conversion.ToTheta $(TH.conT $ toName enumName) where
        toTheta $(TH.varP argName) = Theta.Value
          { Theta.value =
              $(TH.caseE (TH.varE argName) $ symbolToTheta <$> constructors)
          , Theta.type_ =
              $(thetaType moduleName enumName)
          }

        avroEncoding $(TH.varP argName) =
          $(TH.caseE (TH.varE argName) $ encodeSymbol <$> constructors)
    |]
  where symbolToTheta (symbol, constructor, _) =
          match constructor [e| Theta.Enum symbol |]
        encodeSymbol (_, constructor, i) =
          match constructor [e| Conversion.encodeInt i |]

        constructors = [ (symbol, TH.conP constructor [], i)
                       | symbol <- NonEmpty.toList symbols
                       | constructor <- enumConstructors enumName symbols
                       | i <- [0::Int ..]
                       ]

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
--     , Theta.type_   =
--         let name = Name { namespace = ["com", "example"], name = "Foo" } in
--         case Theta.lookupName name theta'com'example of
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
recordToTheta :: TH.Name
                 -- ^ The top-level haskell name for the module (ie
                 -- @theta'com'example@).
              -> Name.Name
                 -- ^ The name of the Theta record.
              -> Theta.Fields Theta.Type
                 -- ^ The fields of the Theta record.
              -> TH.Q [TH.Dec]
recordToTheta moduleName name Theta.Fields { Theta.fields } =
  [d| instance Conversion.ToTheta $(TH.conT $ toName name) where
        toTheta $(TH.varP argName) = Theta.Value
          { Theta.value   = record
          , Theta.type_   = $(thetaType moduleName name)
          }
          where record = Theta.Record $ Vector.fromList $(TH.listE $ field <$> fields)

        avroEncoding $(TH.varP argName) = mconcat encodedFields
          where encodedFields = $(TH.listE $ encodeField <$> fields)
    |]
  where -- an expression for a single field:
        -- ("a", let Foo { a = x } = value in toTheta x)
        field :: Theta.Field Theta.Type -> TH.Q TH.Exp
        field Theta.Field { Theta.fieldName } =
          [e| let $(patternMatch fieldName) = $(TH.varE argName) in Conversion.toTheta x |]

        -- an expression encoding the value of a single field to binary:
        -- let Foo { a = x } = value in avroEncoding x
        encodeField :: Theta.Field Theta.Type -> TH.Q TH.Exp
        encodeField Theta.Field { Theta.fieldName } =
          [e| let $(patternMatch fieldName) = $(TH.varE argName) in
                Conversion.avroEncoding x |]

        patternMatch fieldName =
          TH.recP (toName name) [TH.fieldPat (toFieldName fieldName) [p| x |]]

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
--     , Theta.type_   =
--         let name = Name { namespace = ["com", "example"], name = "Foo" } in
--         case Theta.lookupName name theta'com'example of
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
variantToTheta :: TH.Name -> Name.Name -> NonEmpty (Theta.Case Theta.Type) -> TH.Q [TH.Dec]
variantToTheta moduleName name cases =
  [d| instance Conversion.ToTheta $(TH.conT $ toName name) where
        toTheta $(TH.varP argName) = Theta.Value
          { Theta.value   = variant
          , Theta.type_   = $(thetaType moduleName name)
          }
         where variant = $(TH.caseE (TH.varE argName) $ caseToTheta <$> toList cases)

        avroEncoding $(TH.varP argName) =
          $(TH.caseE (TH.varE argName) $ encodeCase <$> [0::Int ..] `zip` toList cases)
    |]
   where caseToTheta case_@Theta.Case { Theta.caseName } =
           caseMatch case_ $ \ parameters ->
             let wrapped = TH.listE $ [[e| Conversion.toTheta $(arg)|] | arg <- parameters] in
             [e| Theta.Variant caseName (Vector.fromList $(wrapped)) |]

         encodeCase (i, case_) = caseMatch case_ $ \ parameters ->
           let wrap exp = [e| Conversion.avroEncoding $(exp) |] in
           [e| Conversion.encodeInt i <> mconcat $(TH.listE $ wrap <$> parameters) |]

         caseMatch Theta.Case { Theta.caseName, Theta.caseParameters } body = do
           parameterVars <- replicateM (length $ Theta.fields caseParameters) (TH.newName "x")

           let casePattern = TH.conP constructor (TH.varP <$> parameterVars)
               constructor = TH.mkName $ Text.unpack $ Name.name caseName

           match casePattern $ body $ TH.varE <$> parameterVars

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
                  -> TH.Q [TH.Dec]
fromThetaInstance type_ = case Theta.baseType type_ of
  Theta.Enum' name symbols  -> enumFromTheta name symbols
  Theta.Record' name fields -> recordFromTheta name fields
  Theta.Variant' name cases -> variantFromTheta name cases
  Theta.Newtype' name _     ->
    [d| instance Conversion.FromTheta $(TH.conT $ toName name) where
          fromTheta' value@Theta.Value { Theta.type_ } =
            $(TH.conE $ toName name) <$>
              Conversion.withContext type_ (Conversion.fromTheta' value)

          avroDecoding = $(TH.conE $ toName name) <$> Conversion.avroDecoding
      |]

  _                         -> [d| |]

-- | Generates a 'Conversion.FromTheta' instance for an enum.
--
-- Given an enum:
--
-- @
-- enum Foo = Bar | baz | _Baz
-- @
--
-- we get the following 'Conversion.FromTheta' instance:
--
-- @
-- instance FromTheta Foo where
--   fromTheta' v@Theta.Value { Theta.type_, Theta.value } = case value of
--     Theta.Enum symbol -> do
--       checkSchema @Foo v
--       case symbol of
--         "Bar"   -> pure Bar
--         "baz"   -> pure Baz
--         "_Baz"  -> pure Baz_
--         invalid -> error $ "Invalid enum symbol ‘" <> Text.unpack invalid <> "’!"
--     _ -> mismatch (theta @Foo) type_
--
--   avroDecoding = do
--     tag <- Avro.getLong
--     case tag of
--       0 -> pure Bar
--       1 -> pure Baz
--       2 -> pure Baz_
--       invalid ->
--         fail ("Invalid enum tag. Expected [0..2] but got " <> show invalid <> ".")
-- @
enumFromTheta :: Name.Name -> NonEmpty Theta.EnumSymbol -> TH.Q [TH.Dec]
enumFromTheta name symbols =
  [d|
   instance Conversion.FromTheta $(TH.conT $ toName name) where
     fromTheta' v@Theta.Value { Theta.type_, Theta.value } = case value of
       Theta.Enum symbol -> do
         $(apCheckSchema) v
         $(enumCase)
       _ -> Conversion.mismatch $(apTheta) type_

     avroDecoding = do
       tag <- Avro.getLong
       $(decodingCase)
    |]
  where
    enumCase = TH.caseE [e| symbol |] $ (caseBranch <$> constructors) <> [baseCase]
      where
        caseBranch (name, constructor) =
          match (TH.litP $ TH.stringL name) [e| pure $(constructor) |]

        baseCase = match (TH.varP invalid) [e|
            error $ "Invalid enum symbol ‘"
                 <> Text.unpack (Theta.enumSymbol $(TH.varE invalid))
                 <> "’!"
          |]

    decodingCase =
      TH.caseE [e| tag |] $ [ decodeTag tag c | tag <- [0..] | (_, c) <- constructors ]
                         <> [ baseCase ]
      where
        decodeTag tag constructor =
          match (TH.litP $ TH.integerL tag) [e| pure $(constructor) |]
        baseCase = match (TH.varP invalid)
          [e| fail (message <> show $(TH.varE invalid) <> ".") |]
        message = "Invalid enum tag. Expected [0.."
               <> show (length symbols - 1)
               <> "] but got"

    constructors = [ (Text.unpack name, TH.conE constructor)
                   | Theta.EnumSymbol name <- NonEmpty.toList symbols
                   | constructor <- enumConstructors name symbols
                   ]

    -- Apply `checkSchema` to _ and then name, since the first
    -- quantified type in checkSchema is `m`
    apCheckSchema =
      TH.appTypeE [e| Conversion.checkSchema @_ |] (TH.conT $ toName name)
    apTheta = TH.appTypeE [e| theta |] (TH.conT $ toName name)

    invalid = TH.mkName "invalid"

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
recordFromTheta :: Name.Name -> Theta.Fields Theta.Type -> TH.Q [TH.Dec]
recordFromTheta name Theta.Fields { Theta.fields } = do
  -- names for the do-notation variables
  names <- replicateM (length fields) (TH.newName "x")
  [d| instance Conversion.FromTheta $(TH.conT $ toName name) where
        fromTheta' v@Theta.Value { Theta.type_, Theta.value } = case value of
          Theta.Record $(TH.varP $ TH.mkName "fields") -> do
            $(apCheckSchema) v
            Conversion.withContext type_ $(extractFields names)
          _                   -> Conversion.mismatch $(apTheta) type_

        avroDecoding = $(TH.doE $ (fieldDecoding <$> names)
                    <> [TH.noBindS [e| pure $(record names) |]])
    |]
  where extractFields :: [TH.Name] -> TH.Q TH.Exp
        extractFields names = TH.doE $ (field <$> zip names [0..])
                           <> [TH.noBindS [e| pure $(record names) |]]

        field :: (TH.Name, Integer) -> TH.Q TH.Stmt
        field (bindingName, index) = TH.bindS (TH.varP bindingName)
          [e| Conversion.fromTheta' $ $(TH.varE fieldMap) ! index |]

        fieldDecoding :: TH.Name -> TH.Q TH.Stmt
        fieldDecoding bindingName =
          TH.bindS (TH.varP bindingName) [e| Conversion.avroDecoding |]

        record :: [TH.Name] -> TH.Q TH.Exp
        record names = TH.recConE (toName name) $ binding <$> zip names fields

        binding :: (TH.Name, Theta.Field Theta.Type) -> TH.Q (TH.Name, TH.Exp)
        binding (bindingName, Theta.Field { Theta.fieldName }) =
          (toFieldName fieldName,) <$> TH.varE bindingName

        fieldMap = TH.mkName "fields"

        -- Apply `checkSchema` to _ and then name, since the first
        -- quantified type is `m`
        apCheckSchema = TH.appTypeE [e| Conversion.checkSchema @_ |]
                        (TH.conT $ toName name)
        apTheta       = TH.appTypeE [e| theta |] (TH.conT $ toName name)

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
variantFromTheta :: Name.Name -> NonEmpty (Theta.Case Theta.Type) -> TH.Q [TH.Dec]
variantFromTheta name (toList -> cases) =
  [d| instance Conversion.FromTheta $(TH.conT $ toName name) where
        fromTheta' v@Theta.Value { Theta.type_, Theta.value } = case value of
          Theta.Variant $(TH.varP caseName) $(TH.varP parameters) -> do
            $(apCheckSchema) v
            Conversion.withContext type_ $(chooseCase)
          _                             ->
            Conversion.mismatch $(apTheta) type_

        avroDecoding = do
          tag <- Avro.getLong
          $(decodingCase)
    |]
  where
    chooseCase = TH.caseE (TH.varE caseName) $ (matchCase <$> cases) <> [baseCase]
      where
        matchCase Theta.Case { Theta.caseName, Theta.caseParameters } =
          match caseString matchBody
          where
            caseString = TH.litP $ TH.stringL $ Text.unpack $ Name.render caseName
            matchBody =
              foldl' apply [e| pure $(TH.conE $ toName caseName) |] $ getValue <$> indices
            indices = [i | i <- [0::Int ..] | _ <- Theta.fields caseParameters]
            getValue index =
              [e| Conversion.fromTheta' ($(TH.varE parameters) ! index) |]

        baseCase = match (TH.varP invalid) [e|
            error $ "Invalid case name '"
                 <> Text.unpack (Name.render $(TH.varE invalid)) <> "'!"
          |]

    apCheckSchema = TH.appTypeE [e| Conversion.checkSchema @_ |] (TH.conT $ toName name)
    apTheta = TH.appTypeE [e| theta |] (TH.conT $ toName name)

    decodingCase = TH.caseE [e| tag |] $ (decodeTag <$> [0..] `zip` cases) <> [baseCase]
      where
        decodeTag (tag, case_) =
          match (TH.litP $ TH.integerL tag) (matchBody case_)
        matchBody (Theta.Case name _ Theta.Fields { Theta.fields }) =
          foldl' apply [e| pure $(TH.conE $ toName name) |] $
            [e| Conversion.avroDecoding |] <$ fields

        baseCase = match (TH.varP invalid) [e|
          fail $ message <> show $(TH.varE invalid) <> "!" |]
        message = "Invalid case tag. Expected [0.." <> show maxTag <> "but got"

    apply exp arg = [e| $(exp) <*> $(arg) |]

    maxTag = length cases - 1

    invalid    = TH.mkName "invalid"
    caseName   = TH.mkName "caseName"
    parameters = TH.mkName "_parameters"

-- ** Types

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
generateType :: Theta.Type -> TH.Q TH.Type
generateType type_ = case Theta.baseType type_ of
  Theta.Primitive' p    -> case p of
    Primitive.Bool          -> [t| Bool |]
    Primitive.Bytes         -> [t| ByteString |]
    Primitive.Int           -> [t| Int32 |]
    Primitive.Long          -> [t| Int64 |]
    Primitive.Float         -> [t| Float |]
    Primitive.Double        -> [t| Double |]
    Primitive.String        -> [t| Text |]
    Primitive.Date          -> [t| Day |]
    Primitive.Datetime      -> [t| UTCTime |]
    Primitive.UUID          -> [t| UUID |]
    Primitive.Time          -> [t| TimeOfDay |]
    Primitive.LocalDatetime -> [t| LocalTime |]

  Theta.Fixed' size ->
    let sizeType = TH.litT $ TH.numTyLit $ fromIntegral size in
    [t| FixedBytes $sizeType |]

  Theta.Array' items    -> [t| [$(generateType items)] |]
  Theta.Map' values     -> [t| HashMap Text $(generateType values) |]
  Theta.Optional' type_ -> [t| Maybe $(generateType type_) |]

  Theta.Enum' name _    -> toType name
  Theta.Record' name _  -> toType name
  Theta.Variant' name _ -> toType name

  Theta.Reference' name -> toType name
  Theta.Newtype' name _ -> toType name

-- ** Names

-- TODO: how to support namespaces here?
-- | Wrap a 'Name.Name' into a Template Haskell identifier.
toName :: Name.Name -> TH.Name
toName Name.Name { Name.name } = TH.mkName $ Text.unpack name

-- | Wrap a 'Theta.FieldName' into a Template Haskell identifier.
toFieldName :: Theta.FieldName -> TH.Name
toFieldName = TH.mkName . Text.unpack . Theta.textName

-- | Wrap a 'Name.Name' into a Template Haskell type that references
-- the same name.
toType :: Name.Name -> TH.Q TH.Type
toType = TH.conT . toName

-- | Define a type synonym with the given name and Haskell type. This
-- can be used to deal with aliases from Theta (ie @type Foo = Bool@).
typeSynonym :: Name.Name -> TH.Q TH.Type -> TH.Q [TH.Dec]
typeSynonym (toName -> name) type_ = pure <$> TH.tySynD name [] type_

-- ** Utility Functions

-- | Generate a standard branch of a case statement.
--
-- @
-- pattern -> body
-- @
match :: TH.Q TH.Pat -> TH.Q TH.Exp -> TH.Q TH.Match
match pat body = TH.match pat (TH.normalB body) []
