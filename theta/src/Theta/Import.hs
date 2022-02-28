{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

-- | This module defines how we resolve and import Theta modules.
--
-- Each Theta file defines a module with multiple definitions that can
-- import other modules and be imported itself. Circular imports will
-- not work. Naming conflicts are handled by deferring to the
-- /importing/ module.
module Theta.Import
  ( module Theta.LoadPath

  , importModule
  , resolveModule

  , toPath
  , fromPath

  , getDefinition
  , getModule
  , getModuleDefinition
  )
where

import           Control.Monad          (foldM, unless, when)
import           Control.Monad.Except   (MonadError, throwError)
import           Control.Monad.Fix      (MonadFix)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (runReaderT)
import           Control.Monad.State    (modify, runStateT)

import           Data.Either            (isLeft, isRight)
import qualified Data.Foldable          as Foldable
import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Void              (Void)

import           GHC.Exts               (IsList (..))

import           System.FilePath        (joinPath, splitDirectories,
                                         takeBaseName, takeDirectory, (<.>),
                                         (</>))

import qualified Text.Megaparsec        as Megaparsec

import           Theta.Error            (Error (..), ModuleError (..))
import           Theta.LoadPath
import qualified Theta.Metadata         as Metadata
import           Theta.Name             (Name)
import qualified Theta.Name             as Name
import qualified Theta.Parser           as Parser
import           Theta.Types
import qualified Theta.Versions         as Versions

-- * Module files

-- ** Individual Module Files

-- | Convert an import into a /relative/ file path to the
-- corresponding @.theta@ file.
toPath :: Name.ModuleName -> FilePath
toPath Name.ModuleName { Name.baseName, Name.namespace } =
  joinPath (Text.unpack <$> namespace) </> Text.unpack baseName <.> "theta"

-- | Convert a valid path into its corresponding 'Name'. The path has
-- to point to a file with a .theta extension that complies with the
-- naming rules for Theta modules.
--
-- Example:
-- @
-- λ> fromPath "foo.theta"
-- Name {namespace = [], name = "foo"}
-- λ> fromPath "com/target/foo.theta"
-- Name {namespace = ["com","target"], name = "foo"}
-- @
fromPath :: FilePath -> Name.ModuleName
fromPath path = Name.ModuleName { Name.baseName, Name.namespace }
  where baseName  = Text.pack $ takeBaseName path
        namespace = case takeDirectory path of
          "."       -> [] -- special case for paths like "foo.theta"
                          -- with no directory component
          directory -> Text.pack <$> splitDirectories directory

-- | Add the definitions from the first (imported) module into the
-- second (importing) module. If there are naming conflicts, the
-- importing module takes precedence.
--
-- If multiple modules are imported into the same module and have a
-- naming conflict, the *later* module takes precendence.
--
-- The returned module will have the same name as the second
-- (importing) module.
importModule :: Module
             -- ^ The imported module. In case of conflict, this
             -- module's definitions will be overridden.
             -> Module
             -- ^ The importing module. In case of conflict, this
             -- module's definitions will override.
             -> Module
importModule imported importing = importing { imports = imported : imports importing }

-- | Given a type name, return the definition with that name from the
-- given load path.
--
-- Two possible errors if the type name cannot be found:
--  1. 'MissingModule' if the type's 'moduleName' is not found.
--  2. 'MissingName' if the type is not in its own module.
getDefinition :: forall m. (MonadIO m, MonadError Error m, MonadFix m)
              => LoadPath
              -> Name
              -> m (Definition Type)
getDefinition loadPath typeName = do
  module_ <- getModule loadPath (Name.moduleName typeName)

  case lookupDefinition typeName module_ of
    Right t -> pure t
    Left _  -> throwError $ MissingName typeName

-- | Load a module with the given name from the given load path.
--
-- This takes care of loading and parsing the specified module as well
-- as any module it imports.
getModule :: forall m. (MonadIO m, MonadError Error m, MonadFix m)
          => LoadPath
          -> Name.ModuleName
          -> m Module
getModule loadPath moduleName = do
  (definition, _) <- getModuleDefinition loadPath moduleName
  (module_, _)    <- resolveModule loadPath definition
  pure module_

-- | Fetches and parses a module, searching through the set of paths
-- given in the 'LoadPath'. Returns a parsed module definition as well
-- as the full path for the @*.theta@ file it was defined in.
--
-- A module @com.target.foo@ with a load path of
-- @specs/theta:foo/theta@ will be retrieved from the path
-- @specs/theta/com/target/foo.theta@ or, if that doesn't exist, from
-- @foo/theta/com/target/foo.theta@.
--
-- This function checks to make sure the Theta and Avro versions
-- needed by the module are supported by this release of the Theta
-- package.
--
-- Raises an error if the module itself has errors or cannot be found
-- in the load path.
getModuleDefinition :: forall m. (MonadIO m, MonadError Error m)
                    => LoadPath
                    -- ^ The load path to search for module files.
                    -> Name.ModuleName
                    -- ^ The modules name which will be turned into a file path.
                    -> m (ModuleDefinition, FilePath)
getModuleDefinition loadPath moduleName = do
  loaded <- liftIO $ findInPath loadPath $ toPath moduleName

  (contents, path) <- case loaded of
    Just success -> pure success
    Nothing      -> throwError $ MissingModule loadPath moduleName

  let parse' :: Megaparsec.Parsec Void Text a -> m a
      parse' parser = case Megaparsec.parse parser path contents of
        Left err  -> throwError $ ParseError err
        Right res -> pure res

  metadata <- parse' (Parser.metadataSection moduleName)
  checkVersions metadata
  body     <- parse' $ runReaderT (Parser.moduleBody moduleName) metadata

  pure (ModuleDefinition metadata body, path)
  where checkVersions metadata@Metadata.Metadata { Metadata.avroVersion, Metadata.languageVersion } = do
          unless (Versions.inRange Versions.theta languageVersion) $
            throwError $ UnsupportedVersion metadata Versions.theta languageVersion
          unless (Versions.inRange Versions.avro avroVersion) $
            throwError $ UnsupportedVersion metadata Versions.avro avroVersion

-- * Module resolution

-- | Produce a full module from a set of statements (imports and
-- definitions), also returning the path of every imported module.
--
-- This validates both the final module and all the imported modules
-- (transitively). If any module is not valid, this results in an
-- error which contains all the 'ModuleError's for _every_ module (the
-- final module being processed and all modules it depends on).
--
-- Also throws an error if any of the imported modules cannot be
-- resolved.
resolveModule :: (MonadFix m, MonadIO m, MonadError Error m)
              => LoadPath
              -- ^ The load path to search for imported modules.
              -> ModuleDefinition
              -- ^ The header and body of the module as parsed from a
              -- file.
              -> m (Module, [FilePath])
resolveModule loadPath moduleDefinition@ModuleDefinition { header, body } = do
  -- recursive do here is used to validate names because named types
  -- can be defined lexically *after* they are used
  --
  -- we calculate this by lazily referring *to the fully evaluated
  -- module* when validating a Reference', using recursive do to tie
  -- the knot
  rec ((finalModule, paths), moduleErrors) <- runStateT (resolve finalModule) []
  case moduleErrors of
    []   -> pure (finalModule, paths)
    errs -> throwError $ InvalidModule errs
  where
    moduleName = moduleDefinitionName moduleDefinition

    resolve finalModule = foldM go (Module moduleName Map.empty [] header, []) body
      where
        go (module_, dependencies) (DefinitionStatement definition)  = do
          let Definition {..} = definition
          when (isRight $ lookupName definitionName module_) $
            modify ((finalModule, DuplicateTypeName definitionName) :)

          let type_       = withModule finalModule definitionType
              definition' = definition { definitionType = type_ }
              module'     =
                module_ { types = Map.insert definitionName definition' $ types module_ }

          modify (validateType type_ <>)
          pure (module', dependencies)

        go (module_, dependencies) (ImportStatement import_) = do
          (definition, importPath) <- getModuleDefinition loadPath import_
          (imported, _)            <- resolveModule loadPath definition
          pure (importModule imported module_, importPath : dependencies)

-- | Recursively collect errors about types (duplicate fields,
-- missing references... etc)
validateType :: Type
             -- ^ The type to validate. If the type has other types
             -- (ie it's a container, record or variant), those types
             -- will also be validated.
             -> [(Module, ModuleError)]
             -- ^ A list of errors along with the module that caused
             -- them.
validateType Type { module_, baseType } = case baseType of
  Record' name Fields { fields } ->
    duplicateFields name fields <>
    (validateType . fieldType =<< fields)
  Variant' name cases            ->
    duplicateCases name (Foldable.toList cases) <>
    (validateType =<< caseTypes cases)

  Reference' name     ->
    [(module_, UndefinedType name) | isLeft $ lookupName name module_]

  Array' type_        -> validateType type_
  Map' type_          -> validateType type_
  Optional' type_     -> validateType type_
  Newtype' _ type_    -> validateType type_
  _                   -> []

  where caseTypes :: NonEmpty (Case Type) -> [Type]
        caseTypes (toList -> cases) =
          map fieldType . fields . caseParameters =<< cases

        duplicateFields recordName fields =
          [ (module_, DuplicateRecordField recordName fieldName)
          | (fieldName, quantity) <- count $ fieldName <$> fields, quantity > 1 ]

        duplicateCases variantName cases = duplicateCases <> duplicateFields
          where
            duplicateCases =
              [ (module_, DuplicateCaseName variantName caseName)
              | (caseName, quantity) <- count $ caseName <$> cases, quantity > 1 ]
            duplicateFields =
              [ (module_, DuplicateCaseField variantName caseName fieldName)
              | Case { caseName, caseParameters } <- cases
              , (fieldName, quantity) <- count $ fieldName <$> fields caseParameters
              , quantity > 1
              ]

        count names = Map.toList $ foldr go Map.empty names
          where go name = Map.insertWith (+) name 1
