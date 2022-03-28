{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-- | The Theta load path controls the directories that Theta searches
-- for modules. A valid load path always contains /at least/ one
-- directory.
--
-- Let's say we have a load path that specifies two directories:
--
--  1. @shared-modules@
--  2. @my-project/private-modules@
--
-- Then, if we try to import a module named @com.example.definitions@,
-- Theta will look for the module in two possible file locations:
--
--  1. @shared-modules/com/example/definitions.theta@
--  2. @my-project/private-modules/com/example/definitions.theta@
--
-- Currently, Theta will choose the *first* of these files that
-- exist—however, in the future, it might raise an error if it finds
-- multiple definitions of the same module.
--
-- == Syntax
--
-- Theta load paths have the same syntax as the Unix @PATH@ variable:
-- a list of directories separated with colons. For the @theta@
-- command-line utility, the path can be specified as an environment
-- variable:
--
-- @
-- export THETA_LOAD_PATH="shared-modules:my-project/private-modules"
-- @
--
-- or with the @(--path|-p)@ flag, either as one argument or by
-- concatenating multiple results into a single load path:
--
-- @
-- theta -p shared-modules:my-project/private-modules
-- theta -p shared-modules -p my-project/private-modules
-- @
--
-- We can use the same syntax to specifying load paths in Haskell with
-- the @OverloadedStrings@ extension:
--
-- @
-- loadModule "shared-modules:my-project/private-modules" "com.example.definitions"
-- @
--
-- You can also do the same thing with @OverloadedLists@ instead:
--
-- @
-- loadModule ["shared-modules", "my-project/private-modules"] "com.example.definitions"
-- @
--
-- Both of these examples will try to load the @definitions@ module
-- from one of the following paths:
--
--  1. @shared-modules/com/example/definitions.theta@
--  2. @my-project/private-modules/com/example/definitions.theta@
--
-- __NOTE__: when using Template Haskell functions like @loadPath@,
-- relative paths will be resolved relative to your working directory
-- /during compilation/—usually the root of the project where the
-- @.cabal@ file is defined.
module Theta.LoadPath where

import           Control.Exception  (IOException, catch, displayException)
import           Control.Monad      (forM, unless)

import           Data.HashSet       (HashSet)
import qualified Data.HashSet       as HashSet
import qualified Data.List          as List
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Sequence      ((|>))
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Data.Text.IO       as Text

import           GHC.Exts           (IsList (..), IsString (..))

import qualified System.Directory   as Directory
import           System.FilePath    ((</>))
import qualified System.FilePath    as FilePath
import           System.IO          (hPutStrLn, stderr)
import           System.IO.Error    (isDoesNotExistError)

import           Text.Printf        (printf)

import qualified Theta.Name         as Theta
import           Theta.Pretty       (Pretty (..))

-- | The Theta load path determines which root directories Theta
-- searches through to find module files.
--
-- When finding a module called @com.example.foo@ with a load path
-- containing @["specs", "types"]@, Theta will first try to look up
-- @specs/com/example/foo.theta@ and, if that doesn't exist, it will
-- try @types/com/example/foo.theta@.
--
-- Theta's load path can be specified as a string containing multiple
-- paths separated by colons (like a Unix-style PATH variable).
--
-- Example (with @OverloadedStrings@):
--
-- @
-- "specs:types" :: LoadPath
-- @
newtype LoadPath = LoadPath (NonEmpty FilePath)
  deriving stock (Eq)
  deriving newtype (Semigroup)

instance Show LoadPath where
  show (LoadPath (NonEmpty.toList -> paths)) = "\"" <> List.intercalate ":" paths <> "\""

instance IsList LoadPath where
  type Item LoadPath = FilePath

  toList (LoadPath paths) = NonEmpty.toList paths
  fromList = \case
    []    -> error "Cannot construct an empty LoadPath."
    paths -> LoadPath $ NonEmpty.fromList paths

instance Pretty LoadPath where
  pretty = Text.pack . List.intercalate ":" . toList

-- | A 'LoadPath' is represented as a string with any number of paths
-- separated by @':'@. Example: @"/foo:/home/bob/specs:types@.
instance IsString LoadPath where
  fromString str = case go str of
    []    -> error "Cannot construct an empty LoadPath."
    paths -> fromList paths
    where go ""  = []
          go str = takeWhile (/= ':') str : go (drop 1 $ dropWhile (/= ':') str)

-- | This function tries to find the given path using every entry in
-- the load path as a root. It will lazily read the /first/ valid file
-- it finds or will throw an error.
--
-- If the file is successfully found in the load path, this also
-- returns the path the file was loaded from.
--
-- Think of this as a version of 'readFile' that tries each directory
-- in the Theta load path until it finds one that works.
findInPath :: LoadPath -> FilePath -> IO (Maybe (Text, FilePath))
findInPath (LoadPath paths) path = go $ NonEmpty.toList paths
  where go []            = pure Nothing
        go (root : rest) =
          fetch (root </> path) `catch` \ (e :: IOException) -> do
            unless (isDoesNotExistError e) $ do
              let err = displayException e
                  message =
                    printf "Warning: failed accessing %s \
                           \while loading path %s \
                           \with unexpected error:\n %s" root path err
              hPutStrLn stderr message
            go rest

        fetch path = Just . (,path) <$> Text.readFile path

-- | The names and paths of every single Theta module found in the
-- given load path.
--
-- This does not look at the contents of the modules, just at file
-- names.
moduleNames :: LoadPath -> IO (HashSet (Theta.ModuleName, FilePath))
moduleNames (LoadPath (toList -> paths)) = HashSet.fromList . concat <$> modules
  where modules = forM paths $ \ root -> do
          contents <- Directory.listDirectory root
          concat <$> mapM (go root []) contents

        go root namespace path = do
          isDir <- Directory.doesDirectoryExist (root </> path)
          if isDir then do
            contents <- Directory.listDirectory (root </> path)
            let namespace' = namespace |> Text.pack path
            concat <$> mapM (go (root </> path) namespace') contents
          else do
            case FilePath.splitExtension path of
              (base, ".theta") ->
                pure [ (Theta.ModuleName (toList namespace) (Text.pack base)
                     , root </> path) ]
              (_, _) -> pure []
