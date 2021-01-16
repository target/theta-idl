{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE ViewPatterns      #-}
module Apps.Python where

import           Control.Monad           (forM_)
import           Control.Monad.Trans     (liftIO)

import qualified Data.List               as List
import           Data.String.Interpolate (__i)
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text

import           Options.Applicative

import           System.Directory        (createDirectoryIfMissing)
import           System.FilePath         ((<.>), (</>), joinPath)

import           Theta.Import            (getModule)
import           Theta.Name              (ModuleName)
import qualified Theta.Name              as Name
import qualified Theta.Types             as Theta

import           Theta.Target.Python     (Python (..))
import qualified Theta.Target.Python     as Python

import           Apps.Subcommand

pythonCommand :: Mod CommandFields Subcommand
pythonCommand = command "python" $ runPython <$> parser
  where parser = info
          (opts <**> helper)
          (fullDesc <> progDesc pythonDescription)

pythonDescription :: String
pythonDescription = [__i|
  Compile a Theta module and its transitive dependencies to Python modules.
|]

data Opts = Opts
  { moduleNames :: [ModuleName]
  , target      :: FilePath
  , packagePath :: Maybe Python
  }

opts :: Parser Opts
opts = Opts <$> modules
            <*> targetDirectory "the theta directory"
            <*> importPrefix
  where importPrefix = optional $ strOption
          ( long "prefix"
          <> metavar "PREFIX"
          <> help "A prefix to use for imports in the generated Python code."
          )

runPython :: Opts -> Subcommand
runPython Opts { moduleNames, target, packagePath } path = do
  liftIO $ do
    createDirectoryIfMissing True target
    initPy target

  modules <- traverse (getModule path) moduleNames

  -- create .py files for each module
  forM_ (Theta.transitiveImports modules) $ \ module_ -> do
    Python python <- Python.toModule module_ packagePath
    liftIO $ do
      path <- createDirectories $ Theta.moduleName module_
      Text.writeFile path python

  where initPy path = writeFile (path </> "__init__" <.> "py") ""

        createDirectories moduleName = do
          let namespace = Text.unpack <$> Name.namespace moduleName
              fullPath  = target </> joinPath namespace
          createDirectoryIfMissing True fullPath

          forM_ (List.inits namespace) $ \ parts ->
            initPy $ target </> joinPath parts

          pure $ fullPath </> Text.unpack (Name.baseName moduleName) <.> "py"
