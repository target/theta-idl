{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module Apps.Avro where

import           Control.Monad           (forM_)
import           Control.Monad.IO.Class  (liftIO)

import qualified Data.Aeson              as Aeson
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text               as Text

import           Options.Applicative

import           System.Directory        (createDirectoryIfMissing)
import           System.FilePath         (joinPath, (<.>), (</>))

import qualified Theta.Import            as Theta
import           Theta.Name              (ModuleName, Name (..))
import qualified Theta.Name              as Name
import           Theta.Pretty            (pr)
import           Theta.Target.Avro.Types (toSchema)
import qualified Theta.Types             as Theta

import           Apps.Subcommand

-- * avro

avroCommand :: Mod CommandFields Subcommand
avroCommand = command "avro" opts
  where opts = info
          (subcommands <**> helper)
          (fullDesc <> progDesc avroDescription)

        subcommands = subparser $ mconcat
          [ typeCommand, allCommand ]

avroDescription :: String
avroDescription = [pr|
  Commands for working with Avro schemas generate from Theta types.
|]

-- * avro type

typeCommand :: Mod CommandFields Subcommand
typeCommand = command "type" $ runType <$> opts
  where opts = info
          (typeOpts <**> helper)
          (fullDesc <> progDesc "Compile a specific Theta type to an Avro schema.")

data TypeOptions = TypeOptions
  { targetStream :: TargetStream
  , toExport     :: Name
  }

data TargetStream = TargetFile FilePath | TargetStdout

typeOpts :: Parser TypeOptions
typeOpts = TypeOptions <$> stream <*> typeName
  where stream = parseStream <$> strOption
          (  short 'o'
          <> long "out"
          <> value "-"
          <> metavar "FILE"
          <> help "File to dump Avro schema to. \
                  \If not specified, print to stdout. The filename '-'\
                  \ may also be used to write to stdout. Use \
                  \ ./- if you want '-' as a filename."
          )

        -- | Interpret the file target '-' filepath as stdout,
        --   while passing all other values through as a FilePath
        parseStream :: String -> TargetStream
        parseStream "-" = TargetStdout
        parseStream x   = TargetFile x

runType :: TypeOptions -> Subcommand
runType TypeOptions { targetStream, toExport } loadPath = do
  type_    <- Theta.getDefinition loadPath toExport
  exported <- toSchema type_

  let write = case targetStream of
        TargetStdout        -> LBS.putStr
        TargetFile filePath -> LBS.writeFile filePath
  liftIO $ write $ Aeson.encode exported

-- * avro all

allCommand :: Mod CommandFields Subcommand
allCommand = command "all" $ runAll <$> opts
  where opts = info
          (allOpts <**> helper)
          (fullDesc <> progDesc allDescription)

allDescription :: String
allDescription = [pr|
  Compile every exportable type from the given module and every module
  it (transitively) imports to an Avro schema, placing all the schemas
  in $TARGET_DIRECTORY/avro.
|]

data AllOptions = AllOptions
  { target      :: FilePath
  , moduleNames :: [ModuleName]
  }

allOpts :: Parser AllOptions
allOpts = AllOptions <$> targetDirectory "the avro directory" <*> modules

runAll :: AllOptions -> Subcommand
runAll AllOptions { target, moduleNames } loadPath = do
  modules <- traverse (Theta.getModule loadPath) moduleNames

  liftIO $ createDirectoryIfMissing True (target </> "avro")

  forM_ (Theta.transitiveImports modules) $ \ module_ -> do
    forM_ (Theta.types module_) $ \ definition ->
      export (Theta.moduleName module_) definition

  where
      export moduleName definition = do
        schema <- toSchema definition
        liftIO $ do
          let path = Text.unpack <$> Name.moduleParts moduleName
              out  = target </> "avro" </> joinPath path
          createDirectoryIfMissing True out

          let filename =
                Text.unpack $ Name.name $ Theta.definitionName definition
          LBS.writeFile (out </> filename <.> "avsc") $ Aeson.encode schema

