{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE ViewPatterns #-}
module Apps.Subcommand where

import           Control.Monad.Except    (ExceptT, runExceptT)

import           Data.String.Interpolate (i)
import           Data.Text               (Text)
import qualified Data.Text               as Text

import           System.Exit             (exitFailure)
import           System.IO               (hPutStr, stderr)

import           Options.Applicative

import           Theta.Error             (Error (..))
import qualified Theta.Import            as Theta
import           Theta.Name              (ModuleName, Name)
import qualified Theta.Name              as Name
import qualified Theta.Pretty            as Theta

type Subcommand = Theta.LoadPath -> ExceptT Error IO ()

-- | Run a Theta computation in IO, printing a user-formatted error
-- message if it fails.
runTheta :: ExceptT Error IO a -> IO a
runTheta t = runExceptT t >>= \case
  Left err -> do
    hPutStr stderr $ Text.unpack $ Theta.pretty err
    exitFailure
  Right a  -> return a

-- * Flags useful for different subcommands

-- $ Defining shared flags here reduces code duplication and helps
-- ensure that Theta's command-line interface stays consistent.

typeName :: Parser Name
typeName = option (eitherReader readName)
  (  long "type"
  <> short 't'
  <> metavar "TYPE"
  <> help "A fully-qualified type name. Example: ‘com.example.Foo’."
  )
  where readName (Text.pack -> text) = case Name.parse' text of
          Left Name.Unqualified -> Left $ Theta.showPretty $ UnqualifiedName text
          Left Name.Invalid     -> Left $ Theta.showPretty $ InvalidName text
          Right name            -> Right name


targetDirectory :: Text -> Parser FilePath
targetDirectory target = strOption
  (  short 'o'
  <> long "out"
  <> value "."
  <> metavar "TARGET_DIRECTORY"
  <> help [i|"Where to create #{target} (default ‘.’)."|]
  )

modules :: Parser [ModuleName]
modules = some $ strOption
  (  metavar "MODULE_NAME"
  <> long "module"
  <> short 'm'
  <> help "The name of a Theta module. Flag can be passed multiple times."
  )
