{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import qualified Data.Text.IO        as Text

import           GHC.Exts            (fromString)
import qualified GHC.IO.Encoding     as Encoding

import           Options.Applicative

import           System.Environment  (lookupEnv)
import           System.Exit         (exitFailure)
import           System.IO           (stderr)

import qualified Theta.Import        as Theta
import           Theta.Pretty        (pr, pretty)
import           Theta.Versions      (packageVersion')

import qualified Apps.Avro           as Avro
import qualified Apps.Hash           as Hash
import qualified Apps.Kotlin         as Kotlin
import qualified Apps.List           as List
import qualified Apps.Python         as Python
import qualified Apps.Rust           as Rust
import           Apps.Subcommand     (Subcommand, runTheta)

main :: IO ()
main = do
  Encoding.setLocaleEncoding Encoding.utf8

  options <- customExecParser (prefs subparserInline) parser
  run options
  where parser =
          info ((version <|> thetaOptions) <**> helper)
               (fullDesc <> description <> title)

-- * Help

title :: InfoMod a
title = header [pr|
  theta ({pretty packageVersion'}) - define interfaces with algebraic data types
|]

description :: InfoMod a
description = progDesc [pr|
  Theta is a language for formally specifying interfaces using
  algebraic data types.

  The ‘theta’ command lets you work with Theta schemas and compile
  them to different targets like Avro schemas, Python and Rust.
  |]

-- * Subcommands

subcommands :: Parser Subcommand
subcommands = subparser $ mconcat
  [ Avro.avroCommand
  , Hash.hashCommand
  , Kotlin.kotlinCommand
  , List.listCommand
  , Python.pythonCommand
  , Rust.rustCommand
  ]

-- * Global Options

data ThetaOptions =
    Version
  | ThetaOptions
    { loadPath   :: Maybe Theta.LoadPath
    , subcommand :: Subcommand
    }

version :: Parser ThetaOptions
version = flag' Version
  (  long "version"
  <> help "The version of the Theta package that built this executable."
  )

thetaOptions :: Parser ThetaOptions
thetaOptions = ThetaOptions <$> (mconcat <$> loadPath) <*> subcommands
  where loadPath = many $ Just <$> strOption
          (  long "path"
          <> short 'p'
          <> metavar "THETA_LOAD_PATH"
          <> help "The paths that Theta searches for modules, separated by :. \
                  \Defaults to THETA_LOAD_PATH environment variable."
          )

run :: ThetaOptions -> IO ()
run Version = putStrLn [pr|Theta {packageVersion'}|]
run ThetaOptions { loadPath, subcommand } = do
  path <- maybe lookupPath pure loadPath
  runTheta $ subcommand path
  where lookupPath = lookupEnv "THETA_LOAD_PATH" >>= \case
          Just path -> pure $ fromString path
          Nothing   -> do
            Text.hPutStrLn stderr [pr|
              No Theta load path set. You can either:
                • specify the load path on the command line with the (--path|-p) argument
                • set the THETA_LOAD_PATH environment variable
            |]
            exitFailure
