{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
module Main where

import           Data.String.Interpolate (__i, i)

import           GHC.Exts                (fromString)
import qualified GHC.IO.Encoding         as Encoding

import           Options.Applicative

import           System.Environment      (getEnv)

import qualified Theta.Import            as Theta
import           Theta.Versions          (packageVersion')

import qualified Apps.Avro               as Avro
import qualified Apps.Hash               as Hash
import qualified Apps.Kotlin             as Kotlin
import qualified Apps.Python             as Python
import qualified Apps.Rust               as Rust
import           Apps.Subcommand         (Subcommand, runTheta)

main :: IO ()
main = do
  Encoding.setLocaleEncoding Encoding.utf8

  options <- execParser parser
  run options
  where parser =
          info ((version <|> thetaOptions) <**> helper)
               (fullDesc <> description <> title)

-- * Help

title :: InfoMod a
title = header [__i|
  theta (#{packageVersion'}) - define interfaces with algebraic data types
|]

description :: InfoMod a
description = progDesc [__i|
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
thetaOptions = ThetaOptions <$> optional loadPath <*> subcommands
  where loadPath = fromString <$> strOption
          (  long "path"
          <> short 'p'
          <> metavar "THETA_LOAD_PATH"
          <> help "The paths that Theta searches for modules, separated by :. \
                  \Defaults to THETA_LOAD_PATH environment variable."
          )

run :: ThetaOptions -> IO ()
run Version = putStrLn [i|Theta #{packageVersion'}|]
run ThetaOptions { loadPath, subcommand } = do
  path <- case loadPath of
    Just path -> pure path
    Nothing   -> fromString <$> getEnv "THETA_LOAD_PATH"
  runTheta $ subcommand path
