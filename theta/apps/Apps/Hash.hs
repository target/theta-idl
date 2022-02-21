{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module Apps.Hash where

import           Control.Monad           (forM_, unless)
import           Control.Monad.IO.Class  (liftIO)

import           Data.String.Interpolate (i)

import           Options.Applicative

import qualified Theta.Import            as Theta
import           Theta.Name              (ModuleName, Name (..))
import qualified Theta.Name              as Name
import qualified Theta.Types             as Theta

import           Apps.Subcommand

hashCommand :: Mod CommandFields Subcommand
hashCommand = command "hash" $ runHash <$> opts
  where opts = info
          (hashOpts <**> helper)
          (fullDesc <> progDesc "Calculate hashes for Theta types and modules.")

data HashOptions = HashOptions
  { typeNames   :: [Name]
  , moduleNames :: [ModuleName]
  }

hashOpts :: Parser HashOptions
hashOpts = HashOptions <$> many typeName
                       <*> (modules <|> pure [])

runHash :: HashOptions -> Subcommand
runHash HashOptions { typeNames, moduleNames } loadPath = do
  unless (null typeNames) $ hashTypes typeNames loadPath
  unless (null moduleNames) $ hashModules moduleNames loadPath


hashTypes :: [Name] -> Subcommand
hashTypes types loadPath = forM_ types $ \ typeName -> do
  definition <- Theta.getDefinition loadPath typeName

           -- Note the tab character
  let hash = Theta.hash . Theta.definitionType
  liftIO $ putStrLn [i|#{Name.render typeName}	#{hash definition}|]

hashModules :: [ModuleName] -> Subcommand
hashModules moduleNames loadPath = do
  modules <- traverse (Theta.getModule loadPath) moduleNames

  forM_ (Theta.transitiveImports modules) $ \ module_ -> 
    forM_ (Theta.types module_) $ \ Theta.Definition
    { Theta.definitionType = type_, Theta.definitionName = name } ->
         -- Note the tab character
    liftIO $ putStrLn [i|#{Name.render name}	#{Theta.hash type_}|]

