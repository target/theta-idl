{-# LANGUAGE MultiWayIf     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}
-- | The @theta list@ subcommand: lists things like module names,
-- types... etc. Handy for automation and querying the environment.
--
-- Current implementation can:
--
--  * list all modules in the load path with @theta list modules@
module Apps.List where

import           Data.String.Interpolate (__i)

import           Options.Applicative     (CommandFields, Mod, Parser, command,
                                          fullDesc, help, helper, info, long,
                                          progDesc, subparser, switch, (<**>))

import           Apps.Subcommand         (Subcommand)

import           Control.Monad           (forM_)
import           Control.Monad.IO.Class  (liftIO)

import           Text.Printf             (printf)

import qualified Theta.LoadPath          as LoadPath
import           Theta.Pretty            (showPretty)
import qualified Data.List as List
import Data.Foldable (Foldable(toList))

-- * list

listCommand :: Mod CommandFields Subcommand
listCommand = command "list" opts
  where opts = info
          (subcommands <**> helper)
          (fullDesc <> progDesc listDescription)

        subcommands = subparser $ mconcat [ modulesCommand ]

listDescription :: String
listDescription = [__i|
  Commands for listing available Theta objects.
  |]

-- * list modules

modulesCommand :: Mod CommandFields Subcommand
modulesCommand = command "modules" $ runModules <$> opts
  where opts = info (moduleOpts <**> helper) (fullDesc <> progDesc modulesDescription)

runModules :: ModuleOptions -> Subcommand
runModules options@ModuleOptions { paths, names } loadPath
  | not (paths || names) =
      runModules (options { names = True }) loadPath
  | otherwise = liftIO $ do
      modules <- LoadPath.moduleNames loadPath
      let sortedModules = List.sortOn (showPretty . fst) $ toList modules
      forM_ sortedModules $ \ (moduleName, path) -> if
        | paths && names -> printf "%s\t%s\n" (showPretty moduleName) path
        | paths          -> putStrLn path
        | otherwise      -> putStrLn (showPretty moduleName)

modulesDescription :: String
modulesDescription = [__i|
  List every Theta module available in the Theta load path:

    • list module names: `theta list modules` or `theta list modules --names`
    • list module paths: `theta list modules --paths`
    • list both, tab-separated: `theta list modules --names --paths`
  |]

data ModuleOptions = ModuleOptions
  { paths :: Bool
  , names :: Bool
  }

moduleOpts :: Parser ModuleOptions
moduleOpts = ModuleOptions <$> paths <*> names
  where paths = switch ( long "paths"
                      <> help "List the file path for each module."
                       )
        names = switch ( long "names"
                      <> help "List the name of each module."
                       )
