{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}

module Apps.Rust where

import           Control.Monad.Except

import           Data.String.Interpolate        (__i)
import           Data.Text.Prettyprint.Doc.Util (putDocW)

import qualified Language.Rust.Pretty           as Rust
import           Options.Applicative

import qualified Theta.Import                   as Theta
import qualified Theta.Name                     as Theta
import qualified Theta.Types                    as Theta

import           Theta.Target.Rust              (toFile)

import           Apps.Subcommand

rustCommand :: Mod CommandFields Subcommand
rustCommand = command "rust" $ runRust <$> opts
  where opts = info
          (rustOpts <**> helper)
          (fullDesc <> progDesc rustDescription)

rustDescription :: String
rustDescription = [__i|
  Compile the given Theta modules and their transitive imports to Rust modules.
|]


data Opts = Opts { moduleNames :: [Theta.ModuleName] }

rustOpts :: Parser Opts
rustOpts = Opts <$> modules

-- | Convert a list of modules to Rust definitions.
--
-- To get fully valid Rust code, the list of modules needs to be
-- complete and without duplicates:
--
--  * complete: the list should include every module imported by a
--    module in the list (ie transitive closure)
--
--  * without duplicates: modules should be included in the list at
--    most once, and the names of both modules and type definitions
--    should be unique inside the entire set
--
-- If either of these conditions is violated, the generated Rust code
-- is not guaranteed to be compiled—it might refer to Rust definitions
-- that are out of scope, or define conflicting names.
runRust :: Opts -> Subcommand
runRust Opts { moduleNames } path = do
  modules <- traverse (Theta.getModule path) moduleNames
  let rust = Rust.pretty' $ toFile $ Theta.transitiveImports modules
  liftIO $ putDocW 80 rust
