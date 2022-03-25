{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}

-- | This module defines types for handling version metadata in Theta
-- schemas.
--
-- Each Theta schema starts with a section that specifies the version
-- of the language and Avro encoding the schema expects:
--
-- @
-- language-version: 1.0.0
-- avro-version: 1.2.0
-- ---
-- @
--
-- Theta then guarantees that two identical schemas that specify the
-- same language and encoding version will produce compatible Avro
-- schemas and objects with different versions—or even different
-- /implementations/—of the Theta compiler.
module Theta.Metadata where

import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Versions              (SemVer (..), prettySemVer)
import qualified Data.Versions              as Version

import           GHC.Exts                   (IsString (..))

import           Language.Haskell.TH.Syntax (Lift (liftTyped))

import           Test.QuickCheck            (Arbitrary (arbitrary))

import           Text.Megaparsec            (errorBundlePretty)

import qualified Theta.Name                 as Name
import           Theta.Pretty               (Pretty (..))

-- | The data included in a module's metadata section.
data Metadata = Metadata
  { languageVersion :: Version
    -- ^ The version of the Theta language determines what language
    -- features the schema can use and how those features work.
  , avroVersion     :: Version
    -- ^ The Avro version determines how a schema is converted to
    -- Avro. The same schema compiled at the same Avro version should
    -- always generate compatible Avro data.
  , moduleName      :: Name.ModuleName
    -- ^ The name of the module that this section belongs to.
  }
  deriving stock (Show, Eq, Lift)

instance Arbitrary Metadata where
  arbitrary = Metadata <$> arbitrary <*> arbitrary <*> arbitrary

-- | A semantic version that's compliant with the semver spec.
--
-- This is just a wrapper over 'SemVer' that lets me add typeclass
-- instances.
newtype Version = Version SemVer
  deriving newtype (Show, Eq, Ord)

instance Arbitrary Version where
  arbitrary = Version <$> semver
    where semver = SemVer <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> pure []
                          <*> pure Nothing

instance Lift Version where
  liftTyped (pretty -> v) = [|| either error id (fromText v) ||]

-- | Render a 'Version' in a compact, human-readable format.
--
-- @
-- λ> show ("1.2.1" :: Version)
-- "SemVer {_svMajor = 1, _svMinor = 2, _svPatch = 1, _svPreRel = [], _svMeta = []}"
-- λ> pretty ("1.2.1" :: Version)
-- "1.2.1"
-- @
instance Pretty Version where
  pretty (Version semVer) = prettySemVer semVer

-- | Turns a literal "1.2.0" into a 'Version'. Errors out if the
-- format is not compliant with semver.
instance IsString Version where
  fromString = either error id . fromText . Text.pack

-- | Parse a 'Version' from a string formatted as semver.
--
-- Returns a formatted parse error message if the string is not a
-- compliant with semver.
fromText :: Text -> Either String Version
fromText text = case Version.semver text of
  Left parseError -> Left $ errorBundlePretty parseError
  Right version   -> Right $ Version version
