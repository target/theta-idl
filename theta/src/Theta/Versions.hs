{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module specifies which versions of the Theta protocol are
-- supported by this version of the Haskell package (library and
-- executables).
--
-- Each release of the package can support a range of versions. Theta
-- modules specify the versions needed to work with the module in the
-- module metadata; any module whose versions are not supported by
-- this release will fail with a graceful error message.
module Theta.Versions where

import           Data.Text      (Text)
import qualified Data.Text      as Text
import qualified Data.Version   as Cabal

import           Test.QuickCheck (Arbitrary (arbitrary)) 
import qualified Test.QuickCheck as QuickCheck

import           Theta.Metadata (Version)

import qualified Paths_theta

-- * Language Versions

-- | A version range with an /inclusive/ lower bound and an
-- /exclusive/ upper bound.
--
-- @
-- Range { lower = "1.0.0", upper = "1.1.0" }
-- @
--
-- represents
--
-- @
-- version >= "1.0.0" && version < "1.1.0"
-- @
data Range = Range
  { lower :: Version
    -- ^ The /inclusive/ lower bound.
  , upper :: Version
    -- ^ The /exclusive/ upper bound.
  , name  :: Text
    -- ^ A human-readable description of what this version represents
    -- (ie "language-version" vs "avro-version").
  } deriving (Show, Eq, Ord)

instance Arbitrary Range where
  arbitrary = Range <$> arbitrary <*> arbitrary <*> name
    where name = QuickCheck.elements ["avro-version", "theta-version"]

-- | Check whether a version is within the specified 'Range'.
inRange :: Range -> Version -> Bool
inRange Range { lower, upper } version = version >= lower && version < upper

-- | Is the given version of the Theta language supported by this
-- version of the package?
--
-- Specified as @language-version@ in the header of every Theta
-- module.
theta :: Range
theta = Range { name = "theta-version", lower = "1.0.0", upper = "1.1.0" }

-- | Is the given version of the Theta Avro encoding supported by this
-- version of the package?
--
-- Specified as @avro-version@ in the header of every Theta module.
avro :: Range
avro = Range { name = "avro-version", lower = "1.0.0", upper = "1.2.0" }

-- * Package Version

-- | Which version of the Theta package this is.
packageVersion :: Cabal.Version
packageVersion = Paths_theta.version

-- | Which version of the Theta package this is as 'Text'.
packageVersion' :: Text
packageVersion' = Text.pack $ Cabal.showVersion packageVersion
