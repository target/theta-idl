{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
module Test.Everything where

import           Data.TreeDiff.Class  (ToExpr)

import           Theta.Target.Haskell

import           Test.Primitives

loadModule "modules" "everything"

deriving anyclass instance ToExpr Var

deriving anyclass instance ToExpr IntWrapper

deriving anyclass instance ToExpr Everything

deriving anyclass instance ToExpr Options

deriving anyclass instance ToExpr RecursiveList
