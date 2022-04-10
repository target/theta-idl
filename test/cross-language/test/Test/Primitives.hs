{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
module Test.Primitives where

import           Data.TreeDiff.Class   (ToExpr)

import           Theta.Test.Assertions ()

import           Theta.Target.Haskell

loadModule "modules" "primitives"

deriving anyclass instance ToExpr Primitives

deriving anyclass instance ToExpr Containers
