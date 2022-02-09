{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ParallelListComp      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
module Test.Theta.Value.Generators where

import           Prelude                       hiding (map)

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Theta.Target.Haskell          (loadModule)
import           Theta.Target.Haskell.HasTheta (HasTheta (theta))

import           Theta.Types                   (primitiveTypes)
import           Theta.Value                   (Value (type_), checkValue)
import           Theta.Value.Generators


loadModule "test/data/modules" "primitives"
loadModule "test/data/modules" "enums"

tests :: TestTree
tests = testGroup "Generators"
  [ testProperty "primitive types" $ do
      values <- mapM genValue primitiveTypes
      pure $ and [ type_ v == prim | v <- values | prim <- primitiveTypes ]

  , testProperty "primitives.Primitives" $ do
      value <- genValue (theta @Primitives)
      pure $ checkValue value && type_ value == theta @Primitives

  , testProperty "primitives.Containers" $ do
      value <- genValue (theta @Containers)
      pure $ checkValue value && type_ value == theta @Containers

  , testProperty "enums.SimpleEnum" $ do
      value <- genValue (theta @SimpleEnum)
      pure $ checkValue value && type_ value == theta @SimpleEnum

  , testProperty "enums.TrickyEnum" $ do
      value <- genValue (theta @TrickyEnum)
      pure $ checkValue value && type_ value == theta @TrickyEnum
  ]
