{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ParallelListComp      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
module Test.Theta.Value where

import           Prelude                       hiding (map)

import qualified Data.HashMap.Strict           as HashMap

import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.QuickCheck         (Gen, getSize, resize,
                                                testProperty)

import           Theta.Target.Haskell          (loadModule)
import           Theta.Target.Haskell.HasTheta (HasTheta (theta))

import           Theta.Types                   (primitiveTypes)
import qualified Theta.Types                   as Theta
import           Theta.Value                   (BaseValue (Map, Variant),
                                                Value (Value, type_),
                                                checkValue, genValue, genValue')

loadModule "test/data/modules" "primitives"
loadModule "test/data/modules" "enums"
loadModule "test/data/modules" "recursive"

tests :: TestTree
tests = testGroup "Value"
  [ testProperty "primitive types" $ do
      values <- mapM genValue primitiveTypes
      pure $ all checkValue values

  , testProperty "Fixed(10)" $ do
      value <- genValue (Theta.fixed' 10)
      pure $ checkValue value && type_ value == Theta.fixed' 10

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

  , testProperty "recursive.Recursive" $ do
      value <- genRecursive
      pure $ checkValue value && type_ value == theta @Recursive
  ]

  -- generate recursive.Recursive values with overrides to make sure
  -- the generated values don't get too big—a bit hacky, but seems to
  -- work well enough
genRecursive :: Gen Value
genRecursive = genValue' [("recursive.Recursive", recursive)] (theta @Recursive)
  where recursive = do
          size <- getSize
          if size <= 10 then base else recurse size

        base = pure $ case_ "recursive.Nil" []
        recurse size = do
          contents <- genValue Theta.int'
          boxed <- resize size genRecursive
          let unboxed = Value (Theta.map' (theta @Recursive)) $ Map HashMap.empty
          pure $ case_ "recursive.Recurse" [contents, boxed, unboxed]

        case_ name values = Value (theta @Recursive) $ Variant name values
