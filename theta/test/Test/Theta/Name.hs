{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Theta.Name where

import           Data.Tree             (Tree (..))

import           GHC.Exts              (fromList, toList)

import qualified Theta.Name            as Name

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Name"
  [ test_Name
  , test_ModuleName
  ]

test_Name :: TestTree
test_Name = testGroup "Name"
  [ testCase "parts" $ do
      Name.parts "com.Foo"         @?= ["com", "Foo"]
      Name.parts "com.example.Foo" @?= ["com", "example", "Foo"]

  , testCase "render" $ do
      Name.render "com.Foo"         @?= "com.Foo"
      Name.render "com.example.Foo" @?= "com.example.Foo"

  , testCase "parse & parse'" $ do
      Name.parse' ""                @?= Left Name.Invalid
      Name.parse  ""                @?= Nothing

      Name.parse' "Foo"             @?= Left Name.Unqualified
      Name.parse  "Foo"             @?= Nothing

      Name.parse' "12"              @?= Left Name.Invalid
      Name.parse  "12"              @?= Nothing

      Name.parse' "com.Foo"         @?= Right (Name.Name "com" "Foo")
      Name.parse  "com.Foo"         @?= Just (Name.Name "com" "Foo")

      Name.parse' "com.example.Foo" @?=
        Right (Name.Name (Name.ModuleName ["com"] "example") "Foo")
      Name.parse  "com.example.Foo" @?=
        Just (Name.Name (Name.ModuleName ["com"] "example") "Foo")

  , testProperty "render ⇔ parse" $ \ name ->
      case Name.parse' (Name.render name) of
        Left Name.Unqualified -> error "Rendered name had no namespace."
        Left Name.Invalid     -> error "Rendered name did not parse."
        Right parsed          -> parsed == name
  ]

test_ModuleName :: TestTree
test_ModuleName = testGroup "ModuleName"
  [ testCase "renderModuleName" $ do
      let render = Name.renderModuleName . Name.moduleName
      render "com.Foo"         @?= "com"
      render "com.example.Foo" @?= "com.example"

  , testCase "moduleRoot" $ do
      Name.moduleRoot "foo"             @?= "foo"
      Name.moduleRoot "example.foo"     @?= "example"
      Name.moduleRoot "com.example.foo" @?= "com"

  , testCase "moduleHierarchy" $ do
      let got = Name.moduleHierarchy
            [ "com.example.foo"
            , "com.example.bar"
            , "com.example2.foo"
            , "com.bar"
            , "com.example2.bar"
            , "org.example.foo"
            ]
          expected =
            [ Node "com"
              [ Node "bar" []
              , Node "example" [ Node "bar" [], Node "foo" [] ]
              , Node "example2" [ Node "bar" [], Node "foo" [] ]
              ]
            , Node "org"
              [ Node "example" [ Node "foo" [] ] ]
            ]
      got @?= expected

  , testProperty "renderModuleName ⇔ parseModuleName" $ \ moduleName ->
      Name.parseModuleName (Name.renderModuleName moduleName) == moduleName

  , testProperty "toList ⇔ fromList" $ \ (moduleName :: Name.ModuleName) ->
      fromList (toList moduleName) == moduleName

  , testProperty "toList ⇒ moduleRoot" $ \ moduleName ->
      head (toList moduleName) == Name.moduleRoot moduleName
  ]
