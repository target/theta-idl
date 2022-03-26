{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module Test.Theta.Types where

import           Control.Monad                 (forM_)

import qualified Data.List.NonEmpty            as NonEmpty
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.String.Interpolate       (i)
import qualified Data.Text                     as Text

import           Text.Printf                   (printf)

import           Test.Tasty
import           Test.Tasty.HUnit

import           Theta.Pretty                  (pretty)
import           Theta.Primitive               (primitives)
import qualified Theta.Primitive               as Theta
import           Theta.Target.Haskell          (loadModule)
import qualified Theta.Target.Haskell.HasTheta as HasTheta
import           Theta.Types                   (moduleName, transitiveImports)
import qualified Theta.Types                   as Theta

loadModule "test/data/transitive" "c"
loadModule "test/data/transitive" "d"

loadModule "test/data/modules" "named_types"
loadModule "test/data/modules" "nested_newtypes"
loadModule "test/data/modules" "recursive"
loadModule "test/data/modules" "enums"

tests :: TestTree
tests = testGroup "Types"
  [ test_eq
  , test_definedNames
  , test_allNames
  , test_transitiveImports
  , test_underlyingType
  , test_lookupName
  , test_HasDoc
  , test_hashing
  ]

-- At one point, I added new types to Theta, but forgot to update the
-- manually-written Eq instance—this test will help prevent that
-- happening in the future.
test_eq :: TestTree
test_eq = testGroup "Eq instance"
  [ testCase "reflexivity" $
      mapM_ checkType basicTypes
  , testCase "inequality" $
      forM_ (zip basicTypes $ tail basicTypes) $ \ (s, t) ->
        let message =
              Text.unpack (Theta.prettyType s) <>
              " should not be equal to " <>
              Text.unpack (Theta.prettyType s)
        in
        assertBool message (s /= t)
  ]
  where basicTypes :: [Theta.Type]
        basicTypes =
          [ Theta.bool'
          , Theta.bytes'
          , Theta.int'
          , Theta.long'
          , Theta.float'
          , Theta.double'
          , Theta.string'
          , Theta.date'
          , Theta.datetime'
          , Theta.uuid'
          , Theta.time'

          , Theta.array' Theta.int'
          , Theta.map' Theta.int'
          , Theta.optional' Theta.int'

          , HasTheta.theta @TrickyEnum
          , HasTheta.theta @Record
          , HasTheta.theta @Variant
          , HasTheta.theta @Newtype

          , Theta.withModule' theta'named_types $
              Theta.Reference' "named_types.Record"
          ]

        -- test that each constructor equals itself, using a case to
        -- raise incomplete pattern warnings if we add new
        -- constructors but forget to update the test
        --
        -- when you update this case, make sure to add to the
        -- basicTypes list too
        checkType type_ = case Theta.baseType type_ of
          -- primitive types
          Theta.Primitive' p -> case p of
            Theta.Bool     -> type_ @?= type_
            Theta.Bytes    -> type_ @?= type_
            Theta.Int      -> type_ @?= type_
            Theta.Long     -> type_ @?= type_
            Theta.Float    -> type_ @?= type_
            Theta.Double   -> type_ @?= type_
            Theta.String   -> type_ @?= type_
            Theta.Date     -> type_ @?= type_
            Theta.Datetime -> type_ @?= type_
            Theta.UUID     -> type_ @?= type_
            Theta.Time     -> type_ @?= type_

          -- containers
          Theta.Array'{}     -> type_ @?= type_
          Theta.Map'{}       -> type_ @?= type_
          Theta.Optional'{}  -> type_ @?= type_

          -- named types
          Theta.Enum'{}      -> type_ @?= type_
          Theta.Reference'{} -> type_ @?= type_
          Theta.Record'{}    -> type_ @?= type_
          Theta.Variant'{}   -> type_ @?= type_
          Theta.Newtype'{}   -> type_ @?= type_

test_definedNames :: TestTree
test_definedNames = testGroup "definedNames"
  [ testCase "a" $
      Theta.definedNames theta'a @?= ["a.A"]
  , testCase "b" $
      Theta.definedNames theta'b @?= ["b.B"]
  , testCase "c" $
      Theta.definedNames theta'c @?= ["c.C"]
  , testCase "d" $
      Theta.definedNames theta'd @?= ["d.D"]
  ]

test_allNames :: TestTree
test_allNames = testGroup "allNames"
  [ testCase "a" $
      Theta.allNames theta'a @?= ["a.A"]
  , testCase "b" $
      Theta.allNames theta'b @?= ["a.A", "b.B"]
  , testCase "c" $
      Theta.allNames theta'c @?= ["a.A", "b.B", "c.C"]
  , testCase "d" $
      Theta.allNames theta'd @?= ["a.A", "b.B", "c.C", "d.D"]
  ]

test_transitiveImports :: TestTree
test_transitiveImports = testCase "transitiveImports" $ do
  names (transitiveImports [theta'c])          @?= Set.fromList ["a", "b", "c"]
  names (transitiveImports [theta'd])          @?= Set.fromList ["a", "b", "c", "d"]
  names (transitiveImports [theta'c, theta'd]) @?= Set.fromList ["a", "b", "c", "d"]
  where names = Set.fromList . map moduleName

test_lookupName :: TestTree
test_lookupName = testCase "lookupName" $ do
  assertPresent "a.A" theta'd
  assertPresent "b.B" theta'd
  assertPresent "c.C" theta'd
  assertPresent "d.D" theta'd
  where assertPresent name module_ = case Theta.lookupName name module_ of
          Left _ -> assertFailure
            [i|Could not find #{pretty name} in #{pretty $ Theta.moduleName module_}.|]
          Right _  -> pure ()

test_underlyingType :: TestTree
test_underlyingType = testCase "underlyingType" $ do
  Theta.underlyingType Theta.int'                @?= Theta.int'
  Theta.underlyingType (Theta.array' Theta.int') @?= Theta.array' Theta.int'

  check "nested_newtypes.Newtype_0" @?= Theta.int'
  check "nested_newtypes.Newtype_1" @?= Theta.int'
  check "nested_newtypes.Alias"     @?= Theta.int'
  where check name = case Theta.lookupName name theta'nested_newtypes of
          Left err    -> error err
          Right type_ -> Theta.underlyingType type_

test_HasDoc :: TestTree
test_HasDoc = testGroup "HasDoc"
  [ testCase "modifyDoc" $ do
      let foo (Just d) = Just $ d <> " foo"
          foo Nothing  = Just "foo"

      Theta.modifyDoc foo withDoc @?=
        Theta.Definition "test.Foo" (Just "some docs foo") Theta.int'
      Theta.modifyDoc foo withoutDoc @?=
        Theta.Definition "test.Foo" (Just "foo") Theta.int'

      Theta.modifyDoc foo field @?=
        Theta.Field "foo" (Just "docs foo") Theta.int'
      Theta.modifyDoc foo case_ @?=
        Theta.Case "test.Foo" (Just "docs foo") [field]

  , testCase "getDoc" $ do
      Theta.getDoc withDoc    @?= Just "some docs"
      Theta.getDoc withoutDoc @?= Nothing

      Theta.getDoc field @?= Just "docs"
      Theta.getDoc case_ @?= Just "docs"

  , testCase "setDoc" $ do
      Theta.setDoc (Just "foo") withDoc @?=
        Theta.Definition "test.Foo" (Just "foo") Theta.int'
      Theta.setDoc Nothing withDoc @?=
        Theta.Definition "test.Foo" Nothing Theta.int'
      Theta.setDoc (Just "foo") withoutDoc @?=
        Theta.Definition "test.Foo" (Just "foo") Theta.int'

      Theta.setDoc (Just "foo") field @?=
        Theta.Field "foo" (Just "foo") Theta.int'
      Theta.setDoc (Just "foo") case_ @?=
        Theta.Case "test.Foo" (Just "foo") [field]
  ]
  where withDoc    = Theta.Definition "test.Foo" (Just "some docs") Theta.int'
        withoutDoc = Theta.Definition "test.Foo" Nothing Theta.int'

        field = Theta.Field "foo" (Just "docs") Theta.int'
        case_ = Theta.Case "test.Foo" (Just "docs") [field]

test_hashing :: TestTree
test_hashing = testGroup "hashing"
  [ -- in case hashes change version-to-version
    --
    -- this is fine according to the contract spelled out in
    -- Theta.Hash, but it shouldn't happen unless you intentionally
    -- change the hashing logic
    testGroup "unchanged"
    [ testGroup "primitive" $
        -- defining this as a function gives us a warning if we add
        -- new primitive types but don't update this test case
        let expectedPrimitive t = case t of
              Theta.Bool     -> "368b9de4188c87e7afa1d7867dcf4413"
              Theta.Bytes    -> "0fa7cf75524be7f38afd4c156a8806ca"
              Theta.Int      -> "43d51ed7739b75a3636f33b75c599255"
              Theta.Long     -> "bd1906da131060d558fff8de56142006"
              Theta.Float    -> "f3e66d7cfcf8cec4e28182bfaee67c00"
              Theta.Double   -> "d42662766a376211229617337bd427a9"
              Theta.String   -> "4b041ed503e3481e6341363865732ab2"
              Theta.Date     -> "f9796917d9806a980c089cae18f6d57e"
              Theta.Datetime -> "e5158c40b7bc0dd7b50340fdb08e3c2b"
              Theta.UUID     -> "d2e4edd151c5d24637c63111311b13d1"
              Theta.Time     -> "d25ae4f8007c356e61dcd841309ca82e"
            test name t =
              testCase name (Theta.wrapPrimitive t ?= expectedPrimitive t)
        in [ test (show t) t | t <- primitives ]

    , testGroup "container"
      [ testCase "[Bool]" $ Theta.array' Theta.bool' ?= "a923b66caa3b5848189d5f889979bd36"
      , testCase "[Int]"  $ Theta.array' Theta.int'  ?= "10638db762caad1bc8d8a24c6a5f2561"

      , testCase "{Long}"   $ Theta.map' Theta.long'   ?= "ae09ab852e0b160a795bf522d6cf2a97"
      , testCase "{String}" $ Theta.map' Theta.string' ?= "2f68eeb4857f3642bc49a3928c57ca9e"

      , testCase "Bytes?" $ Theta.optional' Theta.bytes' ?= "1dbb7970dcf10823968dd187a01ba969"
      , testCase "Date?"  $ Theta.optional' Theta.date'  ?= "e788ea9060022bad189298d2e253199d"
      ]

    , testGroup "named types"
      [ testCase "enum" $ do
          HasTheta.theta @TrickyEnum ?= "d82ce0b1c6f11f6f35538f04fccfbf50"

      , testCase "record" $ do
          HasTheta.theta @Record   ?= "1a93bc566e8d4a6ab6c61e9942ff66f5"

      , testCase "variant" $ do
          HasTheta.theta @Variant  ?= "6963fdd3e362d41b69dc873d829a3462"

      , testGroup "newtype"
        [ testCase "Newtype" $
            HasTheta.theta @Newtype  ?= "f881fda8539d6269a4c8a69a019a6eca"
        , testCase "Newtype2" $
            HasTheta.theta @Newtype2 ?= "4056071b1f86b14d95f349ba02014633"

          -- from nested_types.theta
        , testCase "Newtype_0" $
            HasTheta.theta @Newtype_0 ?= "05d345b5db4df5c3a158bc5309a1383f"
        , testCase "Newtype_1" $
            HasTheta.theta @Newtype_1 ?= "02c3ddf457aa85e993d10f89315d0f78"
        ]

      , testCase "alias" $ do
          HasTheta.theta @Alias ?= "02c3ddf457aa85e993d10f89315d0f78"
      ]

    , testCase "recursive" $ do
        HasTheta.theta @Recursive ?= "926649e7e828763c4333257b4f586af2"

    , testGroup "mutually recursive"
      [ testCase "MutualA" $
          HasTheta.theta @MutualA ?= "5ed64a512c84ba695a337c0b0ac66ce6"
      , testCase "MutualB" $
          HasTheta.theta @MutualB ?= "4589f9047afc3114e5fc9b48e2e08524"
      , testCase "Wrapper" $
          HasTheta.theta @Wrapper ?= "e425e524f643bc1905a6ffc79242c5c7"
      ]

    , testGroup "references"
      [ testCase "named_types.Record" $
          reference "named_types.Record" ?= "1a93bc566e8d4a6ab6c61e9942ff66f5"
      , testCase "named_types.Variant" $
          reference "named_types.Variant" ?= "6963fdd3e362d41b69dc873d829a3462"
      , testCase "named_types.Newtype" $
          reference "named_types.Newtype" ?= "f881fda8539d6269a4c8a69a019a6eca"
      , testCase "named_types.Newtype2" $
          reference "named_types.Newtype2" ?= "4056071b1f86b14d95f349ba02014633"
      , testCase "named_types.Alias1" $
          reference "named_types.Alias1" ?= "1a93bc566e8d4a6ab6c61e9942ff66f5"
      ]
    ]

  -- check that changes to structure of types changes hash
  , testGroup "structural changes"
    [ testGroup "records"
      [ testCase description $ do
        let message = "Hashes of fields and %s fields should be different."
        assertBool (printf message description) $
          Theta.hash (record "named_types.Example" fields) /=
          Theta.hash (record "named_types.Example" fields')
      | (description, fields') <- differentFields
      ]

    , testGroup "variant"
      [ testCase "case order does not matter" $
          Theta.hash variant @?= Theta.hash variant'
      ]
    ]
  ]
  where Theta.Type { hash } ?= string = show hash @?= string

        reference = Theta.withModule' theta'named_types . Theta.Reference'

        fields = [ Theta.Field "foo" Nothing Theta.int'
                 , Theta.Field "bar" Nothing Theta.string' ]

        variant = HasTheta.theta @Variant
        variant' =
          case variant of
            Theta.Type { baseType = Theta.Variant' _ cases } ->
              variant {
                Theta.baseType = Theta.Variant' "named_types.Variant" $
                                   NonEmpty.reverse cases
              }
            other ->
              error $ "named_types.Variant was not a Variant!\n" <> show other

        differentFields :: [(String, Theta.Fields Theta.Type)]
        differentFields =
          [ ("reordered", [ Theta.Field "bar" Nothing Theta.string'
                          , Theta.Field "foo" Nothing Theta.int' ])
          , ("added", [ Theta.Field "foo" Nothing Theta.int'
                      , Theta.Field "bar" Nothing Theta.string'
                      , Theta.Field "baz" Nothing Theta.date' ])
          , ("removed", [ Theta.Field "foo" Nothing Theta.int' ])
          , ("typeChanged", [ Theta.Field "foo" Nothing Theta.long'
                            , Theta.Field "bar" Nothing Theta.string' ])
          ]

        record name fields = type_
          where module_ = theta'named_types
                  { Theta.types = Map.insert name definition $
                      Theta.types theta'named_types }
                type_ = Theta.withModule' module_ $ Theta.Record' name fields
                definition = Theta.Definition name Nothing type_
