{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
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
import           Theta.Target.Haskell
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
          Theta.Bool'        -> type_ @?= type_
          Theta.Bytes'       -> type_ @?= type_
          Theta.Int'         -> type_ @?= type_
          Theta.Long'        -> type_ @?= type_
          Theta.Float'       -> type_ @?= type_
          Theta.Double'      -> type_ @?= type_
          Theta.String'      -> type_ @?= type_
          Theta.Date'        -> type_ @?= type_
          Theta.Datetime'    -> type_ @?= type_

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
          Left _ -> assertFailure $
            [i|Could not find #{pretty name} in #{pretty $ Theta.moduleName module_}.|]
          Right _  -> pure ()

test_underlyingType :: TestTree
test_underlyingType = testCase "underlyingType" $ do
  Theta.underlyingType Theta.int'                @?= Theta.int'
  Theta.underlyingType (Theta.array' Theta.int') @?= (Theta.array' Theta.int')

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
          foo Nothing  = Just $ "foo"

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
    testGroup "unchanged"
    [ testCase "primitive" $ do
        Theta.bool'     ?= "4919965a95bd3a2429452fd4a69276e4"
        Theta.bytes'    ?= "0a54013f5513d0f2d6453c252b97f356"
        Theta.int'      ?= "17ea9a080335a6f8f92212e20687d09b"
        Theta.long'     ?= "1d9635869eeca793d715038fed6ad728"
        Theta.float'    ?= "0e7bbb7af69b70f68553e17122313028"
        Theta.double'   ?= "a5666b00d3697ec4ba60f56ebaff00b3"
        Theta.string'   ?= "3f66b8f128b9b450ed674e5895366d2d"
        Theta.date'     ?= "982300d27a39ddac604eb5b31f5681dd"
        Theta.datetime' ?= "c3258332c6f795f29ed884f9399468ad"

    , testCase "container" $ do
        Theta.array' Theta.bool' ?= "be04590193fe0b46274546386bbae04a"
        Theta.array' Theta.int'  ?= "9e5c1459ccfdde08dd93f23cdf0c4656"

        Theta.map' Theta.long'   ?= "2fd4b7df7008e79ecdaadfee4ed36056"
        Theta.map' Theta.string' ?= "4748560da73c3354613dd674e7b42a05"

        Theta.optional' Theta.bytes' ?= "fe8555ea03aa8ac9de9b74cda03313a3"
        Theta.optional' Theta.date'  ?= "fb84e2ea1a4379adcb3ce3eb6cae3c6c"

    , testGroup "named types"
      [ testCase "enum" $ do
          HasTheta.theta @TrickyEnum ?= "a5598bd1d8c1126f98add389ed96086c"

      , testCase "record" $ do
          HasTheta.theta @Record   ?= "12830fe0301c2dbf8fbd09c85ed2bef8"

      , testCase "variant" $ do
          HasTheta.theta @Variant  ?= "d018a023a21f379ee6cefc2270e06760"

      , testCase "newtype" $ do
          HasTheta.theta @Newtype  ?= "20f7cf73ddc12ea5aeb866dbcc147fa4"
          HasTheta.theta @Newtype2 ?= "9a186dc3ed7f0a7206f313121ba5e98b"

          -- from nested_types.theta
          HasTheta.theta @Newtype_0 ?= "9afe3955179b3c12b134ffc8b708f83a"
          HasTheta.theta @Newtype_1 ?= "b7dec420e20dc6ac129007a565aca39c"

      , testCase "alias" $ do
          HasTheta.theta @Alias     ?= "b7dec420e20dc6ac129007a565aca39c"
      ]

    , testCase "recursive" $ do
        HasTheta.theta @Recursive ?= "b49f702e7f64ff003622e4c476d81e3a"

    , testCase "mutually recursive" $ do
        HasTheta.theta @MutualA ?= "acbcf64168fe48a58e855ae625d610b4"
        HasTheta.theta @MutualB ?= "1292adf5796a27df68a49d3bc049e190"
        HasTheta.theta @Wrapper ?= "4479f3b3ab034cb491de22afe3527901"

    , testCase "references" $ do
        reference "named_types.Record"   ?= "12830fe0301c2dbf8fbd09c85ed2bef8"
        reference "named_types.Variant"  ?= "d018a023a21f379ee6cefc2270e06760"
        reference "named_types.Newtype"  ?= "20f7cf73ddc12ea5aeb866dbcc147fa4"
        reference "named_types.Newtype2" ?= "9a186dc3ed7f0a7206f313121ba5e98b"
        reference "named_types.Alias1"   ?= "12830fe0301c2dbf8fbd09c85ed2bef8"
    ]

  -- check that changes to structure of types changes hash
  , testGroup "structure"
    [ testCase "records" $
        forM_ differentFields $ \ (description, fields') -> do
          let message = "Hashes of fields and %s fields should be different."
          assertBool (printf message description) $
            Theta.hash (record "named_types.Example" fields) /=
            Theta.hash (record "named_types.Example" fields')

    , testCase "variants" $
        Theta.hash variant @?= Theta.hash variant'
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
