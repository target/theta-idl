{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

-- | Tests for the python code generation from Theta.
module Test.Theta.Target.Python where

import           Prelude                 hiding (toEnum)

import           Data.Aeson              (object, (.=))
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as Text (toStrict)

import           System.FilePath         ((<.>), (</>))

import           Text.Mustache           (compileMustacheFile, renderMustache)

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Theta.Error             as Theta
import           Theta.Metadata          (Metadata (..))
import qualified Theta.Pretty            as Theta
import           Theta.Target.Avro.Types (toSchema)
import           Theta.Target.Haskell    (loadModule)
import           Theta.Target.Python
import qualified Theta.Types             as Theta
import qualified Theta.Versions          as Theta

import           Theta.Test.Assertions   ((?=))

import qualified Paths_theta             as Paths

loadModule "test/data/modules" "newtype"
loadModule "test/data/modules" "primitives"
loadModule "test/data/modules" "importing_foo"

tests :: TestTree
tests = testGroup "Python"
  [ test_toReference
  , test_toEnum
  , test_toRecord
  , test_toVariant
  , test_toModule
  ]

test_toReference :: TestTree
test_toReference = testGroup "toReference"
  [ testCase "primitive types" $ do
      toReference' "base" Theta.bool'          ?= [python|bool|]
      toReference' "base" Theta.bytes'         ?= [python|bytes|]
      toReference' "base" Theta.int'           ?= [python|int|]
      toReference' "base" Theta.long'          ?= [python|int|]
      toReference' "base" Theta.float'         ?= [python|float|]
      toReference' "base" Theta.double'        ?= [python|float|]
      toReference' "base" Theta.string'        ?= [python|str|]
      toReference' "base" Theta.date'          ?= [python|date|]
      toReference' "base" Theta.datetime'      ?= [python|datetime|]
      toReference' "base" Theta.uuid'          ?= [python|UUID|]
      toReference' "base" Theta.time'          ?= [python|time|]
      toReference' "base" Theta.localDatetime' ?= [python|datetime|]
      toReference' "base" (Theta.fixed' 10)    ?= [python|bytes|]

  , testCase "containers" $ do
      toReference' "base" (Theta.array' Theta.int')       ?= [python|List[int]|]
      toReference' "base" (Theta.array' Theta.string')    ?= [python|List[str]|]
      toReference' "base" (Theta.map' Theta.int')         ?= [python|Mapping[str, int]|]
      toReference' "base" (Theta.map' Theta.string')      ?= [python|Mapping[str, str]|]
      toReference' "base" (Theta.optional' Theta.int')    ?= [python|Optional[int]|]
      toReference' "base" (Theta.optional' Theta.string') ?= [python|Optional[str]|]

  , testCase "nested containers" $ do
      toReference' "base" (Theta.array' (Theta.map' Theta.float')) ?=
        [python|List[Mapping[str, float]]|]
      toReference' "base" (Theta.map' (Theta.array' Theta.float')) ?=
        [python|Mapping[str, List[float]]|]

  , testGroup "named types"
    [ testCase "local" $ do
        let reference = wrap $ Theta.Reference' "base.FooReference"
            record    = wrap $ Theta.Record' "base.FooRecord" []
            variant   = wrap $ Theta.Variant' "base.FooVariant" [Theta.Case "base.Foo" Nothing []]
            newtype_  = wrap $ Theta.Newtype' "base.FooNewtype" record

        toReference' "base" reference ?= [python|FooReference|]
        toReference' "base" record    ?= [python|FooRecord|]
        toReference' "base" variant   ?= [python|FooVariant|]
        toReference' "base" newtype_  ?= [python|FooNewtype|]

    , testCase "imported" $ do
        let reference = wrap $ Theta.Reference' "base.FooReference"
            record    = wrap $ Theta.Record' "base.FooRecord" []
            variant   = wrap $ Theta.Variant' "base.FooVariant" [Theta.Case "base.Foo" Nothing []]
            newtype_  = wrap $ Theta.Newtype' "base.FooNewtype" record

        toReference' "foo" reference ?= [python|base.FooReference|]
        toReference' "foo" record    ?= [python|base.FooRecord|]
        toReference' "foo" variant   ?= [python|base.FooVariant|]
        toReference' "foo" newtype_  ?= [python|base.FooNewtype|]

    , testCase "with prefix" $ do
        let reference = wrap $ Theta.Reference' "base.FooReference"

        -- same module: no prefix
        toReference (Just "prefix") "base" reference ?= [python|FooReference|]

        -- different module: prefix + qualified
        toReference (Just "prefix") "foo" reference ?= [python|prefix.base.FooReference|]
    ]
  ]
  where wrap = Theta.withModule' (Theta.emptyModule "base" metadata)
        metadata = Metadata "1.0.0" "1.0.0" "base"

        toReference' = toReference Nothing

test_toEnum :: TestTree
test_toEnum = testGroup "toEnum"
  [ testCase "enum" $ do
      Python expected <- loadPython "enum"
      toEnum Nothing "test" "test.Foo" ["Bar", "baz", "_Baz"] @?=
        Python (Text.strip expected)
  ]

test_toRecord :: TestTree
test_toRecord = testGroup "toRecord"
  [ testCase "empty record" $ do
      expected <- loadPython "empty_record"
      toRecord' "test.Empty" [] ??= expected

  , testGroup "simple types" $
      let foo = Theta.Field "foo" Nothing Theta.int'
          bar = Theta.Field "bar" Nothing Theta.string'
      in
      [ testCase "one_field" $ do
          expected <- loadPython "one_field"
          toRecord' "test.OneField" [foo] ??= expected

      , testCase "two_fields" $ do
          expected <- loadPython "two_fields"
          toRecord' "test.TwoFields" [foo, bar] ??= expected
      ]

  , testGroup "local references"
    [ testCase "foo_reference" $ do
        expected <- loadPython "foo_reference"
        toRecord' "test.Foo" [fooField] ??= expected

    , testCase "bar_reference" $ do
        expected <- loadPython "bar_reference"
        toRecord' "test.Bar" [fooField] ??= expected
    ]

  , testCase "imported references" $ do
      expected <- loadPython "importing_reference"
      toRecord' "test.Foo" [importingField] ??= expected
  ]
  where toRecord' name fields =
          let type_ = Theta.withModule' module_ (Theta.Record' name fields) in
            case toSchema $ Theta.Definition name Nothing type_ of
              Right schema -> toRecord Nothing "test" schema name fields
              Left err     -> error (show err)

        module_ = Theta.Module
          { Theta.moduleName = "test"
          , Theta.types      = [ ("test.Foo", Theta.Definition "test.Foo" Nothing fooRecord)
                               , ("test.Bar", Theta.Definition "test.Bar" Nothing barRecord)
                               ]
          , Theta.imports    = [importedModule]
          , Theta.metadata   = Metadata "1.0.0" "1.0.0" "test"
          }

        fooRecord = Theta.withModule' module_ $ Theta.Record' "test.Foo" [fooField]
        barRecord = Theta.withModule' module_ $ Theta.Record' "test.Bar" [fooField]
        fooField  = Theta.Field "foo" Nothing $
                      Theta.withModule' module_ $ Theta.Reference' "test.Foo"

        importingField =
          Theta.Field "importing" Nothing $
            Theta.withModule' importedModule $ Theta.Reference' "imported.Foo"
        importedRecord =
          Theta.withModule' importedModule $ Theta.Record' "imported.Foo" []
        importedModule = Theta.Module
          { Theta.moduleName = "imported"
          , Theta.types      =
            [("imported.Foo",
              Theta.Definition "imported.Foo" Nothing importedRecord)]
          , Theta.imports    = []
          , Theta.metadata   = Metadata "1.0.0" "1.0.0" "imported"
          }

test_toVariant :: TestTree
test_toVariant = testGroup "toVariant"
  [ testCase "single case" $ do
      let cases = [Theta.Case "test.Case" Nothing [Theta.Field "foo" Nothing Theta.int']]

      expected <- loadPython "one_case"
      toVariant' "test.Variant" cases ??= expected

  , testCase "two cases" $ do
      let cases =
            [ Theta.Case "test.One" Nothing [ Theta.Field "foo" Nothing Theta.int']
            , Theta.Case "test.Two" Nothing [ Theta.Field "foo" Nothing Theta.int'
                                            , Theta.Field "bar" Nothing Theta.string']]

      expected <- loadPython "two_cases"
      toVariant' "test.Variant" cases ??= expected

  , testCase "imported references" $ do
      let cases = [Theta.Case "test.Case" Nothing [importingField]]

      expected <- loadPython "one_case_importing"
      toVariant' "test.Variant" cases ??= expected
  ]
  where toVariant' name cases =
          let type_ = Theta.withModule' module_ (Theta.Variant' name cases) in
            case toSchema $ Theta.Definition name Nothing type_ of
              Right schema -> toVariant Nothing "test" schema name cases
              Left err     -> error (show err)

        module_ = Theta.Module
          { Theta.moduleName = "test"
          , Theta.types      = [("test.Variant", Theta.Definition "test.Variant" Nothing variant)]
          , Theta.imports    = [importedModule]
          , Theta.metadata   = Metadata "1.0.0" "1.0.0" "test"
          }

        variant =
          Theta.withModule' module_ $ Theta.Variant' "test.Variant" [one, two]

        one = Theta.Case "test.One" Nothing [ Theta.Field "foo" Nothing Theta.int' ]
        two = Theta.Case "test.Two" Nothing [ Theta.Field "foo" Nothing Theta.int'
                                            , Theta.Field "bar" Nothing Theta.string']

        importingField =
          Theta.Field "importing" Nothing $
            Theta.withModule' importedModule $ Theta.Reference' "imported.Foo"
        importedRecord =
          Theta.withModule' importedModule $ Theta.Record' "imported.Foo" []
        importedModule = Theta.Module
          { Theta.moduleName = "imported"
          , Theta.types      =
            [("imported.Foo",
              Theta.Definition "imported.Foo" Nothing importedRecord)]
          , Theta.imports    = []
          , Theta.metadata   = Metadata "1.0.0" "1.0.0" "imported"
          }

test_toModule :: TestTree
test_toModule = testGroup "toModule"
  [ testCase "newtype.theta" $ do
      expected <- loadPython "newtype"
      toModule theta'newtype Nothing ??= expected

  , testCase "importing_foo.theta" $ do
      expected <- loadPython "importing_foo"
      toModule theta'importing_foo Nothing ??= expected

      expected <- loadPython "importing_foo_qualified"
      toModule theta'importing_foo (Just "theta") ??= expected
  ]

-- | Compare a @Python@ value that might error with an expected
-- @Python@ value, ignoring leading/trailing whitespace.
(??=) :: Either Theta.Error Python -> Python -> Assertion
got ??= expected = case got of
  Left err  -> fail $ Text.unpack $ Theta.pretty err
  Right res -> res ?= expected

-- | Load a @.mustache@ file with the given base name (without the
-- @.mustache@) from @test/data/python@.
--
-- For example, to load the Python template for @newtype.py@, you
-- would use @loadPython "newtype"@.
loadPython :: String -> IO Python
loadPython name = do
  dataDir <- Paths.getDataDir
  let path = dataDir
         </> "test/data/python"
         </> name
         <.> "mustache"
  template <- compileMustacheFile path
  let values = object [ "version" .= Theta.packageVersion' ]
  pure $ Python $ Text.toStrict $ renderMustache template values
