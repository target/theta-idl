{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
module Test.Theta.Target.Avro.Types where

import           Control.Monad                 (forM_)
import           Control.Monad.State           (StateT, evalStateT)

import qualified Data.Avro.Schema              as Avro
import           Data.Foldable                 (toList)
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.String.Interpolate       (__i)
import qualified Data.Text                     as Text
import qualified Data.Vector                   as Vector

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Theta.Error                   as Theta
import qualified Theta.Metadata                as Metadata
import           Theta.Name                    (Name)
import qualified Theta.Pretty                  as Theta
import           Theta.Types
import qualified Theta.Versions                as Theta

import           Theta.Target.Avro.Types

import           Theta.Target.Haskell          (loadModule)
import qualified Theta.Target.Haskell.HasTheta as HasTheta

loadModule "test/data/modules" "documentation"

tests :: TestTree
tests = testGroup "Types"
  [ testGroup "primitive"
    [ test_bool
    , test_bytes
    , test_int
    , test_long
    , test_float
    , test_double
    , test_string
    , test_date
    , test_datetime
    ]

  , testGroup "containers"
    [ test_array
    , test_map
    , test_optional
    ]

  , testGroup "named types"
    [ test_record
    , test_variant
    , test_newtype
    ]

  , test_toSchema

  , test_documentation
  ]

-- * Primitives

test_bool :: TestTree
test_bool = testCase "Bool" $ check (typeToAvro bool') Avro.Boolean

test_bytes :: TestTree
test_bytes = testCase "Bytes" $ check (typeToAvro bool') Avro.Boolean

test_int :: TestTree
test_int = testCase "Int" $ check (typeToAvro int') Avro.Int

test_long :: TestTree
test_long = testCase "Long" $ check (typeToAvro long') Avro.Long

test_float :: TestTree
test_float = testCase "Float" $ check (typeToAvro float') Avro.Float

test_double :: TestTree
test_double = testCase "Double" $ check (typeToAvro double') Avro.Double

test_string :: TestTree
test_string = testCase "String" $ check (typeToAvro string') Avro.String

test_date :: TestTree
test_date = testCase "Date" $ check (typeToAvro date') Avro.Int

test_datetime :: TestTree
test_datetime = testCase "Datetime" $ check (typeToAvro datetime') Avro.Long

-- * Containers

test_array :: TestTree
test_array = testCase "Array" $ do
  check (typeToAvro (array' string'))          $ Avro.Array Avro.String
  check (typeToAvro (array' (array' string'))) $ Avro.Array $ Avro.Array Avro.String
  check (typeToAvro (array' (map' string')))   $ Avro.Array $ Avro.Map Avro.String

test_map :: TestTree
test_map = testCase "Map" $ do
  check (typeToAvro (map' string'))          $ Avro.Map Avro.String
  check (typeToAvro (map' (map' string')))   $ Avro.Map $ Avro.Map Avro.String
  check (typeToAvro (map' (array' string'))) $ Avro.Map $ Avro.Array Avro.String

test_optional :: TestTree
test_optional = testCase "Optional" $ do
  let wrap type_ = Avro.mkUnion [Avro.Null, type_]
  check (typeToAvro (optional' string'))          $ wrap Avro.String
  check (typeToAvro (optional' (array' string'))) $ wrap (Avro.Array Avro.String)
  check (typeToAvro (map' (optional' string')))   $ Avro.Map (wrap Avro.String)

-- * Named Types

test_record :: TestTree
test_record = testGroup "record"
  [ testCase "empty" $ do
      check (typeToAvro $ get "test.EmptyRecord") $
        Avro.Record "test.EmptyRecord" [] Nothing Nothing []

  , testCase "one field" $ do
      let foo = Avro.Field "foo" [] Nothing Nothing Avro.String Nothing
      check (typeToAvro $ get "test.OneField") $
        Avro.Record "test.OneField" [] Nothing Nothing [foo]

  , testCase "two fields" $ do
      let foo = Avro.Field "foo" [] Nothing Nothing Avro.String Nothing
          bar = Avro.Field "bar" [] Nothing Nothing (Avro.Array Avro.String) Nothing
      check (typeToAvro $ get "test.TwoFields") $
        Avro.Record "test.TwoFields" [] Nothing Nothing [foo, bar]

  , testCase "recursive" $ do
      let recursive = Avro.NamedType "test.Recursive"
          foo       = Avro.Field "foo" [] Nothing Nothing recursive Nothing
      check (typeToAvro $ get "test.Recursive") $
        Avro.Record "test.Recursive" [] Nothing Nothing [foo]

  , testCase "nested" $ do
      -- note how the first use of the inner type (Foo) in the Avro
      -- schema has its whole definition inlined and the second use is
      -- a named reference
      let foo           = Avro.Field "foo" [] Nothing Nothing fooDefinition Nothing
          bar           = Avro.Field "bar" [] Nothing Nothing reference Nothing
          fooDefinition = Avro.Record "test.Foo" [] Nothing Nothing []
          reference     = Avro.NamedType "test.Foo"
      check (typeToAvro $ get "test.Nested") $
        Avro.Record "test.Nested" [] Nothing Nothing [foo, bar]
  ]

test_variant :: TestTree
test_variant = testGroup "variant"
  [ testCase "one case, no fields" $ do
      let case_ = Avro.Record "test.Enum1_A" [] Nothing Nothing []
      check (typeToAvro $ get "test.Enum1") $
        Avro.Record "test.Enum1" [] Nothing Nothing [constructor [case_]]

  , testCase "two cases, no fields" $ do
      let caseA = Avro.Record "test.Enum2_A" [] Nothing Nothing []
          caseB = Avro.Record "test.Enum2_B" [] Nothing Nothing []
      check (typeToAvro $ get "test.Enum2") $
        Avro.Record "test.Enum2" [] Nothing Nothing [constructor [caseA, caseB]]

  , testCase "one case" $ do
      let one           = Avro.Record "test.One" [] Nothing Nothing [foo, bar]
          foo           = Avro.Field "foo" [] Nothing Nothing fooDefinition Nothing
          bar           = Avro.Field "bar" [] Nothing Nothing optional Nothing
          fooDefinition = Avro.Record "test.Foo" [] Nothing Nothing []
          optional      = Avro.mkUnion [Avro.Null, Avro.NamedType "test.Foo"]
      check (typeToAvro $ get "test.OneCase") $
        Avro.Record "test.OneCase" [] Nothing Nothing [constructor [one]]

  , testCase "two cases" $ do
      let one           = Avro.Record "test.One" [] Nothing Nothing [foo, bar]
          foo           = Avro.Field "foo" [] Nothing Nothing fooDefinition Nothing
          bar           = Avro.Field "bar" [] Nothing Nothing optional Nothing
          fooDefinition = Avro.Record "test.Foo" [] Nothing Nothing []
          optional      = Avro.mkUnion [Avro.Null, Avro.NamedType "test.Foo"]

          two           = Avro.Record "test.Two" [] Nothing Nothing []
      check (typeToAvro $ get "test.TwoCases") $
        Avro.Record "test.TwoCases" [] Nothing Nothing [constructor [one, two]]
  ]
  where constructor cases = Avro.Field "constructor" [] Nothing Nothing union Nothing
          where union = Avro.mkUnion cases

test_newtype :: TestTree
test_newtype = testCase "newtype" $ do
  forM_ (toList $ types testModule) $ \ (Definition _ _ type_) -> do
    let got = evalStateT (typeToAvro newtype_) Set.empty
        newtype_ = wrapNewtype type_
        expected = evalStateT (typeToAvro type_) Set.empty
    case (got, expected) of
      (Right got', Right expected') -> got' @?= expected'
      (_, Left err)                 ->
        fail $ "Got error on *expected* typeToAvro:\n" <> Text.unpack (Theta.pretty err)
      (Left err, _)                 ->
        fail $ "Error when decoding newtype:\n" <> Text.unpack (Theta.pretty err)
  where wrapNewtype type_ = wrapped
          where wrapped =
                  withModule' module_ (Newtype' "test.Newtype" type_)
                module_ =
                  testModule
                    { types = types testModule
                            <> [("test.Newtype", Definition "test.Newtype" Nothing wrapped)]
                    }

test_documentation :: TestTree
test_documentation = testGroup "documentation"
  [ testCase "record" $ do
      let user = typeToAvro $ HasTheta.theta @User
      check (Avro.doc <$> user) (Just "User metadata.")

      let username_id_doc = [__i|
        An opaque identifier to distinguish users with identical
        usernames.
      |]
      check (Avro.fldDoc . head . Avro.fields <$> user) (Just username_id_doc)

  , testCase "variant" $ do
      let permission = typeToAvro $ HasTheta.theta @Permission
          permission_doc = "Security capabilities user accounts can have."
      check (Avro.doc <$> permission) (Just permission_doc)

      let avroUnion = Avro.fldType . head . Avro.fields <$> permission
          readRecord = Vector.head . Avro.options <$> avroUnion
      check (Avro.doc <$> readRecord) (Just "Read access to the given resource.")
  ]

-- * Schemas

test_toSchema :: TestTree
test_toSchema = testCase "toSchema" $ do
  let expected = Avro.Record "test.OneField" [] docs Nothing [foo]
      foo      = Avro.Field "foo" [] Nothing Nothing Avro.String Nothing
      docs     = Just
        [__i|
          Generated with Theta #{Theta.packageVersion'}
          Type hash: 9bea6d3431cc3238ff611c832dfcd4d9
            |]

  case toSchema $ getDefinition "test.OneField" of
    Left err  -> fail $ Text.unpack $ Theta.pretty err
    Right res -> do
      res @?= expected

      -- check top-level doc comment
      Avro.doc res @?= docs

-- * Reusable module for testing

testModule :: Module
testModule = Module
  { moduleName = "test"
  , types
  , imports    = []
  , metadata
  }
  where types    =
          [ -- records
            (def "test.EmptyRecord" $ Record' "test.EmptyRecord" [])
          , (def "test.OneField" $ Record' "test.OneField" [Field "foo" Nothing string'])
          , (def "test.TwoFields" $ Record' "test.TwoFields"
             [ Field "foo" Nothing string'
             , Field "bar" Nothing $ array' string'
             ])
          , (def "test.Recursive" $ Record' "test.Recursive"
              [ Field "foo" Nothing $ wrap $ Reference' "test.Recursive" ])
          , (def "test.Foo" $ Record' "test.Foo" [])
          , (def "test.Nested" $ Record' "test.Nested"
              [ Field "foo" Nothing $ wrap $ Reference' "test.Foo"
              , Field "bar" Nothing $ wrap $ Reference' "test.Foo"
              ])

            -- enums (variants with no fields on their constructors)
          , (def "test.Enum1" $ Variant' "test.Enum1" [ Case "test.Enum1_A" Nothing [] ])
          , (def "test.Enum2" $ Variant' "test.Enum2" [ Case "test.Enum2_A" Nothing []
                                                      , Case "test.Enum2_B" Nothing []
                                                      ])

            -- variants
          , (def "test.OneCase" $
                Variant' "test.OneCase"
                  [Case "test.One" Nothing
                   [ Field "foo" Nothing $ wrap $ Reference' "test.Foo"
                   , Field "bar" Nothing $ optional' $ wrap $ Reference' "test.Foo" ]])
          , (def "test.TwoCases" $
                Variant' "test.TwoCases"
                  [ Case "test.One" Nothing
                    [ Field "foo" Nothing $ wrap $ Reference' "test.Foo"
                    , Field "bar" Nothing $ optional' $ wrap $ Reference' "test.Foo" ]
                  , Case "test.Two" Nothing []
                  ])
          ]
        metadata = Metadata.Metadata
          { Metadata.languageVersion = "1.0.0"
          , Metadata.avroVersion     = "1.0.0"
          , Metadata.moduleName      = "test"
          }

        wrap = withModule' testModule

        def :: Name -> BaseType Type -> (Name, Definition Type)
        def name baseType = (name, Definition name Nothing $ wrap baseType)

getDefinition :: Name -> Definition Type
getDefinition name = case lookupDefinition name testModule of
  Right res -> res
  Left err  -> error err

get :: Name -> Type
get = definitionType . getDefinition

-- * Helper functions

check :: (Show a, Eq a)
      => StateT (Set Name) (Either Theta.Error) a
      -> a
      -> Assertion
check result expected = assertSuccess (evalStateT result Set.empty) expected

assertSuccess :: (Theta.Pretty err, Eq a, Show a)
              => Either err a
              -- ^ Either an error or a result.
              -> a
              -- ^ Expected value.
              -> Assertion
assertSuccess either expected = case either of
  Right result -> result @?= expected
  Left err     -> fail $ Text.unpack $ Theta.pretty err
