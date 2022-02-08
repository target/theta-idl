{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Test.Theta.Parser where

import           Prelude                 hiding (map)

import           Control.Monad.Reader    (runReaderT)

import           Data.Maybe              (fromJust)
import           Data.String.Interpolate (__i)
import           Data.Text               (Text)
import           Data.Void               (Void)

import           Text.Megaparsec         hiding (optional)

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Theta.Metadata          as Metadata
import           Theta.Metadata          (Version)
import           Theta.Parser
import           Theta.Types

tests :: TestTree
tests = testGroup "Parser"
  [ testGroup "metadata"
    [ test_metadata
    , test_metadataComments
    ]

  , testGroup "primitive types"
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
    [ test_enum
    , test_record
    , test_variant
    , test_newtype
    , test_reference
    ]

  , testGroup "documentation"
    [ test_doc
    , test_docTypes
    ]

  , test_backwardsCompatibility
  ]

-- * Metadata

test_metadata :: TestTree
test_metadata = testCase "Metadata" $
  go (metadataSection "test") testMetadataSection ?= testMetadata "1.0.0"
  where go parser input = parse parser "<test>" input

test_metadataComments :: TestTree
test_metadataComments = testGroup "Metadata with comments"
  [ testCase "end-of-line" $ check [__i|
      language-version: 1.0.0 // some language version
      avro-version: 1.0.0 /* yay */
      ---
    |]

  , testCase "before" $ check [__i|
      // Comments before
      /* More comments before
       */
      language-version: 1.0.0
      avro-version: 1.0.0
      ---
    |]

   , testCase "after" $ check [__i|
       language-version: 1.0.0
       avro-version: 1.0.0
       /* after */
       // after
       ---
       // also after?
     |]

    , testCase "interspersed" $ check [__i|
        language-version: /* blarg? */ 1.0.0
        // stuff
        avro-version: /* yep */ 1.0.0
        ---
      |]
  ]
  where check input =
          parse (metadataSection "test") "<test>" input ?= testMetadata "1.0.0"

-- * Primitive Types

test_bool :: TestTree
test_bool = testCase "Bool" $ do
  parse' "1.0.0" bool "Bool" ?= BaseType' Bool'
  parse' "1.0.0" atom "Bool" ?= BaseType' Bool'

test_bytes :: TestTree
test_bytes = testCase "Bytes" $ do
  parse' "1.0.0" bytes "Bytes" ?= BaseType' Bytes'
  parse' "1.0.0" atom "Bytes"  ?= BaseType' Bytes'

test_int :: TestTree
test_int = testCase "Int" $ do
  parse' "1.0.0" int "Int"  ?= BaseType' Int'
  parse' "1.0.0" atom "Int" ?= BaseType' Int'

test_long :: TestTree
test_long = testCase "Long" $ do
  parse' "1.0.0" long  "Long" ?= BaseType' Long'
  parse' "1.0.0" atom  "Long" ?= BaseType' Long'

test_float :: TestTree
test_float = testCase "Float" $ do
  parse' "1.0.0" float  "Float" ?= BaseType' Float'
  parse' "1.0.0" atom  "Float"  ?= BaseType' Float'

test_double :: TestTree
test_double = testCase "Double" $ do
  parse' "1.0.0" double  "Double" ?= BaseType' Double'
  parse' "1.0.0" atom  "Double"   ?= BaseType' Double'

test_string :: TestTree
test_string = testCase "String" $ do
  parse' "1.0.0" str  "String"  ?= BaseType' String'
  parse' "1.0.0" atom  "String" ?= BaseType' String'

test_date :: TestTree
test_date = testCase "Date" $ do
  parse' "1.0.0" date "Date" ?= BaseType' Date'
  parse' "1.0.0" atom "Date" ?= BaseType' Date'

test_datetime :: TestTree
test_datetime = testCase "Datetime" $ do
  parse' "1.0.0" datetime "Datetime" ?= BaseType' Datetime'
  parse' "1.0.0" atom "Datetime"     ?= BaseType' Datetime'


-- * Containers

test_array :: TestTree
test_array = testCase "Array" $ do
  parse' "1.0.0" array  "[String]" ?= BaseType' (Array' (BaseType' String'))
  parse' "1.0.0" atom  "[String]"  ?= BaseType' (Array' (BaseType' String'))

  parse' "1.0.0" array  "[{Int}]" ?=
    BaseType' (Array' (BaseType' (Map' (BaseType' Int'))))
  parse' "1.0.0" atom  "[{Int}]"  ?=
    BaseType' (Array' (BaseType' (Map' (BaseType' Int'))))

  parse' "1.0.0" array  "[[Long]]" ?=
    BaseType' (Array' (BaseType' (Array' (BaseType' Long'))))
  parse' "1.0.0" atom  "[[Long]]"  ?=
    BaseType' (Array' (BaseType' (Array' (BaseType' Long'))))

  -- "Long" is a subset of "Longs"
  parse' "1.0.0" array "[foo.Longs]" ?=
    BaseType' (Array' (BaseType' (Reference' "foo.Longs")))
  parse' "1.0.0" atom "[foo.Longs]" ?=
    BaseType' (Array' (BaseType' (Reference' "foo.Longs")))

test_map :: TestTree
test_map = testCase "Map" $ do
  parse' "1.0.0" map  "{String}"  ?= BaseType' (Map' (BaseType' String'))
  parse' "1.0.0" atom  "{String}" ?= BaseType' (Map' (BaseType' String'))

  parse' "1.0.0" map  "{[Int]}"  ?=
    BaseType' (Map' (BaseType' (Array' (BaseType' Int'))))
  parse' "1.0.0" atom  "{[Int]}" ?=
    BaseType' (Map' (BaseType' (Array' (BaseType' Int'))))

  parse' "1.0.0" map  "{{Long}}"  ?=
    BaseType' (Map' (BaseType' (Map' (BaseType' Long'))))
  parse' "1.0.0" atom  "{{Long}}" ?=
    BaseType' (Map' (BaseType' (Map' (BaseType' Long'))))

  -- "Long" is a subset of "Longs"
  parse' "1.0.0" map "{foo.Longs}" ?=
    BaseType' (Map' (BaseType' (Reference' "foo.Longs")))
  parse' "1.0.0" atom "{foo.Longs}" ?=
    BaseType' (Map' (BaseType' (Reference' "foo.Longs")))

test_optional :: TestTree
test_optional = testCase "Optional" $ do
  parse' "1.0.0" optional_  "String?"  ?= BaseType' (Optional' (BaseType' String'))
  parse' "1.0.0" signature'  "String?" ?= BaseType' (Optional' (BaseType' String'))

  parse' "1.0.0" optional_  "[String]?"  ?=
    BaseType' (Optional' (BaseType' (Array' (BaseType' String'))))
  parse' "1.0.0" signature'  "[String]?" ?=
    BaseType' (Optional' (BaseType' (Array' (BaseType' String'))))

  parse' "1.0.0" optional_  "[String?]?"  ?=
    BaseType' (Optional' (BaseType' (Array' (BaseType' (Optional' (BaseType' String'))))))
  parse' "1.0.0" signature'  "[String?]?" ?=
    BaseType' (Optional' (BaseType' (Array' (BaseType' (Optional' (BaseType' String'))))))

  -- "Long" is a subset of "Longs"
  parse' "1.0.0" optional_ "foo.Longs?" ?=
    BaseType' (Optional' (BaseType' (Reference' "foo.Longs")))
  parse' "1.0.0" signature' "foo.Longs?" ?=
    BaseType' (Optional' (BaseType' (Reference' "foo.Longs")))

-- * Named Types

test_enum :: TestTree
test_enum = testGroup "Enum"
  [ testCase "one case" $ do
      let one = "enum Foo = Bar"
          expected = BaseType' $ Enum' "test.Foo" ["Bar"]
      parse' "1.1.0" enumDefinition one ?=
        Definition "test.Foo" Nothing expected

  , testCase "two cases" $ do
      let two = "enum Foo = Bar | Baz"
          expected = BaseType' $ Enum' "test.Foo" ["Bar", "Baz"]
      parse' "1.1.0" enumDefinition two ?=
        Definition "test.Foo" Nothing expected

  , testCase "three cases" $ do
      let three = "enum Foo = Bar | baz | _Baz"
          expected = BaseType' $ Enum' "test.Foo" ["Bar", "baz", "_Baz"]
      parse' "1.1.0" enumDefinition three ?=
        Definition "test.Foo" Nothing expected
  ]

test_record :: TestTree
test_record = testGroup "Record"
  [ testCase "empty" $ do
      let empty    = "type Foo = {\n}\n"
          expected = BaseType' $ Record' "test.Foo" []
      parse' "1.0.0" definition empty ?=
        Definition "test.Foo" Nothing expected

  , testCase "one field" $ do
      let oneField = "type Foo = {\n\
                     \  foo : Int?\n\
                     \}\n"
          expected = BaseType' $ Record' "test.Foo" [field]
          field    = Field "foo" Nothing $ BaseType' (Optional' (BaseType' Int'))
      parse' "1.0.0" definition oneField ?=
        Definition "test.Foo" Nothing expected

  , testCase "two fields" $ do
      let twoFields = "type Foo = {\n\
                      \  foo : Int?,\n\
                      \  bar : test2.Foo\n\
                      \}\n"
          expected  = BaseType' $ Record' "test.Foo" [field1, field2]
          field1    = Field "foo" Nothing $ BaseType' (Optional' (BaseType' Int'))
          field2    = Field "bar" Nothing $ BaseType' (Reference' "test2.Foo")
      parse' "1.0.0" definition twoFields ?=
        Definition "test.Foo" Nothing expected

  , testCase "date fields" $ do
      let twoFields = "type Foo = {\n\
                      \  foo : Date?,\n\
                      \  bar : Datetime\n\
                      \}\n"
          expected  = BaseType' $ Record' "test.Foo" [field1, field2]
          field1    = Field "foo" Nothing $ BaseType' (Optional' (BaseType' Date'))
          field2    = Field "bar" Nothing $ BaseType' Datetime'
      parse' "1.0.0" definition twoFields ?=
        Definition "test.Foo" Nothing expected
  ]

test_variant :: TestTree
test_variant = testGroup "Variant"
  [ testCase "single case" $ do
      let withField = "type Foo = Foo { a : String }"
          expected  = BaseType' $ Variant' "test.Foo" [foo]
          foo       = Case "test.Foo" Nothing [Field "a" Nothing $ BaseType' String']
      parse' "1.0.0" definition  withField ?=
        Definition "test.Foo" Nothing expected

      let noField  = "type Foo = Foo {}"
          expected = BaseType' $ Variant' "test.Foo" [foo]
          foo       = Case "test.Foo" Nothing []
      parse' "1.0.0" definition  noField ?=
        Definition "test.Foo" Nothing expected

  , testCase "multiple cases" $ do
      let two      = "type Foo = Foo { a : String } | Bar {}"
          expected = BaseType' $ Variant' "test.Foo" [foo, bar]
          foo      = Case "test.Foo" Nothing [Field "a" Nothing $ BaseType' String']
          bar      = Case "test.Bar" Nothing []
      parse' "1.0.0" definition  two ?=
        Definition "test.Foo" Nothing expected

      let three    = "type Foo = Foo { a : String } \n\
                     \         | Bar {}\n      | Baz { a : Int? }\n"
          expected = BaseType' $ Variant' "test.Foo" [foo, bar, baz]
          foo      = Case "test.Foo" Nothing [Field "a" Nothing $ BaseType' String']
          bar      = Case "test.Bar" Nothing []
          baz      = Case "test.Baz" Nothing [Field "a" Nothing $
                                              BaseType' $ Optional' $ BaseType' Int']
      parse' "1.0.0" definition  three ?=
        Definition "test.Foo" Nothing expected

  , testCase "no-argument cases" $ do
      let two      = "type Foo = Bar | Baz"
          expected = BaseType' $ Variant' "test.Foo" [bar, baz]
          bar      = Case "test.Bar" Nothing []
          baz      = Case "test.Baz" Nothing []

      parse' "1.0.0" (definition <* eof) two ?=
        Definition "test.Foo" Nothing expected

      let mix      = "type Foo = Bar | Baz { a : Int }"
          expected = BaseType' $ Variant' "test.Foo" [bar, baz]
          bar      = Case "test.Bar" Nothing []
          baz      = Case "test.Baz" Nothing [Field "a" Nothing $ BaseType' Int']

      parse' "1.0.0" (definition <* eof) mix ?=
        Definition "test.Foo" Nothing expected

      let three    = "type Foo = Bar | Baz\n\n  | Qux"
          expected = BaseType' $ Variant' "test.Foo" [bar, baz, qux]
          bar      = Case "test.Bar" Nothing []
          baz      = Case "test.Baz" Nothing []
          qux      = Case "test.Qux" Nothing []

      parse' "1.0.0" (definition <* eof) three ?=
        Definition "test.Foo" Nothing expected
  ]

test_newtype :: TestTree
test_newtype = testCase "Newtype" $ do
  let newtype' = "type Foo = Bar"
      expected = BaseType' $ Newtype' "test.Foo" $ BaseType' $ Reference' "test.Bar"
  parse' "1.0.0" definition  newtype' ?=
    Definition "test.Foo" Nothing expected

test_reference :: TestTree
test_reference = testCase "Reference" $ do
  parse' "1.0.0" reference "test.Foo" ?= BaseType' (Reference' "test.Foo")
  parse' "1.0.0" atom "test.Bar"      ?= BaseType' (Reference' "test.Bar")

  -- "Long" is a substring of "Longs"
  parse' "1.0.0" reference "test.Longs" ?= BaseType' (Reference' "test.Longs")
  parse' "1.0.0" atom "test.Longs"      ?= BaseType' (Reference' "test.Longs")

-- * Documentation

test_doc :: TestTree
test_doc = testGroup "doc comments"
  [ testGroup "block docs"
    [ testCase "simple" $ do
        parseDoc "/** Some documentation. */"    ?= Doc "Some documentation."
        parseDoc "/**    Leading whitespace. */" ?= Doc "Leading whitespace."
        parseDoc "/** */"                        ?= Doc ""

        -- make sure docs *require* a comment
        assertFails $ parseDoc "blarg"

    , testCase "multiline" $ do
        parseDoc "/** Some \n documentation. */" ?= Doc "Some\ndocumentation."

        let multiline = [__i|
          /** This is a multiline block comment.
              I expect to see whitespace... etc managed correctly.
                  */
        |]
        parseDoc multiline ?= [__i|
          This is a multiline block comment.
          I expect to see whitespace... etc managed correctly.
        |]

    , testCase "leading *" $ do
        let multiline = [__i|
          /** This is a multiline block comment.
           * I expect to see whitespace... etc managed correctly.
           */
        |]
        parseDoc multiline ?= [__i|
          This is a multiline block comment.
          I expect to see whitespace... etc managed correctly.
        |]
    ]

  , testGroup "line docs"
    [ testCase "single line" $ do
        parseDoc "/// Some documentation."   ?= Doc "Some documentation."
        parseDoc "///   leading whitespace " ?= Doc "leading whitespace"
        parseDoc "///    "                   ?= Doc ""

    , testCase "multiline" $ do
        parseDoc "/// Some\n/// documentation."    ?= Doc "Some\ndocumentation."
        parseDoc "/// Some\n   /// documentation." ?= Doc "Some\ndocumentation."
    ]

  , testCase "misplaced docs" $ do
      let wrong = wrapModule [__i|
        type TCIN = Long
        /// This doc comment is not attached to a definition
        /// so it should cause a parse error
      |]

      assertFails $ parse' "1.0.0" (moduleBody "foo") wrong
  ]
  where parseDoc doc = fromJust . getDoc <$>
          parse' "1.0.0" definition (doc <> "\ntype Foo = Int")

-- test how documentation is parsed and included in type definitions
test_docTypes :: TestTree
test_docTypes = testGroup "docs on types"
  [ testCase "newtypes" $ do
      let newtype_ = wrapModule [__i|
              /// Foo is an Int!
              type Foo = Int
            |]
          type_ = BaseType' (Newtype' "test.Foo" (BaseType' Int'))
      parse' "1.0.0" (moduleBody "test") newtype_ ?=
        [DefinitionStatement
         (Definition "test.Foo" (Just $ Doc "Foo is an Int!") type_)]

  , testCase "aliases" $ do
      let alias = wrapModule [__i|
          /** Foo is an Int! */
          alias Foo = Int
        |]
      parse' "1.0.0" (moduleBody "test") alias ?=
        [DefinitionStatement
         (Definition "test.Foo" (Just $ Doc "Foo is an Int!") (BaseType' Int'))]

  , testGroup "records"
    [ testCase "definitions" $ do
        let record = wrapModule [__i|
            /// Foo docs
            type Foo = { bar : Int }
          |]
            type_ =
              BaseType' (Record' "test.Foo" [Field "bar" Nothing (BaseType' Int')])
        parse' "1.0.0" (moduleBody "test") record ?=
          [DefinitionStatement (Definition "test.Foo" (Just $ Doc "Foo docs") type_)]

    , testCase "fields" $ do
        let record = wrapModule [__i|
            type Foo = {
              /// Bar docs
              bar : Int,
              /** baz
                  docs */
              baz : String
            }
          |]
            type_ =
              BaseType' (Record' "test.Foo"
                         [ Field "bar" (Just "Bar docs") (BaseType' Int')
                         , Field "baz" (Just "baz\ndocs") (BaseType' String') ])
        parse' "1.0.0" (moduleBody "test") record ?=
          [DefinitionStatement (Definition "test.Foo" Nothing type_)]
    ]

  , testGroup "variants"
    [ testCase "definitions" $ do
        let variant = wrapModule [__i|
            /// Foo docs
            type Foo = Bar | Baz
          |]
            type_ =
              BaseType' (Variant' "test.Foo" [ Case "test.Bar" Nothing []
                                             , Case "test.Baz" Nothing [] ])
        parse' "1.0.0" (moduleBody "test") variant ?=
          [DefinitionStatement (Definition "test.Foo" (Just "Foo docs") type_)]

    , testCase "one case" $ do
        let variant = wrapModule [__i|
            type Foo = /** One docs */ One {}
          |]
            type_ = BaseType' (Variant' "test.Foo" [Case "test.One" (Just "One docs") []])
        parse' "1.0.0" (moduleBody "test") variant ?=
          [DefinitionStatement (Definition "test.Foo" Nothing type_)]

    , testCase "two cases" $ do
        let variant = wrapModule [__i|
            type Foo = /** One docs */ One {}
                     | /// Two docs
                       Two { /** foo docs
                               */
                             foo : Int }
          |]
            type_ =
              BaseType' (Variant' "test.Foo"
                          [ Case "test.One" (Just "One docs") []
                          , Case "test.Two" (Just "Two docs")
                            [Field "foo" (Just "foo docs") (BaseType' Int')]])
        parse' "1.0.0" (moduleBody "test") variant ?=
          [DefinitionStatement (Definition "test.Foo" Nothing type_)]
    ]
  ]

-- * Backwards Compatibility

-- Check that we fail on new language additions for older
-- language-version modules.
test_backwardsCompatibility :: TestTree
test_backwardsCompatibility = testGroup "backwards compatibility"
  [ testCase "enum <1.1.0" $ do
      let enum = "enum Foo = Bar | Baz"
      assertFails $ parse' "1.0.0" statement enum
  ]

-- * Utilities

parse' :: Version -> Parser a -> Text -> Either (ParseErrorBundle Text Void) a
parse' languageVersion parser = parse (runReaderT parser $ testMetadata languageVersion) "<tests>"

testMetadata :: Version -> Metadata.Metadata
testMetadata languageVersion = Metadata.Metadata
  { Metadata.languageVersion
  , Metadata.avroVersion = "1.0.0"
  , Metadata.moduleName  = "test"
  }

testMetadataSection :: Text
testMetadataSection = [__i|
    language-version: 1.0.0
    avro-version: 1.0.0
    ---
  |]

-- | Add version metadata on top of the given module body so that it
-- can be parsed with moduleBody.
wrapModule :: Text -> Text
wrapModule body = [__i|
    #{testMetadataSection}
    #{body}
  |]

-- | Assert that the parser fails with an error and doesn't return a
-- result.
assertFails :: Show b => Either a b -> Assertion
assertFails = \case
  Left _    -> pure ()
  Right res -> assertFailure [__i|
      expected: parser to fail with error
      but got: #{show res}
    |]

-- | Assert that the parser succeeds without specifying what the
-- result should be.
assertSucceeds :: (VisualStream s, TraversableStream s, Stream s, ShowErrorComponent e)
               => Either (ParseErrorBundle s e) b
               -> Assertion
assertSucceeds = \case
  Right _  -> pure ()
  Left err -> assertFailure [__i|
      expected correct parse
      but got parse error:
      #{errorBundlePretty err}
    |]

-- | Check whether the given parse result has no errors and returns
-- the expected value.
(?=) :: (Show a, Eq a, VisualStream s, TraversableStream s, Stream s, ShowErrorComponent e)
     => Either (ParseErrorBundle s e) a
     -> a
     -> Assertion
Right result ?= expected = result @?= expected
Left err ?= expected     = assertFailure [__i|
    expected correct parse: #{show expected}
    but got parse error:
    #{errorBundlePretty err}
  |]
