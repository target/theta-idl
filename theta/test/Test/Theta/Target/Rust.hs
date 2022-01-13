{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module Test.Theta.Target.Rust where

import           Control.Monad                 (zipWithM_)

import qualified Data.Text.IO                  as Text

import           System.FilePath               ((<.>), (</>))
import qualified System.IO                     as IO

import qualified Theta.Metadata                as Theta
import           Theta.Target.Haskell          (loadModule)
import           Theta.Target.Rust
import qualified Theta.Types                   as Theta

import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Paths_theta                   as Paths

loadModule "test/data/modules" "newtype"
loadModule "test/data/modules" "recursive"

tests :: TestTree
tests = testGroup "Rust"
  [ test_toFile
  , test_toModule
  , test_toReference
  , test_toRecord
  , test_toVariant
  , test_toNewtype
  , test_alias
  ]

test_toFile :: TestTree
test_toFile = testCase "toFile" $ do
  expected <- loadRust "single_file"
  toFile [theta'newtype, theta'recursive] @?= expected

test_toModule :: TestTree
test_toModule = testGroup "toModule"
  [ testCase "newtype.theta" $ do
      expected <- loadRust "newtype"
      toModule theta'newtype @?= [expected]

  , testCase "recursive.theta" $ do
      expected <- loadRust "recursive"
      toModule theta'recursive @?= [expected]
  ]

test_toReference :: TestTree
test_toReference = testGroup "toReference"
  [ testCase "primitive types" $ do
      toReference Theta.bool'     @?= "bool"
      toReference Theta.bytes'    @?= "Vec<u8>"
      toReference Theta.int'      @?= "i32"
      toReference Theta.long'     @?= "i64"
      toReference Theta.float'    @?= "f32"
      toReference Theta.double'   @?= "f64"
      toReference Theta.date'     @?= "Date<Utc>"
      toReference Theta.datetime' @?= "DateTime<Utc>"

  , testCase "containers" $ do
      toReference (Theta.array' Theta.int')       @?= "Vec<i32>"
      toReference (Theta.array' Theta.string')    @?= "Vec<String>"
      toReference (Theta.map' Theta.int')         @?= "HashMap<String, i32>"
      toReference (Theta.map' Theta.string')      @?= "HashMap<String, String>"
      toReference (Theta.optional' Theta.int')    @?= "Option<i32>"
      toReference (Theta.optional' Theta.string') @?= "Option<String>"

      toReference (Theta.array' $ Theta.map' Theta.int')     @?= "Vec<HashMap<String, i32>>"
      toReference (Theta.map' $ Theta.array' Theta.int')     @?= "HashMap<String, Vec<i32>>"
      toReference (Theta.array'$ Theta.optional' Theta.int') @?= "Vec<Option<i32>>"

  , testCase "named types" $ do
      let reference = wrap $ Theta.Reference' "base.FooReference"
          record    = wrap $ Theta.Record' "base.FooRecord" []
          variant   = wrap $ Theta.Variant' "base.FooVariant"
            [ Theta.Case "base.Foo" Nothing [] ]
          newtype_  = wrap $ Theta.Newtype' "base.FooNewtype" record

      toReference reference @?= "base::FooReference"
      toReference record    @?= "base::FooRecord"
      toReference variant   @?= "base::FooVariant"
      toReference newtype_  @?= "base::FooNewtype"
  ]
  where wrap = Theta.withModule' Theta.baseModule

test_toRecord :: TestTree
test_toRecord = testGroup "toRecord"
  [ testCase "empty record" $ do
      toRecord "foo.Empty" [] @?=
        [rust|
          #[derive(Clone, Debug, PartialEq)]
          pub struct Empty {}

          impl ToAvro for Empty {
            fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
            }
          }

          impl FromAvro for Empty {
             fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
               context("foo.Empty", |input| {
                 Ok((input, Empty {}))
               })(input)
             }
          }
        |]
        

  , testCase "simple types" $ do
      let foo = Theta.Field "foo" Nothing Theta.int'
          input = Theta.Field "input" Nothing Theta.string'
          -- fields named "input" need special handling

      toRecord "foo.OneField" [foo] @?=
        [rust|
          #[derive(Clone, Debug, PartialEq)]
          pub struct OneField {
             pub foo: i32,
          }

          impl ToAvro for OneField {
            fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
              self.foo.to_avro_buffer(buffer);
            }
          }

          impl FromAvro for OneField {
             fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
               context("foo.OneField", |input| {
                   let (input, foo) = i32::from_avro(input)?;
                   Ok((input, OneField { foo }))
               })(input)
             }
          }
        |]
        

      toRecord "foo.TwoFields" [foo, input] @?=
        [rust|
          #[derive(Clone, Debug, PartialEq)]
          pub struct TwoFields {
             pub foo: i32,
             pub input: String,
          }

          impl ToAvro for TwoFields {
            fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
              self.foo.to_avro_buffer(buffer);
              self.input.to_avro_buffer(buffer);
            }
          }

          impl FromAvro for TwoFields {
             fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
               context("foo.TwoFields", |input| {
                 let (input, foo) = i32::from_avro(input)?;
                 let (input, __input__) = String::from_avro(input)?;
                 Ok((input, TwoFields { foo, input: __input__ }))
               })(input)
             }
          }
        |]
        

  , testCase "references" $ do
      let foo = Theta.Field "foo" Nothing (reference "foo.Foo")
      toRecord "foo.Foo" [foo] @?=
        [rust|
          #[derive(Clone, Debug, PartialEq)]
          pub struct Foo {
             pub foo: Box<foo::Foo>,
          }

          impl ToAvro for Foo {
            fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
              self.foo.to_avro_buffer(buffer);
            }
          }

          impl FromAvro for Foo {
             fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
               context("foo.Foo", |input| {
                 let (input, foo) = foo::Foo::from_avro(input)?;
                 Ok((input, Foo { foo: Box::new(foo) }))
               })(input)
             }
          }
        |]
        

      toRecord "foo.Bar" [foo] @?=
        [rust|
          #[derive(Clone, Debug, PartialEq)]
          pub struct Bar {
             pub foo: foo::Foo,
          }

          impl ToAvro for Bar {
            fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
              self.foo.to_avro_buffer(buffer);
            }
          }

          impl FromAvro for Bar {
             fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
               context("foo.Bar", |input| {
                 let (input, foo) = foo::Foo::from_avro(input)?;
                 Ok((input, Bar { foo }))
               })(input)
             }
          }
        |]
  ]
  where reference = Theta.withModule' Theta.baseModule . Theta.Reference'

test_toVariant :: TestTree
test_toVariant = testGroup "toVariant"
  [ testCase "single case" $ do
      let cases = [Theta.Case "foo.Case" Nothing [Theta.Field "foo" Nothing Theta.int']]
      toVariant "foo.Variant" cases @?=
        [rust|
          #[derive(Clone, Debug, PartialEq)]
          pub enum Variant {
             Case {
                foo: i32,
             },
          }

           impl ToAvro for Variant {
             fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
               match self {
                 Variant::Case { foo } => {
                   0i64.to_avro_buffer(buffer);
                   foo.to_avro_buffer(buffer);
                 },
               };
             }
           }

           impl FromAvro for Variant {
             fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
               context("foo.Variant", |input| {
                 let (input, tag) = i64::from_avro(input)?;
                 match tag {
                   0 => {
                     let (input, foo) = i32::from_avro(input)?;
                     Ok((input, Variant::Case { foo }))
                   },
                   _ => Err(Err::Error((input, ErrorKind::Tag))),
                 }
               })(input)
             }
           }
        |]
        
  , testCase "two cases" $ do
      let cases = [ Theta.Case "foo.One" Nothing [ Theta.Field "foo"   Nothing Theta.int' ]
                  , Theta.Case "foo.Two" Nothing [ Theta.Field "foo"   Nothing Theta.int'
                                                 , Theta.Field "input" Nothing Theta.string'
                                                 ]
                  ]
      toVariant "foo.Variant" cases @?=
        [rust|
          #[derive(Clone, Debug, PartialEq)]
          pub enum Variant {
             One {
                 foo: i32,
             },
             Two {
                 foo: i32,
                 input: String,
             },
          }

          impl ToAvro for Variant {
            fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
              match self {
                Variant::One { foo } => {
                  0i64.to_avro_buffer(buffer);
                  foo.to_avro_buffer(buffer);
                },
                Variant::Two { foo, input } => {
                  1i64.to_avro_buffer(buffer);
                  foo.to_avro_buffer(buffer);
                  input.to_avro_buffer(buffer);
                },
              };
            }
          }

          impl FromAvro for Variant {
            fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
              context("foo.Variant", |input| {
                let (input, tag) = i64::from_avro(input)?;
                match tag {
                  0 => {
                    let (input, foo) = i32::from_avro(input)?;
                    Ok((input, Variant::One { foo }))
                  },
                  1 => {
                    let (input, foo) = i32::from_avro(input)?;
                    let (input, __input__) = String::from_avro(input)?;
                    Ok((input, Variant::Two { foo, input: __input__ }))
                  },
                  _ => Err(Err::Error((input, ErrorKind::Tag))),
                }
              })(input)
            }
          }
        |]
  ]

test_toNewtype :: TestTree
test_toNewtype = testGroup "toNewtype"
  [ testCase "primitive" $ do
      toNewtype "foo.Foo" Theta.int' @?=
       [rust|
         #[derive(Copy, Clone, Debug, PartialEq)]
         pub struct Foo(pub i32);

         impl ToAvro for Foo {
            fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
              self.0.to_avro_buffer(buffer);
            }
         }

         impl FromAvro for Foo {
           fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
             context("foo.Foo", |input| {
               let (input, value) = i32::from_avro(input)?;
               Ok((input, Foo(value)))
             })(input)
           }
         }
       |]
       
  , testCase "reference" $ do
      toNewtype "foo.Foo" (reference "foo.Bar") @?=
       [rust|
         #[derive(Copy, Clone, Debug, PartialEq)]
         pub struct Foo(pub foo::Bar);

         impl ToAvro for Foo {
            fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
              self.0.to_avro_buffer(buffer);
            }
         }

         impl FromAvro for Foo {
           fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
             context("foo.Foo", |input| {
               let (input, value) = foo::Bar::from_avro(input)?;
               Ok((input, Foo(value)))
             })(input)
           }
         }
       |]

  , testCase "container" $ do
     toNewtype "foo.Foo" (Theta.array' Theta.double') @?=
       [rust|
         #[derive(Clone, Debug, PartialEq)]
         pub struct Foo(pub Vec<f64>);

         impl ToAvro for Foo {
            fn to_avro_buffer(&self, buffer: &mut Vec<u8>) {
              self.0.to_avro_buffer(buffer);
            }
         }

         impl FromAvro for Foo {
           fn from_avro(input: &[u8]) -> IResult<&[u8], Self> {
             context("foo.Foo", |input| {
               let (input, value) = Vec::from_avro(input)?;
               Ok((input, Foo(value)))
             })(input)
           }
         }
       |]
  ]
  where reference = Theta.withModule'  fooModule . Theta.Reference'
        fooModule = Theta.Module "foo"
          [("foo.Bar", Theta.Definition "foo.Bar" Nothing Theta.int')] [] metadata

        metadata = Theta.Metadata "1.0.0" "1.0.0" "foo"

test_alias :: TestTree
test_alias = testGroup "alias"
  [ testCase "primitive" $ do
      toDefinition (Theta.Definition "foo.Foo" Nothing Theta.int') @?=
        "pub type Foo = i32;"

  , testCase "reference" $ do
      toDefinition (Theta.Definition "foo.Foo" Nothing (reference "foo.Bar")) @?=
        "pub type Foo = foo::Bar;"

  , testCase "container" $ do
     toDefinition (Theta.Definition "foo.Foo" Nothing (Theta.array' Theta.double')) @?=
       "pub type Foo = Vec<f64>;"
  ]
  where reference = Theta.withModule' fooModule . Theta.Reference'
        fooModule = Theta.Module
          { Theta.moduleName = "foo"
          , Theta.types      =
            [("foo.Bar", Theta.Definition "foo.Bar" Nothing Theta.int')]
          , Theta.imports    = []
          , Theta.metadata   = Theta.Metadata "1.0.0" "1.0.0" "foo"
          }

-- | Load a @.rs@ file with the given base name (adding the @.rs@
-- extension) from @test/data/rust@.
loadRust :: String -> IO Rust
loadRust name = do
  dataDir <- Paths.getDataDir
  let path = dataDir </> "test/data/rust" </> name <.> "rs"
  Rust <$> Text.readFile path
