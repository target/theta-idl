-- NOTE: This file *intentionally* doesn't use OverloadedStrings to
-- ensure the quasiquoter works without the extension enabled (which
-- was a problem with an initial version of the code)
{-# LANGUAGE QuasiQuotes #-}

-- | Tests to check that the Python quasiquoter works as
-- advertised. See the documentation in the
-- 'Theta.Target.Python.QuasiQuoter' module for details.
module Test.Theta.Target.Python.QuasiQuoter where

import qualified Data.Text                       as Text

import           Theta.Target.Python.QuasiQuoter

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Python.QuasiQuoter"
  [ test_literal
  , test_indentation
  , test_interpolation
  ]

test_literal :: TestTree
test_literal = testCase "literal text" $ do
  [python| foo + bar * baz |] @?= Python (Text.pack "foo + bar * baz ")
  [python| $$foo + __bar * $$$$$$ |] @?= Python (Text.pack "$foo + __bar * $$$ ")
  [python|
         foo + bar * baz
         |] @?= Python (Text.pack "foo + bar * baz")

    -- note trailing whitespace after ‘baz’:
  [python|
         foo + bar * baz 
         |] @?= Python (Text.pack "foo + bar * baz ")


test_indentation :: TestTree
test_indentation = testCase "indentation" $ do
  let class_ = [python|
    class Foo:
      def bar(x):
        x += 10
        print(x)

      def foo(y):
        y *= 10
        return y
  |]
      expected = "class Foo:\n\
                 \  def bar(x):\n\
                 \    x += 10\n\
                 \    print(x)\n\
                 \\n\
                 \  def foo(y):\n\
                 \    y *= 10\n\
                 \    return y"
  class_ @?= Python (Text.pack expected)

test_interpolation :: TestTree
test_interpolation = testGroup "interpolation"
  [ testCase "basic interpolation" $ do
      let x  = [python|x|]
          y  = [python|y|]
          x_ = [python|x|]
          
      [python| $x |]     @?= Python (Text.pack "x ")
      [python| $x_ |]    @?= Python (Text.pack "x ")
      [python| $x$x |]   @?= Python (Text.pack "xx ")
      [python| $x$y |]   @?= Python (Text.pack "xy ")
      [python| $x$$$y |] @?= Python (Text.pack "x$y ")
      [python| $x$$a |]  @?= Python (Text.pack "x$a ")

  , testCase "indented single-line interpolation" $ do
        let body     = Python (Text.pack "return x")
            function = [python|
               def foo(x):
                 $body
                 |]
            expected = "def foo(x):\n\
                       \  return x"
        function @?= Python (Text.pack expected)

  , testCase "indented multi-line interpolation" $ do
        let arg    = Python (Text.pack "x")
            method = [python|
          def foo($arg):
            return $arg + 1
          |]
            class_ = [python|
          class Foo:
            $method
          |]
            expected = "class Foo:\n\
                       \  def foo(x):\n\
                       \    return x + 1"
        class_ @?= Python (Text.pack expected)

  , testCase "indented multi-line interpolation with text following" $ do
      let lines = [python|
            print('abc '\
                  ' def'
            |]
          context = [python|
            def foo():
              $lines)
            |]
          expected = "def foo():\n\
                     \  print('abc '\\\n\
                     \        ' def')"
      context @?= Python (Text.pack expected)

  , testCase "indented multi-line interpolation with blank lines" $ do
      let methods = [python|
        def foo(x):
          return x

        def bar(x):
          return x
        |]
          class_ = [python|
        class Foo:
          $methods
        |]
          expected = "class Foo:\n\
                     \  def foo(x):\n\
                     \    return x\n\
                     \\n\
                     \  def bar(x):\n\
                     \    return x"
      class_ @?= Python (Text.pack expected)
  ]
