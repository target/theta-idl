{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

-- | A quasiquoter that generates 'Python' values. This makes it easy
-- for us to generate Python code dynamically.
--
-- @
-- -- generate a simple Python class with the given name:
-- class' :: Python -> Python
-- class' name = [python|
--   class $name:
--     pass
--   |]
-- @
--
-- Right now, this just does string interpolation under the hood with
-- some simple rules to help manage indentation:
--
--  * leading and trailing blank lines are ignored completely
--    (including for applying the rest of these rules)
--
--  * leading indentation is ignored—you can indent the internals of a
--    @[python| |]@ block as part of your code layout without
--    affecting the semantics of the generated Python code
--
--  * interpolating a multi-line snippet in a variable will indent
--    every line to the same level
--
-- This lets us define a multi-line method, include it in a class body
-- and have all the indentation work out correctly:
--
-- @
-- class' :: Python
-- class' = [python|
--   class Fooable:
--     $method
--   |]
--  where method = [python|
--      def foo():
--        print("foo")
--        print ("bar")
--     |]
-- @
--
-- This produces the following Python snippet:
--
-- @
-- class Fooable:
--   def foo():
--     print("foo")
--     print("bar")
-- @
module Theta.Target.Python.QuasiQuoter
  ( Python (..)
  , python
  )
where

import           Data.Text                   (Text)

import           GHC.Exts                    (IsString)

import           Language.Haskell.TH.Quote   (QuasiQuoter (..))

import           Theta.Target.LanguageQuoter (Interpolable (..), quoter)

-- | A valid Python snippet (stored as text).
--
-- This could either be a standalone Python program (class, function,
-- statement... etc) or a fragment of a program (identifier,
-- expression... etc).
newtype Python = Python { fromPython :: Text }
  deriving stock (Show, Eq)
  deriving newtype (IsString, Semigroup, Monoid)

instance Interpolable Python where
  toText       = fromPython
  fromText     = Python

-- | The 'python' quasiquoter generates 'Python' snippets, allowing
-- you to interpolate other Python snippets from Haskell variables:
--
-- @
-- function :: Python
-- function = [python|
--   def foo($variableName):
--     return $variableName + 1
--   |]
--   where variableName :: Python
--         variableName = Python "x"
-- @
--
-- You can get a literal @$@ by typing two in a row: @$$@.
--
-- The quasiquoter does straightforward string interpolation with a
-- few rules to make managing indentation easier:
--
--  * the first and last lines are ignored completely if they are all
--    whitespace (including for applying the rest of these rules)
--
--  * leading indentation is ignored—you can indent the internals of a
--    @[python| |]@ block as part of your code layout without
--    affecting the semantics of the generated Python code
--
--  * interpolating a multi-line snippet in a variable will indent
--    every line to the same level
--
-- This lets us define a multi-line method, include it in a class body
-- and have all the indentation work out correctly:
--
-- @
-- class' :: Python
-- class' = [python|
--   class Fooable:
--     $method
--   |]
--  where method = [python|
--      def foo():
--        print("foo")
--        print ("bar")
--     |]
-- @
--
-- This produces the following Python snippet:
--
-- @
-- class Fooable:
--   def foo():
--     print("foo")
--     print("bar")
-- @
python :: QuasiQuoter
python = quoter ''Python
