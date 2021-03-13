{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
-- | A quasiquoter that generates Kotlin code, with basic templating
-- support. This makes our Kotlin-generating code much easier to read
-- and write.
--
-- @
-- -- generate a Kotlin class with the given name and fields:
-- class' :: Kotlin -> Kotlin
-- class' name = [kotlin|
--   class $name() {
--   }
--   |]
-- @
--
-- Currently this quasiquoter just does simple string interpolation
-- without checking whether the generated Kotlin code is syntactically
-- valid. Checking syntactic validity at compile time is probably
-- infeasible, but we might add some kind of runtime checking in the
-- future.
--
-- To make the generated code readable, we have a few rules to handle
-- whitespace:
--
--  * leading and trailing blank lines are ignored
--
--  * leading indentation is ignored
--
--  * interpolating a multi-line snippet will indent each line to the
--  * same level

module Theta.Target.Kotlin.QuasiQuoter where

import           Data.Text                   (Text)

import           GHC.Exts                    (IsString)

import           Language.Haskell.TH.Quote   (QuasiQuoter (..))

import           Theta.Target.LanguageQuoter (Interpolable (..), quoter)

-- | A valid Kotlin snippet (stored as text).
--
-- This could either be a standalone Kotlin program (class, function,
-- statement... etc) or a fragment of a program (identifier,
-- expression... etc).
newtype Kotlin = Kotlin { fromKotlin :: Text }
  deriving stock (Show, Eq)
  deriving newtype (IsString, Semigroup, Monoid)

instance Interpolable Kotlin where
  toText   = fromKotlin
  fromText = Kotlin

-- | The 'kotlin' quasiquoter generates 'Kotlin' snippets, allowing
-- you to interpolate other Kotlin snippets from Haskell variables:
--
-- @
-- function :: Kotlin
-- function = [kotlin|
--   fun foo($variableName: Int): Int {
--     return $variableName + 1
--   }
--   |]
--   where variableName :: Kotlin
--         variableName = Kotlin "x"
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
--    @[kotlin| |]@ block as part of your code layout without
--    affecting the generated Kotlin code
--
--  * interpolating a multi-line snippet in a variable will indent
--    every line to the same level
--
-- This lets us define a multi-line method, include it in a class body
-- and have all the indentation work out correctly:
--
-- @
-- class' :: Kotlin
-- class' = [kotlin|
--   class Fooable() {
--     $method
--   }
--   |]
--  where method = [kotlin|
--      fun foo() {
--        println("foo")
--        println("bar")
--      }
--     |]
-- @
--
-- This produces the following Kotlin snippet:
--
-- @
-- class Fooable() {
--   def foo() {
--     println("foo")
--     println("bar")
--   }
-- }
-- @
kotlin :: QuasiQuoter
kotlin = quoter ''Kotlin
