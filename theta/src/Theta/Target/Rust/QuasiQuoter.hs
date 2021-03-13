{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

-- | A quasiquoter that generates 'Rust' values. This makes it easy
-- for us to generate Rust code dynamically.
--
-- @
-- -- generate a simple Rust struct with the given name:
-- struct :: Rust -> Rust
-- struct name = [rust|
--   #[derive(Debug)]
--   struct $name {
--       field: i32,
--   }
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
--    @[rust| |]@ block as part of your code layout without affecting
--    the indentation of the generated Rust code
--
--  * interpolating a multi-line snippet in a variable will indent
--    every line to the same level
--
-- This lets us define a multi-line method, include it in an impl
-- statement and have all the indentation work out correctly:
--
-- @
-- struct :: Rust -> Rust
-- struct = [rust|
--   #[derive(Debug)]
--   struct Foo {
--       field: i32,
--   }
--
--   impl Foo {
--       $method
--   }
--   |]
--   where method = [rust|
--        pub fn foo() -> i32 {
--            // ...
--        }
--      |]
-- @
--
-- This produces the following Rust snippet:
--
-- @
-- #[derive(Debug)]
-- struct Foo {
--     field: i32,
-- }
--
-- impl Foo {
--     pub fn foo() -> i32 {
--          // ...
--     }
-- }
-- @
module Theta.Target.Rust.QuasiQuoter
  ( Rust (..)
  , rust
  )
where

import           Data.Text                   (Text)

import           GHC.Exts                    (IsString)

import           Language.Haskell.TH.Quote   (QuasiQuoter (..))

import           Theta.Target.LanguageQuoter (Interpolable (..), quoter)

-- | A valid Rust snippet (stored as text).
--
-- This could either be standalone code or a fragment.
newtype Rust = Rust { fromRust :: Text }
  deriving stock (Show, Eq)
  deriving newtype (IsString, Semigroup, Monoid)

instance Interpolable Rust where
  toText   = fromRust
  fromText = Rust

-- | The 'rust' quasiquoter generates 'Rust' snippets, allowing you to
-- interpolate other Rust snippets from Haskell variables:
--
-- @
-- function :: Rust
-- function = [rust|
--   fn foo($variableName: i32) -> i32 {
--       $variableName + 1
--   }
--   |]
--   where variableName :: Rust
--         variableName = Rust "x"
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
--    @[rust| |]@ block as part of your code layout without affecting
--    the indentation of the generated Rust code
--
--  * interpolating a multi-line snippet in a variable will indent
--    every line to the same level
--
-- This lets us define a multi-line method, include it in an impl
-- statement and have all the indentation work out correctly:
--
-- @
-- struct :: Rust -> Rust
-- struct = [rust|
--   #[derive(Debug)]
--   struct Foo {
--       field: i32,
--   }
--
--   impl Foo {
--       $method
--   }
--   |]
--   where method = [rust|
--        pub fn foo() -> i32 {
--            // ...
--        }
--      |]
-- @
--
-- This produces the following Rust snippet:
--
-- @
-- #[derive(Debug)]
-- struct Foo {
--     field: i32,
-- }
--
-- impl Foo {
--     pub fn foo() -> i32 {
--          // ...
--     }
-- }
-- @
rust :: QuasiQuoter
rust = quoter ''Rust
