# Theta User Guide

Theta lets you define protocols as algebraic data types and reuse those types with different formats (like Avro) and different programming languages (like Haskell).

  * [Theta Type Language](#theta-type-language)
    * [Versioning](#versioning)
    * [Namespaces](#namespaces)
    * [Modules](#modules)
    * [Types](#types)
      * [Primitive Types](#primitive-types)
      * [Containers](#containers)
      * [Records](#records)
      * [Variants](#variants)
      * [Newtypes and Aliases](#newtypes-and-aliases)
  * [Targets](#targets)
    * [Avro](#avro)
    * [Haskell](#haskell)

# Theta Type Language

## Versioning

Each Theta module starts with a metadata section specifying the version of the language and encodings (ie the Avro-based format) the schema uses:

```
language-version: 1.0.0
avro-version: 1.0.0
```

This lets different clients consistently communicate even if they are using different versions—or even different *implementations*—of the Theta compiler. Theta modules at a specific language and Avro version should always compile to compatible Avro encodings. The concrete guarantee is that a Theta compiler will either:

  * compile a schema at a given version to the same Avro and encode/decode it correctly
  * fail to process the schema if it does not support the specified language/encoding versions

## Namespaces

The names of types and variant constructors in Theta have two components: a base name and a namespace made up of the name of the module where the name was defined. Modules themselves can be namespaced, with multiple names separated by dots (.). 

For example, you might have a module called `com.example`. When you import `com.example`, Theta will search through your load path to find `com/example.theta`. A type called `Foo` defined in this module will have the fully qualified name `com.example.foo`.

Fully qualified names have to be globally unique in a Theta schema. In practice, this means that you can't have multiple modules with *exactly* the same name in the same project.

When a name is used in the same module that it is defined, it can be used without specifying the namespace.

When you import a name, you always have to refer to it with the full namespace.

```
import com.example

type Bar = ...

type Record = {
  foo : com.example.Foo,
  bar : Bar
}
```

We will probably add more flexible ways to use names in the future.

## Modules

Theta type definitions are grouped together in modules. Each module is a file ending in `.theta`, with the file name and path determining the module name. For example, the module `com.example.foo` has to be located in a file with the following path:

```
<ROOT>/com/example/foo.theta
```

`<ROOT>` is one of the directories that Theta searches through for modules, based on the Theta path which is set in different ways depending on the target you're compiling to.

Modules can import other modules, which will expose *every* type defined in the imported module. The `import` statement uses the fully qualified name of the module to import:

```
import com.example.foo
```

## Types

Theta protocols are algebraic data types, similar to what you'd see in Haskell or OCaml. Types are build up from records (products) and variants (sums), along with a small set of primitive types and containers built into the language.

Theta does not currently support defining polymorphic types. That capability might come in the future.

### Primitive Types

Theta supports a small set of primitive types that match up to the primitive types exposed by Avro:

  * `Bool`: booleans, `true` or `false`
  * `Bytes`: raw binary data
  * `Int`: 32-bit integers
  * `Long`: 64-bit integers
  * `Float`: 32-bit floating point numbers
  * `Double`: 64-bit floating point numbers
  * `String`: Unicode strings
  * `Date`: Absolute dates (in days)
  * `Datetime`: Absolute timestamps (in seconds)

### Containers

Theta has a few container types built in. Because Theta does not support polymorphism, these containers have to be specialized to their item types when they are used.

  * arrays: `[Int]`, `[[String]]`
  * maps: `{Int}`, `{[String]}`
    * maps currently only support string keys, similar to Avro
  * optionals (nullable types): `Int?`, `[String]?`

Containers can be layered arbitrarily. For example, an optional map to arrays of optional ints would look like this:

```
{[Int?]}?
```

### Records

Records are product types: they let us combine multiple types into one. Records can have any number of fields, where each field has a name and a type:

```
type Album = {
  title       : String,
  artist      : String,
  label       : String,
  track_count : Int
}
```

You can refer to a record type in other contexts using its name:

```
type Collection = {
  albums : [Album]
}
```

The order of definitions in a module does not matter—`Collection` would be a valid definition even if `Album` is defined later in the module.

Records can also be recursive or even mutually recursive:

```
type List = Empty {}
          | Cons {
              element : Int,
              rest    : List
            }
```

### Variants

Variants are sum types: a value of a variant is one of several options (called "cases").

Every case in a variant has a constructor (tag) and a set of fields, just like a record:

```
type OrderStatus = Shipped { eta : Date }
                 | Delivered { eta       : Date
                             , delivered : Date
                             }
```

`OrderStatus` represents a type which can be one of two values:

  * `Shipped`, with a single field (`eta : Date`)
  * `Delivered` with two fields (`eta : Date` and `delivered : Date`)

The fields in each case can be different or have different types.

### Newtypes and Aliases

Apart from records and variants, we can also define named types that correspond to other types:

```
type ProductId = String
alias FilePath = String
```

We have two options with different semantics:

  * `alias` defines a synonym that is semantically identical to the underlying type—lexically replacing every occurrence of an alias with its underlying type does not change the meaning of the Theta schema
  * `type` defines a semantically distinct new type with the same underlying representation (a `newtype` in Haskell parlance)

While `alias` and `type` look similar when defining Theta types, they can have different behavior in certain targets. For example, `alias` compiles to a type synonym in Haskell while `type` compiles to a `newtype`.

# Targets

Theta schemas can be compiled to different formats like communication protocols (Avro) or definitions in programming languages (Haskell).

## Avro

An individual Theta record or variant can be compiled to a fully self-contained Avro schema. Fully self-contained means that the Avro definition will inline definitions for all the other types used in the schema.

### Encoding

#### Names and Namespaces

Theta names have the same lexical and namespacing rules as Avro, so the fully qualified names in Avro schemas are the same as the fully qualified names in the Theta schema.

#### Primitive Types

Theta's primitive types directly match up with Avro's primitive types:

  * `Bool`: `"boolean"`
  * `Bytes`: `"bytes"`
  * `Int`: `"int"`
  * `Long`: `"long"`
  * `Float`: `"float"`
  * `Double`: `"double"`
  * `String`: `"string"`
  * `Date`: `"long"`
  * `Datetime`: `"long"`

#### Containers

Theta arrays are compiled to Avro arrays. For example, `[Int]` compiles to:

```
{
  "type" : "array",
  "items" : "int"
}
```

Theta maps similarly compile to Avro maps. For `{Int}`:

```
{
  "type" : "map",
  "values" : "int"
}
```

Optional values compile to a union of `"null"` and the underlying type. For `Int?`:

```
["null", "int"]
```

#### Records

Theta records are compiled to Avro records with corresponding fields.

Here's a simple Theta record:

```
type Album = {
  title       : String,
  artist      : String,
  label       : String,
  track_count : Int
}
```

Here's the corresponding Avro definition:

```
{
  "type" : "record",
  "name" : "Album",
  "fields" : [
    { "name" : "title", "type" : "string" },
    { "name" : "artists", "type" : "string" },
    { "name" : "label", "type" : "string" },
    { "name" : "track_count", "type" : "int" }
  ]
}
```

#### Variants

Variants in Theta are compiled to records in Avro, which lets us use variants as the top-level type in an Avro schema and refer to variant types by name. This record has a single field called `"constructor"` whose type is a union of the cases in the record. Each case is compiled to a record so that we can distinguish cases by name.

```
type OrderStatus = Shipped { eta : Date }
                 | Delivered { eta       : Date
                             , delivered : Date
                             }
```

becomes

```
{
  "type" : "record",
  "name" : "OrderStatus",
  "fields" : [
    {
      "name" : "constructor",
      "type" : [
        {
          "type" : "record",
          "name" : "Shipped",
          "fields" : [{ "name" : "eta", "type" : "int" }]
        },
        {
          "type" : "record",
          "name" : "Delivered",
          "fields" : [
            { "name" : "eta", "type" : "int" },
            { "name" : "delivered", "type" : "int" }
          ]
        }
      ]
    }
  ]
}
```

Note: because cases are compiled to records in Avro, the fully qualified names of cases in all the variants in the Theta schema have to be unique and cannot overlap with names of Theta records, unless the overlapping records/cases have exactly the same fields.

#### Aliases and Newtypes

The Avro encoding does not distinguish aliases and newtypes in Theta. Aliases and newtypes are compiled to whatever their underlying Theta type is compiled to.

```
type ProductId = String

alias Quantity = Int
```

`ProductId` will be transparently compiled to `"string"` in an Avro schema and `Quantity` will become `"int"`.

## Haskell

Theta types can be exposed as Haskell types via Template Haskell using the `loadModule` function:

```
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Policy where

import Theta.Target.Haskell (loadModule)

--        Theta path   module name
--             ↓          ↓
loadModule "examples" "album"
```

A call to `loadModule` will generate a top-level reference `theta'module_name :: Theta.Module` (`theta'album` in this example) which gives us access to the entire parsed Theta module.

It will also generate a Haskell type for every definition in the module, with several typeclass instances:

  * `Show`
  * `Eq`
  * `HasTheta`
    * Lets you get the corresponding Theta schema for a Haskell type. You can use this for functions that require an explicit Theta schame (like converting from Avro).
  * `ToTheta`
    * Lets you convert a value of the type to a generic Theta value.
  * `FromTheta`
    * Lets you convert a generic Theta value to a value of the type (assuming the schemas match up).

Given the three Theta-specific instances, we can convert to and from other formats Theta supports. For example, we can read a binary Avro file, convert it to a Theta value and convert that to a generate Haskell type.

### Names

Theta names have slightly different conventions from Haskell. To keep a Theta schema compatible with Haskell, names have a few extra restrictions:

  * type names (including record and variant names) have to start with uppercase letters
  * case names in variants have to start with uppercase letters
  * field names have to start with lowercase letters

### Namespaces

The Template Haskell code does not currently handle namespaces properly—only the base names of modules and types are used. Expect this to change in the future.

Currently, the module `com.example.foo` containing the following definition:

```
type com.example.Bar = {
  com.example.baz : Int
}
```

will generate the following Haskell definitions:

```
theta'foo :: Theta.Module

data Bar = Bar { baz :: Int32 }
```

### Types

#### Primitive Types

Theta's primitive types map to the following Haskell types:

  * `Bool`: `Bool`
  * `Bytes`: lazy `ByteString` (from `Data.ByteString.Lazy`)
  * `Int`: `Int32` (from `Data.Int`)
  * `Long`: `Int64` (from `Data.Int`)
  * `Float`: `Float`
  * `Double`: `Double`
  * `String`: `Text`
  * `Date`: `Day` (from `Data.Time.Calendar`)
  * `Datetime`: `UTCTime` (from `Data.Time.Clock`)

#### Containers

Theta arrays are compiled to Haskell lists: `[Int]` in Theta becomes `[Int32]` in Haskell.

Theta maps are compiled to hash maps (from `Data.HashMap.Lazy`) with `Text` keys: `{Int}` becomes `HashMap Text Int32`.

Optional values in Theta become `Maybe` values in Haskell: `Int?` becomes `Maybe Int`.

#### Records

Theta records are compiled to Haskell records with corresponding fields. The Haskell record has a constructor with the same name as the type.

```
type Album = {
  title       : String,
  artist      : String,
  label       : String,
  track_count : Int
}
```

turns into the following Haskell type:

```
data Album = Album
  { title       :: Text
  , artisst     :: Text
  , label       :: Text
  , tract_count :: Int
  }
```

#### Variants

Variants are compiled to Haskell sum types, with each case having its own field names:

```
type OrderStatus = Shipped { eta : Date }
                 | Delivered { eta       : Date
                             , delivered : Date
                             }
```

turns into the following Haskell type:

```
data OrderStatus = Shipped { eta :: Day }
                 | Delivered { eta :: Day, delivered :: Day }
```

One pitfall with this approach is that it can create *partial* field accessors if all the cases of the variant do not have exactly the same field names. In the above example, `delivered` becomes a `OrderStatus → Day` function that will throw an exception if applied to `Shipped`. (`eta` is fine because it appears in both `Shipped` and `Delivered`.)

We generate field names in the Haskell type because field names make the code significantly more self-documenting and help programmers keep track of which field is which. This was motivated by several people confusing the order of arguments in constructors like `Delivered`—it's too easy to swap `eta` and `delivered`, which leads to confusing bugs when your code is run.

The other downside with this approach is that Haskell does not let us define variants where different constructors have the *same* name with *different* types.

```
type Weird = Foo { a : Int }
           | Bar { a : String }
```

Generating the Haskell type for this directly would not compile because `a` has two *different* types (if `a` had the same type in both cases, it would not be a problem):

```
-- does not compile
data Weird = Foo { a :: Int }
           | Bar { a :: Text }
```

To deal with this edge case, the generated Haskell types have the case's constructor added to any conflicting names:

```
data Weird = Foo { a'Foo :: RequestPolicy }
           | Bar { a'Bar :: DcPolicy }
```

Note that this disambiguation happens *only* to field names that overlap with different types. If the same definition had another field that did not cause this problem, the generated Haskell would keep its name intact:

```
type Weird = Foo { a : Int,    b : Int }
           | Bar { a : String, b : Int }
```

would generate:

```
data Weird = Foo { a'Foo :: Int,  b :: Int }
           | Bar { a'Bar :: Text, b :: Int }
```

#### Aliases and Newtypes

Aliases in Theta are compiled to type synonyms in Haskell:

```
alias Quantity = Int
```

becomes

```
type Quantity = Int32
```

Theta newtypes are compiled to Haskell `newtype`s, with the same name for the type and the constructor:

```
type ProductId = String
```

becomes

```
newtype ProductId = ProductId Text
```
