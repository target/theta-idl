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
      * [Enums](#enums)
      * [Newtypes and Aliases](#newtypes-and-aliases)
  * [Targets](#targets)
    * [Avro](#avro)
    * [Haskell](#haskell)

# Theta Type Language

A Theta schema is defined as a set of [type](#types) definitions grouped into [modules](#modules), with each module in its own `*.theta` file. The [example](example/) directory shows this in action with two files (`ids.theta` and `music.theta`) defining Theta modules named `ids` and `music` respectively.

A set of Theta modules can then be used with different [**targets**](#targets)—either generating schemas for serialization formats like [Avro](#avro) or generating code in programming languages like [Haskell](#haskell).

## Versioning

Each Theta module file starts with a metadata section with two version specifications:

  1.  `language-version`: controls the features supported by the Theta language itself
  2. `avro-version`: controls how Theta gets encoded and decoded as Avro

```
language-version: 1.0.0
avro-version: 1.0.0
---
```

Changes and additions to Theta require a minimum `language-version` *in the module where they are used*. For example, if you use `enum` in a module called `com.example` with `language-version` < 1.1.0, you'll get an error:

> Support for enum requires language-version ≥ 1.1.0
> Current module being parsed has language-version = 1.0.0

The `language-version` constraint is only checked in the module where a language feature is *used*, so if you set `language-version: 1.1.0` in `com.example`, importing `com.example` in a module with
`language-version: 1.0.0` will *not* raise an error.

### Version Guarantee

Explicitly specifying versions means that changes to the Theta language are always opt-in on a per module basis. Two clients can use different versions of `theta`—or, in principle, different *implementations* of Theta—and still work together as long as both versions support the `language-version` and `avro-version` for each Theta module they share.

Every version of Theta will either:

  * compile a schema at a given `language-version` and `avro-version` to an Avro schema that is structurally identical[^compatible] with the output of any other Theta release
  * fail to process the schema if it does not support the specified language or avro versions

[^compatible]: Two structurally identical Avro schemas will have the same types/field names/etc, but the exact JSON encoding may not be identical (for example, key order could be different) and `"doc"` fields in the schema may contain different strings or be absent altogether.

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

`<ROOT>` needs to be one of the directories specified in Theta's [load path](#load-path).

Modules can import other modules, which will expose *every* type defined in the imported module. The `import` statement uses the fully qualified name of the module to import:

```
import com.example.foo
```

### Comments and Documentation

You can use C/Java-style syntax for leaving comments in Theta files:

```
// This is a line comment

/* This is a block comment
   which can span over multiple lines
 */

type UserId = /* can be inline too */ Int
```

Comments that start with `///` or `/**` are **documentation comments** that have to appear directly before a type definition, variant case or record field. Using `///` or `/**` anywhere else is a parse error.

```
/** Documentation for the User type definition. */
type User = {
  /// Documentation for the user_id field
  user_id: UserId
}
```

Documentation comments will be preserved in the `doc` property of Avro schemas. In the future, we will have additional tool support like generating human-readable documentation from the `theta` command-line tool.

### Example Module

Here is a full module definition which would be saved in a `*.theta` file:

```
language-version: 1.0.0
avro-version: 1.0.0
---

/// User IDs are ints, but do not have to be contiguous
type UserId = Int

type User = {
  /// The user's globally unique id
  user_id: UserId,

  /// A user's display name—does not have to be unique
  display_name: String

  /** Users are marked as inactive after 30 days
    * of inactivity.
    */
  active: Bool
}
```

### Load Path

The Theta load path controls which directories Theta searches through to find module files. A load path can specify multiple relative or absolute directories separated with `:`, similar to the standard `PATH` variable:

```
/home/tikhon/example/specs:relative/path/specs
```

The Theta executable can get the load path either from the `THETA_LOAD_PATH` environment variable

``` shell
export THETA_LOAD_PATH="example/specs:other/specs"
theta ...
```

or through a command-line argument (`--path` or `-p` for short):

``` shell
theta --path "example/specs:other/specs" ...
```

If the command-line argument is used, the `THETA_LOAD_PATH` environment variable is ignored.

Note: Fully qualified names should be unique; a load path with multiple definitions of a module with the same fully qualified name may lead to errors or incorrect behavior.

## Types

Theta protocols are algebraic data types, similar to what you'd see in Haskell or OCaml. Types are build up from records (products) and variants (sums), along with a small set of primitive types and containers built into the language.

Theta does not currently support defining polymorphic types. That capability might come in the future.

### Primitive Types

Theta supports a small set of primitive types that match up to [primitive][avro-primitive] and [logical][avro-logical] types in Avro.

`language-version` ≥ 1.0.0:

  * `Bool`: booleans, `true` or `false`
  * `Bytes`: raw binary data
  * `Int`: 32-bit integers
  * `Long`: 64-bit integers
  * `Float`: 32-bit floating point numbers
  * `Double`: 64-bit floating point numbers
  * `String`: Unicode strings
  * `Date`: Absolute dates (in days)
  * `Datetime`: Absolute timestamps (in microseconds)

`language-version` ≥ 1.1.0:

  * `UUID`: Universally unique identifiers (UUIDs) (see [RFC 4122][rfc-4122])
  * `Time`: A time of day, starting at midnight (in microseconds)
  * `LocalDatetime`: An absolute timestamp in whatever timezone is considered local (in microseconds)
  * `Fixed(n)`: Fixed-size binary values with `n` bytes (`Fixed(1)`, `Fixed(10)`... etc)

[avro-primitive]: https://avro.apache.org/docs/current/spec.html#schema_primitive

[avro-logical]: https://avro.apache.org/docs/current/spec.html#Logical+Types

[rfc-4122]: https://www.ietf.org/rfc/rfc4122.txt

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

### Enums

Enums are value that can be one of several explicitly listed symbols.

```
enum Suit = Spades | Hearts | Diamonds | Clubs
```

Requires `language-version` ≥ 1.1.0.

From one perspective, an enum is just a restricted form of variant: it's a variant where each case only has a constructor and no fields. Enums can also differ from variants in how they are compiled to different targets—for example, a variant compiles to a union of records in Avro, while an enum compiles to an [Avro enum][avro-enum-spec]. Enums also generate different code from variants in languages like Python and Kotlin.

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

Theta's primitive types match up with Avro's [primitive][avro-primitive]  and [logical][avro-logical] types:

`language-version` ≥ 1.0.0

  * `Bool`: `"boolean"`
  * `Bytes`: `"bytes"`
  * `Int`: `"int"`
  * `Long`: `"long"`
  * `Float`: `"float"`
  * `Double`: `"double"`
  * `String`: `"string"`
  * `Date`: `"int"`
  * `Datetime`: `"long"`

`language-version` ≥ 1.1.0

  * `UUID`: `"uuid"` logical type (physical type: `"string"`)
  * `Time`: `"time"` logical type (physical type: `"long"`)
  * `LocalDateTime`: `"local-timestamp-micros"` (physical type: `"long"`)
  * `Fixed(n)`: a `"fixed"` type with size `n` and a standardized name

With `avro-version` ≥ 1.1.0, `Date` is encoded as the logical `"date"` type and `Datetime` is encoded as the logical `"timestamp-micros"` type.

Fixed types are named types in Avro, but do not have to have a name in Theta. Theta compiles `Fixed(n)` to a `"fixed"` type named `"theta.fixed.Fixed_n"`.

For example, the first use of `Fixed(10)` compiles to:

``` json
{
  "type": "fixed",
  "size": 10,
  "name": "theta.fixed.Fixed_10"
}
```

Every subsequent use refers to `theta.fixed.Fixed_10` by name.

With `avro-version` ≥ 1.1.0, `Date` is encoded as the logical `"date"` type and `Datetime` is encoded as the logical `"timestamp-micros"` type.

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

``` haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Policy where

import Theta.Target.Haskell (loadModule)

--            load path       module name
--                ↓                ↓
loadModule "example/specs" "com.example.album"
```

This example will generate definitions for the module defined in `example/specs/com/example/album.theta` as well as any Theta modules it imports.

The first argument to `loadModule` is the [load path](#load-path): one or more directories separated by `:`. Relative paths are interpreted relative to your working directory during compilation, which should be the root of your project (ie the location of your `.cabal` file).

The second argument is the *fully qualified* name of the module. A module's name and namespace correspond to the module's filepath: `com.example.name` would be located in `com/example/name.theta` somewhere in the load path.

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
p
``` haskell
theta'foo :: Theta.Module

data Bar = Bar { baz :: Int32 }
```

### Types

#### Primitive Types

Theta's primitive types map to the following Haskell types:

`language-version` ≥ 1.0.0

  * `Bool`: `Bool`
  * `Bytes`: lazy `ByteString` ([`bytestring`][haskell-bytestring])
  * `Int`: `Int32`
  * `Long`: `Int64`
  * `Float`: `Float`
  * `Double`: `Double`
  * `String`: `Text` (from [`text`][haskell-text])
  * `Date`: `Day` (from [`time`][haskell-time])
  * `Datetime`: `UTCTime` (from [`time`][haskell-time])

`language-version` ≥ 1.1.0

  * `UUID`: `UUID` (from [`uuid`][haskell-uuid])
  * `Time`: `TimeOfDay` (from [`time`][haskell-time])
  * `LocalDatetime`: `LocalTime` (from [`time`][haskell-time])
  * `Fixed(n)`: `FixedBytes n` (from `Theta.Fixed`)

Note: `FixedBytes` uses `DataKinds` to get type-level natural literals so that a type like `Fixed(10)` would compile to `FixedBytes 10`.

[haskell-text]: https://hackage.haskell.org/package/text
[haskell-bytestring]: https://hackage.haskell.org/package/bytestring
[haskell-time]: https://hackage.haskell.org/package/time
[haskell-uuid]: https://hackage.haskell.org/package/uuid

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

``` haskell
data Album = Album
  { title       :: Text
  , artist      :: Text
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

``` haskell
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

``` haskell
-- does not compile
data Weird = Foo { a :: Int }
           | Bar { a :: Text }
```

To deal with this edge case, the generated Haskell types have the case's constructor added to any conflicting names:

``` haskell
data Weird = Foo { a'Foo :: RequestPolicy }
           | Bar { a'Bar :: DcPolicy }
```

Note that this disambiguation happens *only* to field names that overlap with different types. If the same definition had another field that did not cause this problem, the generated Haskell would keep its name intact:

```
type Weird = Foo { a : Int,    b : Int }
           | Bar { a : String, b : Int }
```

would generate:

``` haskell
data Weird = Foo { a'Foo :: Int,  b :: Int }
           | Bar { a'Bar :: Text, b :: Int }
```

#### Enums

Enums are compiled to Haskell sum types, just like variants.

```
enum Status = Active | Inactive
```

would generate

``` haskell
data Status = Active | Inactive
```

Enum symbols in Theta can be any symbol legal in Avro[^avro-symbols], which means that some enums will have symbols that are not lexically valid Haskell constructors[^haskell-constructors]. To turn symbols into valid Haskell constructors, we drop any leading underscores and capitalize the first letter.

```
enum Tricky = weird | _odd
```

becomes:

``` haskell
data Tricky = Weird | Odd
```

If making symbols into valid constructors results in duplicate names, we go through the list of symbols left-to-right and add a `_` to the end of any symbol that would otherwise be duplicated.

```
enum Trickier = odd | Odd | __odd_
```

becomes:

``` haskell
data Trickier = Odd | Odd_ | Odd__
```

[^avro-symbols]: [Enum symbols in Avro][avro-enum-spec] can be any string that matches the regular expression `[a-zA-Z_][a-zA-Z0-9_]*`—that is, the first character can be an upper- or lowercase letter or an underscore, and the rest of the symbol can have any of those characters as well as numbers.

[^haskell-constructors]: Haskell constructors cannot start with lowercase letters or underscores.

[avro-enum-spec]: https://avro.apache.org/docs/current/spec.html#Enums

**Note**: While enum symbols in Theta are not namespaced and do not conflict with constructor names, an enum with the same constructor name as another enum or variant *will* cause conflicts in Haskell.

#### Aliases and Newtypes

Aliases in Theta are compiled to type synonyms in Haskell:

```
alias Quantity = Int
```

becomes

``` haskell
type Quantity = Int32
```

Theta newtypes are compiled to Haskell `newtype`s, with the same name for the type and the constructor:

```
type ProductId = String
```

becomes

``` haskell
newtype ProductId = ProductId Text
```
