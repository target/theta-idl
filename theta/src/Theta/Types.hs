{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ParallelListComp           #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskellQuotes      #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Theta.Types where

import           Control.Monad.State        (evalState, get, modify)

import           Data.Either                (isRight)
import qualified Data.Foldable              as Foldable
import           Data.Functor.Const         (Const (..))
import           Data.Functor.Identity      (Identity (..))
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Hashable              (Hashable)
import           Data.List                  (sortOn)
import           Data.List.NonEmpty         (NonEmpty)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes, listToMaybe)
import           Data.Sequence              (Seq ((:<|)), (|>))
import qualified Data.Sequence              as Seq
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

import           GHC.Exts                   (IsList (..), IsString)
import           GHC.Generics               (Generic)

import           Language.Haskell.TH.Syntax (Lift (..))

import           Theta.Hash                 (Hash, hashList, hashText)
import           Theta.Metadata             (Metadata)
import qualified Theta.Metadata             as Metadata
import           Theta.Name                 (ModuleName, Name)
import qualified Theta.Name                 as Name
import           Theta.Pretty               (Pretty (..), pr)
import           Theta.Primitive            (Primitive (..), hashPrimitive,
                                             primitiveName, primitives)

-- * Types

-- | Theta types have both a /base type/ and the module the type was
-- defined in.
--
-- A /base type/ needs a module to be fully defined so that we can
-- look up the actual type that corresponds to named references.
data Type = Type
  { baseType :: !(BaseType Type)
  , hash     :: Hash -- _has_ to be lazy (see hashType for details)
  , module_  :: !Module
  }

-- | The Show instance only shows the module name, not the whole
-- module.
--
-- This instance is intended for debugging and GHCi. The string
-- representation can lose information and change at any time, so do
-- not rely on it for serialization or testing.
instance Show Type where
  show Type { baseType, hash, module_ } = Text.unpack [pr|
        Type {{
          baseType = {pretty baseType},
          hash     = "{show hash}",
          module_  = "{pretty $ moduleName module_}"
        }}
  |]

-- | Types are compared for equality structurally.
--
-- Equality is implemented on top of 'hashType' see its documentation
-- for more details.
instance Eq Type where
  s == t = hash s == hash t

-- | Annotate a 'BaseType'' with the given module.
withModule :: Module -> BaseType' -> Type
withModule module_ (BaseType' baseType) = Type
  { module_
  , hash     = hashType module_ (BaseType' baseType)
  , baseType = withModule module_ <$> baseType
  }

-- | Annotate a 'BaseType Type' with the given module.
withModule' :: Module -> BaseType Type -> Type
withModule' module_ baseType = type_
  where type_ = Type
          { module_
          , hash     = hashType module_ $ toBaseType' type_
          , baseType
          }

-- ** Hashing

-- | Calculate the hash of the given 'BaseType''.
--
-- This function calculates hash of a type based on its
-- *structure*. The hash of two types is equal if and only if they
-- are 100% interchangeable.
--
-- In particular:
--
--  1. Aliases are ignored for hashing purposes.
--  2. Primitive types have canonical hashes.
--  3. Containers have a distinct hash based on the hash of their
--     item type.
--  4. Named types are hashed based on their names + structure.
--    a. Records: field names + field types, in order.
--    b. Variants: constructor names + field names + field types,
--       with constructors in /alphabetical/ order.
--  5. Newtypes are hashed based on their name and the hash of the
--     underlying type.
--  6. References have the same hash as the type they refer to.
--
-- Changes to anything above will change the hash of the type.
--
-- Since we need to be able to look up the hashes of types referenced
-- by our input type, we need to take the input type's module as an
-- input—and the module contains the input type /including its hash/.
-- This works because the 'hash' field is lazy and we "tie the
-- knot". (We use a similar pattern when resolving modules during
-- importing; see 'Theta.Import' for more details.)
--
-- This function's behavior is not defined if there is more than one
-- named type (record, variant or newtype) with the same
-- fully-qualified name. Fully-qualified names have to be unique in
-- Theta.
hashType :: Module -> BaseType' -> Hash
hashType module_ type_ = evalState (go type_) Set.empty
  where
    go (BaseType' baseType) = do
      case baseType of
        -- primitive types
        Primitive' t -> pure $ hashPrimitive t
        Fixed' size -> pure $ Name.hashName $ fixedName size

        -- containers
        Array' t    -> hashArray <$> go t
        Map' t      -> hashMap <$> go t
        Optional' t -> hashOptional <$> go t

        Enum' n symbols ->
          pure $ foldr (<>) (Name.hashName n) $ hashText . enumSymbol <$> symbols

        -- Special handling to avoid infinite recursion:
        --
        -- If we've already seen a type, we hash it solely based
        -- on its name; this respects the structure outlined in
        -- the comment but doesn't loop forever on recursive
        -- types.
        Record' n fields -> whenUnseen n $ do
          fieldHashes <- mapM hashField (toList fields)
          pure $ Name.hashName n <> hashList fieldHashes

        Variant' n cases -> whenUnseen n $ do
          let caseList = sortOn caseName $ toList cases
          caseHashes <- mapM hashCase caseList
          pure $ Name.hashName n <> hashList (toList caseHashes)

        Newtype' n type_ -> do
          typeHash <- go type_
          pure $ Name.hashName n <> typeHash

        Reference' n -> case lookupName n module_ of
          Right type_ -> go $ toBaseType' type_
          Left err    ->
            error $ "Error while hashing type:\n"
                 <> err
                 <> "\nThis is a bug in Theta."

    whenUnseen name doThis = do
      seen <- get
      if Set.member name seen
        then pure $ Name.hashName name
        else modify (Set.insert name) *> doThis

    hashArray hash    = hashText "[" <> hash <> hashText "]"
    hashMap hash      = hashText "{" <> hash <> hashText "}"
    hashOptional hash =                 hash <> hashText "?"

    hashField Field { fieldName, fieldType } = do
      fieldHash <- go fieldType
      pure $ hashText (textName fieldName) <> fieldHash
    hashCase Case { caseName, caseParameters } = do
      parameterHash <- mapM hashField (toList caseParameters)
      pure $ Name.hashName caseName <> hashList parameterHash

-- ** Base Types

-- | A version of 'BaseType' with no module annotations. This is the
-- type produced by the parser, before we've had a chance to build the
-- Theta module we're parsing.
newtype BaseType' = BaseType' (BaseType BaseType')
  deriving stock (Show, Lift)

-- | Turn a 'Type' into a 'BaseType'', discarding hash and module
-- information.
toBaseType' :: Type -> BaseType'
toBaseType' Type { baseType } = BaseType' $ toBaseType' <$> baseType

-- | Equality on 'BaseType'' is /syntactic/—references are only equal
-- to references with the same name, not to the underlying type they
-- refer to. Variants and records are compared structurally rather
-- than just by name.
--
-- This is the only reasonable equality we can have on 'BaseType''
-- since it does not carry any module information to derference
-- references.
--
-- Since 'BaseType'' is mostly an internal type for Theta, this
-- instance is mostly meant for testing purposes.
instance Eq BaseType' where
  (BaseType' s) == (BaseType' t) = case (s, t) of
            (Primitive' s, Primitive' t)           -> s == t
            (Fixed' size, Fixed' size')            -> size == size'

            -- containers
            (Array' s, Array' t)                   -> s == t
            (Map' s, Map' t)                       -> s == t
            (Optional' s, Optional' t)             -> s == t

            -- named types
            (Enum' n symbols, Enum' n' symbols')   -> n == n' && symbols == symbols'
            (Record' n fields, Record' n' fields') -> n == n' && fields == fields'
            (Variant' n cases, Variant' n' cases') -> n == n' && cases == cases'
            (Newtype' n t, Newtype' n' t')         -> n == n' && t == t'

            -- comparing with references:
            (Reference' n, Reference' n')          -> n == n'

            -- catch-all
            (_, _)                                 -> False

-- | All the types Theta supports. These are generally designed to
-- play well with Avro, which leads to a few design limitations.
--
-- The type parameter controls how recursive types (arrays,
-- optionals... etc) work:
--
--  * @BaseType'@ means that recursive types do not have modules
--    attached—this is the result we get from parsing, before we build
--    modules.
--
--  * @BaseType Type@ means that all the types /inside/ the base type
--    have modules attached—this is what we get /after/ we've built a
--    Theta module.
data BaseType t = Primitive' Primitive
                  -- ^ Int, String... etc

                | Fixed' !Word
                  -- ^ A binary value with the given number of bytes.

                | Array' !t
                  -- ^ Arrays which have a specified type as elements
                  -- (can be nested).
                | Map' !t
                  -- ^ Maps from string keys to values of the specified
                  -- type. The limitation to string keys is to play well
                  -- with Avro—could be relaxed in the future.
                | Optional' !t
                  -- ^ Optional, nullable types. Think 'Maybe' in
                  -- Haskell or @["null", "Foo"]@ in Avro.

                | Reference' !Name
                  -- ^ A reference to some other named type. This could
                  -- be a record, a variant or a newtype.

                | Enum' !Name !(NonEmpty EnumSymbol)
                  -- ^ An Enum value is one of a set of
                  -- symbols. Symbols have the same lexical
                  -- restrictions as Avro names: they have to match
                  -- the regular expression @[A-Za-z_][A-Za-z0-9_]*@.
                  --
                  -- The order of names defined in an Enum has no
                  -- semantic value, but can affect the generated Avro
                  -- schema and details of the binary encoding for the
                  -- type.
                | Record' !Name !(Fields t)
                  -- ^ A record is a named type with a set of fields
                  -- that can have differen types.
                | Variant' !Name (NonEmpty (Case t))
                  -- ^ A variant is a sum type, with each case
                  -- containing a record.
                | Newtype' !Name !t
                  -- ^ A newtype is an alias for a type that can be
                  -- treated distinctly by some targets. The practical
                  -- upside is that we can compile Newtypes to different
                  -- types in Haskell while keeping the Avro
                  -- representation the same.
                deriving stock (Functor, Foldable, Traversable, Show, Lift)

-- | see 'prettyType'
instance Pretty (BaseType Type) where
  pretty = \case
    -- primitive types
    Primitive' t -> pretty t

    Fixed' size  -> "Fixed " <> pretty size

    -- containers
    Array' t     -> "[" <> pretty t <> "]"
    Map' t       -> "{" <> pretty t <> "]"
    Optional' t  -> pretty t <> "?"

    -- named types
    Enum' n _    -> pretty n
    Reference' n -> pretty n
    Record' n _  -> pretty n
    Variant' n _ -> pretty n
    Newtype' n _ -> pretty n


newtype EnumSymbol = EnumSymbol { enumSymbol :: Text }
  deriving stock (Show, Eq, Ord, Generic, Lift)
  deriving anyclass (Hashable)
  deriving newtype (IsString)

instance Pretty EnumSymbol where pretty (EnumSymbol symbol) = symbol

-- | Returns the "underlying" type of a newtype or reference,
-- recursively. This goes through any number of references and
-- newtypes until it finds a primitive type, a container, a record or
-- a variant.
--
-- This will throw an exception if it encounters an invalid
-- 'Reference'' type.
underlyingType :: Type -> Type
underlyingType t@Type { module_, baseType } = case baseType of
  Newtype' _ type_ -> underlyingType type_
  Reference' name  -> case lookupName name module_ of
    Left err    -> error err
    Right type_ -> underlyingType type_

  _ -> t

-- | Primitive types are types like Int and Double that are not
-- defined in Theta and do not have extra structure.
--
-- Fixed size types for any size are considered primitive.
--
-- Records, variants, arrays, maps, optional type and references are
-- not primitive.
--
-- A newtype is primitive itself if its underlying type is primitive.
isPrimitive :: Type -> Bool
isPrimitive Type { baseType } = case baseType of
  -- primitive types
  Primitive' {}    -> True
  Fixed' {}        -> True

  -- non-primitive types
  Array' {}        -> False
  Map' {}          -> False
  Enum' {}         -> False
  Optional' {}     -> False
  Reference' {}    -> False
  Record' {}       -> False
  Variant' {}      -> False

  -- special handling
  Newtype' _ type_ -> isPrimitive type_

-- | Fixed-size types in Theta have a canonical name in the form
-- @theta.fixed.Fixed_n@ where @n@ is the size.
--
-- >>> fixedName 0
-- "theta.fixed.Fixed_0"
--
-- >>> fixedName 100
-- "theta.fixed.Fixed_100"
--
-- We (currently?) can't use these names to reference fixed-size types
-- /in Theta/, but they're used for hashing and generating Avro
-- schemas.
fixedName :: Word -> Name
fixedName s = Name.Name "theta.fixed" ("Fixed_" <> s')
  where s' = Text.pack $ show s

-- | Renders a type in a human-readable way—great for error
-- messages. The pretty representation looks like the syntax you would
-- use to /refer/ to that type in a Theta schema.
--
-- Primitive types are rendered with their canonical names (@Int@,
-- @Double@... etc).
--
-- Named types (records, variants, newtypes) and references are
-- rendered as their fully qualified name.
--
-- Arrays are rendered with their item type in square brackets (@[Int]@,
-- @[[com.exampleFoo]]@).
--
-- Maps are rendered with their value type in curly braces (@{Int}@,
-- @{[com.example.Foo]}@).
--
-- Optional types are rendered as the underlying type with a question
-- mark (?) (@Int?@, @[Int]?@, @[[com.example.Foo]?]@).
prettyType :: Type -> Text
prettyType Type { baseType } = pretty baseType

-- | see 'prettyType'
instance Pretty Type where
  pretty = prettyType


-- | The name of a field in a record or variant case.
--
-- These names do not have namespaces, and only have to be unique
-- /within/ a record or variant case.
newtype FieldName = FieldName { textName :: Text }
  deriving stock (Show, Eq, Ord, Lift)
  deriving newtype (Hashable, IsString)

instance Pretty FieldName where pretty = textName

-- | Every field of a record has a name and a type.
data Field t = Field
  { fieldName :: !FieldName
  , fieldDoc  :: !(Maybe Doc)
  , fieldType :: !t
  }
  deriving stock (Functor, Foldable, Traversable, Show, Eq, Lift)

instance HasDoc (Field t) where
  doc f field = setDoc <$> f (fieldDoc field)
    where setDoc doc = field { fieldDoc = doc }

-- | All the fields defined in a record or a branch of a variant.
--
-- The order of the fields is important and affects how Theta values
-- are represented. Two records are only equivalent if they have the
-- same fields defined /in the same order/.
data Fields t = Fields
  { fields  :: [Field t]
    -- ^ The field definitions, in order.
  , mapping :: !(HashMap FieldName Int)
    -- ^ A mapping from the name of each field to its position in the
    -- 'fields' list, starting with 0.
  }
  deriving stock (Functor, Foldable, Traversable, Show, Eq)

instance Lift t => Lift (Fields t) where
  liftTyped Fields { fields } = [|| wrapFields fields ||]

-- | Return the 0-based index of the field with the given name if it
-- is defined in the given field definitions.
--
-- Returns 'Nothing' if the field is not in the record.
fieldIndex :: Fields t -> FieldName -> Maybe Int
fieldIndex Fields { mapping } name = HashMap.lookup name mapping
{-# INLINE fieldIndex #-}

-- | Returns the names of all the fields, in the order they were
-- declared.
fieldNames :: Fields t -> [FieldName]
fieldNames Fields { fields } = fieldName <$> fields

-- | Build a full 'Fields' value out of a list of field definitions.
wrapFields :: [Field t] -> Fields t
wrapFields fields = Fields { fields, mapping = HashMap.fromList indices }
  where indices = [(fieldName field, n) | n <- [0..] | field <- fields]

instance IsList (Fields t) where
  type Item (Fields t) = Field t
  fromList = wrapFields
  toList = fields

-- | Every branch of a variant is a record with some number of named
-- fields.
data Case t = Case
  { caseName       :: !Name
  , caseDoc        :: !(Maybe Doc)
  , caseParameters :: !(Fields t)
  }
  deriving stock (Functor, Foldable, Traversable, Show, Eq, Lift)

instance HasDoc (Case t) where
  doc f case_ = setDoc <$> f (caseDoc case_)
    where setDoc doc = case_ { caseDoc = doc }

-- ** Wrapped Types

-- | Every single primitive type supported by this version of the
-- Theta library.
primitiveTypes :: [Type]
primitiveTypes = wrapPrimitive <$> primitives

-- | Wrap a primitive type into a type associated with the
-- @theta.primitive@ module.
wrapPrimitive :: Primitive -> Type
wrapPrimitive = withModule primitiveModule . BaseType' . Primitive'

bool' :: Type
bool' = wrapPrimitive Bool

bytes' :: Type
bytes' = wrapPrimitive Bytes

int' :: Type
int' = wrapPrimitive Int

long' :: Type
long' = wrapPrimitive Long

float' :: Type
float' = wrapPrimitive Float

double' :: Type
double' = wrapPrimitive Double

string' :: Type
string' = wrapPrimitive String

date' :: Type
date' = wrapPrimitive Date

datetime' :: Type
datetime' = wrapPrimitive Datetime

uuid' :: Type
uuid' = wrapPrimitive UUID

time' :: Type
time' = wrapPrimitive Time

localDatetime' :: Type
localDatetime' = wrapPrimitive LocalDatetime

fixed' :: Word -> Type
fixed' = withModule primitiveModule . BaseType' . Fixed'

array' :: Type -> Type
array' t = Type
  { baseType = Array' t
  , hash     = hashType (module_ t) (BaseType' (Array' (toBaseType' t)))
  , module_  = module_ t
  }

map' :: Type -> Type
map' t = Type
  { baseType = Map' t
  , hash     = hashType (module_ t) (BaseType' (Map' (toBaseType' t)))
  , module_  = module_ t
  }

optional' :: Type -> Type
optional' t = Type
  { baseType = Optional' t
  , hash     = hashType (module_ t) (BaseType' (Optional' (toBaseType' t)))
  , module_  = module_ t
  }

-- * Documentation

-- | A Theta docstring which can be associated with a type definition,
-- an alias or a record/variant field.
newtype Doc = Doc { getText :: Text }
  deriving stock (Show, Eq, Ord, Lift)
  deriving newtype (Hashable, IsString, Semigroup, Monoid)

-- | Theta entities that have documentation attached.
class HasDoc a where
  -- | Lens-style accessor for docs.
  --
  -- You can use this directly with a lens library, and it's used to
  -- implement 'getDoc' and 'setDoc' (which are going to be easier to
  -- use /without/ a lens library).
  doc :: forall f. Functor f => (Maybe Doc -> f (Maybe Doc)) -> a -> f a

-- | Apply a function to the doc attached to the given element, if
-- any.
modifyDoc :: HasDoc a => (Maybe Doc -> Maybe Doc) -> a -> a
modifyDoc f = runIdentity . doc (Identity . f)

-- | Get the doc attached to the given element, if any.
getDoc :: HasDoc a => a -> Maybe Doc
getDoc = getConst . doc Const

-- | Replace the doc attached to the given element.
setDoc :: HasDoc a => Maybe Doc -> a -> a
setDoc = modifyDoc . const

-- * Modules

-- | A Theta module is a collection of named types. A module is
-- defined in a single file, can import other modules and can be
-- imported itself.
--
-- A module has the same name as its file without the .theta
-- extension. So the file @foo.theta@ would define a module named
-- @foo@.
--
-- Namespaces are handled in the Java style, with each component of
-- the namespace being a directory. So the module @com.target.foo@
-- would be located in a file @com/target/foo.theta@.
data Module = Module
  { moduleName :: !Name.ModuleName
    -- ^ The name used to import the module.
  , types      :: !(Map Name (Definition Type))
    -- ^ All of the types defined in this module.
    --
    -- Note that this map /has/ to be lazy in its values for several
    -- of our internal algorithms to work correctly.
  , imports    :: [Module]
    -- ^ All the modules imported by this module, in the order they
    -- were imported.
  , metadata   :: !Metadata
    -- ^ Metadata for the module, specifying the version of the Theta
    -- language it represents and the version of the Theta → Avro
    -- encoding it expects.
  }
  deriving stock (Show)

-- | Create an empty module with the given name and metadata.
--
-- This module does not define any types or import any modules.
emptyModule :: Name.ModuleName -> Metadata -> Module
emptyModule moduleName metadata = Module
  { moduleName = moduleName
  , types      = []
  , imports    = []
  , metadata   = metadata
  }

data Definition t = Definition
  { definitionName :: Name
    -- ^ The name of the /definition/. If the definition is an alias,
    -- this is the name of the /alias/, not the type it aliases.
  , definitionDoc  :: Maybe Doc
    -- ^ Documentation specific to the definition/alias.
  , definitionType :: t
    -- ^ The underlying type. If this is an alias, the type could have
    -- a different name than the definition.
  }
  deriving stock (Show, Eq, Lift)

instance HasDoc (Definition t) where
  doc f definition = setDoc <$> f (definitionDoc definition)
    where setDoc doc = definition { definitionDoc = doc }

-- | A module named @theta.primitive@ that contains all the primitive
-- types.
primitiveModule :: Module
primitiveModule = Module
  { moduleName = "theta.primitive"
  , types
  , imports    = []
  , metadata   = Metadata.Metadata "1.0.0" "1.0.0" "theta.primitive"
  }
  where types = Map.fromList
          [ (name, Definition name Nothing $ wrapPrimitive type_)
          | type_ <- primitives
          , let name = primitiveName type_
          ]

-- | The set of names defined in a module, including aliases.
--
-- Does not include the names of any imported types.
definedNames :: Module -> Set Name
definedNames Module { types } = Map.keysSet types

-- | The set of all the names accessible from a module, including:
--
--  * names defined in the module
--  * names defined in imported modules
--  * names in transitive imports
--
-- This last category means that this set includes names that cannot
-- be used directly in the module without adding an @import@
-- statement.
allNames :: Module -> Set Name
allNames module_ = Set.unions $ definedNames <$> transitiveImports [module_]

-- | Does a module define or import a name?
--
-- Includes both direct and transitive imports.
contains :: Name -> Module -> Bool
contains name = isRight . lookupName name

-- | Look up the definition of the given name.
--
-- If the definition is an alias, this will return the definition /of
-- the alias/, not of its underlying type—which includes the
-- documentation /for the alias/, not the type.
--
-- If the name is not in the module, returns a descriptive error
-- message.
lookupDefinition :: Name -> Module -> Either String (Definition Type)
lookupDefinition name module_ =
  case listToMaybe $ catMaybes $ Map.lookup name <$> definitions of
    Just res -> pure res
    Nothing  -> Left [pr|
      {pretty name} is not defined in module {pretty $ moduleName module_}.
    |]
  where definitions = types <$> transitiveImports [module_]

-- | Look up the type for the given name in the module.
--
-- If the name is an alias, this returns the underlying type.
--
-- If the name is not in the module, returns a descriptive error
-- message.
lookupName :: Name -> Module -> Either String Type
lookupName name module_ = definitionType <$> lookupDefinition name module_


-- | Return a duplicate-free list of modules imported by the given
-- modules, directly or indirectly, as well as the modules
-- themselves. You get every module imported by the given module, each
-- module imported by those modules and so on. The order the modules
-- are returned in is implementation defined, except that it respect's
-- Theta's shadowing rules—an identifier defined in a module earlier
-- in the list is always the one that shadows definitions later in the
-- list.
--
-- Modules are compared for equality by /name/ because fully-qualified
-- module names have to be unique in a Theta schema. If you have
-- different modules with the same fully qualified name, only one of
-- them will be returned, chosen in some arbitrary,
-- implementation-specific way.
transitiveImports :: [Module] -> [Module]
transitiveImports (Seq.fromList -> rootModules) =
  Foldable.toList $ go Set.empty Seq.empty rootModules
  where -- breadth-first traversal of module imports
        go :: Set Name.ModuleName -> Seq Module -> Seq Module -> Seq Module
        go _ modules Seq.Empty = modules
        go seen modules (module_ :<| rest)
          | moduleName module_ `Set.member` seen =
            go seen modules rest
          | otherwise =
            let seen' = Set.insert (moduleName module_) seen in
            go seen' (modules |> module_)
                     (rest <> Seq.fromList (imports module_))

-- ** Module Definitions

-- | A Theta module definition has two components:
--
--    * a metadata section specifying langauge and encoding versions
--    * a body made up of imports and definitions
data ModuleDefinition = ModuleDefinition
  { header :: !Metadata
  , body   :: ![Statement]
  } deriving (Show)

-- | The name of the module for this definition.
moduleDefinitionName :: ModuleDefinition -> ModuleName
moduleDefinitionName ModuleDefinition { header } = Metadata.moduleName header

-- | A Theta statement is either a definition or an import. A Theta
-- file is a sequence of statements; the imports need to be imported
-- before it represents a module.
data Statement = DefinitionStatement !(Definition BaseType')
               | ImportStatement !ModuleName
               deriving (Show, Eq, Lift)
