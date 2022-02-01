{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
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
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Theta.Types where

import           Control.Monad.State     (evalState, get, modify)

import           Data.Binary             (Binary, encode)
import qualified Data.ByteString.Lazy    as LBS
import           Data.Digest.Pure.MD5    (MD5Digest, md5)
import           Data.Either             (isRight)
import qualified Data.Foldable           as Foldable
import           Data.Functor.Const      (Const (..))
import           Data.Functor.Identity   (Identity (..))
import           Data.Hashable           (Hashable)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HashMap
import           Data.List               (sortBy)
import           Data.List.NonEmpty      (NonEmpty)
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Maybe              (catMaybes, listToMaybe)
import           Data.Ord                (comparing)
import           Data.Sequence           (Seq ((:<|)), (|>))
import qualified Data.Sequence           as Seq
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.String.Interpolate (__i)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text

import           GHC.Exts                (IsList (..), IsString)
import           GHC.Generics            (Generic)

import           Theta.Metadata          (Metadata)
import qualified Theta.Metadata          as Metadata
import           Theta.Name              (ModuleName, Name)
import qualified Theta.Name              as Name
import           Theta.Pretty            (Pretty (..), p)

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
  show Type { baseType, hash, module_ } = Text.unpack [p|
        Type {
          baseType = #{baseType},
          hash     = "#{hash}",
          module_  = "#{pretty $ moduleName module_}"
        }
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

-- | The output of our hashing function.
--
-- Currently uses MD5 under the hood, but this might change in the
-- future—don't rely on the hashing implementation directly.
newtype Hash = Hash MD5Digest
  deriving newtype (Binary, Show, Eq, Ord)

instance Semigroup Hash where
  Hash a <> Hash b = toHash $ encode a <> encode b

-- | The hashing function we're using. Designed to be easy to
-- change—just change this function + the type underneath 'Hash'.
toHash :: LBS.ByteString -> Hash
toHash = Hash . md5

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
        Bool'     -> pure $ hashName "base.Bool"
        Bytes'    -> pure $ hashName "base.Bytes"
        Int'      -> pure $ hashName "base.Int"
        Long'     -> pure $ hashName "base.Long"
        Float'    -> pure $ hashName "base.Float"
        Double'   -> pure $ hashName "base.Double"
        String'   -> pure $ hashName "base.String"
        Date'     -> pure $ hashName "base.Date"
        Datetime' -> pure $ hashName "base.Datetime"

        -- containers
        Array' t    -> hashArray <$> go t
        Map' t      -> hashMap <$> go t
        Optional' t -> hashOptional <$> go t

        Enum' n symbols ->
          pure $ foldr (<>) (hashName n) $ hashText . enumSymbol <$> symbols

        -- Special handling to avoid infinite recursion:
        --
        -- If we've already seen a type, we hash it solely based
        -- on its name; this respects the structure outlined in
        -- the comment but doesn't loop forever on recursive
        -- types.
        Record' n fields -> whenUnseen n $ do
          fieldHashes <- mapM hashField (toList fields)
          pure $ hashName n <> hashList fieldHashes

        Variant' n cases -> whenUnseen n $ do
          let caseList = sortBy (comparing caseName) $ toList cases
          caseHashes <- mapM hashCase caseList
          pure $ hashName n <> hashList (toList caseHashes)

        Newtype' n type_ -> do
          typeHash <- go type_
          pure $ hashName n <> typeHash

        Reference' n -> case lookupName n module_ of
          Right type_ -> go $ toBaseType' type_
          Left err    ->
            error $ "Error while hashing type:\n"
                 <> err
                 <> "\nThis is a bug in Theta."

    whenUnseen name doThis = do
      seen <- get
      if Set.member name seen
        then pure $ hashName name
        else modify (Set.insert name) *> doThis

    hashArray hash    = hashText "[" <> hash <> hashText "]"
    hashMap hash      = hashText "{" <> hash <> hashText "}"
    hashOptional hash =                 hash <> hashText "?"

    hashField Field { fieldName, fieldType } = do
      fieldHash <- go fieldType
      pure $ hashText (textName fieldName) <> fieldHash
    hashCase Case { caseName, caseParameters } = do
      parameterHash <- mapM hashField (toList caseParameters)
      pure $ hashName caseName <> hashList parameterHash

    hashName = hashText . Name.render
    hashText = toHash . LBS.fromStrict . Text.encodeUtf8
    hashList = foldr (<>) (hashText "")

-- ** Base Types

-- | A version of 'BaseType' with no module annotations. This is the
-- type produced by the parser, before we've had a chance to build the
-- Theta module we're parsing.
newtype BaseType' = BaseType' (BaseType BaseType') deriving (Show)

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
            -- primitive types
            (Bool', Bool')                         -> True
            (Bytes', Bytes')                       -> True
            (Int', Int')                           -> True
            (Long', Long')                         -> True
            (Float', Float')                       -> True
            (Double', Double')                     -> True
            (String', String')                     -> True
            (Date', Date')                         -> True
            (Datetime', Datetime')                 -> True

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
data BaseType t = Bool'
                | Bytes'
                  -- ^ An array of bytes. Handy for storing arbitrary
                  -- binary blobs.
                | Int'
                | Long'
                | Float'
                | Double'
                | String'
                | Date'
                  -- ^ An absolute date, with no time attached.
                | Datetime'
                  -- ^ An absolute timestamp.

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
                deriving (Functor, Foldable, Traversable, Show)

newtype EnumSymbol = EnumSymbol { enumSymbol :: Text }
  deriving stock (Show, Eq, Ord, Generic)
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
-- Records, variants, arrays, maps, optional type and references are not
-- primitive.
--
-- A newtype is primitive itself if its underlying type is primitive.
isPrimitive :: Type -> Bool
isPrimitive Type { baseType } = case baseType of
  -- primitive types
  Bool'            -> True
  Bytes'           -> True
  Int'             -> True
  Long'            -> True
  Float'           -> True
  Double'          -> True
  String'          -> True
  Date'            -> True
  Datetime'        -> True

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
prettyType Type { baseType } = case baseType of
  -- primitive types
  Bool'        -> "Bool"
  Bytes'       -> "Bytes"
  Int'         -> "Int"
  Long'        -> "Long"
  Float'       -> "Float"
  Double'      -> "Double"
  String'      -> "String"
  Date'        -> "Date"
  Datetime'    -> "Datetime"

  -- containers
  Array' t     -> "[" <> prettyType t <> "]"
  Map' t       -> "{" <> prettyType t <> "]"
  Optional' t  -> prettyType t <> "?"

  -- named types
  Enum' n _    -> pretty n
  Reference' n -> pretty n
  Record' n _  -> pretty n
  Variant' n _ -> pretty n
  Newtype' n _ -> pretty n

instance Pretty Type where
  pretty = prettyType


-- | The name of a field in a record or variant case.
--
-- These names do not have namespaces, and only have to be unique
-- /within/ a record or variant case.
newtype FieldName = FieldName { textName :: Text }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Hashable, IsString)

instance Pretty FieldName where pretty = textName

-- | Every field of a record has a name and a type.
data Field t = Field
  { fieldName :: !FieldName
  , fieldDoc  :: !(Maybe Doc)
  , fieldType :: !t
  } deriving (Functor, Foldable, Traversable, Show, Eq)

instance HasDoc (Field t) where
  doc f field = fmap setDoc $ f (fieldDoc field)
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
  } deriving (Functor, Foldable, Traversable, Show, Eq)

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
  } deriving (Functor, Foldable, Traversable, Show, Eq)

instance HasDoc (Case t) where
  doc f case_ = fmap setDoc $ f (caseDoc case_)
    where setDoc doc = case_ { caseDoc = doc }

-- ** Wrapped Types

-- $ Primitive types are part of the @base@ module ('baseModule') and
-- have canonical hashes.

bool' :: Type
bool' = withModule baseModule $ BaseType' Bool'

bytes' :: Type
bytes' = withModule baseModule $ BaseType' Bytes'

int' :: Type
int' = withModule baseModule $ BaseType' Int'

long' :: Type
long' = withModule baseModule $ BaseType' Long'

float' :: Type
float' = withModule baseModule $ BaseType' Float'

double' :: Type
double' = withModule baseModule $ BaseType' Double'

string' :: Type
string' = withModule baseModule $ BaseType' String'

date' :: Type
date' = withModule baseModule $ BaseType' Date'

datetime' :: Type
datetime' = withModule baseModule $ BaseType' Datetime'

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
  deriving stock (Show, Eq, Ord)
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
    -- ^ All of the types defined in this module (including
    -- definitions imported from other modules).
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
  } deriving (Show)

data Definition t = Definition
  { definitionName :: Name
    -- ^ The name of the /definition/. If the definition is an alias,
    -- this is the name of the /alias/, not the type it aliases.
  , definitionDoc  :: (Maybe Doc)
    -- ^ Documentation specific to the definition/alias.
  , definitionType :: t
    -- ^ The underlying type. If this is an alias, the type could have
    -- a different name than the definition.
  } deriving (Show, Eq)

instance HasDoc (Definition t) where
  doc f definition = fmap setDoc $ f (definitionDoc definition)
    where setDoc doc = definition { definitionDoc = doc }

-- | A module with the name @base@ that contains all the primitive
-- types. Useful if you need an empty module for something.
--
-- This is the module to which all built-in types belong.
baseModule :: Module
baseModule = Module
  { moduleName = "base"
  , types
  , imports    = []
  , metadata   = Metadata.Metadata "1.0.0" "1.0.0" "base"
  }
  where types = [ ("base.Bool",     Definition "base.Bool"     Nothing bool')
                , ("base.Bytes",    Definition "base.Bytes"    Nothing bytes')
                , ("base.Int",      Definition "base.Int"      Nothing int')
                , ("base.Long",     Definition "base.Long"     Nothing long')
                , ("base.Float",    Definition "base.Float"    Nothing float')
                , ("base.Double",   Definition "base.Double"   Nothing double')
                , ("base.String",   Definition "base.String"   Nothing string')
                , ("base.Date",     Definition "base.Date"     Nothing date')
                , ("base.Datetime", Definition "base.Datetime" Nothing datetime')
                ]

-- | Does the given module define the given name?
defines :: Name -> Module -> Bool
defines name = isRight . lookupName name

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
    Nothing  -> Left [__i|
      #{pretty name} is not defined in module #{pretty $ moduleName module_}.
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

-- | A Theta statement is either a definition or an import. A Theta
-- file is a sequence of statements; the imports need to be imported
-- before it represents a module.
data Statement = DefinitionStatement !(Definition BaseType')
               | ImportStatement !ModuleName
               deriving (Show, Eq)

