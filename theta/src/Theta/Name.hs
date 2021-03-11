{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- | A module for working with names and namespaces in Theta.
module Theta.Name where

import qualified Data.Char       as Char
import           Data.Hashable   (Hashable)
import           Data.Maybe      (fromMaybe)
import qualified Data.Map        as Map
import           Data.Text       (Text)
import qualified Data.Text       as Text
import           Data.Tree       (Tree (..))

import           GHC.Exts        (IsList (..), IsString (..))
import           GHC.Generics    (Generic)

import           Test.QuickCheck (Arbitrary (..))
import qualified Test.QuickCheck as QuickCheck

import           Theta.Pretty    (Pretty (..))

-- * Definitions

-- | An identifier which can refer to types, or constructors.
--
-- All identifiers have a base name as well as the name of the module
-- where they were defined, which acts as a namespace to disambiguate
-- definitions with the same base name.
--
-- A 'Name' is uniquely determined by 'name' and 'namespace'—'Name'
-- values are always fully qualified. We might have some kind of
-- namespace inference at the language level (similar to Avro), but
-- all namespaces will be fully resolved by the time they are parsed
-- into 'Name' values. This is important because it lets us compare
-- 'Name's for equality directly and use them in sets and maps without
-- worrying.
data Name = Name { moduleName :: ModuleName
                 , name       :: Text
                 } deriving (Show, Eq, Ord, Generic, Hashable)

-- | Parses string literals as dot-sperated names.
--
-- Will throw an exception if the name is not valid.
--
-- @
-- > "com.target.foo" :: Name
-- Name (ModuleName ["com"] "target") "foo"
-- @
instance IsString Name where
  fromString string = fromMaybe invalid $ parse $ Text.pack string
    where invalid = error $ "Name " <> show string <> " is not valid."

instance Arbitrary Name where
  arbitrary = Name <$> arbitrary <*> randomPart

  -- TODO: Generate valid non-ASCII names as well
-- | A QuickCheck generator that outputs a valid /part/ of a Theta
-- name.
--
-- In the fully-qualified name @com.example.Foo@, @"com"@, @"example"@
-- and @"Foo"@ are the parts.
randomPart :: QuickCheck.Gen Text
randomPart = do
  first <- QuickCheck.elements $ '_' : letters
  rest  <- randomList $ '_' : (letters <> digits)
  pure $ Text.pack $ first : rest
  where letters = ['a'..'z'] <> ['A'..'Z']
        digits  = ['0'..'9']

        randomList =
          QuickCheck.resize 20 . QuickCheck.listOf . QuickCheck.elements

-- | Return the parts of a name (ie module name and base name) as a
-- list.
--
-- In the fully-qualified name @com.example.Foo@, @"com"@, @"example"@
-- and @"Foo"@ are the parts.
parts :: Name -> [Text]
parts Name { moduleName, name } =
  namespace moduleName <> [baseName moduleName, name]

-- | The canonical string represenation of a name. This includes the
-- module name with each component separated by "." followed by the
-- name itself.
--
-- @
-- > pretty $ Name ["com"] "Foo"
-- "com.Foo"
-- > pretty $ Name ["com", "target"] "Foo"
-- "com.target.Foo"
-- @
instance Pretty Name where
  pretty Name { moduleName, name } = pretty moduleName <> "." <> name

-- | Why a name doesn't parse.
data Reason = Unqualified
              -- ^ The name needs to specify a namespace.
            | Invalid
              -- ^ The name is not syntactically valid in Theta.
              deriving (Show, Eq)

-- | Parse a text representation of a 'Name', returning an error with
-- a 'Reason' if the name doesn't parse.
parse' :: Text -> Either Reason Name
parse' (Text.splitOn "." -> components)
  | any (not . valid) components = Left Invalid
  | otherwise                    = case components of
      []  -> Left Invalid
      [_] -> Left Unqualified
      parts -> Right $ Name
        { name       = last parts
        , moduleName = ModuleName { namespace, baseName }
        }
  where baseName  = last $ init components
        namespace = init $ init components

        valid ""   = False
        valid part = first (Text.head part) && rest (Text.tail part)

        first x = Char.isLetter x || x == '_'
        rest xs = Text.all (\ x -> Char.isAlphaNum x || x == '_') xs

-- | Parse a text representation of a 'Name'.
--
-- Will return 'Nothing' if the name does not have an explicit module
-- name supplied.
parse :: Text -> Maybe Name
parse text = case parse' text of
  Right name -> Just name
  Left _     -> Nothing

-- | Render a name to a fully-qualified 'Text' representation.
render :: Name -> Text
render = pretty

-- * Modules

-- | An identifier which refers to a module.
--
-- Modules have a base name and an /optional/ namespace.
--
-- A module name is uniquely determined by its base name and
-- namespace. In the future, we might have some kind of "namespace
-- inference" similar to Avro, but that should happen /before/ we
-- produce values of this type. Once you have a 'ModuleName', it
-- should be fully qualified and you can compare it for equality
-- directly.
data ModuleName = ModuleName
  { namespace :: [Text]
  , baseName  :: Text
  } deriving (Show, Eq, Ord, Generic, Hashable)

instance Pretty ModuleName where
  pretty = renderModuleName

instance Arbitrary ModuleName where
  arbitrary = ModuleName <$> ns <*> randomPart
    where ns = QuickCheck.resize 3 $ QuickCheck.listOf randomPart

-- | Render a module name as a string with dots between namespace
-- components.
renderModuleName :: ModuleName -> Text
renderModuleName = Text.intercalate "." . toList

-- | Parse a module name as a series of names separated by dots (.).
--
-- @
-- > "foo" :: ModuleName
-- ModuleName [] "foo"
-- > "com.example.foo" :: ModuleName
-- ModuleName ["com", "example"] "foo"
-- @
instance IsString ModuleName where
  fromString = fromList . Text.splitOn "." . Text.pack

instance IsList ModuleName where
  type Item ModuleName = Text

  fromList []         = error "Cannot have an empty module name."
  fromList components = ModuleName
    { namespace = init components
    , baseName  = last components
    }

  toList ModuleName { baseName, namespace } = namespace <> [baseName]

-- | Return a list of the module name's parts.
--
-- @
-- > moduleParts "foo"
-- ["foo"]
-- > moduleParts "com.example.foo"
-- ["com", "example", "foo"]
-- @
moduleParts :: ModuleName -> [Text]
moduleParts = toList

-- | Build a module name from its parts.
fromModuleParts :: [Text] -> ModuleName
fromModuleParts = fromList

-- | Parses a module name, which is any set of identifiers separated
-- by dots (.).
--
-- Will error out if the string is empty.
parseModuleName :: Text -> ModuleName
parseModuleName (Text.splitOn "." -> components) = fromList components

-- | Extract the root part of the module name. If the module has a
-- namespace, the root is the /first/ component of the namespace;
-- otherwise, the root is the module's base name.
--
-- @
-- > moduleRoot "foo"
-- "foo"
-- > moduleRoot "example.foo"
-- "example"
-- > moduleRoot "com.example.foo"
-- "com"
-- @
moduleRoot :: ModuleName -> Text
moduleRoot ModuleName { namespace, baseName } = case namespace of
  []         -> baseName
  (root : _) -> root

newtype ModuleTree = ModuleTree (Map.Map Text ModuleTree)

-- | Given a list of modules, consolidate them into a hierarchy
-- according to their namespaces.
--
-- Given @"com.example"@, @"com.example.foo"@ and @"com.bar"@, the
-- hierarchy would be:
--
-- @
-- com
--  ↳ example
--    ↳ foo
--  ↳ bar
-- @
moduleHierarchy :: [ModuleName] -> [Tree Text]
moduleHierarchy names = toTree $ foldr insert empty (moduleParts <$> names)
  where insert [] tree                   = tree
        insert (p : ps) (ModuleTree map) =
          ModuleTree $ Map.insertWith (\ _ -> insert ps) p (expand ps) map

        expand []       = empty
        expand (p : ps) = ModuleTree [(p, expand ps)]

        empty = ModuleTree $ Map.empty

        toTree (ModuleTree map) =
          [Node part (toTree tree) | (part, tree) <- Map.toList map]
