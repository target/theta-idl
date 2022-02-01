{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

-- | Testing how we find, load and resolve Theta modules. This covers
-- a few distinct areas:
--
--  * LoadPath and loading Theta files
--  * resolving modules (ie parsing and imports)
--  * module validation (handling Theta-level errors in modules)
module Test.Theta.Import where

import           Control.Monad.Except          (runExceptT)

import           Data.String.Interpolate       (__i)
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

import           System.Directory              (canonicalizePath,
                                                withCurrentDirectory)
import           System.FilePath               (equalFilePath, joinPath,
                                                takeExtension, (<.>), (</>))

import           Text.Printf                   (printf)

import           Theta.Error
import           Theta.Import
import qualified Theta.Metadata                as Metadata
import qualified Theta.Name                    as Name
import           Theta.Pretty                  (pretty)
import           Theta.Types                   (Module (..))
import qualified Theta.Types                   as Theta
import qualified Theta.Versions                as Versions

import           Theta.Target.Haskell          (loadModule)
import qualified Theta.Target.Haskell.HasTheta as HasTheta

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import qualified Paths_theta                   as Paths

loadModule "test/data/modules" "foo"

tests :: TestTree
tests = testGroup "Imports"
  [ test_toFromPath
  , test_importModule
  , test_findInPath
  , test_getModuleDefinition
  , test_getDefinition
  ]

test_toFromPath :: TestTree
test_toFromPath = testGroup "toPath, fromPath"
  [ testCase "toPath" $ do
      toPath             "foo" @?=             "foo.theta"
      toPath "com.example.foo" @?= "com/example/foo.theta"

  , testCase "fromPath" $ do
      fromPath             "foo.theta" @?=             "foo"
      fromPath "com/example/foo.theta" @?= "com.example.foo"

  , testProperty "ends in .theta" $ forAll components $ \ (baseName : namespace) ->
      takeExtension (toPath (Name.ModuleName { Name.namespace, Name.baseName })) == ".theta"

  , testProperty "toPath ↔ fromPath" $ forAll components $ \ (baseName : namespace) ->
      and ([ let thetaName = Name.ModuleName { Name.namespace, Name.baseName } in
               fromPath (toPath thetaName) == thetaName
           , let path = joinPath (Text.unpack <$> (baseName : namespace)) <.> ".theta" in
               toPath (fromPath path) == path
           ] :: [Bool])
  ]
  where components =
          listOf1 $ Text.pack <$> listOf1 (elements $ ['a'..'z'] <> ['A'..'Z'])

test_importModule :: TestTree
test_importModule = testGroup "importModule"
  [ testCase "simple import" $ do
      let imported  =
            Module "imported"
              [def "test.Foo" Nothing Theta.int'] [] testMetadata
          importing =
            Module "importing"
              [def "test.Bar" Nothing foo] [] testMetadata
          combined  = importModule imported importing

          foo = Theta.withModule combined $
            Theta.BaseType' $ Theta.Reference' "test.Foo"

      (moduleName <$> imports combined) @?= [moduleName imported]
        -- TODO: add a more detailed comparison between modules when
        -- the PR that defines Eq instance for types gets merged

      assertBool "‘test.Foo’ *not* in module before importing." $
        not $ Theta.defines "test.Foo" importing
      assertBool "‘test.Foo’ *is* in module after importing." $
        Theta.defines "test.Foo" combined

  , -- with the current namespace design modules can't actually shadow
    -- names any more—the module's name is the namespace for each type
    -- defined in the module—but I'm keeping this test around in case
    -- we relax that restriction in the future
    testCase "shadowing import" $ do
      let shadowed  = Module "shadowed"
            [def "test.Foo" Nothing Theta.int'] [] testMetadata
          shadowing = Module "shadowing"
            [def "test.Foo" Nothing Theta.long'] [] testMetadata
          importing = Module "importing" [] [] testMetadata

          combined1 = shadowing `importModule` (shadowed `importModule` importing)
          combined2 = (shadowed `importModule` shadowing) `importModule` importing

      assertBool "‘test.Foo’ *not* in module before importing." $
        not $ Theta.defines "test.Foo" importing
      assertBool "‘test.Foo’ *in* in module before importing." $
        and ([ Theta.defines "test.Foo" combined1
             , Theta.defines "test.Foo" combined2
             ] :: [Bool])

      case Theta.lookupName "test.Foo" combined1 of
        Left err                              -> assertFailure err
        Right (Theta.Type { Theta.baseType }) -> case baseType of
          Theta.Long' -> pure ()
          Theta.Int'  ->
            assertFailure "Wrong type shadowed: should be ‘Long’, not ‘Int’."
          invalid     ->
            assertFailure $ "Unexpected type in module—expected ‘Long’ but got ‘"
                         <> show invalid <> "’"

      case Theta.lookupName "test.Foo" combined2 of
        Left err                              -> assertFailure err
        Right (Theta.Type { Theta.baseType }) -> case baseType of
          Theta.Long' -> pure ()
          Theta.Int'  ->
            assertFailure "Wrong type shadowed: should be ‘Long’, not ‘Int’."
          invalid     ->
            assertFailure $ "Unexpected type in module—expected ‘Long’ but got ‘"
                         <> show invalid <> "’"
  ]
  where testMetadata =
          Metadata.Metadata
            { Metadata.languageVersion = "1.0.0"
            , Metadata.avroVersion     = "1.0.0"
            , Metadata.moduleName      = "test"
            }

        def definitionName definitionDoc definitionType =
          (definitionName, Theta.Definition { Theta.definitionName
                                            , Theta.definitionDoc
                                            , Theta.definitionType })

test_findInPath :: TestTree
test_findInPath = testGroup "findInPath"
  [ testCase "relative paths" $ do
      dir <- Paths.getDataDir
      withCurrentDirectory dir $ do
        let loadPath = LoadPath [root1, root2]

        let pathA = dir </> "test" </> "data" </> "root-1" </> "a.theta"
        fileA <- Text.readFile pathA
        Just (fileA', pathA') <- findInPath loadPath "a.theta"
        assertEqualPath pathA' pathA
        fileA' @?= fileA

        let pathB = dir </> "test" </> "data" </> "root-2" </> "b.theta"
        fileB <- Text.readFile pathB
        Just (fileB', pathB') <- findInPath loadPath "b.theta"
        assertEqualPath pathB' pathB
        fileB' @?= fileB

  , testCase "absolute paths" $ do
      dir <- Paths.getDataDir
      let loadPath = LoadPath [dir </> root1, dir </> root2]

      let pathA = dir </> "test" </> "data" </> "root-1" </> "a.theta"
      fileA <- Text.readFile pathA
      Just (fileA', pathA') <- findInPath loadPath "a.theta"
      assertEqualPath pathA' pathA
      fileA' @?= fileA

      let pathB = dir </> "test" </> "data" </> "root-2" </> "b.theta"
      fileB <- Text.readFile pathB
      Just (fileB', pathB') <- findInPath loadPath "b.theta"
      assertEqualPath pathB' pathB
      fileB' @?= fileB

  -- What if we have a .. or . in the middle of a load path?
  --
  -- This has caused problems in the past, for uncertain reasons.
  , testGroup ".. and ."
    [ testCase ".. in start" $ do
        dir <- Paths.getDataDir
        withCurrentDirectory (dir </> "test") $ do
          let loadPath = LoadPath [".." </> root1]

          let pathA = dir </> "test" </> "data" </> "root-1" </> "a.theta"
          fileA <- Text.readFile pathA
          Just (fileA', pathA') <- findInPath loadPath "a.theta"
          assertEqualPath pathA' pathA
          fileA' @?= fileA

    , testCase ".. in middle" $ do
        dir <- Paths.getDataDir
        let loadPath = LoadPath [dir </> "test" </> ".." </> root1]

        let pathA = dir </> "test" </> "data" </> "root-1" </> "a.theta"
        fileA <- Text.readFile pathA
        Just (fileA', pathA') <- findInPath loadPath "a.theta"
        assertEqualPath pathA' pathA
        fileA' @?= fileA

    , testCase ". in start" $ do
        dir <- Paths.getDataDir
        let loadPath = LoadPath ["." </> dir </> root1]

        let pathA = dir </> "test" </> "data" </> "root-1" </> "a.theta"
        fileA <- Text.readFile pathA
        Just (fileA', pathA') <- findInPath loadPath "a.theta"
        assertEqualPath pathA' pathA
        fileA' @?= fileA

    , testCase ". in middle" $ do
        dir <- Paths.getDataDir
        let loadPath = LoadPath [dir </> "." </> root1]

        let pathA = dir </> "test" </> "data" </> "root-1" </> "a.theta"
        fileA <- Text.readFile pathA
        Just (fileA', pathA') <- findInPath loadPath "a.theta"
        assertEqualPath pathA' pathA
        fileA' @?= fileA
    ]

  , testCase "missing files" $ do
      dir <- Paths.getDataDir
      let loadPath = LoadPath [dir </> root1, dir </> root2]

      a <- findInPath loadPath ("blarg" </> "foo.theta")
      a @?= Nothing

      b <- findInPath "" "a.theta"
      b @?= Nothing

      c <- findInPath (LoadPath [dir </> root1]) "b.theta"
      c @?= Nothing
  ]
  where root1 = "test" </> "data" </> "root-1"
        root2 = "test" </> "data" </> "root-2"

        assertEqualPath got expected = do
          got'      <- canonicalizePath got
          expected' <- canonicalizePath expected
          assertBool (printf "expected: %s\n but got: %s" expected' got') $
            equalFilePath got' expected'

test_getModuleDefinition :: TestTree
test_getModuleDefinition = testGroup "getModuleDefinition"
  [ testCase "invalid version" $ do
      dir <- Paths.getDataDir
      let loadPath = LoadPath [dir </> "test" </> "data" </> "modules"]

      avro <- runExceptT $ getModuleDefinition loadPath "unsupported_avro_version"
      unsupported ("unsupported_avro_version", Versions.avro, "999.0.0") avro

      theta <- runExceptT $ getModuleDefinition loadPath "unsupported_theta_version"
      unsupported ("unsupported_theta_version", Versions.theta, "999.0.0") theta

      -- should *not* raise an error if the versions are valid
      valid <- runExceptT $ getModuleDefinition "test/data/modules" "foo"
      assertValid valid

  , testCase "documentation" $ do
      valid <- runExceptT $ getModuleDefinition "test/data/modules" "documentation"
      assertValid valid
  ]
  where
    unsupported (name, range, version) = \case
      Left (UnsupportedVersion name' range' version') -> do
        name'    @?= name
        range'   @?= range
        version' @?= version
      Left err ->
        assertFailure $ "Raised wrong kind of error. Expected UnsupportedVersion, got:\n" <> show err
      Right _  ->
        assertFailure "Did not raise an error as expected."

    assertValid Right{}    = pure ()
    assertValid (Left err) = assertFailure $ [__i|
      Expected module to load without errors, but got:

      #{pretty err}
    |]

test_getDefinition :: TestTree
test_getDefinition = testGroup "getDefinition"
  [ testCase "present" $ do
      dir <- Paths.getDataDir
      let loadPath = LoadPath [dir </> "test" </> "data" </> "modules" ]
          expected = HasTheta.theta @Bar

      definition <- runExceptT $ getDefinition loadPath "foo.Bar"
      case Theta.definitionType <$> definition of
        Right t  -> t @?= expected
        Left err -> assertFailure [__i|
          Except type #{pretty expected} but got error:
          #{pretty err}
        |]

  , testCase "missing" $ do
      dir <- Paths.getDataDir
      let loadPath = LoadPath [dir </> "test" </> "data" </> "modules"]

      failed <- runExceptT $ getDefinition loadPath "foo.NotInScope"
      case Theta.definitionType <$> failed of
        Left (MissingName name) -> name @?= "foo.NotInScope"
        Left err -> assertFailure [__i|
          Wrong kind of error. Expected missing name, got:
          #{pretty err}
        |]
        Right type_ -> assertFailure [__i|
          Expected missing name error but got:
          #{pretty type_}
        |]

      noModule <- runExceptT $ getDefinition loadPath "notInScope.NotInScope"
      case Theta.definitionType <$> noModule of
        Left (MissingModule _ name) -> name @?= "notInScope"
        Left err -> assertFailure [__i|
          Wrong kind of error. Expected missing name, got:
          #{pretty err}
        |]
        Right type_ -> assertFailure [__i|
          Expected missing name error but got:
          #{pretty type_}
        |]
  ]
