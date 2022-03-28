{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Theta.LoadPath where

import qualified Data.Text.IO     as Text

import           System.Directory (canonicalizePath, withCurrentDirectory)
import           System.FilePath  (equalFilePath, (</>))

import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (assertBool, testCase, (@?=))

import           Text.Printf      (printf)

import           Theta.LoadPath

import qualified Paths_theta      as Paths

tests :: TestTree
tests = testGroup "LoadPath"
  [ testGroup "findInPath"
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

        b <- findInPath "nope" "a.theta"
        b @?= Nothing

        c <- findInPath (LoadPath [dir </> root1]) "b.theta"
        c @?= Nothing
    ]

  , testCase "moduleNames" $ do
      dir <- Paths.getDataDir
      let loadPath = LoadPath [dir </> root1, dir </> root2, dir </> root3]
      names <- moduleNames loadPath
      names @?= [ ("a", dir </> root1 </> "a.theta")
                , ("b", dir </> root2 </> "b.theta")
                , ("com.example", dir </> root3 </> "com" </> "example.theta")
                , ("com.example.nested", dir </> root3 </> "com" </> "example" </> "nested.theta")
                , ("com.example.nested_2", dir </> root3 </> "com" </> "example" </> "nested_2.theta")
                , ("com.example.nested.deeply", dir </> root3 </> "com" </> "example" </> "nested" </> "deeply.theta")
                ]
  ]
  where root1 = "test" </> "data" </> "root-1"
        root2 = "test" </> "data" </> "root-2"
        root3 = "test" </> "data" </> "root-3"

        assertEqualPath got expected = do
          got'      <- canonicalizePath got
          expected' <- canonicalizePath expected
          assertBool (printf "expected: %s\n but got: %s" expected' got') $
            equalFilePath got' expected'
