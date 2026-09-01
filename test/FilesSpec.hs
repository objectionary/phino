-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module FilesSpec where

import Control.Exception (bracket, try)
import Data.List (sort)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Files (FsException (..), allPathsIn, ensuredFile)
import System.Directory
  ( createDirectoryIfMissing
  , getTemporaryDirectory
  , removeDirectoryRecursive
  )
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

withScratchDir :: (FilePath -> IO a) -> IO a
withScratchDir action =
  bracket
    ( do
        tmp <- getTemporaryDirectory
        stamp <- getPOSIXTime
        let dir = tmp </> ("phino-files-spec-" ++ show (floor (stamp * 1000000) :: Integer))
        createDirectoryIfMissing True dir
        pure dir
    )
    removeDirectoryRecursive
    action

spec :: Spec
spec = do
  describe "ensuredFile" $ do
    it "returns the path of an existing file" $ withScratchDir $ \dir -> do
      let path = dir </> "existing.txt"
      writeFile path "content"
      ensuredFile path >>= (`shouldBe` path)

    it "throws FileDoesNotExist for a missing file" $ withScratchDir $ \dir -> do
      let path = dir </> "missing.txt"
      result <- try (ensuredFile path) :: IO (Either FsException FilePath)
      case result of
        Left (FileDoesNotExist file) -> file `shouldBe` path
        _ -> fail "expected FileDoesNotExist to be thrown"

    it "shows a readable message for FileDoesNotExist" $
      show (FileDoesNotExist "/no/such/file") `shouldBe` "File '/no/such/file' does not exist"

  describe "allPathsIn" $ do
    it "collects every leaf file path recursively" $ withScratchDir $ \dir -> do
      let nested = dir </> "a" </> "b"
      createDirectoryIfMissing True nested
      writeFile (dir </> "top.txt") "top"
      writeFile (dir </> "a" </> "mid.txt") "mid"
      writeFile (nested </> "leaf.txt") "leaf"
      paths <- allPathsIn dir
      sort paths
        `shouldBe` sort
          [ dir </> "top.txt"
          , dir </> "a" </> "mid.txt"
          , nested </> "leaf.txt"
          ]

    it "returns an empty list for an empty directory" $ withScratchDir $ \dir -> do
      paths <- allPathsIn dir
      paths `shouldBe` []

    it "throws DirectoryDoesNotExist for a missing directory" $ withScratchDir $ \dir -> do
      let missing = dir </> "does-not-exist"
      result <- try (allPathsIn missing) :: IO (Either FsException [FilePath])
      case result of
        Left (DirectoryDoesNotExist directory) -> directory `shouldBe` missing
        _ -> fail "expected DirectoryDoesNotExist to be thrown"

    it "shows a readable message for DirectoryDoesNotExist" $
      show (DirectoryDoesNotExist "/no/such/dir") `shouldBe` "Directory '/no/such/dir' does not exist"

  describe "these exceptions are Show instances" $
    it "FsException values can be inspected without throwing" $
      show (FileDoesNotExist "x") `shouldSatisfy` (not . null)
