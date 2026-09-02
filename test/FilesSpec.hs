-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module FilesSpec where

import Control.Exception (bracket, try)
import Control.Monad (forM_, void)
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

exceptionPath :: FsException -> FilePath
exceptionPath (FileDoesNotExist file) = file
exceptionPath (DirectoryDoesNotExist directory) = directory

withScratchDir :: (FilePath -> IO a) -> IO a
withScratchDir =
  bracket
    ( do
        tmp <- getTemporaryDirectory
        stamp <- getPOSIXTime
        let dir = tmp </> ("phino-files-spec-" ++ show (floor (stamp * 1000000) :: Integer))
        createDirectoryIfMissing True dir
        pure dir
    )
    removeDirectoryRecursive

spec :: Spec
spec = do
  describe "ensuredFile" $
    it "returns the path of an existing file" $
      withScratchDir $ \dir -> do
        let path = dir </> "existing.txt"
        writeFile path "content"
        ensuredFile path >>= (`shouldBe` path)

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

  describe "FsException" $ do
    forM_
      [ ("throws FileDoesNotExist for a missing file", "missing.txt", void . ensuredFile)
      , ("throws DirectoryDoesNotExist for a missing directory", "does-not-exist", void . allPathsIn)
      ]
      ( \(desc, name, action) -> it desc $ withScratchDir $ \dir -> do
          let path = dir </> name
          result <- try (action path) :: IO (Either FsException ())
          case result of
            Left exc -> exceptionPath exc `shouldBe` path
            _ -> fail "expected an FsException to be thrown"
      )

    forM_
      [
        ( "shows a readable message for FileDoesNotExist"
        , FileDoesNotExist "/no/such/file"
        , "File '/no/such/file' does not exist"
        )
      ,
        ( "shows a readable message for DirectoryDoesNotExist"
        , DirectoryDoesNotExist "/no/such/dir"
        , "Directory '/no/such/dir' does not exist"
        )
      ]
      (\(desc, exc, message) -> it desc (show exc `shouldBe` message))

    it "FsException values can be inspected without throwing" $
      show (FileDoesNotExist "x") `shouldSatisfy` (not . null)
