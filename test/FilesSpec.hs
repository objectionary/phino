-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module FilesSpec where

import Control.Exception (ErrorCall, bracket, try)
import Control.Monad (forM_, void)
import Data.ByteString qualified as BS
import Data.List (sort)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Files (FsException (..), allPathsIn, ensuredFile, overwrite)
import System.Directory
  ( createDirectoryIfMissing
  , createSymbolicLink
  , executable
  , getPermissions
  , getTemporaryDirectory
  , listDirectory
  , removeDirectoryRecursive
  , setOwnerExecutable
  , setPermissions
  )
import System.FilePath ((</>))
import System.Info (os)
import Test.Hspec (Spec, describe, it, pendingWith, shouldBe, shouldReturn, shouldSatisfy)

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

  describe "overwrite" $ do
    it "replaces the content of an existing file with utf-8 bytes" $ withScratchDir $ \dir -> do
      let path = dir </> "φ-replaced.phi"
      BS.writeFile path (TE.encodeUtf8 (T.pack "{⟦ x ↦ ξ.y ⟧}"))
      overwrite path "{⟦ ψ ↦ Φ.org.eolang ⟧}"
      TE.decodeUtf8 <$> BS.readFile path `shouldReturn` T.pack "{⟦ ψ ↦ Φ.org.eolang ⟧}"
    it "keeps the previous content when the new one fails half-way" $ withScratchDir $ \dir -> do
      let path = dir </> "kept.phi"
      BS.writeFile path (TE.encodeUtf8 (T.pack "{⟦ original ↦ ∅ ⟧}"))
      void (try (overwrite path ("{⟦ partial ↦ " ++ error "broken content")) :: IO (Either ErrorCall ()))
      TE.decodeUtf8 <$> BS.readFile path `shouldReturn` T.pack "{⟦ original ↦ ∅ ⟧}"
    it "leaves no temporary file when the new content fails half-way" $ withScratchDir $ \dir -> do
      BS.writeFile (dir </> "lonely.phi") BS.empty
      void (try (overwrite (dir </> "lonely.phi") (replicate 100000 'ω' ++ error "broken tail")) :: IO (Either ErrorCall ()))
      listDirectory dir `shouldReturn` ["lonely.phi"]
    it "keeps the executable permission of the replaced file" $ withScratchDir $ \dir -> do
      let path = dir </> "script.sh"
      BS.writeFile path BS.empty
      getPermissions path >>= setPermissions path . setOwnerExecutable True
      overwrite path "#!/bin/sh\necho ∀"
      if os == "mingw32"
        then pendingWith "Windows derives the executable permission from the file extension"
        else executable <$> getPermissions path `shouldReturn` True

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

    it "does not follow symbolic links to directories" $ withScratchDir $ \dir -> do
      let nested = dir </> "nested"
          link = nested </> "back"
      createDirectoryIfMissing True nested
      writeFile (dir </> "top.txt") "top"
      if os == "mingw32"
        then pendingWith "Windows does not create directory symbolic links without elevated privileges"
        else do
          createSymbolicLink ".." link
          paths <- allPathsIn dir
          sort paths `shouldBe` [dir </> "top.txt"]

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
