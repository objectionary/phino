{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Files (FsException (..), ensuredFile, allPathsIn, overwrite) where

import Control.Exception (Exception, IOException, catch, onException, throwIO)
import Control.Monad (forM, when)
import System.Directory (canonicalizePath, copyPermissions, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, pathIsSymbolicLink, removeFile, renameFile)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (Handle, hClose, hPutStr, hSetEncoding, openTempFileWithDefaultPermissions, utf8)
import Text.Printf (printf)

data FsException
  = FileDoesNotExist {_file :: FilePath}
  | DirectoryDoesNotExist {_dir :: FilePath}
  deriving (Exception)

instance Show FsException where
  show FileDoesNotExist{..} = printf "File '%s' does not exist" _file
  show DirectoryDoesNotExist{..} = printf "Directory '%s' does not exist" _dir

ensuredFile :: FilePath -> IO FilePath
ensuredFile pth = do
  exists <- doesFileExist pth
  if exists then pure pth else throwIO (FileDoesNotExist pth)

overwrite :: FilePath -> String -> IO ()
overwrite path content = do
  link <- pathIsSymbolicLink path `catch` \(_ :: IOException) -> pure False
  file <- if link then canonicalizePath path else pure path
  createDirectoryIfMissing True (takeDirectory file)
  (temp, handle) <- openTempFileWithDefaultPermissions (takeDirectory file) (takeFileName file)
  replace file temp handle `onException` (hClose handle >> removeFile temp)
  where
    replace :: FilePath -> FilePath -> Handle -> IO ()
    replace file temp handle = do
      hSetEncoding handle utf8
      hPutStr handle content
      hClose handle
      exists <- doesFileExist file
      when exists (copyPermissions file temp)
      renameFile temp file

allPathsIn :: FilePath -> IO [FilePath]
allPathsIn dir = do
  exists <- doesDirectoryExist dir
  names <- if exists then listDirectory dir else throwIO (DirectoryDoesNotExist dir)
  let nested = map (dir </>) names
  paths <-
    forM
      nested
      ( \path -> do
          isLink <- pathIsSymbolicLink path
          isDir <- doesDirectoryExist path
          if isLink
            then return []
            else
              if isDir
                then allPathsIn path
                else return [path]
      )
  return (concat paths)
