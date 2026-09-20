{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module accesses the filesystem: it ensures a file exists,
-- collects every file path under a directory and replaces a file atomically.
module Files (FsException (..), ensuredFile, allPathsIn, overwrite) where

import Control.Exception (Exception, onException, throwIO)
import Control.Monad (forM, when)
import System.Directory (copyPermissions, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, pathIsSymbolicLink, removeFile, renameFile)
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
overwrite file content = do
  createDirectoryIfMissing True (takeDirectory file)
  (temp, handle) <- openTempFileWithDefaultPermissions (takeDirectory file) (takeFileName file)
  replace temp handle `onException` (hClose handle >> removeFile temp)
  where
    replace :: FilePath -> Handle -> IO ()
    replace temp handle = do
      hSetEncoding handle utf8
      hPutStr handle content
      hClose handle
      exists <- doesFileExist file
      when exists (copyPermissions file temp)
      renameFile temp file

-- Recursively collect all file paths in provided directory
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
