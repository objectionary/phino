{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module accesses the filesystem: it ensures a file exists and
-- collects every file path under a directory.
module Files (FsException (..), ensuredFile, allPathsIn) where

import Control.Exception (Exception, throwIO)
import Control.Monad (forM)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
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
          isDir <- doesDirectoryExist path
          if isDir
            then allPathsIn path
            else return [path]
      )
  return (concat paths)
