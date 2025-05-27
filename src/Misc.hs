{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module provides commonly used helper functions for other modules
module Misc (withoutAt, isMetaBinding, ensuredFile, allPathsIn) where

import Ast
import Control.Exception
import Control.Monad
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Text.Printf (printf)

data FsException
  = FileDoesNotExist {file :: FilePath}
  | DirectoryDoesNotExist {dir :: FilePath}
  deriving (Exception)

instance Show FsException where
  show FileDoesNotExist {..} = printf "File '%s' does not exist" file
  show DirectoryDoesNotExist {..} = printf "Directory '%s' does not exist" dir

-- List without element by given index
withoutAt :: Int -> [a] -> [a]
withoutAt i xs = take i xs ++ drop (i + 1) xs

-- Returns True if given binding is BiMeta (!B)
isMetaBinding :: Binding -> Bool
isMetaBinding (BiMeta _) = True
isMetaBinding _ = False

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
