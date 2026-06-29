{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module provides commonly used helper functions for other modules
module Misc
  ( withVoidRho
  , recoverFormations
  , allPathsIn
  , ensuredFile
  , shuffle
  , toDouble
  , fqnToAttrs
  , attributesFromBindings
  , attributesFromBindings'
  , attributeFromBinding
  , uniqueBindings
  , uniqueBindings'
  , orThrow
  , validateYamlObject
  , matchDataObject
  , pattern DataObject
  , pattern DataString
  , pattern DataNumber
  , pattern BaseObject
  )
where

import AST
import Control.Exception
import Control.Monad
import Data.Aeson (Object)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

-- import Printer (printExpression)

import Data.Functor ((<&>))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.Random.Stateful
import Text.Printf (printf)

-- Unwrap a pure 'Either String' in IO, throwing the built exception on 'Left'
orThrow :: (Exception e) => (String -> e) -> Either String a -> IO a
orThrow _ (Right value) = pure value
orThrow asException (Left err) = throwIO (asException err)

data FsException
  = FileDoesNotExist {_file :: FilePath}
  | DirectoryDoesNotExist {_dir :: FilePath}
  deriving (Exception)

instance Show FsException where
  show FileDoesNotExist{..} = printf "File '%s' does not exist" _file
  show DirectoryDoesNotExist{..} = printf "Directory '%s' does not exist" _dir

matchBaseObject :: Expression -> Maybe T.Text
matchBaseObject (ExDispatch ExRoot (AtLabel label)) = Just label
matchBaseObject _ = Nothing

pattern BaseObject :: T.Text -> Expression
pattern BaseObject label <- (matchBaseObject -> Just label)
  where
    BaseObject label = ExDispatch ExRoot (AtLabel label)

-- Minimal matcher function (required for view pattern)
matchDataObject :: Expression -> Maybe (T.Text, Bytes)
matchDataObject (ExApplication outer (ArAlpha (Alpha 0) inner)) = case (matchOuter outer, matchInner inner) of
  (Just label, Just bts) -> Just (label, bts)
  _ -> Nothing
  where
    matchOuter :: Expression -> Maybe T.Text
    matchOuter (BaseObject label) = Just label
    matchOuter (ExPhiAgain _ _ (BaseObject label)) = Just label
    matchOuter _ = Nothing
    matchInner :: Expression -> Maybe Bytes
    matchInner (ExPhiAgain _ _ inner') = matchInner inner'
    matchInner inner' = matchInner' inner'
    matchInner' :: Expression -> Maybe Bytes
    matchInner' (ExApplication bytes (ArAlpha (Alpha 0) formation)) = case (matchesBytes bytes, matchFormation formation) of
      (True, Just bts) -> Just bts
      _ -> Nothing
    matchInner' _ = Nothing
    matchesBytes :: Expression -> Bool
    matchesBytes (BaseObject "bytes") = True
    matchesBytes (ExPhiAgain _ _ (BaseObject "bytes")) = True
    matchesBytes _ = False
    matchFormation :: Expression -> Maybe Bytes
    matchFormation (ExFormation [BiDelta bts, BiVoid AtRho]) = Just bts
    matchFormation (ExPhiAgain _ _ (ExFormation [BiDelta bts, BiVoid AtRho])) = Just bts
    matchFormation _ = Nothing
matchDataObject _ = Nothing

pattern DataString :: Bytes -> Expression
pattern DataString bts = DataObject "string" bts

pattern DataNumber :: Bytes -> Expression
pattern DataNumber bts = DataObject "number" bts

pattern DataObject :: T.Text -> Bytes -> Expression
pattern DataObject label bts <- (matchDataObject -> Just (label, bts))
  where
    DataObject label bts =
      ExApplication
        (BaseObject label)
        ( ArAlpha
            (Alpha 0)
            ( ExApplication
                (BaseObject "bytes")
                ( ArAlpha
                    (Alpha 0)
                    (ExFormation [BiDelta bts, BiVoid AtRho])
                )
            )
        )

-- Extract attribute from binding
attributeFromBinding :: Binding -> Maybe Attribute
attributeFromBinding (BiTau attr _) = Just attr
attributeFromBinding (BiVoid attr) = Just attr
attributeFromBinding (BiDelta _) = Just AtDelta
attributeFromBinding (BiLambda _) = Just AtLambda
attributeFromBinding (BiMeta _) = Nothing

-- Extract attributes from bindings
attributesFromBindings :: [Binding] -> [Attribute]
attributesFromBindings [] = []
attributesFromBindings bds = catMaybes (attributesFromBindings' bds)

attributesFromBindings' :: [Binding] -> [Maybe Attribute]
attributesFromBindings' = map attributeFromBinding

uniqueBindings' :: [Binding] -> IO [Binding]
uniqueBindings' bds = case uniqueBindings bds of
  Left msg -> throwIO (userError msg)
  Right _ -> pure bds

-- Check if given binding list consists of unique attributes
uniqueBindings :: [Binding] -> Either String [Binding]
uniqueBindings bds = case duplicated bds Set.empty of
  Just attr ->
    Left
      ( printf
          "Duplicated attribute '%s' found in %s"
          (show attr)
          (intercalate ", " (map show (attributesFromBindings bds)))
      )
  _ -> Right bds
  where
    duplicated :: [Binding] -> Set.Set Attribute -> Maybe Attribute
    duplicated [] _ = Nothing
    duplicated (bd : rest) seen = case attributeFromBinding bd of
      Just attr
        | attr `Set.member` seen -> Just attr
        | otherwise -> duplicated rest (Set.insert attr seen)
      Nothing -> duplicated rest seen

-- Add void rho binding to the end of the list of any rho binding is not present
withVoidRho :: [Binding] -> [Binding]
withVoidRho bds = withVoidRho' bds False
  where
    withVoidRho' :: [Binding] -> Bool -> [Binding]
    withVoidRho' [] hasRho = [BiVoid AtRho | not hasRho]
    withVoidRho' (bd : rest) hasRho =
      case bd of
        BiMeta _ -> bd : rest
        BiVoid (AtMeta _) -> bd : rest
        BiTau (AtMeta _) _ -> bd : rest
        BiVoid AtRho -> bd : withVoidRho' rest True
        BiTau AtRho _ -> bd : withVoidRho' rest True
        _ -> bd : withVoidRho' rest hasRho

-- Recursively ensure all formations have a BiVoid AtRho binding (ρ ↦ ∅).
-- Fixes in-memory ExFormation [] to ExFormation [BiVoid AtRho] after rewriting,
-- keeping the invariant that the parser enforces via withVoidRho.
recoverFormations :: Expression -> Expression
recoverFormations (ExFormation bindings) = ExFormation (withVoidRho (map recoverFormations' bindings))
recoverFormations (ExDispatch expr attr) = ExDispatch (recoverFormations expr) attr
recoverFormations (ExApplication expr arg) = ExApplication (recoverFormations expr) (recoverArgument arg)
recoverFormations expr = expr

recoverFormations' :: Binding -> Binding
recoverFormations' (BiTau attr expr) = BiTau attr (recoverFormations expr)
recoverFormations' binding = binding

recoverArgument :: Argument -> Argument
recoverArgument (ArTau attr expr) = ArTau attr (recoverFormations expr)
recoverArgument (ArAlpha alpha expr) = ArAlpha alpha (recoverFormations expr)

-- Transform dispatch to list of attributes
-- >>> fqnToAttrs (ExDispatch (ExDispatch (ExDispatch ExRoot (AtLabel "org")) (AtLabel "eolang")) (AtLabel "number"))
-- Just [org,eolang,number]
-- >>> fqnToAttrs (ExFormation [])
-- Nothing
-- >>> fqnToAttrs ExRoot
-- Just []
fqnToAttrs :: Expression -> Maybe [Attribute]
fqnToAttrs expr = fqnToAttrs' expr <&> reverse
  where
    fqnToAttrs' :: Expression -> Maybe [Attribute]
    fqnToAttrs' ExRoot = Just []
    fqnToAttrs' (ExDispatch ex at) = fqnToAttrs' ex <&> (:) at
    fqnToAttrs' _ = Nothing

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

-- >>> toDouble 5
-- 5.0
toDouble :: Int -> Double
toDouble = fromIntegral

-- Fast Fisher-Yates with mutable vectors.
-- The function is generated by ChatGPT and claimed as
-- fastest approach comparing to usage IOArray.
-- >>> shuffle [1..20]
-- [7,15,5,18,13,19,3,11,20,2,1,8,14,16,17,12,9,10,6,4]
shuffle :: [a] -> IO [a]
shuffle xs = do
  gen <- newIOGenM =<< newStdGen
  let n = length xs
  v <- V.thaw (V.fromList xs) -- Mutable copy
  forM_ [n - 1, n - 2 .. 1] $ \i -> do
    j <- uniformRM (0, i) gen
    M.swap v i j
  V.toList <$> V.freeze v

validateYamlObject :: (MonadFail a) => Object -> [String] -> a ()
validateYamlObject v keys = do
  let present = filter (`KeyMap.member` v) (map Key.fromString keys)
      current = KeyMap.keys v
  when
    (length current > 1)
    (fail ("Exactly one condition type is expected, when multiple condition types specified: " ++ show current))
  when
    (null present)
    (fail (printf "Unknown condition type '%s', expected one of: %s" (show current) (show keys)))
