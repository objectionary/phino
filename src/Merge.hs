{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Merge (merge) where

import AST
import Control.Exception.Base (Exception, throwIO)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Encoding (Encoding (UNICODE))
import Lining (LineFormat (SINGLELINE))
import Margin (defaultMargin)
import Misc
import Printer (PrintConfig, printAttribute, printExpression, printExpression')
import Sugar (SugarType (SALTY))
import Text.Printf (printf)

data MergeException
  = WrongExpressionFormat Expression
  | CanNotMergeBinding [Attribute] (FilePath, Binding) (FilePath, Binding)
  | EmptyExpressionList
  deriving (Exception)

instance Show MergeException where
  show (WrongExpressionFormat expr) =
    printf
      "Invalid expression format, only expressions with top level formations are supported for 'merge' command, given:\n%s"
      (printExpression expr)
  show (CanNotMergeBinding path (firstFile, first) (secondFile, second)) =
    printf
      "Can't merge two bindings, conflict found:\nat %s, '%s' has %s and '%s' has %s"
      (locator path)
      firstFile
      (render first)
      secondFile
      (render second)
  show EmptyExpressionList = "Nothing to merge: provide at least one expression"

render :: Binding -> String
render binding = printExpression' (ExFormation [binding]) config
  where
    config :: PrintConfig
    config = (SALTY, UNICODE, SINGLELINE, defaultMargin)

locator :: [Attribute] -> String
locator [] = "Φ"
locator path = printf "Φ.%s" (intercalate "." (map printAttribute path))

data Located = Located FilePath Binding [Located]

place :: FilePath -> Binding -> Located
place file binding = case binding of
  BiTau _ (ExFormation bindings) -> Located file binding (map (place file) bindings)
  _ -> Located file binding []

bindingOf :: Located -> Binding
bindingOf (Located _ (BiTau attr (ExFormation _)) children) = BiTau attr (ExFormation (map bindingOf children))
bindingOf (Located _ binding _) = binding

mergeBinding :: [Attribute] -> Located -> Located -> IO Located
mergeBinding path first@(Located firstFile one xs) second@(Located secondFile two ys)
  | bindingOf first == bindingOf second = pure first
  | otherwise = case (one, two) of
      (BiTau a (ExFormation _), BiTau b (ExFormation _))
        | a == b ->
            mergeBindings
              (path <> [a])
              xs
              ys
              <&> \united -> Located firstFile (BiTau a (ExFormation (map bindingOf united))) united
      _ -> throwIO (CanNotMergeBinding path (firstFile, one) (secondFile, two))

mergeBindings :: [Attribute] -> [Located] -> [Located] -> IO [Located]
mergeBindings path xs ys = unmarked <$> anchored xs ys
  where
    anchored :: [Located] -> [Located] -> IO [Located]
    anchored [] rest = pure rest
    anchored (x : more) rest = case break (same x) rest of
      (before, y : after) -> do
        let later = attributesFromBindings' (map bindingOf more)
            (pending, ahead) =
              ( filter ((`elem` later) . attributeFromBinding . bindingOf) before
              , filter ((`notElem` later) . attributeFromBinding . bindingOf) before
              )
        united <- mergeBinding path x y
        (ahead <>) . (united :) <$> anchored more (pending <> after)
      (_, []) -> (x :) <$> anchored more rest
    same :: Located -> Located -> Bool
    same x y = attributeFromBinding (bindingOf x) == attributeFromBinding (bindingOf y)
    unmarked :: [Located] -> [Located]
    unmarked bindings
      | any (marker . bindingOf) xs == any (marker . bindingOf) ys = bindings
      | otherwise = filter (not . marker . bindingOf) bindings
    marker :: Binding -> Bool
    marker (BiLambda (Function "Package")) = True
    marker _ = False

merge' :: [(FilePath, Expression)] -> IO [Located]
merge' [] = throwIO EmptyExpressionList
merge' [(file, ExFormation bindings)] = pure (map (place file) bindings)
merge' ((file, ExFormation bindings) : rest) = do
  united <- merge' rest
  mergeBindings [] united (map (place file) bindings)
merge' ((_, expr) : _) = throwIO (WrongExpressionFormat expr)

merge :: [(FilePath, Expression)] -> IO Expression
merge exprs = merge' (reverse exprs) <&> ExFormation . map bindingOf
