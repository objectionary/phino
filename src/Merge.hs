{-# LANGUAGE DeriveAnyClass #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Merge (merge) where

import AST
import Control.Exception.Base (Exception, throwIO)
import Data.Functor ((<&>))
import Misc
import Printer (printExpression)
import Text.Printf (printf)

data MergeException
  = WrongExpressionFormat Expression
  | CanNotMergeBinding Binding Binding
  | EmptyExpressionList
  deriving (Exception)

instance Show MergeException where
  show (WrongExpressionFormat expr) =
    printf
      "Invalid expression format, only expressions with top level formations are supported for 'merge' command, given:\n%s"
      (printExpression expr)
  show (CanNotMergeBinding first second) =
    printf
      "Can't merge two bindings, conflict found:\n%s"
      (printExpression (ExFormation [first, second]))
  show EmptyExpressionList = "Nothing to merge: provide at least one expression"

mergeBinding :: Binding -> Binding -> IO Binding
mergeBinding first@(BiTau a (ExFormation xs)) second@(BiTau b (ExFormation ys))
  | a == b = mergeBindings xs ys <&> BiTau a . ExFormation
  | otherwise = throwIO (CanNotMergeBinding first second)
mergeBinding x y
  | x == y = pure x
  | otherwise = throwIO (CanNotMergeBinding x y)

mergeBindings :: [Binding] -> [Binding] -> IO [Binding]
mergeBindings xs ys = do
  let as = attributesFromBindings' xs
      bs = attributesFromBindings' ys
      xs' = [x | x <- xs, attributeFromBinding x `notElem` bs]
      ys' = [y | y <- ys, attributeFromBinding y `notElem` as]
      collisions = [(x, y) | x <- xs, y <- ys, attributeFromBinding x == attributeFromBinding y]
  ws <- mapM (uncurry mergeBinding) collisions
  pure (xs' <> ys' <> ws)

merge' :: [Expression] -> IO Expression
merge' [] = throwIO EmptyExpressionList
merge' [p@(ExFormation _)] = pure p
merge' (ExFormation bindings : rest) = do
  ExFormation bds' <- merge' rest
  merged <- mergeBindings bds' bindings
  pure (ExFormation merged)
merge' (expr : _) = throwIO (WrongExpressionFormat expr)

merge :: [Expression] -> IO Expression
merge exprs = merge' (reverse exprs)
