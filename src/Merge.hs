{-# LANGUAGE DeriveAnyClass #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Merge (merge) where

import AST
import Control.Exception.Base (Exception, throwIO)
import Data.Functor ((<&>))
import Misc
import Printer (printExpression, printProgram)
import Text.Printf (printf)

data MergeException
  = WrongProgramFormat Program
  | CanNotMergeBinding Binding Binding
  | EmptyProgramList
  deriving (Exception)

instance Show MergeException where
  show (WrongProgramFormat prog) =
    printf
      "Invalid program format, only programs with top level formations are supported for 'merge' command, given:\n%s"
      (printProgram prog)
  show (CanNotMergeBinding first second) =
    printf
      "Can't merge two bindings, conflict found:\n%s"
      (printExpression (ExFormation [first, second]))
  show EmptyProgramList = "Nothing to merge: provide at least one program"

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

merge' :: [Program] -> IO Program
merge' [] = throwIO EmptyProgramList
merge' [p@(Program (ExFormation _))] = pure p
merge' (Program (ExFormation bindings) : rest) = do
  Program (ExFormation bds') <- merge' rest
  merged <- mergeBindings bds' bindings
  pure (Program (ExFormation merged))
merge' (prog : _) = throwIO (WrongProgramFormat prog)

merge :: [Program] -> IO Program
merge progs = merge' (reverse progs)
