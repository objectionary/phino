{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Merge (merge) where

import AST
import Control.Exception (throwIO)
import Control.Exception.Base
import Data.Functor ((<&>))
import Deps (BuildTermFunc, Term (TeBindings))
import Matcher (substEmpty)
import Misc
import Printer (printBinding, printProgram, printExpression)
import Text.Printf (printf)
import qualified Yaml as Y

data MergeException
  = WrongProgramFormat {program :: Program}
  | CanNotMergeBinding {first :: Binding, second :: Binding}
  deriving (Exception)

instance Show MergeException where
  show WrongProgramFormat {..} =
    printf
      "Invalid program format, only programs with top level formations are supported for 'merge' command, given:\n%s"
      (printProgram program)
  show CanNotMergeBinding {..} =
    printf
      "Can't merge two bindings, conflict found:\n%s"
      (printExpression (ExFormation [first, second]))

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
merge' [prog@(Program (ExFormation bds))] = pure prog
merge' (prog@(Program (ExFormation bds)) : rest) = do
  Program (ExFormation bds') <- merge' rest
  merged <- mergeBindings bds' bds
  pure (Program (ExFormation merged))
merge' (prog : rest) = throwIO (WrongProgramFormat prog)

merge :: [Program] -> IO Program
merge progs = merge' (reverse progs)
