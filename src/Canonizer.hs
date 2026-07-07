-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- Canonization is the process of replacing function names attached to
-- lambda bindings with numbered identifiers prefixed with 'Fn'
-- like 'Fn1', 'Fn2', etc.
module Canonizer (canonize, canonizeExpr) where

import AST
import qualified Data.Text as T
import Rewriter (Rewritten)

canonizeBindings :: [Binding] -> Int -> ([Binding], Int)
canonizeBindings [] idx = ([], idx)
canonizeBindings ((BiLambda (Function _)) : rest) idx =
  let (bds', idx') = canonizeBindings rest (idx + 1)
   in (BiLambda (Function (T.pack ("Fn" <> show idx))) : bds', idx')
canonizeBindings (BiTau attr expr : rest) idx =
  let (expr', idx') = canonizeExpression expr idx
      (bds', idx'') = canonizeBindings rest idx'
   in (BiTau attr expr' : bds', idx'')
canonizeBindings (bd : rest) idx =
  let (bds', idx') = canonizeBindings rest idx
   in (bd : bds', idx')

canonizeExpression :: Expression -> Int -> (Expression, Int)
canonizeExpression (ExFormation bds) idx =
  let (bds', idx') = canonizeBindings bds idx
   in (ExFormation bds', idx')
canonizeExpression (ExDispatch expr attr) idx =
  let (expr', idx') = canonizeExpression expr idx
   in (ExDispatch expr' attr, idx')
canonizeExpression (ExApplication expr arg) idx =
  let (expr', idx') = canonizeExpression expr idx
      (arg', idx'') = canonizeArgument arg idx'
   in (ExApplication expr' arg', idx'')
canonizeExpression (ExPhiMeet prefix num expr) idx =
  let (expr', idx') = canonizeExpression expr idx
   in (ExPhiMeet prefix num expr', idx')
canonizeExpression (ExPhiAgain prefix num expr) idx =
  let (expr', idx') = canonizeExpression expr idx
   in (ExPhiAgain prefix num expr', idx')
canonizeExpression expr idx = (expr, idx)

canonizeArgument :: Argument -> Int -> (Argument, Int)
canonizeArgument (ArTau attr expr) idx =
  let (expr', idx') = canonizeExpression expr idx
   in (ArTau attr expr', idx')
canonizeArgument (ArAlpha alpha expr) idx =
  let (expr', idx') = canonizeExpression expr idx
   in (ArAlpha alpha expr', idx')

-- Canonize a single expression, restarting the 'Fn' counter from 1 so the
-- numbering is local to that expression.
canonizeExpr :: Expression -> Expression
canonizeExpr expr = fst (canonizeExpression expr 1)

canonize :: [Rewritten] -> [Rewritten]
canonize [] = []
canonize ((expr, maybeRule) : rest) = (canonizeExpr expr, maybeRule) : canonize rest
