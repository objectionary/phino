-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Canonizer (canonize) where

import AST
import Rewriter (Rewritten)

canonizeBindings :: [Binding] -> Int -> ([Binding], Int)
canonizeBindings [] idx = ([], idx)
canonizeBindings ((BiLambda _) : rest) idx =
  let (bds', idx') = canonizeBindings rest (idx + 1)
   in (BiLambda ('F' : show idx) : bds', idx')
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
canonizeExpression (ExApplication expr (BiTau attr arg)) idx =
  let (expr', idx') = canonizeExpression expr idx
      (arg', idx'') = canonizeExpression arg idx'
   in (ExApplication expr' (BiTau attr arg'), idx'')
canonizeExpression expr idx = (expr, idx)

canonize :: [Rewritten] -> [Rewritten]
canonize [] = []
canonize ((Program expr, maybeRule) : rest) = (Program (fst (canonizeExpression expr 1)), maybeRule) : canonize rest
