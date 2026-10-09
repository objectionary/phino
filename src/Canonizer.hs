-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Canonizer (canonize, canonizeExpr, lambdaNames) where

import AST
import qualified Data.Map as Map
import qualified Data.Text as T
import Rewriter (Rewritten)

type CanonState = (Map.Map T.Text T.Text, Int)

canonizeBindings :: [Binding] -> CanonState -> ([Binding], CanonState)
canonizeBindings [] st = ([], st)
canonizeBindings ((BiLambda (Function name)) : rest) st
  | name == T.pack "Package" =
      let (bds', st') = canonizeBindings rest st
       in (BiLambda (Function name) : bds', st')
canonizeBindings ((BiLambda (Function name)) : rest) (names, idx) =
  case Map.lookup name names of
    Just canonical ->
      let (bds', st') = canonizeBindings rest (names, idx)
       in (BiLambda (Function canonical) : bds', st')
    Nothing ->
      let canonical = T.pack ("Fn" <> show idx)
          (bds', st') = canonizeBindings rest (Map.insert name canonical names, idx + 1)
       in (BiLambda (Function canonical) : bds', st')
canonizeBindings (BiTau attr expr : rest) st =
  let (expr', st') = canonizeExpression expr st
      (bds', st'') = canonizeBindings rest st'
   in (BiTau attr expr' : bds', st'')
canonizeBindings (bd : rest) st =
  let (bds', st') = canonizeBindings rest st
   in (bd : bds', st')

canonizeExpression :: Expression -> CanonState -> (Expression, CanonState)
canonizeExpression (ExFormation bds) st =
  let (bds', st') = canonizeBindings bds st
   in (ExFormation bds', st')
canonizeExpression (ExDispatch expr attr) st =
  let (expr', st') = canonizeExpression expr st
   in (ExDispatch expr' attr, st')
canonizeExpression (ExApplication expr arg) st =
  let (expr', st') = canonizeExpression expr st
      (arg', st'') = canonizeArgument arg st'
   in (ExApplication expr' arg', st'')
canonizeExpression (ExPhiMeet prefix num expr) st =
  let (expr', st') = canonizeExpression expr st
   in (ExPhiMeet prefix num expr', st')
canonizeExpression (ExPhiAgain prefix num expr) st =
  let (expr', st') = canonizeExpression expr st
   in (ExPhiAgain prefix num expr', st')
canonizeExpression expr st = (expr, st)

canonizeArgument :: Argument -> CanonState -> (Argument, CanonState)
canonizeArgument (ArTau attr expr) st =
  let (expr', st') = canonizeExpression expr st
   in (ArTau attr expr', st')
canonizeArgument (ArAlpha alpha expr) st =
  let (expr', st') = canonizeExpression expr st
   in (ArAlpha alpha expr', st')

lambdaNames :: Expression -> [T.Text]
lambdaNames (ExFormation bds) = concatMap named bds
  where
    named :: Binding -> [T.Text]
    named (BiLambda (Function name)) = [name]
    named (BiTau _ expr) = lambdaNames expr
    named _ = []
lambdaNames (ExDispatch expr _) = lambdaNames expr
lambdaNames (ExApplication expr (ArTau _ arg)) = lambdaNames expr ++ lambdaNames arg
lambdaNames (ExApplication expr (ArAlpha _ arg)) = lambdaNames expr ++ lambdaNames arg
lambdaNames (ExPhiMeet _ _ expr) = lambdaNames expr
lambdaNames (ExPhiAgain _ _ expr) = lambdaNames expr
lambdaNames _ = []

canonizeExpr :: Expression -> Expression
canonizeExpr expr = fst (canonizeExpression expr (Map.empty, 1))

canonize :: [Rewritten] -> [Rewritten]
canonize [] = []
canonize ((expr, maybeRule) : rest) = (canonizeExpr expr, maybeRule) : canonize rest
