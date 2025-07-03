-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT
{-# LANGUAGE LambdaCase #-}

module Dataize where

import Ast
import Condition (isNF)
import Control.Exception (throwIO)
import Data.List (partition)
import Rewriter (rewrite')
import Text.Printf (printf)
import Yaml (normalizationRules)

maybeLambda :: [Binding] -> (Maybe Binding, [Binding])
maybeLambda [] = (Nothing, [])
maybeLambda bds =
  let isLambda = \case
        BiLambda _ -> True
        _ -> False
      (lambdas, rest) = partition isLambda bds
   in case lambdas of
        [lambda] -> (Just lambda, rest)
        _ -> (Nothing, bds)

-- Returns (X, Y, Z) triple
-- X - maybe labmda binding in deepest formation, if present
-- Y - constructor for expression
-- Z - deepst formation without lambda binding, if present
atomInHead :: Expression -> (Maybe Binding, Expression -> Expression, Expression)
atomInHead (ExApplication expr tau) =
  let (lambda, ctor, formation) = atomInHead expr
   in (lambda, \exp -> ExApplication (ctor exp) tau, formation)
atomInHead (ExDispatch expr attr) =
  let (lambda, ctor, formation) = atomInHead expr
   in (lambda, \exp -> ExDispatch (ctor exp) attr, formation)
atomInHead (ExFormation bds) =
  let (lambda, rest) = maybeLambda bds
   in (lambda, id, ExFormation rest)
atomInHead expr = (Nothing, id, ExFormation [])

morphAtom :: String -> Expression -> (Expression -> Expression) -> IO (Maybe Expression)
morphAtom func formation ctor
  | isNF formation = do
      obj <- atom func formation
      pure (Just (ctor obj))
  | otherwise = pure Nothing

morph' :: Expression -> IO (Maybe Expression)
morph' expr = case atomInHead expr of
  (Just (BiLambda func), ctor, formation) -> morphAtom func formation ctor
  (Nothing, _, _)
    | isNF expr -> pure Nothing
    | otherwise -> do
        (Program expr') <- rewrite' (Program expr) normalizationRules 25
        morph expr'
  (_, _, _) -> pure Nothing

-- The Morphing function M:<B,S> -> <P,S> maps objects to
-- primitives, possibly modifying the state of evaluation.
-- Terminology:
-- P(e) - is e Primitive, which is either formation without λ binding or termination ⊥
-- N(e1) -> e2 - normalization of e1 leads to e2
-- NF(e) - is e in normal form (can't be normalized anymore)
--
-- Rules:
-- M1: M(e) -> e; if P(e)
-- M2: M([[B1, λ -> F, B2]] * !t) -> M(e2 * !t); where e3 = [[B1,B2]] and NF(e3) and F(e3) -> e2 and !t - possibly empty tail
-- M3: M(e1) -> M(e2); if N(e1) -> e2 and e1 != e2
-- M4: M(e) -> Nothing; in any other cases
morph :: Expression -> IO (Maybe Expression)
morph ExTermination = pure (Just ExTermination) -- M1
morph (ExFormation bds) = case atomInHead (ExFormation bds) of
  (Just (BiLambda func), ctor, formation) -> morphAtom func formation ctor
  (_, _, formation) -> pure (Just formation) -- M1
morph (ExApplication expr tau) = morph' (ExApplication expr tau)
morph (ExDispatch expr attr) = morph' (ExDispatch expr attr)
morph _ = pure Nothing

atom :: String -> Expression -> IO Expression
atom "L_org_eolang_number_plus" self = pure (ExFormation [])
atom func _ = throwIO (userError (printf "Atom '%s' does not exist" func))