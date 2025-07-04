{-# LANGUAGE LambdaCase #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

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

formation :: [Binding] -> IO (Maybe Expression)
formation bds = do
  let (lambda, bds') = maybeLambda bds
  case lambda of
    Just (BiLambda func) -> do
      obj' <- atom func (ExFormation bds')
      pure (Just obj')
    _ -> pure Nothing

phiDispatch :: String -> Expression -> Maybe Expression
phiDispatch attr expr = case expr of
  ExFormation bds -> boundExpr bds
  _ -> Nothing
  where
    boundExpr :: [Binding] -> Maybe Expression
    boundExpr [] = Nothing
    boundExpr (bd : bds) = case bd of
      BiTau (AtLabel attr') expr' -> if attr' == attr then Just expr' else boundExpr bds
      _ -> boundExpr bds

withTail :: Expression -> Program -> IO (Maybe Expression)
withTail (ExApplication (ExFormation _) _) _ = pure Nothing
withTail (ExApplication (ExDispatch ExGlobal _) _) _ = pure Nothing
withTail (ExApplication expr tau) prog = do
  Just exp <- withTail expr prog
  pure (Just (ExApplication exp tau))
withTail (ExDispatch (ExFormation bds) attr) _ = do
  Just obj <- formation bds
  pure (Just (ExDispatch obj attr))
withTail (ExFormation bds) _ = formation bds
withTail (ExDispatch (ExDispatch ExGlobal (AtLabel label)) attr) (Program expr) = case phiDispatch label expr of
  Just obj -> pure (Just (ExDispatch obj attr))
  _ -> pure Nothing
withTail (ExDispatch ExGlobal (AtLabel label)) (Program expr) = pure (phiDispatch label expr)
withTail (ExDispatch expr attr) prog = do
  Just exp <- withTail expr prog
  pure (Just (ExDispatch exp attr))
withTail _ _ = pure Nothing

-- The Morphing function M:<B,S> -> <P,S> maps objects to
-- primitives, possibly modifying the state of evaluation.
-- Terminology:
-- P(e) - is e Primitive, which is either formation without λ binding or termination ⊥
-- N(e) - normalize e
-- NF(e) - is e in normal form (can't be normalized anymore)
--
-- PRIM:   M(e) -> e                              if P(e)
-- NMZ:    M(e1) -> M(e2)                         if e2 := N(e1) and e1 != e2
-- LAMBDA: M([B1, λ -> F, B2] * t) -> M(e2 * t)   if e3 := [B1,B2] and e2 := F(e3)
-- PHI:    M(Q.tau * t) -> M(e * t)               if Q -> [B1, tau -> e, B2], t is tail started with dispatch
--         M(e) -> nothing                        otherwise
morph :: Expression -> Program -> IO (Maybe Expression)
morph ExTermination _ = pure (Just ExTermination) -- PRIM
morph (ExFormation bds) prog = do
  resolved <- withTail (ExFormation bds) prog
  case resolved of
    Just obj -> morph obj prog -- LAMBDA or PHI
    _ -> pure (Just (ExFormation bds)) -- PRIM
morph expr prog = do
  resolved <- withTail expr prog
  case resolved of
    Just obj -> morph obj prog
    _ ->
      if isNF expr
        then pure Nothing
        else do
          (Program expr') <- rewrite' (Program expr) normalizationRules 25 -- NMZ
          morph expr' prog

atom :: String -> Expression -> IO Expression
atom "L_org_eolang_number_plus" self = pure (ExFormation [])
atom func _ = throwIO (userError (printf "Atom '%s' does not exist" func))