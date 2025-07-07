{-# LANGUAGE LambdaCase #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Dataize (morph, dataize) where

import Ast
import Condition (isNF)
import Control.Exception (throwIO)
import Data.List (partition)
import Rewriter (rewrite')
import Text.Printf (printf)
import Yaml (normalizationRules)

maybeBinding :: (Binding -> Bool) -> [Binding] -> (Maybe Binding, [Binding])
maybeBinding _ [] = (Nothing, [])
maybeBinding func bds =
  let (found, rest) = partition func bds
   in case found of
        [bd] -> (Just bd, rest)
        _ -> (Nothing, bds)

maybeLambda :: [Binding] -> (Maybe Binding, [Binding])
maybeLambda = maybeBinding (\case BiLambda _ -> True; _ -> False)

maybeDelta :: [Binding] -> (Maybe Binding, [Binding])
maybeDelta = maybeBinding (\case BiDelta _ -> True; _ -> False)

maybePhi :: [Binding] -> (Maybe Binding, [Binding])
maybePhi = maybeBinding (\case (BiTau AtPhi _) -> True; _ -> False)

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
  exp' <- withTail expr prog
  case exp' of
    Just exp -> pure (Just (ExApplication exp tau))
withTail (ExDispatch (ExFormation bds) attr) _ = do
  obj' <- formation bds
  case obj' of
    Just obj -> pure (Just (ExDispatch obj attr))
    _ -> pure Nothing
withTail (ExFormation bds) _ = formation bds
withTail (ExDispatch (ExDispatch ExGlobal (AtLabel label)) attr) (Program expr) = case phiDispatch label expr of
  Just obj -> pure (Just (ExDispatch obj attr))
  _ -> pure Nothing
withTail (ExDispatch ExGlobal (AtLabel label)) (Program expr) = pure (phiDispatch label expr)
withTail (ExDispatch expr attr) prog = do
  exp' <- withTail expr prog
  case exp' of
    Just exp -> pure (Just (ExDispatch exp attr))
    _ -> pure Nothing
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

-- The goal of 'dataize' function is retrieve bytes from given expression.
-- 
-- DELTA: D(e) -> data                          if e = [B1, Δ -> data, B2]
-- BOX:   D([B1, 𝜑 -> e, B2]) -> D(e)           if [B1,B2] has no delta/lambda
-- NORM:  D(e1) -> D(e2)                        if e2 := M(e1) and e1 is not primitive
--        nothing                               otherwise
dataize :: Expression -> Program -> IO (Maybe String)
dataize ExTermination _ = pure Nothing
dataize (ExFormation bds) prog = case maybeDelta bds of
  (Just (BiDelta bytes), _) -> pure (Just bytes)
  (Nothing, _) -> case maybePhi bds of
    (Just (BiTau AtPhi expr), bds') -> case maybeLambda bds' of
      (Just (BiLambda _), _) -> throwIO (userError "The 𝜑 and λ can't be present in formation at the same time")
      (_, _) -> dataize expr prog
    (Nothing, _) -> case maybeLambda bds of
      (Just (BiLambda _), _) -> do
        morphed' <- morph (ExFormation bds) prog
        case morphed' of
          Just morphed -> dataize morphed prog
          _ -> pure Nothing
      (Nothing, _) -> pure Nothing
dataize expr prog = do
  morphed' <- morph expr prog
  case morphed' of
    Just morphed -> dataize morphed prog
    _ -> pure Nothing

atom :: String -> Expression -> IO Expression
atom "L_org_eolang_number_plus" self = pure (ExFormation [])
atom func _ = throwIO (userError (printf "Atom '%s' does not exist" func))