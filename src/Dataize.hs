{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Dataize (morph, dataize, dataize', DataizeContext (..)) where

import AST
import Builder (contextualize)
import Control.Exception (throwIO)
import Data.List (partition)
import Deps (BuildTermFunc, SaveStepFunc, Term (TeAttribute))
import Locator (locatedExpression, withLocatedExpression)
import Matcher (substEmpty)
import Misc
import Must (Must (..))
import Rewriter (RewriteContext (RewriteContext), Rewritten, rewrite)
import Rule (RuleContext (RuleContext), isNF)
import Text.Printf (printf)
import Yaml (ExtraArgument (ArgAttribute), normalizationRules)

type Dataized = (Maybe Bytes, [Rewritten])

type Dataizable = (Expression, [Rewritten])

type Morphed = Dataizable

data DataizeContext = DataizeContext
  { _locator :: Expression
  , _program :: Program
  , _maxDepth :: Int
  , _maxCycles :: Int
  , _depthSensitive :: Bool
  , _buildTerm :: BuildTermFunc
  , _saveStep :: SaveStepFunc
  }

switchContext :: DataizeContext -> RewriteContext
switchContext DataizeContext{..} =
  RewriteContext
    _locator
    _maxDepth
    _maxCycles
    _depthSensitive
    _buildTerm
    MtDisabled
    _saveStep

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

-- Resolve formation for LAMBDA Morphing rule.
-- If formation contains λ binding, the called atom
-- result is returned.
formation :: [Binding] -> DataizeContext -> IO (Maybe (Expression, String))
formation bds ctx = do
  let (lambda, bds') = maybeLambda bds
  case lambda of
    Just (BiLambda func) -> do
      obj <- atom func (ExFormation bds') ctx
      pure (Just (obj, "Mlambda"))
    _ -> pure Nothing

-- Resolve dispatch from global object (Q.tau) for PHI Morphing rule.
-- Here tau is the name of the attribute which is taken from Q
-- and expr is expression which program refers to.
-- If Q refers to formation which contains binding with attribute == tau -
-- the expression from this binding is returned.
phiDispatch :: String -> Expression -> Maybe (Expression, String)
phiDispatch tau expr = case expr of
  ExFormation bds -> boundExpr bds
  _ -> Nothing
  where
    boundExpr :: [Binding] -> Maybe (Expression, String)
    boundExpr [] = Nothing
    boundExpr (bd : bds) = case bd of
      BiTau (AtLabel attr) expr' -> if attr == tau then Just (expr', "Mphi") else boundExpr bds
      _ -> boundExpr bds

-- Resolve tail PHI and LAMBDA Morphing rules.
-- Tail MUST start with dispatch, that's why most of the applications return Nothing
withTail :: Expression -> DataizeContext -> IO (Maybe (Expression, String))
withTail (ExApplication (ExFormation _) _) _ = pure Nothing
withTail (ExApplication expr tau) ctx = do
  tailed <- withTail expr ctx
  case tailed of
    Just (expr', rule) -> pure (Just (ExApplication expr' tau, rule))
    _ -> pure Nothing
withTail (ExDispatch (ExFormation bds) attr) ctx = do
  tailed <- formation bds ctx
  case tailed of
    Just (obj, rule) -> pure (Just (ExDispatch obj attr, rule))
    _ -> pure Nothing
withTail (ExFormation bds) ctx = formation bds ctx
withTail (ExDispatch ExGlobal (AtLabel label)) DataizeContext{_program = Program expr} = pure (phiDispatch label expr)
withTail (ExDispatch expr attr) ctx = do
  tailed <- withTail expr ctx
  case tailed of
    Just (exp, rule) -> pure (Just (ExDispatch exp attr, rule))
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
--         M(e) -> ⊥                              otherwise
morph :: Morphed -> DataizeContext -> IO Morphed
morph (expr@ExTermination, seq) ctx = do
  seq' <- leadsTo seq "Mprim" expr ctx -- PRIM
  pure (expr, seq')
morph (form@(ExFormation _), seq) ctx = do
  resolved <- withTail form ctx
  case resolved of
    Just (expr, rule) -> do
      seq' <- leadsTo seq rule expr ctx -- LAMBDA or PHI
      morph (expr, seq') ctx
    _ -> do
      seq' <- leadsTo seq "Mprim" form ctx -- PRIM
      pure (form, seq')
morph (expr, seq) ctx@DataizeContext{..} = do
  resolved <- withTail expr ctx
  case resolved of
    Just (expr', rule) -> do
      seq' <- leadsTo seq rule expr' ctx
      morph (expr', seq') ctx
    _ ->
      if isNF expr (RuleContext _buildTerm)
        then morph (ExTermination, seq) ctx -- PRIM
        else do
          prog' <- withLocatedExpression _locator expr _program
          (rewrittens', _) <- rewrite prog' normalizationRules (switchContext ctx) -- NMZ
          let seq' = reverse rewrittens' <> tail seq
          expr' <- locatedExpression _locator (fst (head seq'))
          morph (expr', seq') ctx

dataize :: DataizeContext -> IO Dataized
dataize ctx@DataizeContext{..} = do
  expr <- locatedExpression _locator _program
  (maybeBytes, seq) <- dataize' (expr, [(_program, Nothing)]) ctx
  pure (maybeBytes, reverse seq)

-- The goal of 'dataize' function is retrieve bytes from given expression.
--
-- DELTA: D(e) -> data                          if e = [B1, Δ -> data, B2]
-- BOX:   D([B1, 𝜑 -> e, B2]) -> D(С(e))        if [B1,B2] has no delta/lambda, where С(e) - contextualization
-- NORM:  D(e1) -> D(e2)                        if e2 := M(e1) and e1 is not primitive
--        nothing                               otherwise
dataize' :: Dataizable -> DataizeContext -> IO Dataized
dataize' (ExTermination, seq) _ = pure (Nothing, seq)
dataize' (form@(ExFormation bds), seq) ctx@DataizeContext{..} = case maybeDelta bds of
  (Just (BiDelta bytes), _) -> pure (Just bytes, seq)
  (Just _, _) -> pure (Nothing, seq)
  (Nothing, _) -> case maybePhi bds of
    (Just (BiTau AtPhi expr), bds') -> case maybeLambda bds' of
      (Just (BiLambda _), _) -> throwIO (userError "The 𝜑 and λ can't be present in formation at the same time")
      (Just _, _) -> pure (Nothing, seq)
      (Nothing, _) -> do
        let expr' = contextualize expr form
        seq' <- leadsTo seq "contextualize" expr' ctx
        dataize' (expr', seq') ctx
    (Just _, _) -> pure (Nothing, seq)
    (Nothing, _) -> case maybeLambda bds of
      (Just (BiLambda _), _) -> morph (form, seq) ctx >>= (`dataize'` ctx)
      (Just _, _) -> pure (Nothing, seq)
      (Nothing, _) -> pure (Nothing, seq)
dataize' dataizable ctx = morph dataizable ctx >>= (`dataize'` ctx)

leadsTo :: [Rewritten] -> String -> Expression -> DataizeContext -> IO [Rewritten]
leadsTo [] _ _ _ = throwIO (userError "Empty rewritten sequence should never met during dataization process")
leadsTo ((prog, _) : rest) rule expr DataizeContext{..} = do
  prog' <- withLocatedExpression _locator expr prog
  pure ((prog', Nothing) : (prog, Just rule) : rest)

-- Synthetic dataize function for internal usage inside atoms
-- Here we modify original program from context by adding new binding
-- which refers to expression we want to dataize.
_dataize :: Expression -> DataizeContext -> IO (Maybe Bytes)
_dataize expr ctx@DataizeContext{_buildTerm = buildTerm, _program = Program (ExFormation bds)} = do
  (TeAttribute attr) <- buildTerm "random-tau" (map ArgAttribute (attributesFromBindings bds)) substEmpty
  let prog = Program (ExFormation (BiTau attr expr : bds))
  (bts, _) <- dataize' (expr, [(prog, Nothing)]) ctx{_program = prog}
  pure bts
_dataize _ _ = throwIO (userError "Can't call _dataize from atoms with non-formation program")

atom :: String -> Expression -> DataizeContext -> IO Expression
atom "L_number_plus" self ctx = do
  left <- _dataize (ExDispatch self (AtLabel "x")) ctx
  right <- _dataize (ExDispatch self AtRho) ctx
  case (left, right) of
    (Just left', Just right') -> do
      let first = either toDouble id (btsToNum left')
          second = either toDouble id (btsToNum right')
          sum = first + second
      pure (DataNumber (numToBts sum))
    _ -> pure ExTermination
atom "L_number_times" self ctx = do
  left <- _dataize (ExDispatch self (AtLabel "x")) ctx
  right <- _dataize (ExDispatch self AtRho) ctx
  case (left, right) of
    (Just left', Just right') -> do
      let first = either toDouble id (btsToNum left')
          second = either toDouble id (btsToNum right')
          sum = first * second
      pure (DataNumber (numToBts sum))
    _ -> pure ExTermination
atom "L_number_eq" self ctx = do
  x <- _dataize (ExDispatch self (AtLabel "x")) ctx
  rho <- _dataize (ExDispatch self AtRho) ctx
  case (x, rho) of
    (Just x', Just rho') -> do
      let self' = either toDouble id (btsToNum rho')
          first = either toDouble id (btsToNum x')
      if self' == first
        then pure (DataNumber (numToBts first))
        else pure (ExDispatch self (AtLabel "y"))
    _ -> pure ExTermination
atom func _ _ = throwIO (userError (printf "Atom '%s' does not exist" func))
