{-# LANGUAGE DeriveAnyClass #-}
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
import Control.Exception (Exception, throwIO)
import Data.List (partition)
import Deps (BuildTermFunc, SaveStepFunc)
import Misc
import Must (Must (..))
import Printer (printExpression)
import Rewriter (RewriteContext (RewriteContext), Rewritten, rewrite')
import Rule (RuleContext (RuleContext), isNF)
import Text.Printf (printf)
import Yaml (normalizationRules)

type Dataized = (Maybe Bytes, [Rewritten])

type Dataizable = (Expression, [Rewritten])

type Morphed = Dataizable

data LocatorException
  = CanNotFindObjectByLocator {fqn :: Expression}
  | InvalidLocatorProvided {fqn :: Expression}
  deriving Exception

instance Show LocatorException where
  show CanNotFindObjectByLocator{..} = printf "Can't find object by locator: '%s'" (printExpression fqn)
  show InvalidLocatorProvided{..} =
    printf
      "Can't dataize object by invalid locator. \
      \'Q' or dispatch started with 'Q' expected, but got: '%s'"
      (printExpression fqn)

data DataizeContext = DataizeContext
  { _program :: Program
  , _maxDepth :: Int
  , _maxCycles :: Int
  , _depthSensitive :: Bool
  , _buildTerm :: BuildTermFunc
  , _saveStep :: SaveStepFunc
  }

switchContext :: DataizeContext -> RewriteContext
switchContext DataizeContext{..} =
  RewriteContext
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
withTail (ExApplication (ExDispatch ExGlobal _) _) _ = pure Nothing
withTail (ExApplication expr tau) ctx = do
  tailed <- withTail expr ctx
  case tailed of
    Just (exp, rule) -> pure (Just (ExApplication exp tau, rule))
    _ -> pure Nothing
withTail (ExDispatch (ExFormation bds) attr) ctx = do
  tailed <- formation bds ctx
  case tailed of
    Just (obj, rule) -> pure (Just (ExDispatch obj attr, rule))
    _ -> pure Nothing
withTail (ExFormation bds) ctx = formation bds ctx
withTail (ExDispatch (ExDispatch ExGlobal (AtLabel label)) attr) (DataizeContext{_program = Program expr}) = case phiDispatch label expr of
  Just (obj, rule) -> pure (Just (ExDispatch obj attr, rule))
  _ -> pure Nothing
withTail (ExDispatch ExGlobal (AtLabel label)) (DataizeContext{_program = Program expr}) = pure (phiDispatch label expr)
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
morph (ExTermination, seq) _ = pure (ExTermination, leadsTo seq "Mprim" ExTermination) -- PRIM
morph (form@(ExFormation _), seq) ctx = do
  resolved <- withTail form ctx
  case resolved of
    Just (expr, rule) -> morph (expr, leadsTo seq rule expr) ctx -- LAMBDA or PHI
    _ -> pure (form, leadsTo seq "Mprim" form) -- PRIM
morph (expr, seq) ctx = do
  resolved <- withTail expr ctx
  case resolved of
    Just (expr', rule) -> morph (expr', leadsTo seq rule expr') ctx
    _ ->
      if isNF expr (RuleContext (_buildTerm ctx))
        then morph (ExTermination, seq) ctx -- PRIM
        else do
          rewrittens' <- rewrite' (Program expr) normalizationRules (switchContext ctx) -- NMZ
          let _seq = reverse rewrittens' <> tail seq
              (Program expr') = fst (head _seq)
          morph (expr', _seq) ctx

dataize :: Expression -> DataizeContext -> IO Dataized
dataize locator ctx@DataizeContext{_program = Program expr} = do
  fqn <- case fqnToAttrs locator of
    Just attrs -> pure attrs
    _ -> throwIO (InvalidLocatorProvided locator)
  exp <- located expr fqn
  (maybeBytes, seq) <- dataize' (exp, [(Program exp, Nothing)]) ctx
  pure (maybeBytes, reverse seq)
  where
    located :: Expression -> [Attribute] -> IO Expression
    located expr [] = pure expr
    located (ExFormation bds) [attr] = case locatedInBindings attr bds of
      Just expr -> pure expr
      _ -> throwIO (CanNotFindObjectByLocator locator)
    located (ExFormation bds) (attr : rest) = case locatedInBindings attr bds of
      Just expr -> located expr rest
      _ -> throwIO (CanNotFindObjectByLocator locator)
    located _ _ = throwIO (CanNotFindObjectByLocator locator)
    locatedInBindings :: Attribute -> [Binding] -> Maybe Expression
    locatedInBindings _ [] = Nothing
    locatedInBindings attr (BiTau attr' expr : rest)
      | attr == attr' = Just expr
      | otherwise = locatedInBindings attr rest
    locatedInBindings attr (_ : rest) = locatedInBindings attr rest

-- The goal of 'dataize' function is retrieve bytes from given expression.
--
-- DELTA: D(e) -> data                          if e = [B1, Δ -> data, B2]
-- BOX:   D([B1, 𝜑 -> e, B2]) -> D(С(e))        if [B1,B2] has no delta/lambda, where С(e) - contextualization
-- NORM:  D(e1) -> D(e2)                        if e2 := M(e1) and e1 is not primitive
--        nothing                               otherwise
dataize' :: Dataizable -> DataizeContext -> IO Dataized
dataize' (ExTermination, seq) _ = pure (Nothing, seq)
dataize' (ExFormation bds, seq) ctx = case maybeDelta bds of
  (Just (BiDelta bytes), _) -> pure (Just bytes, seq)
  (Just _, _) -> pure (Nothing, seq)
  (Nothing, _) -> case maybePhi bds of
    (Just (BiTau AtPhi expr), bds') -> case maybeLambda bds' of
      (Just (BiLambda _), _) -> throwIO (userError "The 𝜑 and λ can't be present in formation at the same time")
      (Just _, _) -> pure (Nothing, seq)
      (Nothing, _) ->
        let expr' = contextualize expr (ExFormation bds)
         in dataize' (expr', leadsTo seq "contextualize" expr') ctx
    (Just _, _) -> pure (Nothing, seq)
    (Nothing, _) -> case maybeLambda bds of
      (Just (BiLambda _), _) -> morph (ExFormation bds, seq) ctx >>= (`dataize'` ctx)
      (Just _, _) -> pure (Nothing, seq)
      (Nothing, _) -> pure (Nothing, seq)
dataize' dataizable ctx = morph dataizable ctx >>= (`dataize'` ctx)

leadsTo :: [Rewritten] -> String -> Expression -> [Rewritten]
leadsTo [] rule expr = [(Program expr, Just rule)]
leadsTo ((prog, _) : rest) rule expr = (Program expr, Nothing) : (prog, Just rule) : rest

atom :: String -> Expression -> DataizeContext -> IO Expression
atom "L_org_eolang_number_plus" self ctx = do
  (left, _) <- dataize' (ExDispatch self (AtLabel "x"), []) ctx
  (right, _) <- dataize' (ExDispatch self AtRho, []) ctx
  case (left, right) of
    (Just left', Just right') -> do
      let first = either toDouble id (btsToNum left')
          second = either toDouble id (btsToNum right')
          sum = first + second
      pure (DataNumber (numToBts sum))
    _ -> pure ExTermination
atom "L_org_eolang_number_times" self ctx = do
  (left, _) <- dataize' (ExDispatch self (AtLabel "x"), []) ctx
  (right, _) <- dataize' (ExDispatch self AtRho, []) ctx
  case (left, right) of
    (Just left', Just right') -> do
      let first = either toDouble id (btsToNum left')
          second = either toDouble id (btsToNum right')
          sum = first * second
      pure (DataNumber (numToBts sum))
    _ -> pure ExTermination
atom "L_org_eolang_number_eq" self ctx = do
  (x, _) <- dataize' (ExDispatch self (AtLabel "x"), []) ctx
  (rho, _) <- dataize' (ExDispatch self AtRho, []) ctx
  case (x, rho) of
    (Just x', Just rho') -> do
      let self' = either toDouble id (btsToNum rho')
          first = either toDouble id (btsToNum x')
      if self' == first
        then pure (DataNumber (numToBts first))
        else pure (ExDispatch self (AtLabel "y"))
    _ -> pure ExTermination
atom func _ _ = throwIO (userError (printf "Atom '%s' does not exist" func))
