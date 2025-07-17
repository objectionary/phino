{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Dataize (morph, dataize, dataize', DataizeContext (..)) where

import Ast
import Builder (contextualize)
import Control.Exception (throwIO)
import Data.List (partition)
import Misc
import Rewriter (RewriteContext (RewriteContext), rewrite')
import Rule (RuleContext (RuleContext), isNF)
import Term (BuildTermFunc)
import Text.Printf (printf)
import XMIR (XmirContext (XmirContext))
import Yaml (normalizationRules)

data DataizeContext = DataizeContext
  { _program :: Program,
    _maxDepth :: Integer,
    _buildTerm :: BuildTermFunc
  }

switchContext :: DataizeContext -> RewriteContext
switchContext DataizeContext {..} = RewriteContext _program _maxDepth _buildTerm 0

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

formation :: [Binding] -> DataizeContext -> IO (Maybe Expression)
formation bds ctx = do
  let (lambda, bds') = maybeLambda bds
  case lambda of
    Just (BiLambda func) -> do
      obj' <- atom func (ExFormation bds') ctx
      case obj' of
        Just obj -> pure (Just obj)
        _ -> pure Nothing
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

withTail :: Expression -> DataizeContext -> IO (Maybe Expression)
withTail (ExApplication (ExFormation _) _) _ = pure Nothing
withTail (ExApplication (ExDispatch ExGlobal _) _) _ = pure Nothing
withTail (ExApplication expr tau) ctx = do
  exp' <- withTail expr ctx
  case exp' of
    Just exp -> pure (Just (ExApplication exp tau))
    _ -> pure Nothing
withTail (ExDispatch (ExFormation bds) attr) ctx = do
  obj' <- formation bds ctx
  case obj' of
    Just obj -> pure (Just (ExDispatch obj attr))
    _ -> pure Nothing
withTail (ExFormation bds) ctx = formation bds ctx
withTail (ExDispatch (ExDispatch ExGlobal (AtLabel label)) attr) (DataizeContext {_program = Program expr}) = case phiDispatch label expr of
  Just obj -> pure (Just (ExDispatch obj attr))
  _ -> pure Nothing
withTail (ExDispatch ExGlobal (AtLabel label)) (DataizeContext {_program = Program expr}) = pure (phiDispatch label expr)
withTail (ExDispatch expr attr) ctx = do
  exp' <- withTail expr ctx
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
morph :: Expression -> DataizeContext -> IO (Maybe Expression)
morph ExTermination _ = pure (Just ExTermination) -- PRIM
morph (ExFormation bds) ctx = do
  resolved <- withTail (ExFormation bds) ctx
  case resolved of
    Just obj -> morph obj ctx -- LAMBDA or PHI
    _ -> pure (Just (ExFormation bds)) -- PRIM
morph expr ctx = do
  resolved <- withTail expr ctx
  case resolved of
    Just obj -> morph obj ctx
    _ ->
      if isNF expr (RuleContext (_program ctx) (_buildTerm ctx))
        then pure Nothing
        else do
          (Program expr') <- rewrite' (Program expr) normalizationRules (switchContext ctx) -- NMZ
          morph expr' ctx

-- The goal of 'dataize' function is retrieve bytes from given expression.
--
-- DELTA: D(e) -> data                          if e = [B1, Δ -> data, B2]
-- BOX:   D([B1, 𝜑 -> e, B2]) -> D(С(e))        if [B1,B2] has no delta/lambda, where С(e) - contextualization
-- NORM:  D(e1) -> D(e2)                        if e2 := M(e1) and e1 is not primitive
--        nothing                               otherwise
dataize :: Program -> DataizeContext -> IO (Maybe Bytes)
dataize (Program expr) = dataize' expr

dataize' :: Expression -> DataizeContext -> IO (Maybe Bytes)
dataize' ExTermination _ = pure Nothing
dataize' (ExFormation bds) ctx = case maybeDelta bds of
  (Just (BiDelta bytes), _) -> pure (Just bytes)
  (Nothing, _) -> case maybePhi bds of
    (Just (BiTau AtPhi expr), bds') -> case maybeLambda bds' of
      (Just (BiLambda _), _) -> throwIO (userError "The 𝜑 and λ can't be present in formation at the same time")
      (_, _) ->
        let expr' = contextualize expr (ExFormation bds) (_program ctx)
         in dataize' expr' ctx
    (Nothing, _) -> case maybeLambda bds of
      (Just (BiLambda _), _) -> do
        morphed' <- morph (ExFormation bds) ctx
        case morphed' of
          Just morphed -> dataize' morphed ctx
          _ -> pure Nothing
      (Nothing, _) -> pure Nothing
dataize' expr prog = do
  morphed' <- morph expr prog
  case morphed' of
    Just morphed -> dataize' morphed prog
    _ -> pure Nothing

toDouble :: Integer -> Double
toDouble = fromIntegral

atom :: String -> Expression -> DataizeContext -> IO (Maybe Expression)
atom "L_org_eolang_number_plus" self ctx = do
  left <- dataize' (ExDispatch self (AtLabel "x")) ctx
  right <- dataize' (ExDispatch self AtRho) ctx
  case (left, right) of
    (Just left', Just right') -> do
      let first = either toDouble id (btsToNum left')
          second = either toDouble id (btsToNum right')
          sum = first + second
      pure (Just (DataNumber (numToBts sum)))
    _ -> pure Nothing
atom "L_org_eolang_number_times" self ctx = do
  left <- dataize' (ExDispatch self (AtLabel "x")) ctx
  right <- dataize' (ExDispatch self AtRho) ctx
  case (left, right) of
    (Just left', Just right') -> do
      let first = either toDouble id (btsToNum left')
          second = either toDouble id (btsToNum right')
          sum = first * second
      pure (Just (DataNumber (numToBts sum)))
    _ -> pure Nothing
atom "L_org_eolang_number_eq" self ctx = do
  x <- dataize' (ExDispatch self (AtLabel "x")) ctx
  rho <- dataize' (ExDispatch self AtRho) ctx
  case (x, rho) of
    (Just x', Just rho') -> do
      let self' = either toDouble id (btsToNum rho')
          first = either toDouble id (btsToNum x')
      if self' == first
        then pure (Just (DataNumber (numToBts first)))
        else pure (Just (ExDispatch self (AtLabel "y")))
    _ -> pure Nothing
atom func _ _ = throwIO (userError (printf "Atom '%s' does not exist" func))