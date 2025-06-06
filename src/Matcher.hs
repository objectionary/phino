-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to traverse given Ast and build substitutions
-- from meta variables to appropriate meta values
module Matcher where

import Ast
import Data.List (findIndex, partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Sequence (foldlWithIndex)
import Misc

-- Meta value
-- The right part of substitution
data MetaValue
  = MvAttribute Attribute -- !a
  | MvBytes String -- !b
  | MvBindings [Binding] -- !B
  | MvFunction String -- !F
  | MvExpression Expression -- !e
  | MvTail [Tail] -- !t
  deriving (Eq, Show)

-- Tail operation after expression
-- Dispatch or application
data Tail
  = TaApplication Binding -- BiTau only
  | TaDispatch Attribute
  deriving (Eq, Show)

-- Substitution
-- Shows the match of meta name to meta value
newtype Subst = Subst (Map String MetaValue)
  deriving (Eq, Show)

-- Empty substitution
substEmpty :: Subst
substEmpty = Subst Map.empty

-- Singleton substitution with one (key -> value) pair
substSingle :: String -> MetaValue -> Subst
substSingle key value = Subst (Map.singleton key value)

-- Combine two substitutions into a single one
-- Fails if values by the same keys are not equal
combine :: Subst -> Subst -> Maybe Subst
combine (Subst a) (Subst b) = combine' (Map.toList b) a
  where
    combine' :: [(String, MetaValue)] -> Map String MetaValue -> Maybe Subst
    combine' [] acc = Just (Subst acc)
    combine' ((key, value) : rest) acc = case Map.lookup key acc of
      Just found
        | found == value -> combine' rest acc
        | otherwise -> Nothing
      Nothing -> combine' rest (Map.insert key value acc)

matchAttribute :: Attribute -> Attribute -> Maybe Subst
matchAttribute (AtMeta meta) tgt = Just (substSingle meta (MvAttribute tgt))
matchAttribute ptn tgt
  | ptn == tgt = Just substEmpty
  | otherwise = Nothing

matchBinding :: Binding -> Binding -> Maybe Subst
matchBinding (BiVoid pattr) (BiVoid tattr) = matchAttribute pattr tattr
matchBinding (BiDelta pbts) (BiDelta tbts)
  | pbts == tbts = Just substEmpty
  | otherwise = Nothing
matchBinding (BiMetaDelta meta) (BiDelta tBts) = Just (substSingle meta (MvBytes tBts))
matchBinding (BiLambda pFunc) (BiLambda tFunc)
  | pFunc == tFunc = Just substEmpty
  | otherwise = Nothing
matchBinding (BiMetaLambda meta) (BiLambda tFunc) = Just (substSingle meta (MvFunction tFunc))
matchBinding (BiTau pattr pexp) (BiTau tattr texp) = do
  mattr <- matchAttribute pattr tattr
  mexp <- matchExpression pexp texp
  combine mattr mexp
matchBinding _ _ = Nothing

-- Match bindings with ordering
-- Function returns tuple (X, Y), where
-- - X - "before bindings" - list of bindings before each exact binding. It's used to join with
--   meta binding which goes before exact binding, if such is exist. If there's no one,
--   it'll be []
-- - Y - Substitution for bindings
--
-- How it works:
-- We start process pattern bindings.
-- 1. If we meet meta binding (!B), we skip for now
--    and go the next recursive cycle with next pattern and wait for the result.
--    Result will contain list of "before bindings". This list will be
--    matched to current meta binding
-- 2. If we meet exact binding in pattern, like void, tau, delta, lambda, etc... we try to match it
--    with current target binding. If it matches, we go further and try to match next patterns with next targets.
--    If it does not match, there may be two options:
--    a) we came here from step 1. It means that we should skip this target binding and go the next
--       cycle. When we get the result, we join skipped target binding with returned list of "before" bindings
--       and return to the step one
--    b) we came from somewhere else. Then we just returns Nothing as substitution and don't go further
matchBindingsInOrder :: [Binding] -> [Binding] -> Bool -> ([Binding], Maybe Subst)
matchBindingsInOrder [] [] _ = ([], Just substEmpty)
matchBindingsInOrder [] tbs True = (tbs, Just substEmpty)
matchBindingsInOrder [] tbs False = ([], Nothing)
matchBindingsInOrder [BiMeta name] tbs _ = ([], Just (substSingle name (MvBindings tbs)))
matchBindingsInOrder ((BiMeta name) : pbs) tbs meta = case matchBindingsInOrder pbs tbs True of
  (_, Nothing) -> ([], Nothing)
  (before, Just subst) -> ([], combine (substSingle name (MvBindings before)) subst)
matchBindingsInOrder (pb : pbs) (tb : tbs) meta = case matchBinding pb tb of
  Nothing ->
    if meta
      then case matchBindingsInOrder (pb : pbs) tbs meta of
        (_, Nothing) -> ([], Nothing)
        (before, Just subst) -> (tb : before, Just subst)
      else ([], Nothing)
  Just subst -> case matchBindingsInOrder pbs tbs False of
    (_, Nothing) -> ([], Nothing)
    (before, Just subst') -> (before, combine subst subst')

-- Match pattern bindings to target bindings
-- !! Pattern bindings list may contain only one BiMeta binding
-- !! Pattern and target bindings may be placed in random order
--
-- If pattern bindings contains only BiMeta binding - all the target bindings are matched
matchBindings :: [Binding] -> [Binding] -> Maybe Subst
matchBindings [] [] = Just substEmpty
matchBindings pbs tbs = do
  let (_, subst) = matchBindingsInOrder pbs tbs False
  subst

-- Recursively go through given target expression and try to find
-- the head expression which matches to given pattern.
-- If there's one - build the list of all the tail operations after head expression.
-- The tail operations may be only dispatches or applications
tailExpressions :: Expression -> Expression -> Maybe (Subst, [Tail])
tailExpressions ptn tgt = do
  (subst, tails) <- tailExpressionsReversed ptn tgt
  return (subst, reverse tails)
  where
    tailExpressionsReversed :: Expression -> Expression -> Maybe (Subst, [Tail])
    tailExpressionsReversed ptn' tgt' = case matchExpression ptn' tgt' of
      Just subst -> Just (subst, [])
      Nothing -> case tgt' of
        ExDispatch expr attr -> do
          (subst, tails) <- tailExpressionsReversed ptn' expr
          return (subst, TaDispatch attr : tails)
        ExApplication expr tau -> do
          (subst, tails) <- tailExpressionsReversed ptn' expr
          return (subst, TaApplication tau : tails)
        _ -> Nothing

matchExpression :: Expression -> Expression -> Maybe Subst
matchExpression (ExMeta meta) tgt = Just (substSingle meta (MvExpression tgt))
matchExpression ExThis ExThis = Just substEmpty
matchExpression ExGlobal ExGlobal = Just substEmpty
matchExpression ExTermination ExTermination = Just substEmpty
matchExpression (ExFormation pbs) (ExFormation tbs) = matchBindings pbs tbs
matchExpression (ExDispatch pexp pattr) (ExDispatch texp tattr) = do
  mexp <- matchExpression pexp texp
  mattr <- matchAttribute pattr tattr
  combine mexp mattr
matchExpression (ExApplication pexp pbd) (ExApplication texp tbd) = do
  mexp <- matchExpression pexp texp
  mbs <- matchBinding pbd tbd
  combine mexp mbs
matchExpression (ExMetaTail exp meta) tgt = do
  (subst, tails) <- tailExpressions exp tgt
  combine subst (substSingle meta (MvTail tails))
matchExpression _ _ = Nothing

-- Match expression with deep nested expression(s) matching
matchExpressionDeep :: Expression -> Expression -> [Subst]
matchExpressionDeep ptn tgt = do
  let here = maybeToList (matchExpression ptn tgt)
      deep = case tgt of
        ExFormation bds -> concatMap matchBindingExpression bds
        ExDispatch dexp _ -> matchExpressionDeep ptn dexp
        ExApplication aexp tau -> matchExpressionDeep ptn aexp ++ matchBindingExpression tau
        _ -> []
        where
          -- Deep match pattern to expression inside binding
          matchBindingExpression :: Binding -> [Subst]
          matchBindingExpression (BiTau _ texp) = matchExpressionDeep ptn texp
          matchBindingExpression _ = []
  case here of
    [] -> deep
    found : _ -> found : deep

matchProgram :: Expression -> Program -> [Subst]
matchProgram ptn (Program exp) = matchExpressionDeep ptn exp
