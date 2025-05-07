-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to traverse given Ast and build substitutions
-- from meta variables to appropriate meta values
module Matcher where

import Ast
import Misc
import Data.List (findIndex, partition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, maybeToList)
import Data.Sequence (foldlWithIndex)

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
  = TaApplication [Binding] -- BiTau only
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

-- Returns True if given binding contains meta attribute on the left:
-- !a -> ...
-- !a -> ?
isBindingWithMetaAttr :: Binding -> Bool
isBindingWithMetaAttr (BiTau (AtMeta _) _) = True
isBindingWithMetaAttr (BiVoid (AtMeta _)) = True
isBindingWithMetaAttr _ = False

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

-- Returns a tuple (X, Y) if given binding B is matched to any of bindings L
-- X is an index of matched B in L
-- Y is substiturion for matched B
-- (-1, Nothing) is returned in case of no binding is matched in L
matchAnyBinding :: Binding -> [Binding] -> (Int, Maybe Subst)
matchAnyBinding pb [] = (-1, Nothing)
matchAnyBinding pb tbs = matchAnyBindingWithIndex 0 tbs
  where
    matchAnyBindingWithIndex :: Integer -> [Binding] -> (Int, Maybe Subst)
    matchAnyBindingWithIndex idx [] = (-1, Nothing)
    matchAnyBindingWithIndex idx (tb : rest) = case matchBinding pb tb of
      Nothing -> matchAnyBindingWithIndex (idx + 1) rest
      subst -> (fromInteger idx, subst)

-- Match bindings with filtering target bindings
-- !! Bindings may be placed in random order
--
-- Returns a tuple (B, S) where:
-- B is a list of rest target bindings excluding matched ones
-- S is a substitution for matched bindings
-- In case of not matching - S as Nothing is returned
matchBindingsWithFiltering :: [Binding] -> [Binding] -> ([Binding], Maybe Subst)
matchBindingsWithFiltering [] [] = ([], Just substEmpty)
matchBindingsWithFiltering [] tbs = (tbs, Just substEmpty)
matchBindingsWithFiltering (pb : rest) tbs = case matchAnyBinding pb tbs of
  (-1, Nothing) -> (tbs, Nothing)
  (idx, Just subst) -> case matchBindingsWithFiltering rest (withoutAt idx tbs) of
    (bs, Just next) -> (bs, combine subst next)
    _ -> (tbs, Nothing)

-- Match non meta bindings
-- !! Pattern bindings don't contain BiMeta
-- !! Target and pattern bindings may be placed in random order
-- !! Lengths of pattern and target bindings must be the same
--
-- First it filters pattern bindings, only bindings without meta attributes are left (B)
-- If B are not empty, it means we have bindings B with exact attributes, like "attr", "foo", etc.
-- Since all the attributes in phi calculus are unique in the scope of formation or application
-- we try to match these exact bindings first. While matching them we drop target bindings which
-- we found a match for. When we're done with exact bindings, we have a list of target
-- unmached bindings (L). Now we're entering a new circle and trying to match rest "not exact"
-- pattern bindings with left unmatched bindings L. If matching is succeeded - we just merge
-- resut substitutions.
matchNonMetaExactBindings :: [Binding] -> [Binding] -> Maybe Subst
matchNonMetaExactBindings [] [] = Just substEmpty
matchNonMetaExactBindings pbs tbs
  | length tbs /= length pbs = Nothing
  | otherwise = case partition isBindingWithMetaAttr pbs of
      -- todo: for one pattern binding there may be more than one matches
      -- it's necessary to resolve it somehow
      -- current approach by finding first any match is not correct
      -- Example:
      -- pattern: [!x -> !e1, !a -> [!B]]
      -- target:  [z -> [a -> $], x -> Q]
      -- !x -> !e1 matches to both z -> [a -> $] and x -> Q, this z -> [a -> $] will found first
      -- but !a -> [!B] matches only to z -> [a -> $] which means !x -> !e1 should match to x -> Q
      (with, []) -> case matchBindingsWithFiltering with tbs of
        ([], Just subst) -> Just subst
        (_, _) -> Nothing
      (with, without) -> case matchBindingsWithFiltering without tbs of
        (rest, Just subst) -> case matchNonMetaExactBindings with rest of
          Just next -> combine subst next
          Nothing -> Nothing
        (_, Nothing) -> Nothing

-- The same as `matchNonMetaExactBindings` but without the "same length" requirement
matchNonMetaBindingsWithFiltering :: [Binding] -> [Binding] -> ([Binding], Maybe Subst)
matchNonMetaBindingsWithFiltering [] [] = ([], Just substEmpty)
matchNonMetaBindingsWithFiltering [] tbs = (tbs, Just substEmpty)
matchNonMetaBindingsWithFiltering pbs [] = ([], Nothing)
matchNonMetaBindingsWithFiltering pbs tbs
  | length pbs > length tbs = ([], Nothing)
  | otherwise = case partition isBindingWithMetaAttr pbs of
      -- todo: for one pattern binding there may be more than one matches
      -- it's necessary to resolve it somehow
      -- current approach by finding first any match is not correct
      -- Example:
      -- pattern: [!x -> !e1, !a -> [!B]]
      -- target:  [z -> [a -> $], x -> Q]
      -- !x -> !e1 matches to both z -> [a -> $] and x -> Q, this z -> [a -> $] will found first
      -- but !a -> [!B] matches only to z -> [a -> $] which means !x -> !e1 should match to x -> Q
      (with, []) -> case matchBindingsWithFiltering with tbs of
        (rest, Just subst) -> (rest, Just subst)
        (_, Nothing) -> ([], Nothing)
      (with, without) -> case matchBindingsWithFiltering without tbs of
        (rest, Just subst) -> case matchNonMetaBindingsWithFiltering with rest of
          (rest', Just next) -> (rest', combine subst next)
          (_, Nothing) -> ([], Nothing)
        (_, Nothing) -> ([], Nothing)

-- Match pattern bindings to target bindings
-- !! Pattern bindings list may contain only one BiMeta binding
-- !! Pattern and target bindings may be placed in random order
--
-- If pattern bindings contains only BiMeta binding - all the target bindings are matched
matchBindings :: [Binding] -> [Binding] -> Maybe Subst
matchBindings [] [] = Just substEmpty
matchBindings pbs tbs = case findIndex isMetaBinding pbs of
  Just idx -> do
    let (BiMeta name) = pbs !! idx
    if length pbs == 1
      then Just (substSingle name (MvBindings tbs))
      else case matchNonMetaBindingsWithFiltering (withoutAt idx pbs) tbs of
        (rest, Just subst) -> combine subst (substSingle name (MvBindings rest))
        (_, Nothing) -> Nothing
  _ -> matchNonMetaExactBindings pbs tbs

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
        ExApplication expr bds -> do
          (subst, tails) <- tailExpressionsReversed ptn' expr
          return (subst, TaApplication bds : tails)
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
matchExpression (ExApplication pexp pbs) (ExApplication texp tbs) = do
  mexp <- matchExpression pexp texp
  mbs <- matchBindings pbs tbs
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
        ExApplication aexp taus -> matchExpressionDeep ptn aexp ++ concatMap matchBindingExpression taus
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
