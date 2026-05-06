{-# LANGUAGE LambdaCase #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to traverse given AST and build substitutions
-- from meta variables to appropriate meta values
module Matcher where

import AST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)

-- Meta value
-- The right part of substitution
data MetaValue
  = MvAttribute Attribute -- !a
  | MvBytes Bytes -- !b
  | MvBindings [Binding] -- !B
  | MvFunction Text -- !F
  | MvExpression Expression Expression -- !e, the second expression is scope, which is closest formation
  | MvTail [Tail] -- !t
  deriving (Eq, Show)

-- Tail operation after expression
-- Dispatch or application
data Tail
  = TaApplication Binding -- BiTau only
  | TaDispatch Attribute
  deriving (Eq, Show)

-- Substitution
-- Shows the match of meta name to meta value. The first field is the
-- matched target subexpression: it's set by `matchExpressionDeep` (the only
-- caller that knows which subexpression of the program fired) and read by
-- the Rewriter to know where to apply the replacement. Inner matchers leave
-- it Nothing. The field is invisible to user templates: the Builder never
-- reads it and the Printer never prints it.
data Subst = Subst (Maybe Expression) (Map Text MetaValue)
  deriving (Eq, Show)

substTarget :: Subst -> Maybe Expression
substTarget (Subst t _) = t

-- Empty substitution
substEmpty :: Subst
substEmpty = Subst Nothing Map.empty

-- Singleton substitution with one (key -> value) pair
substSingle :: Text -> MetaValue -> Subst
substSingle key value = Subst Nothing (Map.singleton key value)

-- Attach the matched target to a substitution.
withTarget :: Expression -> Subst -> Subst
withTarget tgt (Subst _ mp) = Subst (Just tgt) mp

defaultScope :: Expression
defaultScope = ExFormation [BiVoid AtRho]

-- Combine two substitutions into a single one
-- Fails if values by the same keys are not equal
-- The matched-target field is preserved from whichever side has it set;
-- if both are set, they must agree (this can't actually happen because only
-- `matchExpressionDeep` sets the target and `combine` is only called below
-- that level).
combine :: Subst -> Subst -> Maybe Subst
combine (Subst ta a) (Subst tb b) = case combineTargets ta tb of
  Nothing -> Nothing
  Just t -> combine' (Map.toList b) a t
  where
    combineTargets :: Maybe Expression -> Maybe Expression -> Maybe (Maybe Expression)
    combineTargets Nothing y = Just y
    combineTargets x Nothing = Just x
    combineTargets (Just x) (Just y)
      | x == y = Just (Just x)
      | otherwise = Nothing
    combine' :: [(Text, MetaValue)] -> Map Text MetaValue -> Maybe Expression -> Maybe Subst
    combine' [] acc t = Just (Subst t acc)
    combine' ((key, MvExpression tgt scope) : rest) acc t = case Map.lookup key acc of
      Just (MvExpression expr' _)
        | expr' == tgt -> combine' rest acc t
        | otherwise -> Nothing
      Just _ -> Nothing
      Nothing -> combine' rest (Map.insert key (MvExpression tgt scope) acc) t
    combine' ((key, value) : rest) acc t = case Map.lookup key acc of
      Just found
        | found == value -> combine' rest acc t
        | otherwise -> Nothing
      Nothing -> combine' rest (Map.insert key value acc) t

combineMany :: [Subst] -> [Subst] -> [Subst]
combineMany xs xy = catMaybes [combine x y | x <- xs, y <- xy]

-- Match a meta with optional name: anonymous metas (Nothing) match without
-- adding any binding; named metas (Just) bind the value under that name.
substMeta :: Maybe Text -> MetaValue -> Subst
substMeta Nothing _ = substEmpty
substMeta (Just key) value = substSingle key value

matchAttribute :: Attribute -> Attribute -> [Subst]
matchAttribute (AtMeta meta) tgt = [substMeta meta (MvAttribute tgt)]
matchAttribute ptn tgt
  | ptn == tgt = [substEmpty]
  | otherwise = []

matchBinding :: Binding -> Binding -> Expression -> [Subst]
matchBinding (BiVoid pattr) (BiVoid tattr) _ = matchAttribute pattr tattr
matchBinding (BiDelta (BtMeta meta)) (BiDelta tdata) _ = [substMeta meta (MvBytes tdata)]
matchBinding (BiDelta pdata) (BiDelta tdata) _
  | pdata == tdata = [substEmpty]
  | otherwise = []
matchBinding (BiLambda pFunc) (BiLambda tFunc) _
  | pFunc == tFunc = [substEmpty]
  | otherwise = []
matchBinding (BiMetaLambda meta) (BiLambda tFunc) _ = [substMeta meta (MvFunction tFunc)]
matchBinding (BiTau pattr pexp) (BiTau tattr texp) scope = combineMany (matchAttribute pattr tattr) (matchExpression' pexp texp scope)
matchBinding _ _ _ = []

-- Match bindings with ordering
matchBindings :: [Binding] -> [Binding] -> Expression -> [Subst]
matchBindings [] [] _ = [substEmpty]
matchBindings [] _ _ = []
matchBindings ((BiMeta name) : pbs) tbs scope =
  let splits = [splitAt idx tbs | idx <- [0 .. length tbs]]
   in catMaybes
        [ combine (substMeta name (MvBindings before)) subst
        | (before, after) <- splits
        , subst <- matchBindings pbs after scope
        ]
matchBindings (pb : pbs) (tb : tbs) scope = combineMany (matchBinding pb tb scope) (matchBindings pbs tbs scope)
matchBindings _ _ _ = []

-- Recursively go through given target expression and try to find
-- the head expression which matches to given pattern.
-- If there's one - build the list of all the tail operations after head expression.
-- The tail operations may be only dispatches or applications
tailExpressions :: Expression -> Expression -> Expression -> ([Subst], [Tail])
tailExpressions ptn tgt scope = case tailExpressionsReversed ptn tgt of
  Just (substs, tails) -> (substs, reverse tails)
  _ -> ([], [])
  where
    tailExpressionsReversed :: Expression -> Expression -> Maybe ([Subst], [Tail])
    tailExpressionsReversed ptn' tgt' = case matchExpression' ptn' tgt' scope of
      [] -> case tgt' of
        ExDispatch expr attr -> do
          (substs, tails) <- tailExpressionsReversed ptn' expr
          Just (substs, TaDispatch attr : tails)
        ExApplication expr tau -> do
          (substs, tails@(t : _)) <- tailExpressionsReversed ptn' expr
          if not (null tails) && isDispatch t
            then Just (substs, TaApplication tau : tails)
            else Nothing
          where
            isDispatch :: Tail -> Bool
            isDispatch = \case
              TaDispatch _ -> True
              TaApplication _ -> False
        _ -> Just ([], [])
      substs -> Just (substs, [])

matchExpression' :: Expression -> Expression -> Expression -> [Subst]
matchExpression' (ExMeta meta) tgt scope = [substMeta meta (MvExpression tgt scope)]
matchExpression' ExThis ExThis _ = [substEmpty]
matchExpression' ExGlobal ExGlobal _ = [substEmpty]
matchExpression' ExTermination ExTermination _ = [substEmpty]
matchExpression' (ExFormation pbs) (ExFormation tbs) _ = matchBindings pbs tbs (ExFormation tbs)
matchExpression' (ExDispatch pexp pattr) (ExDispatch texp tattr) scope = combineMany (matchAttribute pattr tattr) (matchExpression' pexp texp scope)
matchExpression' (ExApplication pexp pbd) (ExApplication texp tbd) scope = combineMany (matchExpression' pexp texp scope) (matchBinding pbd tbd scope)
matchExpression' (ExMetaTail expr meta) tgt scope = case tailExpressions expr tgt scope of
  ([], _) -> []
  (substs, tails) -> combineMany substs [substMeta meta (MvTail tails)]
matchExpression' (ExPhiAgain prefix idx expr) (ExPhiAgain prefix' idx' expr') scope
  | prefix == prefix' && idx == idx' = matchExpression' expr expr' scope
  | otherwise = []
matchExpression' (ExPhiMeet prefix idx expr) (ExPhiMeet prefix' idx' expr') scope
  | prefix == prefix' && idx == idx' = matchExpression' expr expr' scope
  | otherwise = []
matchExpression' _ _ _ = []

-- Deep match pattern to expression inside binding. Each returned Subst is
-- decorated with the matched target subexpression so the Rewriter knows
-- where to apply the replacement.
matchBindingExpression :: Binding -> Expression -> Expression -> [Subst]
matchBindingExpression (BiTau _ expr) ptn scope = matchExpressionDeep ptn expr scope
matchBindingExpression _ _ _ = []

-- Match expression with deep nested expression(s) matching. Inner matchers
-- only know how to bind metas; the matched target subexpression — required
-- by the Rewriter to position the replacement — is attached here, the only
-- place that knows which fragment of the program fired. Anonymous metas
-- leave no entry in the substitution, so the Rewriter cannot reconstruct
-- the matched subexpression by re-building the pattern from the
-- substitution alone — hence the target travels in the Subst itself.
matchExpressionDeep :: Expression -> Expression -> Expression -> [Subst]
matchExpressionDeep ptn tgt scope =
  let here = map (withTarget tgt) (matchExpression' ptn tgt scope)
      deep = case tgt of
        ExFormation bds -> concatMap (\bd -> matchBindingExpression bd ptn tgt) bds
        ExDispatch expr _ -> matchExpressionDeep ptn expr scope
        ExApplication expr tau -> matchExpressionDeep ptn expr scope ++ matchBindingExpression tau ptn scope
        _ -> []
   in here ++ deep

matchExpression :: Expression -> Expression -> [Subst]
matchExpression ptn tgt = matchExpressionDeep ptn tgt defaultScope

matchProgram :: Expression -> Program -> [Subst]
matchProgram ptn (Program expr) = matchExpression ptn expr
