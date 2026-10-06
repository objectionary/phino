{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Matcher where

import AST
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)

data MetaValue
  = MvAttribute Attribute
  | MvIndex Int
  | MvBytes Bytes
  | MvBindings [Binding]
  | MvFunction Function
  | MvExpression Expression
  deriving (Eq, Show)

data Meta
  = Named Text
  | Anon Slot
  deriving (Eq, Ord, Show)

newtype Subst = Subst (Map Meta MetaValue)
  deriving (Eq, Show)

type MatchExpressionFunc = Expression -> Expression -> [Subst]

substEmpty :: Subst
substEmpty = Subst Map.empty

substSingle :: Text -> MetaValue -> Subst
substSingle key value = Subst (Map.singleton (Named key) value)

substSlot :: Slot -> MetaValue -> Subst
substSlot slot value = Subst (Map.singleton (Anon slot) value)

combine :: Subst -> Subst -> Maybe Subst
combine (Subst a) (Subst b) = go (Map.toList b) a
  where
    go :: [(Meta, MetaValue)] -> Map Meta MetaValue -> Maybe Subst
    go [] acc = Just (Subst acc)
    go ((key, value) : rest) acc = case Map.lookup key acc of
      Just found
        | found == value -> go rest acc
        | otherwise -> Nothing
      Nothing -> go rest (Map.insert key value acc)

combineMany :: [Subst] -> [Subst] -> [Subst]
combineMany xs xy = catMaybes [combine x y | x <- xs, y <- xy]

matchAttribute :: Attribute -> Attribute -> [Subst]
matchAttribute (AtMeta meta) tgt = [substSingle meta (MvAttribute tgt)]
matchAttribute (AtAny slot) tgt = [substSlot slot (MvAttribute tgt)]
matchAttribute ptn tgt
  | ptn == tgt = [substEmpty]
  | otherwise = []

matchAlpha :: Alpha -> Alpha -> [Subst]
matchAlpha (AlMeta meta) (Alpha idx) = [substSingle meta (MvIndex idx)]
matchAlpha (AlAny slot) (Alpha idx) = [substSlot slot (MvIndex idx)]
matchAlpha ptn tgt
  | ptn == tgt = [substEmpty]
  | otherwise = []

matchFunction :: Function -> Function -> [Subst]
matchFunction (FnMeta meta) tgt
  | named tgt = [substSingle meta (MvFunction tgt)]
matchFunction (FnAny slot) tgt
  | named tgt = [substSlot slot (MvFunction tgt)]
matchFunction ptn tgt
  | ptn == tgt = [substEmpty]
  | otherwise = []

named :: Function -> Bool
named (Function _) = True
named (FnSymbol _) = True
named _ = False

matchBinding :: Binding -> Binding -> [Subst]
matchBinding (BiVoid pattr) (BiVoid tattr) = matchAttribute pattr tattr
matchBinding (BiDelta (BtMeta meta)) (BiDelta tdata) = [substSingle meta (MvBytes tdata)]
matchBinding (BiDelta (BtAny slot)) (BiDelta tdata) = [substSlot slot (MvBytes tdata)]
matchBinding (BiDelta pdata) (BiDelta tdata)
  | pdata == tdata = [substEmpty]
  | otherwise = []
matchBinding (BiLambda pFunc) (BiLambda tFunc) = matchFunction pFunc tFunc
matchBinding (BiTau pattr pexp) (BiTau tattr texp) = combineMany (matchAttribute pattr tattr) (matchExpression' pexp texp)
matchBinding _ _ = []

matchArgument :: Argument -> Argument -> [Subst]
matchArgument (ArTau pattr pexp) (ArTau tattr texp) = combineMany (matchAttribute pattr tattr) (matchExpression' pexp texp)
matchArgument (ArAlpha palpha pexp) (ArAlpha talpha texp) = combineMany (matchAlpha palpha talpha) (matchExpression' pexp texp)
matchArgument _ _ = []

matchBindings :: [Binding] -> [Binding] -> [Subst]
matchBindings [] [] = [substEmpty]
matchBindings [] _ = []
matchBindings ((BiMeta name) : pbs) tbs = matchBindingsMeta (substSingle name) pbs tbs
matchBindings ((BiAny slot) : pbs) tbs = matchBindingsMeta (substSlot slot) pbs tbs
matchBindings (pb : pbs) (tb : tbs) = combineMany (matchBinding pb tb) (matchBindings pbs tbs)
matchBindings _ _ = []

matchBindingsMeta :: (MetaValue -> Subst) -> [Binding] -> [Binding] -> [Subst]
matchBindingsMeta bind [] tbs = [bind (MvBindings tbs)]
matchBindingsMeta bind pbs tbs = go [] tbs
  where
    go :: [Binding] -> [Binding] -> [Subst]
    go before after =
      catMaybes [combine (bind (MvBindings (reverse before))) subst | subst <- matchBindings pbs after]
        ++ case after of
          [] -> []
          (tb : rest) -> go (tb : before) rest

matchExpression' :: MatchExpressionFunc
matchExpression' (ExMeta meta) tgt = [substSingle meta (MvExpression tgt)]
matchExpression' (ExAny slot) tgt = [substSlot slot (MvExpression tgt)]
matchExpression' ExXi ExXi = [substEmpty]
matchExpression' ExRoot ExRoot = [substEmpty]
matchExpression' ExTermination ExTermination = [substEmpty]
matchExpression' (ExFormation pbs) (ExFormation tbs) = matchBindings pbs tbs
matchExpression' (ExDispatch pexp pattr) (ExDispatch texp tattr) = combineMany (matchAttribute pattr tattr) (matchExpression' (pinned pattr tattr pexp) texp)
matchExpression' (ExApplication pexp parg@(ArTau pattr _)) (ExApplication texp targ@(ArTau tattr _)) = combineMany (matchExpression' (pinned pattr tattr pexp) texp) (matchArgument parg targ)
matchExpression' (ExApplication pexp parg) (ExApplication texp targ) = combineMany (matchExpression' pexp texp) (matchArgument parg targ)
matchExpression' (ExPhiAgain prefix idx expr) (ExPhiAgain prefix' idx' expr')
  | prefix == prefix' && idx == idx' = matchExpression' expr expr'
  | otherwise = []
matchExpression' (ExPhiMeet prefix idx expr) (ExPhiMeet prefix' idx' expr')
  | prefix == prefix' && idx == idx' = matchExpression' expr expr'
  | otherwise = []
matchExpression' _ _ = []

pinned :: Attribute -> Attribute -> Expression -> Expression
pinned (AtMeta meta) tattr = goExpr
  where
    goExpr :: Expression -> Expression
    goExpr (ExFormation bds) = ExFormation (map goBinding bds)
    goExpr (ExDispatch expr attr) = ExDispatch (goExpr expr) (goAttribute attr)
    goExpr (ExApplication expr (ArTau attr arg)) = ExApplication (goExpr expr) (ArTau (goAttribute attr) (goExpr arg))
    goExpr (ExApplication expr (ArAlpha alpha arg)) = ExApplication (goExpr expr) (ArAlpha alpha (goExpr arg))
    goExpr expr = expr
    goBinding :: Binding -> Binding
    goBinding (BiTau attr expr) = BiTau (goAttribute attr) (goExpr expr)
    goBinding (BiVoid attr) = BiVoid (goAttribute attr)
    goBinding bd = bd
    goAttribute :: Attribute -> Attribute
    goAttribute (AtMeta meta')
      | meta' == meta = tattr
    goAttribute attr = attr
pinned _ _ = id

matchExpressionDeep :: MatchExpressionFunc
matchExpressionDeep = matchExpressionDeep' False

matchExpressionDeep' :: Bool -> MatchExpressionFunc
matchExpressionDeep' redex ptn tgt = go tgt []
  where
    go :: Expression -> [Subst] -> [Subst]
    go expr rest
      | redex && inert expr = rest
      | fitting ptn expr = matchExpression' ptn expr ++ below expr rest
      | otherwise = below expr rest
    below :: Expression -> [Subst] -> [Subst]
    below (ExFormation bds) rest = foldr inside rest bds
    below (ExDispatch expr _) rest = go expr rest
    below (ExApplication expr (ArTau _ arg)) rest = go expr (go arg rest)
    below (ExApplication expr (ArAlpha _ arg)) rest = go expr (go arg rest)
    below _ rest = rest
    inside :: Binding -> [Subst] -> [Subst]
    inside (BiTau _ expr) rest = go expr rest
    inside _ rest = rest

matchExpression :: MatchExpressionFunc
matchExpression = matchExpressionDeep

reachable :: Expression -> Expression -> Bool
reachable = reachable' False

reachable' :: Bool -> Expression -> Expression -> Bool
reachable' redex ptn = go
  where
    go :: Expression -> Bool
    go tgt
      | redex && inert tgt = False
      | otherwise =
          fitting ptn tgt || case tgt of
            ExFormation bds -> any inside bds
            ExDispatch expr _ -> go expr
            ExApplication expr (ArTau _ arg) -> go expr || go arg
            ExApplication expr (ArAlpha _ arg) -> go expr || go arg
            _ -> False
    inside :: Binding -> Bool
    inside (BiTau _ expr) = go expr
    inside _ = False

fitting :: Expression -> Expression -> Bool
fitting = go
  where
    go :: Expression -> Expression -> Bool
    go (ExMeta _) _ = True
    go (ExAny _) _ = True
    go ExXi ExXi = True
    go ExRoot ExRoot = True
    go ExTermination ExTermination = True
    go (ExFormation pbs) (ExFormation tbs) = all (\pbd -> loose pbd || any (kin pbd) tbs) pbs
    go (ExDispatch pexp pattr) (ExDispatch texp tattr) = same pattr tattr && go pexp texp
    go (ExApplication pexp (ArTau pattr _)) (ExApplication texp (ArTau tattr _)) = same pattr tattr && go pexp texp
    go (ExApplication pexp (ArAlpha _ _)) (ExApplication texp (ArAlpha _ _)) = go pexp texp
    go (ExPhiAgain{}) (ExPhiAgain{}) = True
    go (ExPhiMeet{}) (ExPhiMeet{}) = True
    go _ _ = False
    loose :: Binding -> Bool
    loose (BiMeta _) = True
    loose (BiAny _) = True
    loose _ = False
    kin :: Binding -> Binding -> Bool
    kin (BiTau pattr _) (BiTau tattr _) = same pattr tattr
    kin (BiVoid pattr) (BiVoid tattr) = same pattr tattr
    kin (BiLambda _) (BiLambda _) = True
    kin (BiDelta _) (BiDelta _) = True
    kin _ _ = False
    same :: Attribute -> Attribute -> Bool
    same (AtMeta _) _ = True
    same (AtAny _) _ = True
    same pattr tattr = pattr == tattr

sites :: forall a. Bool -> (Expression -> [a]) -> Expression -> [(Expression, a)]
sites redex rule tgt = go tgt []
  where
    go :: Expression -> [(Expression, a)] -> [(Expression, a)]
    go expr rest
      | redex && inert expr = rest
      | otherwise = map (expr,) (rule expr) ++ below expr rest
    below :: Expression -> [(Expression, a)] -> [(Expression, a)]
    below (ExFormation bds) rest = foldr inside rest bds
    below (ExDispatch expr _) rest = go expr rest
    below (ExApplication expr (ArTau _ arg)) rest = go expr (go arg rest)
    below (ExApplication expr (ArAlpha _ arg)) rest = go expr (go arg rest)
    below _ rest = rest
    inside :: Binding -> [(Expression, a)] -> [(Expression, a)]
    inside (BiTau _ expr) rest = go expr rest
    inside _ rest = rest

hits :: forall a. [(Int, Bool, Maybe Expression -> Expression -> [a])] -> Maybe Expression -> Expression -> [Int]
hits rules universe tgt = go [tgt] rules
  where
    go :: [Expression] -> [(Int, Bool, Maybe Expression -> Expression -> [a])] -> [Int]
    go [] _ = []
    go _ [] = []
    go (expr : rest) pending
      | inert expr && and [redex | (_, redex, _) <- pending] = go rest pending
      | otherwise = case [idx | (idx, redex, rule) <- pending, not (redex && inert expr), not (null (rule universe expr))] of
          [] -> go (below expr rest) pending
          met -> met ++ go (below expr rest) [rule | rule@(idx, _, _) <- pending, idx `notElem` met]
    below :: Expression -> [Expression] -> [Expression]
    below (ExFormation bds) rest = foldr inside rest bds
    below (ExDispatch expr _) rest = expr : rest
    below (ExApplication expr (ArTau _ arg)) rest = expr : arg : rest
    below (ExApplication expr (ArAlpha _ arg)) rest = expr : arg : rest
    below _ rest = rest
    inside :: Binding -> [Expression] -> [Expression]
    inside (BiTau _ expr) rest = expr : rest
    inside _ rest = rest

splits :: [Binding] -> [([Binding], [Binding])]
splits = go []
  where
    go :: [Binding] -> [Binding] -> [([Binding], [Binding])]
    go before after =
      (reverse before, after) : case after of
        [] -> []
        (bd : rest) -> go (bd : before) rest
