{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Forks (Refusal (..), Side (..), forked, joined) where

import AST (Argument (..), Attribute (AtDelta, AtLabel, AtLambda, AtRho), Binding (..), Bytes, Expression (ExApplication, ExDispatch, ExFormation, ExPhiAgain, ExPhiMeet, ExTermination, ExXi), Function (FnSymbol), attributeFromBinding, symbols)
import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.Bifunctor (bimap, first)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

type Joining = (Int, Map (Maybe Int, Maybe Int) Int, [(Int, (Maybe Int, Maybe Int))])

data Side = Lefthand | Righthand
  deriving (Eq, Show)

data Refusal
  = Unnamed
  | Unformed Side
  | Holding Side Attribute
  deriving (Eq, Show)

joined :: Expression -> Expression -> Int -> Either [Attribute] (Expression, [(Int, (Maybe Int, Maybe Int))], Int)
joined left right spent = (\(term, (spent', _, made)) -> (term, reverse made, spent')) <$> goExpr left right (spent, Map.empty, [])
  where
    goExpr :: Expression -> Expression -> Joining -> Either [Attribute] (Expression, Joining)
    goExpr (ExFormation one) (ExFormation two) joining = first ExFormation <$> goBindings one two joining
    goExpr (ExApplication one arg) (ExApplication two arg') joining = within $ do
      (target, joining') <- goExpr one two joining
      first (ExApplication target) <$> goArgument arg arg' joining'
    goExpr (ExDispatch one attr) (ExDispatch two attr') joining
      | attr == attr' = within (first (`ExDispatch` attr) <$> goExpr one two joining)
    goExpr (ExPhiMeet prefix idx one) (ExPhiMeet prefix' idx' two) joining
      | prefix == prefix' && idx == idx' = within (first (ExPhiMeet prefix idx) <$> goExpr one two joining)
    goExpr (ExPhiAgain prefix idx one) (ExPhiAgain prefix' idx' two) joining
      | prefix == prefix' && idx == idx' = within (first (ExPhiAgain prefix idx) <$> goExpr one two joining)
    goExpr ExTermination ExTermination joining = Right (ExTermination, joining)
    goExpr ExTermination two joining = faced (\idx -> (Nothing, Just idx)) two joining
    goExpr one ExTermination joining = faced (\idx -> (Just idx, Nothing)) one joining
    goExpr one two joining = same one two joining
    goBinding :: Binding -> Binding -> Joining -> Either [Attribute] (Binding, Joining)
    goBinding (BiTau attr one) (BiTau attr' two) joining
      | attr == attr' = bimap (attr :) (first (BiTau attr)) (goExpr one two joining)
    goBinding (BiLambda (FnSymbol one)) (BiLambda (FnSymbol two)) joining
      | one /= two = Right (first (BiLambda . FnSymbol) (picked (Just one, Just two) joining))
    goBinding one two joining = first (const (maybe [] pure (attributeFromBinding one))) (same one two joining)
    goArgument :: Argument -> Argument -> Joining -> Either [Attribute] (Argument, Joining)
    goArgument (ArTau attr one) (ArTau attr' two) joining
      | attr == attr' = first (ArTau attr) <$> goExpr one two joining
    goArgument (ArAlpha alpha one) (ArAlpha alpha' two) joining
      | alpha == alpha' = first (ArAlpha alpha) <$> goExpr one two joining
    goArgument one two joining = same one two joining
    goBindings :: [Binding] -> [Binding] -> Joining -> Either [Attribute] ([Binding], Joining)
    goBindings [] [] joining = Right ([], joining)
    goBindings (one : ones) (two : twos) joining
      | attributeFromBinding one == attributeFromBinding two = do
          (item, joining') <- goBinding one two joining
          first (item :) <$> goBindings ones twos joining'
    goBindings ones twos _ = Left (take 1 (absent ones twos ++ absent twos ones ++ mapMaybe attributeFromBinding ones))
    absent :: [Binding] -> [Binding] -> [Attribute]
    absent bds others = [attr | Just attr <- map attributeFromBinding (take 1 bds), attr `notElem` mapMaybe attributeFromBinding others]
    within :: Either [Attribute] (Expression, Joining) -> Either [Attribute] (Expression, Joining)
    within = first (const [])
    same :: (Eq item) => item -> item -> Joining -> Either [Attribute] (item, Joining)
    same one two joining
      | one == two = Right (one, joining)
      | otherwise = Left []
    faced :: (Int -> (Maybe Int, Maybe Int)) -> Expression -> Joining -> Either [Attribute] (Expression, Joining)
    faced pair side joining
      | null (symbols side) = Left []
      | otherwise = Right (runState (goFaced side) joining)
      where
        goFaced :: Expression -> State Joining Expression
        goFaced (ExFormation bds) = ExFormation <$> mapM goFacedBinding bds
        goFaced (ExApplication expr arg) = ExApplication <$> goFaced expr <*> goFacedArgument arg
        goFaced (ExDispatch expr attr) = (`ExDispatch` attr) <$> goFaced expr
        goFaced (ExPhiMeet prefix idx expr) = ExPhiMeet prefix idx <$> goFaced expr
        goFaced (ExPhiAgain prefix idx expr) = ExPhiAgain prefix idx <$> goFaced expr
        goFaced expr = pure expr
        goFacedBinding :: Binding -> State Joining Binding
        goFacedBinding (BiTau attr expr) = BiTau attr <$> goFaced expr
        goFacedBinding (BiLambda (FnSymbol idx)) = BiLambda . FnSymbol <$> state (picked (pair idx))
        goFacedBinding bd = pure bd
        goFacedArgument :: Argument -> State Joining Argument
        goFacedArgument (ArTau attr expr) = ArTau attr <$> goFaced expr
        goFacedArgument (ArAlpha alpha expr) = ArAlpha alpha <$> goFaced expr
    picked :: (Maybe Int, Maybe Int) -> Joining -> (Int, Joining)
    picked pair joining@(spent', names, made) = case Map.lookup pair names of
      Just name -> (name, joining)
      Nothing -> (succ spent', (succ spent', Map.insert pair (succ spent') names, (succ spent', pair) : made))

forked :: Function -> Either Int Bytes -> Expression -> Expression -> Either Refusal Expression
forked func condition left right = fabricated <$> formed Lefthand left <*> formed Righthand right
  where
    formed :: Side -> Expression -> Either Refusal [Binding]
    formed side (ExFormation bds) = maybe (Right bds) (Left . Holding side) (listToMaybe [attr | Just attr <- map attributeFromBinding bds, attr == AtDelta || attr == AtLambda])
    formed side _ = Left (Unformed side)
    fabricated :: [Binding] -> [Binding] -> Expression
    fabricated one two = ExFormation (map BiVoid voids ++ [copy "left🌵" left one, copy "right🌵" right two] ++ map branch (filter (`notElem` voids) (nub (attributes one ++ attributes two))))
      where
        voids :: [Attribute]
        voids = nub (vacant one ++ vacant two)
        branch :: Attribute -> Binding
        branch attr = BiTau attr (ExFormation ([BiVoid AtRho, BiLambda func] ++ map BiVoid (inner attr) ++ [BiTau (AtLabel "on🌵") (ExFormation [either (BiLambda . FnSymbol) BiDelta condition]), ask "left🌵" one attr, ask "right🌵" two attr]))
        inner :: Attribute -> [Attribute]
        inner attr = nub (concat (mapMaybe (`held` attr) [one, two]))
        ask :: Text -> [Binding] -> Attribute -> Binding
        ask name bds attr = BiTau (AtLabel name) (applied (ExDispatch (ExDispatch (ExDispatch ExXi AtRho) (AtLabel name)) attr) (fromMaybe (inner attr) (held bds attr)))
    vacant :: [Binding] -> [Attribute]
    vacant bds = [attr | BiVoid attr <- bds, own attr]
    attributes :: [Binding] -> [Attribute]
    attributes bds = [attr | Just attr <- map attributeFromBinding bds, own attr]
    own :: Attribute -> Bool
    own (AtLabel label) = not ("🌵" `T.isSuffixOf` label)
    own attr = attr `notElem` [AtRho, AtLambda, AtDelta]
    copy :: Text -> Expression -> [Binding] -> Binding
    copy name side bds = BiTau (AtLabel name) (applied side (vacant bds))
    applied :: Expression -> [Attribute] -> Expression
    applied = foldl (\term attr -> ExApplication term (ArTau attr (ExDispatch ExXi attr)))
    held :: [Binding] -> Attribute -> Maybe [Attribute]
    held bds attr = listToMaybe [vacant inside | BiTau label (ExFormation inside) <- bds, label == attr]
