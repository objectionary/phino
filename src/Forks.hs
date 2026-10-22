{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Forks (forked, joined) where

import AST (Argument (..), Attribute (AtDelta, AtLabel, AtLambda, AtRho), Binding (..), Bytes, Expression (ExApplication, ExDispatch, ExFormation, ExPhiAgain, ExPhiMeet), Function (FnSymbol), attributeFromBinding)
import Data.Bifunctor (first)
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Joining = (Int, Map (Int, Int) Int, [(Int, (Int, Int))])

joined :: Expression -> Expression -> Int -> Maybe (Expression, [(Int, (Int, Int))], Int)
joined left right spent = (\(term, (spent', _, made)) -> (term, reverse made, spent')) <$> goExpr left right (spent, Map.empty, [])
  where
    goExpr :: Expression -> Expression -> Joining -> Maybe (Expression, Joining)
    goExpr (ExFormation one) (ExFormation two) joining = first ExFormation <$> goList goBinding one two joining
    goExpr (ExApplication one arg) (ExApplication two arg') joining = do
      (target, joining') <- goExpr one two joining
      first (ExApplication target) <$> goArgument arg arg' joining'
    goExpr (ExDispatch one attr) (ExDispatch two attr') joining
      | attr == attr' = first (`ExDispatch` attr) <$> goExpr one two joining
    goExpr (ExPhiMeet prefix idx one) (ExPhiMeet prefix' idx' two) joining
      | prefix == prefix' && idx == idx' = first (ExPhiMeet prefix idx) <$> goExpr one two joining
    goExpr (ExPhiAgain prefix idx one) (ExPhiAgain prefix' idx' two) joining
      | prefix == prefix' && idx == idx' = first (ExPhiAgain prefix idx) <$> goExpr one two joining
    goExpr one two joining = same one two joining
    goBinding :: Binding -> Binding -> Joining -> Maybe (Binding, Joining)
    goBinding (BiTau attr one) (BiTau attr' two) joining
      | attr == attr' = first (BiTau attr) <$> goExpr one two joining
    goBinding (BiLambda (FnSymbol one)) (BiLambda (FnSymbol two)) joining
      | one /= two = Just (first (BiLambda . FnSymbol) (picked (one, two) joining))
    goBinding one two joining = same one two joining
    goArgument :: Argument -> Argument -> Joining -> Maybe (Argument, Joining)
    goArgument (ArTau attr one) (ArTau attr' two) joining
      | attr == attr' = first (ArTau attr) <$> goExpr one two joining
    goArgument (ArAlpha alpha one) (ArAlpha alpha' two) joining
      | alpha == alpha' = first (ArAlpha alpha) <$> goExpr one two joining
    goArgument one two joining = same one two joining
    goList :: (item -> item -> Joining -> Maybe (item, Joining)) -> [item] -> [item] -> Joining -> Maybe ([item], Joining)
    goList _ [] [] joining = Just ([], joining)
    goList walk (one : ones) (two : twos) joining = do
      (item, joining') <- walk one two joining
      first (item :) <$> goList walk ones twos joining'
    goList _ _ _ _ = Nothing
    same :: (Eq item) => item -> item -> Joining -> Maybe (item, Joining)
    same one two joining
      | one == two = Just (one, joining)
      | otherwise = Nothing
    picked :: (Int, Int) -> Joining -> (Int, Joining)
    picked pair joining@(spent', names, made) = case Map.lookup pair names of
      Just name -> (name, joining)
      Nothing -> (succ spent', (succ spent', Map.insert pair (succ spent') names, (succ spent', pair) : made))

forked :: Function -> Either Int Bytes -> Expression -> Expression -> Maybe Expression
forked func condition left@(ExFormation one) right@(ExFormation two)
  | not (any datum (one ++ two)) = Just (ExFormation (map branch (nub (attributes one ++ attributes two))))
  where
    datum :: Binding -> Bool
    datum (BiDelta _) = True
    datum _ = False
    attributes :: [Binding] -> [Attribute]
    attributes bds = [attr | Just attr <- map attributeFromBinding bds, attr `notElem` [AtRho, AtLambda, AtDelta]]
    branch :: Attribute -> Binding
    branch attr
      | BiVoid attr `elem` one && BiVoid attr `elem` two = BiVoid attr
      | otherwise = BiTau attr (ExFormation [BiLambda func, BiTau (AtLabel "on🌵") (ExFormation [either (BiLambda . FnSymbol) BiDelta condition]), BiTau (AtLabel "left🌵") (ExDispatch left attr), BiTau (AtLabel "right🌵") (ExDispatch right attr)])
forked _ _ _ _ = Nothing
