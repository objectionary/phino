-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Functions where

import Ast
import Yaml
import Matcher
import Builder
import Pretty
import Dataize
import Term

buildTermFromFunction :: String -> [ExtraArgument] -> Subst -> Program -> Maybe Term
buildTermFromFunction "contextualize" [ArgExpression expr, ArgExpression context] subst prog = do
  (expr', _) <- buildExpression expr subst
  (context', _) <- buildExpression context subst
  return (TeExpression (contextualize expr' context' prog))
buildTermFromFunction "scope" [ArgExpression expr] subst prog = do
  (expr', scope) <- buildExpression expr subst
  return (TeExpression scope)
buildTermFromFunction "random-tau" args subst _ = do
  attrs <- argsToAttrs args
  return (TeAttribute (AtLabel (randomTau 0 attrs)))
  where
    argsToAttrs :: [ExtraArgument] -> Maybe [String]
    argsToAttrs [] = Just []
    argsToAttrs (arg : rest) = case arg of
      ArgExpression _ -> argsToAttrs rest
      ArgAttribute attr -> do
        attr' <- buildAttribute attr subst
        rest' <- argsToAttrs rest
        Just (prettyAttribute attr' : rest')
      ArgBinding bd -> do
        bds <- buildBinding bd subst
        rest' <- argsToAttrs rest
        Just (attrsFromBindings bds ++ rest')
    attrsFromBindings :: [Binding] -> [String]
    attrsFromBindings [] = []
    attrsFromBindings (bd : bds) =
      let attr = case bd of
            BiTau attr _ -> attr
            BiDelta _ -> AtDelta
            BiLambda _ -> AtLambda
            BiVoid attr -> attr
       in prettyAttribute attr : attrsFromBindings bds
    randomTau :: Integer -> [String] -> String
    randomTau idx attrs =
      let cactoos = "a🌵"
          tau = if idx == 0 then cactoos else cactoos ++ show idx
       in if tau `elem` attrs then randomTau (idx + 1) attrs else tau
buildTermFromFunction _ _ _ _ = Nothing