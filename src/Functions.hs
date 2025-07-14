{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Functions where

import Ast
import Builder
import Control.Exception (throwIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.Maybe (catMaybes, mapMaybe)
import Dataize
import Matcher
import Misc
import Pretty
import Term
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString ()
import Yaml
import Text.Printf (printf)

buildTermFromFunction :: String -> [ExtraArgument] -> Subst -> Program -> IO Term
buildTermFromFunction "contextualize" [ArgExpression expr, ArgExpression context] subst prog = do
  (expr', _) <- buildExpressionThrows expr subst
  (context', _) <- buildExpressionThrows context subst
  pure (TeExpression (contextualize expr' context' prog))
buildTermFromFunction "scope" [ArgExpression expr] subst _ = do
  (expr', scope) <- buildExpressionThrows expr subst
  pure (TeExpression scope)
buildTermFromFunction "random-tau" args subst _ = do
  attrs <- argsToAttrs args
  pure (TeAttribute (AtLabel (randomTau 0 attrs)))
  where
    argsToAttrs :: [ExtraArgument] -> IO [String]
    argsToAttrs [] = pure []
    argsToAttrs (arg : rest) = case arg of
      ArgExpression _ -> argsToAttrs rest
      ArgAttribute attr -> do
        attr' <- buildAttributeThrows attr subst
        rest' <- argsToAttrs rest
        pure (prettyAttribute attr' : rest')
      ArgBinding bd -> do
        bds <- buildBindingThrows bd subst
        rest' <- argsToAttrs rest
        pure (attrsFromBindings bds ++ rest')
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
buildTermFromFunction "dataize" [ArgExpression expr] subst _ = do
  (expr', _) <- buildExpressionThrows expr subst
  case expr' of
    DataObject _ bytes -> pure (TeBytes bytes)
    _ -> throwIO (userError "Only data objects are supported by 'dataize' function now")
buildTermFromFunction "concat" args subst _ = do
  args' <- traverse argToString args
  pure (TeExpression (DataObject "string" (strToBts (concat args'))))
  where
    argToString :: ExtraArgument -> IO String
    argToString (ArgBytes bytes) = do
      bts <- buildBytesThrows bytes subst
      pure (btsToStr bts)
    argToString _ = throwIO (userError "")
buildTermFromFunction func _ _ _ = throwIO (userError (printf "Function %s() is not supported or does not exist" func))
