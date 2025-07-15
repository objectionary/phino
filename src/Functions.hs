{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Functions where

import Ast
import Builder
import Control.Exception (throwIO)
import qualified Data.ByteString.Char8 as B
import Matcher
import Misc
import Pretty
import Regexp
import Term
import Text.Printf (printf)
import Yaml

argToStrBytes :: ExtraArgument -> Subst -> Program -> IO String
argToStrBytes (ArgBytes bytes) subst _ = do
  bts <- buildBytesThrows bytes subst
  pure (btsToUnescapedStr bts)
argToStrBytes (ArgExpression expr) subst prog = do
  (TeBytes bts) <- buildTermFromFunction "dataize" [ArgExpression expr] subst prog
  pure (btsToUnescapedStr bts)
argToStrBytes arg _ _ = throwIO (userError (printf "Can't extract bytes from given argument: %s" (prettyExtraArg arg)))

buildTermFromFunction :: String -> [ExtraArgument] -> Subst -> Program -> IO Term
buildTermFromFunction "contextualize" [ArgExpression expr, ArgExpression context] subst prog = do
  (expr', _) <- buildExpressionThrows expr subst
  (context', _) <- buildExpressionThrows context subst
  pure (TeExpression (contextualize expr' context' prog))
buildTermFromFunction "contextualize" _ _ _ = throwIO (userError "Function contextualize() requires exactly 2 arguments as expression")
buildTermFromFunction "scope" [ArgExpression expr] subst _ = do
  (expr', scope) <- buildExpressionThrows expr subst
  pure (TeExpression scope)
buildTermFromFunction "scope" _ _ _ = throwIO (userError "Function scope() requires exactly 1 argument as expression")
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
      ArgBytes _ -> throwIO (userError "Bytes can't be argument of random-tau() function")
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
buildTermFromFunction "dataize" _ _ _ = throwIO (userError "Function dataize() requires exactly 1 argument as expression")
buildTermFromFunction "concat" args subst prog = do
  args' <- traverse (\arg -> argToStrBytes arg subst prog) args
  pure (TeExpression (DataObject "string" (strToBts (concat args'))))
buildTermFromFunction "sed" [tgt, ptn] subst prog = do
  [tgt', ptn'] <- traverse (\arg -> argToStrBytes arg subst prog) [tgt, ptn]
  (pat, rep, global) <- parse (B.pack ptn')
  regex <- compile pat
  res <-
    if global
      then replaceAll regex rep (B.pack tgt')
      else replaceFirst regex rep (B.pack tgt')
  pure (TeExpression (DataObject "string" (strToBts (B.unpack res))))
  where
    parse :: B.ByteString -> IO (B.ByteString, B.ByteString, Bool)
    parse input =
      case B.stripPrefix "s/" input of
        Just rest ->
          let parts = B.split '/' rest
           in case parts of
                [pat, rep, "g"] -> pure (pat, rep, True)
                [pat, rep, ""] -> pure (pat, rep, False)
                [pat, rep] -> pure (pat, rep, False)
                _ -> throwIO (userError "sed pattern must be in format s/pat/rep/[g]")
        _ -> throwIO (userError "sed pattern must start with s/")
buildTermFromFunction "sed" _ _ _ = throwIO (userError "Function sed() requires exactly 2 dataizable arguments")
buildTermFromFunction func _ _ _ = throwIO (userError (printf "Function %s() is not supported or does not exist" func))
