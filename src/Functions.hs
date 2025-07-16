{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Functions where

import Ast
import Builder
import Control.Exception (throwIO)
import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as B
import Data.Char (intToDigit)
import Data.IORef
import Data.Set (Set)
import qualified Data.Set
import GHC.IO (unsafePerformIO)
import Matcher
import Misc
import Numeric (showHex)
import Pretty
import Random (randomString)
import Regexp
import System.Random (randomRIO)
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
  tau <- randomTau attrs
  pure (TeAttribute (AtLabel tau))
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
    randomTau :: [String] -> IO String
    randomTau attrs = do
      tau <- randomString "a🌵%d"
      if tau `elem` attrs then randomTau attrs else pure tau
buildTermFromFunction "dataize" [ArgBytes bytes] subst _ = do
  bts <- buildBytesThrows bytes subst
  pure (TeBytes bts)
buildTermFromFunction "dataize" [ArgExpression expr] subst _ = do
  (expr', _) <- buildExpressionThrows expr subst
  case expr' of
    DataObject _ bytes -> pure (TeBytes bytes)
    _ -> throwIO (userError "Only data objects and bytes are supported by 'dataize' function now")
buildTermFromFunction "dataize" _ _ _ = throwIO (userError "Function dataize() requires exactly 1 argument as expression")
buildTermFromFunction "concat" args subst prog = do
  args' <- traverse (\arg -> argToStrBytes arg subst prog) args
  pure (TeExpression (DataString (strToBts (concat args'))))
buildTermFromFunction "sed" [tgt, ptn] subst prog = do
  [tgt', ptn'] <- traverse (\arg -> argToStrBytes arg subst prog) [tgt, ptn]
  (pat, rep, global) <- parse (B.pack ptn')
  regex <- compile pat
  res <-
    if global
      then replaceAll regex rep (B.pack tgt')
      else replaceFirst regex rep (B.pack tgt')
  pure (TeExpression (DataString (strToBts (B.unpack res))))
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
buildTermFromFunction "random-string" [arg] subst prog = do
  pat <- argToStrBytes arg subst prog
  str <- randomString pat
  pure (TeExpression (DataString (strToBts str)))
buildTermFromFunction "random-string" _ _ _ = throwIO (userError "Function random-string() requires exactly 1 dataizable argument")
buildTermFromFunction func _ _ _ = throwIO (userError (printf "Function %s() is not supported or does not exist" func))
