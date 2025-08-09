{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Functions (buildTerm) where

import Ast
import Builder
import Control.Exception (throwIO)
import Control.Monad (replicateM, when)
import qualified Data.ByteString.Char8 as B
import Data.Char (intToDigit)
import Data.Set (Set)
import qualified Data.Set
import GHC.IO (unsafePerformIO)
import Matcher
import Misc
import Numeric (showHex)
import Parser (parseAttributeThrows, parseNumberThrows)
import Pretty
import Random (randomString)
import Regexp
import System.Random (randomRIO)
import Term
import Text.Printf (printf)
import qualified Yaml as Y

buildTerm :: String -> [Y.ExtraArgument] -> Subst -> Program -> IO Term
buildTerm "contextualize" = _contextualize
buildTerm "scope" = _scope
buildTerm "random-tau" = _randomTau
buildTerm "dataize" = _dataize
buildTerm "concat" = _concat
buildTerm "sed" = _sed
buildTerm "random-string" = _randomString
buildTerm "size" = _size
buildTerm "tau" = _tau
buildTerm "string" = _string
buildTerm "number" = _number
buildTerm func  = _unsupported func

argToStrBytes :: Y.ExtraArgument -> Subst -> Program -> IO String
argToStrBytes (Y.ArgBytes bytes) subst _ = do
  bts <- buildBytesThrows bytes subst
  pure (btsToUnescapedStr bts)
argToStrBytes (Y.ArgExpression expr) subst prog = do
  (TeBytes bts) <- _dataize [Y.ArgExpression expr] subst prog
  pure (btsToUnescapedStr bts)
argToStrBytes arg _ _ = throwIO (userError (printf "Can't extract bytes from given argument: %s" (prettyExtraArg arg)))

_contextualize :: [Y.ExtraArgument] -> Subst -> Program -> IO Term
_contextualize [Y.ArgExpression expr, Y.ArgExpression context] subst prog = do
  (expr', _) <- buildExpressionThrows expr subst
  (context', _) <- buildExpressionThrows context subst
  pure (TeExpression (contextualize expr' context' prog))
_contextualize _ _ _ = throwIO (userError "Function contextualize() requires exactly 2 arguments as expression")

_scope :: [Y.ExtraArgument] -> Subst -> Program -> IO Term
_scope [Y.ArgExpression expr] subst _ = do
  (_, scope) <- buildExpressionThrows expr subst
  pure (TeExpression scope)
_scope _ _ _ = throwIO (userError "Function scope() requires exactly 1 argument as expression")

_randomTau :: [Y.ExtraArgument] -> Subst -> Program -> IO Term
_randomTau args subst _ = do
  attrs <- argsToAttrs args
  tau <- randomTau attrs
  pure (TeAttribute (AtLabel tau))
  where
    argsToAttrs :: [Y.ExtraArgument] -> IO [String]
    argsToAttrs [] = pure []
    argsToAttrs (arg : rest) = case arg of
      Y.ArgExpression _ -> argsToAttrs rest
      Y.ArgAttribute attr -> do
        attr' <- buildAttributeThrows attr subst
        rest' <- argsToAttrs rest
        pure (prettyAttribute attr' : rest')
      Y.ArgBinding bd -> do
        bds <- buildBindingThrows bd subst
        rest' <- argsToAttrs rest
        pure (attrsFromBindings bds ++ rest')
      Y.ArgBytes _ -> throwIO (userError "Bytes can't be argument of random-tau() function")
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

_dataize :: [Y.ExtraArgument] -> Subst -> Program -> IO Term
_dataize [Y.ArgBytes bytes] subst _ = do
  bts <- buildBytesThrows bytes subst
  pure (TeBytes bts)
_dataize [Y.ArgExpression expr] subst _ = do
  (expr', _) <- buildExpressionThrows expr subst
  case expr' of
    DataObject _ bytes -> pure (TeBytes bytes)
    _ -> throwIO (userError "Only data objects and bytes are supported by 'dataize' function now")
_dataize _ _ _ = throwIO (userError "Function dataize() requires exactly 1 argument as expression or bytes")

_concat :: [Y.ExtraArgument] -> Subst -> Program -> IO Term
_concat args subst prog = do
  args' <- traverse (\arg -> argToStrBytes arg subst prog) args
  pure (TeExpression (DataString (strToBts (concat args'))))

_sed :: [Y.ExtraArgument] -> Subst -> Program -> IO Term
_sed args subst prog = do
  when (length args < 2) (throwIO (userError "Function sed() requires at least two arguments"))
  args' <-
    traverse
      ( \arg -> do
          bts <- argToStrBytes arg subst prog
          pure (B.pack bts)
      )
      args
  res <- sed (head args') (tail args')
  pure (TeExpression (DataString (strToBts (B.unpack res))))
  where
    sed :: B.ByteString -> [B.ByteString] -> IO B.ByteString
    sed tgt [] = pure tgt
    sed tgt (ptn : ptns) = do
      (pat, rep, global) <- parse ptn
      regex <- compile pat
      next <-
        if global
          then replaceAll regex rep tgt
          else replaceFirst regex rep tgt
      sed next ptns
    parse :: B.ByteString -> IO (B.ByteString, B.ByteString, Bool)
    parse input =
      case B.stripPrefix "s/" input of
        Just body ->
          let (pat, rest) = nextUntilSlash body B.empty False
              (rep, flag) = nextUntilSlash rest B.empty True
           in case [pat, rep, flag] of
                [pat, rep, "g"] -> pure (pat, rep, True)
                [pat, rep, ""] -> pure (pat, rep, False)
                _ -> throwIO (userError "sed pattern must be in format s/pat/rep/[g]")
        _ -> throwIO (userError "sed pattern must start with s/")
    -- Cut part from given string until regular slash.
    nextUntilSlash :: B.ByteString -> B.ByteString -> Bool -> (B.ByteString, B.ByteString)
    nextUntilSlash input acc escape = case B.uncons input of
      Nothing -> (acc, B.empty)
      Just (h, rest)
        | h == '\\' -> case B.uncons rest of
            Just (h', rest') -> nextUntilSlash rest' ((if escape then acc else B.snoc acc '\\') `B.append` B.singleton h') escape
            Nothing -> (if escape then acc else B.snoc acc '\\', B.empty)
        | h == '/' -> (acc, rest)
        | otherwise -> nextUntilSlash rest (B.snoc acc h) escape

_randomString :: [Y.ExtraArgument] -> Subst -> Program -> IO Term
_randomString [arg] subst prog = do
  pat <- argToStrBytes arg subst prog
  str <- randomString pat
  pure (TeExpression (DataString (strToBts str)))
_randomString _ _ _ = throwIO (userError "Function random-string() requires exactly 1 dataizable argument")

_size :: [Y.ExtraArgument] -> Subst -> Program -> IO Term
_size [Y.ArgBinding (BiMeta meta)] subst _ = do
  bds <- buildBindingThrows (BiMeta meta) subst
  pure (TeExpression (DataNumber (numToBts (fromIntegral (length bds)))))
_size _ _ _ = throwIO (userError "Function size() requires exactly 1 meta binding")

_tau :: [Y.ExtraArgument] -> Subst -> Program -> IO Term
_tau [Y.ArgExpression expr] subst prog = do
  TeBytes bts <- _dataize [Y.ArgExpression expr] subst prog
  attr <- parseAttributeThrows (btsToUnescapedStr bts)
  pure (TeAttribute attr)
_tau _ _ _ = throwIO (userError "Function tau() requires exactly 1 argument as expression")

_string :: [Y.ExtraArgument] -> Subst -> Program -> IO Term
_string [Y.ArgExpression expr] subst _ = do
  (expr', _) <- buildExpressionThrows expr subst
  str <- case expr' of
    DataNumber bts -> pure (DataString (strToBts (either show show (btsToNum bts))))
    DataString bts -> pure (DataString bts)
    exp ->
      throwIO
        ( userError
            ( printf
                "Couldn't convert given expression to 'Φ̇.string' object, only 'Φ̇.number' or 'Φ̇.string' are allowed\n%s"
                (prettyExpression' exp)
            )
        )
  pure (TeExpression str)
_string [Y.ArgAttribute attr] subst _ = do
  attr' <- buildAttributeThrows attr subst
  pure (TeExpression (DataString (strToBts (prettyAttribute attr'))))
_string _ _ _ = throwIO (userError "Function string() requires exactly 1 argument as attribute or data expression (Φ̇.number or Φ̇.string)")

_number :: [Y.ExtraArgument] -> Subst -> Program -> IO Term
_number [Y.ArgExpression expr] subst _ = do
  (expr', _) <- buildExpressionThrows expr subst
  case expr' of
    DataString bts -> do
      num <- parseNumberThrows (btsToUnescapedStr bts)
      pure (TeExpression num)
    _ -> throwIO (userError (printf "Function number() expects expression to be 'Φ̇.string', but got:\n%s" (prettyExpression' expr')))
_number _ _ _ = throwIO (userError "Function number() requires exactly 1 argument as 'Φ̇.string'")

_unsupported :: String -> [Y.ExtraArgument] -> Subst -> Program -> IO Term
_unsupported func _ _ _ = throwIO (userError (printf "Function %s() is not supported or does not exist" func))