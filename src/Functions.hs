{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Functions (buildTerm) where

import AST
import Builder
import Control.Exception (throwIO)
import Control.Monad (replicateM, when)
import qualified Data.ByteString.Char8 as B
import Data.Char (intToDigit)
import Data.Functor
import Data.Set (Set)
import qualified Data.Set
import qualified Data.Set as Set
import GHC.IO (unsafePerformIO)
import Matcher
import Misc
import Numeric (showHex)
import Parser (parseAttributeThrows, parseNumberThrows)
import Random (randomString)
import Regexp
import System.Random (randomRIO)
import Deps
import Text.Printf (printf)
import qualified Yaml as Y
import Printer (printAttribute', printExpression', printExtraArg')
import Logger (logDebug)

buildTerm :: BuildTermFunc
buildTerm func args subst prog = do
  logDebug (printf "Building new term using '%s' function..." func)
  buildTerm' func args subst prog

buildTerm' :: BuildTermFunc
buildTerm' "contextualize" = _contextualize
buildTerm' "scope" = _scope
buildTerm' "random-tau" = _randomTau
buildTerm' "dataize" = _dataize
buildTerm' "concat" = _concat
buildTerm' "sed" = _sed
buildTerm' "random-string" = _randomString
buildTerm' "size" = _size
buildTerm' "tau" = _tau
buildTerm' "string" = _string
buildTerm' "number" = _number
buildTerm' "sum" = _sum
buildTerm' "join" = _join
buildTerm' func = _unsupported func

argToBytes :: Y.ExtraArgument -> Subst -> Program -> IO Bytes
argToBytes (Y.ArgBytes bytes) subst _ = buildBytesThrows bytes subst
argToBytes (Y.ArgExpression expr) subst prog = do
  (TeBytes bts) <- _dataize [Y.ArgExpression expr] subst prog
  pure bts
argToBytes arg _ _ = throwIO (userError (printf "Can't extract bytes from given argument: %s" (printExtraArg' arg)))

argToString :: Y.ExtraArgument -> Subst -> Program -> IO String
argToString arg subst prog = argToBytes arg subst prog <&> btsToUnescapedStr

argToNumber :: Y.ExtraArgument -> Subst -> Program -> IO Double
argToNumber arg subst prog = argToBytes arg subst prog <&> either toDouble id . btsToNum

_contextualize :: BuildTermMethod
_contextualize [Y.ArgExpression expr, Y.ArgExpression context] subst prog = do
  (expr', _) <- buildExpressionThrows expr subst
  (context', _) <- buildExpressionThrows context subst
  pure (TeExpression (contextualize expr' context' prog))
_contextualize _ _ _ = throwIO (userError "Function contextualize() requires exactly 2 arguments as expression")

_scope :: BuildTermMethod
_scope [Y.ArgExpression expr] subst _ = do
  (_, scope) <- buildExpressionThrows expr subst
  pure (TeExpression scope)
_scope _ _ _ = throwIO (userError "Function scope() requires exactly 1 argument as expression")

_randomTau :: BuildTermMethod
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
        pure (printAttribute' attr' : rest')
      Y.ArgBinding bd -> do
        bds <- buildBindingThrows bd subst
        rest' <- argsToAttrs rest
        pure (map show (attributesFromBindings bds) ++ rest')
      Y.ArgBytes _ -> throwIO (userError "Bytes can't be argument of random-tau() function")
    randomTau :: [String] -> IO String
    randomTau attrs = do
      tau <- randomString "a🌵%d"
      if tau `elem` attrs then randomTau attrs else pure tau

_dataize :: BuildTermMethod
_dataize [Y.ArgBytes bytes] subst _ = do
  bts <- buildBytesThrows bytes subst
  pure (TeBytes bts)
_dataize [Y.ArgExpression expr] subst _ = do
  (expr', _) <- buildExpressionThrows expr subst
  case expr' of
    DataObject _ bytes -> pure (TeBytes bytes)
    _ -> throwIO (userError "Only data objects and bytes are supported by 'dataize' function now")
_dataize _ _ _ = throwIO (userError "Function dataize() requires exactly 1 argument as expression or bytes")

_concat :: BuildTermMethod
_concat args subst prog = do
  args' <- traverse (\arg -> argToString arg subst prog) args
  pure (TeExpression (DataString (strToBts (concat args'))))

_sed :: BuildTermMethod
_sed args subst prog = do
  when (length args < 2) (throwIO (userError "Function sed() requires at least two arguments"))
  args' <-
    traverse
      ( \arg -> do
          bts <- argToString arg subst prog
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

_randomString :: BuildTermMethod
_randomString [arg] subst prog = do
  pat <- argToString arg subst prog
  str <- randomString pat
  pure (TeExpression (DataString (strToBts str)))
_randomString _ _ _ = throwIO (userError "Function random-string() requires exactly 1 dataizable argument")

_size :: BuildTermMethod
_size [Y.ArgBinding (BiMeta meta)] subst _ = do
  bds <- buildBindingThrows (BiMeta meta) subst
  pure (TeExpression (DataNumber (numToBts (fromIntegral (length bds)))))
_size _ _ _ = throwIO (userError "Function size() requires exactly 1 meta binding")

_tau :: BuildTermMethod
_tau [Y.ArgExpression expr] subst prog = do
  TeBytes bts <- _dataize [Y.ArgExpression expr] subst prog
  attr <- parseAttributeThrows (btsToUnescapedStr bts)
  pure (TeAttribute attr)
_tau _ _ _ = throwIO (userError "Function tau() requires exactly 1 argument as expression")

_string :: BuildTermMethod
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
                (printExpression' exp)
            )
        )
  pure (TeExpression str)
_string [Y.ArgAttribute attr] subst _ = do
  attr' <- buildAttributeThrows attr subst
  pure (TeExpression (DataString (strToBts (printAttribute' attr'))))
_string _ _ _ = throwIO (userError "Function string() requires exactly 1 argument as attribute or data expression (Φ̇.number or Φ̇.string)")

_number :: BuildTermMethod
_number [Y.ArgExpression expr] subst _ = do
  (expr', _) <- buildExpressionThrows expr subst
  case expr' of
    DataString bts -> do
      num <- parseNumberThrows (btsToUnescapedStr bts)
      pure (TeExpression num)
    _ -> throwIO (userError (printf "Function number() expects expression to be 'Φ̇.string', but got:\n%s" (printExpression' expr')))
_number _ _ _ = throwIO (userError "Function number() requires exactly 1 argument as 'Φ̇.string'")

_sum :: BuildTermMethod
_sum args subst prog = do
  nums <- traverse (\arg -> argToNumber arg subst prog) args
  pure (TeExpression (DataNumber (numToBts (sum nums))))

_join :: BuildTermMethod
_join [] _ _ = pure (TeBindings [])
_join args subst _ = do
  bds <- buildBindings args
  pure (TeBindings (join' bds Set.empty))
  where
    buildBindings :: [Y.ExtraArgument] -> IO [Binding]
    buildBindings [] = pure []
    buildBindings (Y.ArgBinding bd : args') = do
      bds <- buildBindingThrows bd subst
      next <- buildBindings args'
      pure (bds ++ next)
    buildBindings _ = throwIO (userError "Function 'join' can work with bindings only")
    join' :: [Binding] -> Set.Set Attribute -> [Binding]
    join' [] _ = []
    join' (bd : bds) attrs =
      let [attr] = attributesFromBindings [bd]
       in if Set.member attr attrs
            then
              if attr == AtRho || attr == AtDelta || attr == AtLambda
                then join' bds attrs
                else
                  let new = case bd of
                        BiTau attr expr -> BiTau (updated attr attrs) expr
                        BiVoid attr -> BiVoid (updated attr attrs)
                   in new : join' bds attrs
            else bd : join' bds (Set.insert attr attrs)
    updated :: Attribute -> Set.Set Attribute -> Attribute
    updated attr attrs =
      let (TeAttribute attr') = unsafePerformIO (_randomTau (map Y.ArgAttribute (Set.toList attrs)) subst (Program ExGlobal))
       in attr'

_unsupported :: BuildTermFunc
_unsupported func _ _ _ = throwIO (userError (printf "Function %s() is not supported or does not exist" func))
