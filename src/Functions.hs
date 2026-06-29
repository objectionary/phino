{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Functions (buildTerm, execFunctions) where

import AST
import Builder
import Bytes (btsToNum, btsToUnescapedStr, numToBts, strToBts)
import Control.Exception (throwIO)
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B
import Data.Functor
import qualified Data.Set as Set
import Deps
import Logger (logDebug)
import Matcher
import Misc
import Parser (parseAttributeThrows, parseNumberThrows)
import Printer (printAttribute, printExpression, printExtraArg)
import Random (randomString)
import Regexp
import Tau (freshTau)
import Text.Printf (printf)
import qualified Yaml as Y

-- Names of build-term functions that need the full evaluation context
-- (program plus atom evaluation) and are therefore provided only by
-- 'Dataize.execBuildTerm', not by 'buildTerm'. They are available while
-- executing dataization and morphing rules, but not rewriting rules.
execFunctions :: [String]
execFunctions = ["evaluate", "morph"]

buildTerm :: BuildTermFunc
buildTerm func args subst = do
  logDebug (printf "Building new term using '%s' function..." func)
  buildTerm' func args subst

buildTerm' :: BuildTermFunc
buildTerm' "contextualize" = _contextualize
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

argToBytes :: Y.ExtraArgument -> Subst -> IO Bytes
argToBytes (Y.ArgBytes bytes) subst = buildBytesThrows bytes subst
argToBytes (Y.ArgExpression expr) subst = do
  (TeBytes bts) <- _dataize [Y.ArgExpression expr] subst
  pure bts
argToBytes arg _ = throwIO (userError (printf "Can't extract bytes from given argument: %s" (printExtraArg arg)))

argToString :: Y.ExtraArgument -> Subst -> IO String
argToString arg subst = argToBytes arg subst <&> btsToUnescapedStr

argToNumber :: Y.ExtraArgument -> Subst -> IO Double
argToNumber arg subst = argToBytes arg subst <&> either toDouble id . btsToNum

_contextualize :: BuildTermMethod
_contextualize [Y.ArgExpression expr, Y.ArgExpression context] subst = do
  expr' <- buildExpressionThrows expr subst
  context' <- buildExpressionThrows context subst
  pure (TeExpression (contextualize expr' context'))
_contextualize _ _ = throwIO (userError "Function contextualize() requires exactly 2 arguments as expression")

-- Uniqueness is the engine's job: 'freshTau' draws from the document-wide
-- avoid-set seeded at the start of the run, so no collision list is needed.
-- The function takes no arguments and rejects any extras so rule mistakes are
-- not silently accepted.
_randomTau :: BuildTermMethod
_randomTau [] _ = TeAttribute . AtLabel <$> freshTau
_randomTau _ _ = throwIO (userError "Function random-tau() requires exactly 0 arguments")

_dataize :: BuildTermMethod
_dataize [Y.ArgBytes bytes] subst = do
  bts <- buildBytesThrows bytes subst
  pure (TeBytes bts)
_dataize [Y.ArgExpression expr] subst = do
  expr' <- buildExpressionThrows expr subst
  case expr' of
    DataObject _ bytes -> pure (TeBytes bytes)
    _ -> throwIO (userError "Only data objects and bytes are supported by 'dataize' function now")
_dataize _ _ = throwIO (userError "Function dataize() requires exactly 1 argument as expression or bytes")

_concat :: BuildTermMethod
_concat args subst = do
  args' <- traverse (`argToString` subst) args
  pure (TeExpression (DataString (strToBts (concat args'))))

_sed :: BuildTermMethod
_sed args subst = do
  when (length args < 2) (throwIO (userError "Function sed() requires at least two arguments"))
  first : rest <-
    traverse
      ( \arg -> do
          bts <- argToString arg subst
          pure (B.pack bts)
      )
      args
  res <- sed first rest
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
           in case flag of
                "g" -> pure (pat, rep, True)
                "" -> pure (pat, rep, False)
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
_randomString [arg] subst = do
  pat <- argToString arg subst
  str <- randomString pat
  pure (TeExpression (DataString (strToBts str)))
_randomString _ _ = throwIO (userError "Function random-string() requires exactly 1 dataizable argument")

_size :: BuildTermMethod
_size [Y.ArgBinding (BiMeta meta)] subst = do
  bds <- buildBindingThrows (BiMeta meta) subst
  pure (TeExpression (DataNumber (numToBts (fromIntegral (length bds)))))
_size _ _ = throwIO (userError "Function size() requires exactly 1 meta binding")

_tau :: BuildTermMethod
_tau [Y.ArgExpression expr] subst = do
  TeBytes bts <- _dataize [Y.ArgExpression expr] subst
  attr <- parseAttributeThrows (btsToUnescapedStr bts)
  pure (TeAttribute attr)
_tau _ _ = throwIO (userError "Function tau() requires exactly 1 argument as expression")

_string :: BuildTermMethod
_string [Y.ArgExpression expr] subst = do
  expr' <- buildExpressionThrows expr subst
  str <- case expr' of
    DataNumber bts -> pure (DataString (strToBts (either show show (btsToNum bts))))
    DataString bts -> pure (DataString bts)
    ex ->
      throwIO
        ( userError
            ( printf
                "Couldn't convert given expression to 'Φ.string' object, only 'Φ.number' or 'Φ.string' are allowed\n%s"
                (printExpression ex)
            )
        )
  pure (TeExpression str)
_string [Y.ArgAttribute attr] subst = do
  attr' <- buildAttributeThrows attr subst
  pure (TeExpression (DataString (strToBts (printAttribute attr'))))
_string _ _ = throwIO (userError "Function string() requires exactly 1 argument as attribute or data expression (Φ.number or Φ.string)")

_number :: BuildTermMethod
_number [Y.ArgExpression expr] subst = do
  expr' <- buildExpressionThrows expr subst
  case expr' of
    DataString bts -> do
      num <- parseNumberThrows (btsToUnescapedStr bts)
      pure (TeExpression num)
    _ -> throwIO (userError (printf "Function number() expects expression to be 'Φ.string', but got:\n%s" (printExpression expr')))
_number _ _ = throwIO (userError "Function number() requires exactly 1 argument as 'Φ.string'")

_sum :: BuildTermMethod
_sum args subst = do
  nums <- traverse (`argToNumber` subst) args
  pure (TeExpression (DataNumber (numToBts (sum nums))))

_join :: BuildTermMethod
_join [] _ = pure (TeBindings [])
_join args subst = do
  bds <- buildBindings args
  TeBindings <$> join' bds Set.empty
  where
    buildBindings :: [Y.ExtraArgument] -> IO [Binding]
    buildBindings [] = pure []
    buildBindings (Y.ArgBinding bd : args') = do
      bds <- buildBindingThrows bd subst
      next <- buildBindings args'
      pure (bds ++ next)
    buildBindings _ = throwIO (userError "Function 'join' can work with bindings only")
    join' :: [Binding] -> Set.Set Attribute -> IO [Binding]
    join' [] _ = pure []
    join' (bd : bds) attrs =
      case attributeFromBinding bd of
        Just attr
          | Set.member attr attrs ->
              if attr == AtRho || attr == AtDelta || attr == AtLambda
                then join' bds attrs
                else do
                  new <- case bd of
                    BiTau _ ex -> (`BiTau` ex) <$> freshAttr
                    BiVoid _ -> BiVoid <$> freshAttr
                    other -> pure other
                  (new :) <$> join' bds attrs
          | otherwise -> (bd :) <$> join' bds (Set.insert attr attrs)
        Nothing -> (bd :) <$> join' bds attrs
    freshAttr :: IO Attribute
    freshAttr = do
      term <- _randomTau [] subst
      case term of
        TeAttribute attr' -> pure attr'
        _ -> throwIO (userError "random-tau() did not return an attribute, internal invariant violated")

_unsupported :: BuildTermFunc
_unsupported func _ _ = throwIO (userError (printf "Function %s() is not supported or does not exist" func))
