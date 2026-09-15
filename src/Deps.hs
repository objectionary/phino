{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The main goal of this module is breaking cyclic dependency:
-- Dataize -> Functions -> Rewriter -> Dataize
-- Here we provide custom type BuildTermFunc and add it to
-- RewriteContext and ReduceContext. Now Dataize and Rewrite depends
-- only on Term module. This allows us to use Rewriter and Dataize in
-- Functions module because Rewriter does not depend on Functions anymore.
module Deps where

import AST
import Data.Aeson.Encoding (Encoding, bool, encodingToLazyByteString, pair, pairs, text)
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Text as T
import Logger (logDebug)
import Matcher
import Printer (printBytes)
import System.Directory (createDirectoryIfMissing)
import System.FilePath
import System.IO (Handle)
import Text.Printf (printf)
import Yaml

data Term
  = TeExpression Expression
  | TeAttribute Attribute
  | TeBytes Bytes
  | TeBindings [Binding]

type BuildTermMethod = [ExtraArgument] -> Subst -> IO Term

-- The state 𝑠 threaded through the Morphing 𝕄(n, e, s), Dataization 𝔻(n, e, s)
-- and Evaluation 𝔼(b, s) functions. The calculus does not yet fix what a state
-- is, so it is a plain string for now, holding the one thing a run has to
-- count across firings: how many fresh symbols the 'symbols' block of a λ
-- function has minted so far (see 'minted' in 'Lambdas'). Unlike the universe
-- 𝑒, which is immutable and threaded unchanged, the state is mutable: 𝔼 takes a
-- state 𝑠1 and returns a new one 𝑠2, and 𝕄/𝔻 propagate that change to their
-- callers. Only the rules that fire a λ function — 'ml' (morphing) and 'fire'
-- (dataization) — can change the state; every other rule threads it through
-- untouched.
type State = String

-- Like 'BuildTermMethod', but it also takes the incoming state and returns the
-- new state alongside the term. Lives here next to 'BuildTermMethod' so the two
-- stay together.
type BuildTermMethodS = [ExtraArgument] -> Subst -> IO (Term, State)

type BuildTermFunc = String -> BuildTermMethod

type SaveStepFunc = Expression -> IO ()

saveStep :: Maybe FilePath -> String -> (Expression -> IO String) -> Int -> SaveStepFunc
saveStep Nothing _ _ _ _ = pure ()
saveStep (Just dir) ext render step expr = do
  createDirectoryIfMissing True dir
  let path = dir </> printf "%05d.%s" step ext
  content <- render expr
  writeFile path content
  logDebug (printf "Saved step '%d' to '%s'" step path)

dontSaveStep :: SaveStepFunc
dontSaveStep = saveStep Nothing "" (\_ -> pure "") 0

-- One line of the protocol the '--evaluations' option writes: what 𝔼 did while
-- it fired a λ function. A firing names the function and every meta the entry
-- of it bound, with the value of each; a 'morph' premise binds a whole term,
-- which has no spelling the protocol can give without printing 𝜑, so it is
-- bracketed by an opening and a closing record instead and whatever fires
-- inside it stands between them. A firing that got stuck — the λ function has
-- no entry, or one of its operands reached such a function — and survived in
-- the residual program of a partial evaluation (see '--partial') names the
-- function alone.
data Evaluation
  = EvFiring T.Text [(T.Text, T.Text)]
  | EvOpening T.Text T.Text
  | EvClosing T.Text T.Text
  | EvStuck T.Text

type SaveEvalFunc = Evaluation -> IO ()

-- What the protocol spells the value of a meta as, so that a reader of the
-- file never parses 𝜑: the hex of the byte array the term carries, or the name
-- of the λ function it is stuck on. A term that is neither — one a 'where'
-- extra built out of something else — has no such spelling and is left out of
-- the record altogether.
carried :: MetaValue -> Maybe T.Text
carried (MvFunction func) = Just func
carried (MvBytes bts) = Just (T.pack (printBytes bts))
carried (MvExpression (DataObject _ bts)) = Just (T.pack (printBytes bts))
carried (MvExpression (ExFormation bds)) = listToMaybe (mapMaybe fact bds)
  where
    fact :: Binding -> Maybe T.Text
    fact (BiDelta bts) = Just (T.pack (printBytes bts))
    fact (BiLambda (Function func)) = Just func
    fact _ = Nothing
carried _ = Nothing

-- Append one evaluation to the protocol as a single JSON object on a line of
-- its own: the λ function under 'λ' and, next to it, either every meta the
-- entry bound, each under its own name, or the 'morph' premise being opened or
-- closed, or the bare fact that the firing got stuck. The handle stays open
-- for the whole run, since a run may fire thousands of λ functions and
-- reopening the file for each of them buys nothing; it is written as bytes,
-- since a JSON object carries its own encoding.
saveEval :: Handle -> SaveEvalFunc
saveEval handle report = do
  BSL.hPut handle (encodingToLazyByteString (recorded report) <> "\n")
  logDebug (printf "Saved the evaluation of '%s'" (T.unpack (about report)))
  where
    -- The λ function comes first and the metas follow in the order the entry
    -- bound them, which an 'Encoding' keeps and a 'Value' would not: a reader
    -- of the protocol sees the record the way the file spells the entry.
    recorded :: Evaluation -> Encoding
    recorded (EvFiring func metas) =
      pairs (pair "λ" (text func) <> mconcat [pair (Key.fromText bound) (text value) | (bound, value) <- metas])
    recorded (EvOpening func bound) = pairs (pair "λ" (text func) <> pair "morph" (text bound) <> pair "at" (text "begin"))
    recorded (EvClosing func bound) = pairs (pair "λ" (text func) <> pair "morph" (text bound) <> pair "at" (text "end"))
    recorded (EvStuck func) = pairs (pair "λ" (text func) <> pair "stuck" (bool True))
    about :: Evaluation -> T.Text
    about (EvFiring func _) = func
    about (EvOpening func _) = func
    about (EvClosing func _) = func
    about (EvStuck func) = func

dontSaveEval :: SaveEvalFunc
dontSaveEval _ = pure ()
