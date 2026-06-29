-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The main goal of this module is breaking cyclic dependency:
-- Dataize -> Functions -> Rewriter -> Dataize
-- Here we provide custom type BuildTermFunc and add it to
-- RewriteContext and DataizeContext. Now Dataize and Rewrite depends
-- only on Term module. This allows us to use Rewriter and Dataize in
-- Functions module because Rewriter does not depend on Functions anymore.
module Deps where

import AST
import Logger (logDebug)
import Matcher
import System.Directory (createDirectoryIfMissing)
import System.FilePath
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
-- is, so it is a plain string for now. Unlike the universe 𝑒, which is immutable
-- and threaded unchanged, the state is mutable: 𝔼 takes a state 𝑠1 and returns a
-- new one 𝑠2, and 𝕄/𝔻 propagate that change to their callers. Only the rules
-- that fire an atom — 'ml' (morphing) and 'fire' (dataization) — can change
-- the state; every other rule threads it through untouched.
type State = String

-- Like 'BuildTermMethod', but it also takes the incoming state and returns the
-- new state alongside the term. Lives here next to 'BuildTermMethod' so the two
-- stay together.
type BuildTermMethodS = [ExtraArgument] -> Subst -> IO (Term, State)

type BuildTermFunc = String -> BuildTermMethod

type SaveStepFunc = Program -> Int -> IO ()

saveStep :: Maybe FilePath -> String -> (Program -> IO String) -> SaveStepFunc
saveStep Nothing _ _ _ _ = pure ()
saveStep (Just dir) ext render prog step = do
  createDirectoryIfMissing True dir
  let path = dir </> printf "%05d.%s" step ext
  content <- render prog
  writeFile path content
  logDebug (printf "Saved step '%d' to '%s'" step path)

dontSaveStep :: SaveStepFunc
dontSaveStep = saveStep Nothing "" (\_ -> pure "")
