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
import Data.List (intercalate)
import qualified Data.Text as T
import Logger (logDebug)
import Matcher
import System.Directory (createDirectoryIfMissing)
import System.FilePath
import System.IO (IOMode (AppendMode), hPutStrLn, hSetEncoding, utf8, withFile)
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

-- One firing of an atom, the way the Evaluation function 𝔼 sees it: the name of
-- the λ function, the formation it fired against with the λ binding removed, and
-- the term it produced.
data Evaluation = Evaluation
  { _function :: T.Text
  , _arguments :: Expression
  , _result :: Expression
  }

type SaveEvalFunc = Evaluation -> IO ()

-- Append one evaluation to the protocol file as a single tab-separated line:
-- the λ function name, its argument formation and its result. Both expressions
-- are rendered by the caller, which flattens them, so a record never spills over
-- more than one line. The encoding is pinned to UTF-8 rather than taken from the
-- locale, since the file is read back by other programs.
saveEval :: FilePath -> (Expression -> IO String) -> SaveEvalFunc
saveEval file render (Evaluation func bindings outcome) = do
  rendered <- mapM render [bindings, outcome]
  withFile file AppendMode $ \handle -> do
    hSetEncoding handle utf8
    hPutStrLn handle (intercalate "\t" (T.unpack func : rendered))
  logDebug (printf "Saved evaluation of '%s' to '%s'" (T.unpack func) file)

dontSaveEval :: SaveEvalFunc
dontSaveEval _ = pure ()
