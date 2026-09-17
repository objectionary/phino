{-# LANGUAGE OverloadedRecordDot #-}
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
import Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Files (overwrite)
import Logger (logDebug)
import Matcher
import Printer (printBytes, printFunction)
import System.Directory (createDirectoryIfMissing)
import System.FilePath
import System.IO (Handle, hPutStrLn)
import Text.Printf (printf)
import Yaml

data Term
  = TeExpression Expression
  | TeAttribute Attribute
  | TeBytes Bytes
  | TeBindings [Binding]

type BuildTermMethod = [ExtraArgument] -> Subst -> IO Term

-- The state 𝑠 threaded through the Morphing 𝕄(n, e, s), Dataization 𝔻(n, e, s)
-- and Evaluation 𝔼(b, s) functions. Unlike the universe 𝑒, which is immutable
-- and threaded unchanged, the state is mutable: 𝔼 takes a state 𝑠1 and returns
-- a new one 𝑠2, and 𝕄/𝔻 propagate that change to their callers. It carries how
-- many symbols the run has minted, so the next 𝜎 an answer asks for is one no
-- term already holds, and which symbol the last datum was manufactured for,
-- since every symbol dataizes to the very same 42 and only the state can tell
-- the protocol which unknown that 42 stood for.
data State = State
  { _minted :: Int
  , _manufactured :: Maybe Int
  }

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
  overwrite path content
  logDebug (printf "Saved step '%d' to '%s'" step path)

dontSaveStep :: SaveStepFunc
dontSaveStep = saveStep Nothing "" (\_ -> pure "") 0

-- One line of the protocol the '--protocol' option writes, which is a tree of
-- the firings of the Evaluation function 𝔼 rather than a list of them. The run
-- itself opens it — '𝕄(Q.φ)' for a morphing, '𝔻(Q)' for a dataization — and
-- under it stands one block per firing, '𝔼(L_number_plus)', naming the entry
-- that answered. Inside a block stand the operands the entry bound and the
-- term it answered with, one to a line, and any firing an operand took while
-- it was being reduced, one level deeper again. A name no entry answers stands
-- there as '?(L_number_nope)', where the block of its firing would have been.
data Evaluation
  = -- The run and the term it was aimed at.
    EvRun T.Text T.Text
  | -- One firing of the entry under that key, at the depth its nesting gives
    -- it.
    EvFiring Int T.Text
  | -- A λ function no entry of the '--symbolic' file answers, at the depth the
    -- firing of it would have stood at. Nothing fired, so the line stands alone
    -- and no block opens under it. It is written whether or not '--partial'
    -- goes on to park the run, since the protocol records what 𝔼 was asked for
    -- and a question it could not answer belongs there as much as one it could.
    EvStuck Int T.Text
  | -- A 'dataize' operand of the firing: the meta it bound and the data it
    -- came down to, or the symbol that data was manufactured for.
    EvData Int T.Text (Either Int Bytes)
  | -- An 'evaluate' operand of the firing: the meta it bound and the normal
    -- form it reached.
    EvTerm Int T.Text Expression
  | -- What the firing answered with.
    EvAnswer Int Expression

type SaveEvalFunc = Evaluation -> IO ()

-- What the protocol has counted so far: how often each entry has fired, so a
-- line of one firing is told from the same line of the next; how many answers
-- the whole run has given, which numbers them; the name last given to each
-- symbol, which is how a term already written out is named instead of written
-- again; and which firing of its entry is open at each depth, since the
-- operands of a firing belong to the firing it was when it started and not to
-- the one another firing of the same entry has made of it since. The order the
-- firings come in carries nothing — it is the order 𝕄 walks the term — so the
-- symbols are what the dependencies are read from: a term carrying 𝜎4 is the
-- term the line that minted 𝜎4 stood for.
data Protocol = Protocol
  { _fired :: Map.Map T.Text Int
  , _answered :: Int
  , _named :: Map.Map Int T.Text
  , _open :: Map.Map Int Int
  }

-- The protocol before a single firing has been written.
emptyProtocol :: Protocol
emptyProtocol = Protocol Map.empty 0 Map.empty Map.empty

-- Append one line to the protocol, indented by the depth of what it reports
-- and numbered by what the protocol has seen before it. The handle stays open
-- for the whole run, since a run may fire thousands of λ functions and
-- reopening the file for each of them buys nothing; the counting rides in an
-- 'IORef' next to it, since it is the cursor of the file and not a property of
-- the reduction. Expressions are rendered by the caller, which flattens them,
-- so a line never spills over more than one.
saveEval :: Handle -> IORef Protocol -> (Expression -> IO String) -> SaveEvalFunc
saveEval handle cursor render report = do
  line <- atomicModify cursor (written report)
  hPutStrLn handle line
  logDebug (printf "Saved one line of the protocol: %s" (dropWhile (== ' ') line))
  where
    -- The line a report is written as, together with what the protocol has
    -- counted once it is written. A term is looked up by the first symbol it
    -- carries and, where that symbol has a name already, written as that name;
    -- otherwise it is written out and the symbol takes the name of this line.
    written :: Evaluation -> Protocol -> IO (Protocol, String)
    written (EvRun judgment locator) protocol =
      pure (protocol, printf "%s(%s)" (T.unpack judgment) (T.unpack locator))
    written (EvFiring depth key) protocol =
      pure
        ( protocol
            { _fired = Map.insert key firings protocol._fired
            , _open = Map.insert depth firings protocol._open
            }
        , indented depth (printf "𝔼(%s)" (T.unpack key))
        )
      where
        firings :: Int
        firings = 1 + fromMaybe 0 (Map.lookup key protocol._fired)
    written (EvStuck depth key) protocol =
      pure (protocol, indented depth (printf "?(%s)" (T.unpack key)))
    written (EvData depth spelling value) protocol =
      pure (protocol, indented depth (printf "%s := %s" (labelled protocol depth spelling) (spelled value)))
      where
        spelled :: Either Int Bytes -> String
        spelled (Left symbol) = printf "𝔻(%s)" (printFunction (FnSymbol symbol))
        spelled (Right bytes) = printBytes bytes
    written (EvTerm depth spelling term) protocol = do
      let naming = labelled protocol depth spelling
      (protocol', value) <- valued protocol naming term
      pure (protocol', indented depth (printf "%s := %s" naming value))
    written (EvAnswer depth term) protocol = do
      let naming = printf "%s.%d" (T.unpack answer) (protocol._answered + 1)
      (protocol', value) <- valued protocol{_answered = protocol._answered + 1} naming term
      pure (protocol', indented depth (printf "%s := %s" naming value))
    -- The value of a term, next to the name this line gives it: the name the
    -- symbol it carries already has, where it has one, and the term itself
    -- otherwise. Either way the symbol takes the name of this line, so the
    -- next term carrying it points back here and not further.
    valued :: Protocol -> String -> Expression -> IO (Protocol, String)
    valued protocol naming term = case denoted term of
      Nothing -> (,) protocol <$> render term
      Just symbol -> do
        value <- maybe (render term) (pure . T.unpack) (Map.lookup symbol protocol._named)
        pure (protocol{_named = Map.insert symbol (T.pack naming) protocol._named}, value)
    -- The name of an operand meta on this firing of its λ function: the meta
    -- the entry spells it with and which firing of that function this is,
    -- since every entry numbers its own metas from 𝛿1 and 𝑛1 and only the
    -- firing tells two 𝛿1 apart. The firing a line belongs to is the one
    -- opened one level above it.
    labelled :: Protocol -> Int -> T.Text -> String
    labelled protocol depth spelling =
      printf "%s.%d" (T.unpack spelling) (fromMaybe 0 (Map.lookup (depth - 1) protocol._open))
    indented :: Int -> String -> String
    indented depth line = replicate (2 * depth) ' ' ++ line

-- Read, change and write the cursor back in one go, which a firing nested in
-- the reduction of an operand of another needs: the outer firing is still
-- half-written when the inner one starts counting.
atomicModify :: IORef a -> (a -> IO (a, b)) -> IO b
atomicModify ref action = readIORef ref >>= action >>= \(value, made) -> writeIORef ref value >> pure made

-- How the calculus spells the meta a λ function writes its answer to, which is
-- the name the protocol numbers the answers of a whole run by.
answer :: T.Text
answer = "𝑛"

dontSaveEval :: SaveEvalFunc
dontSaveEval _ = pure ()
