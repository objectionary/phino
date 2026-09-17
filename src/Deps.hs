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
import XMIR (escapeXML, escapeXMLText)
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
-- that answered. Inside a block stand the operands the entry bound — each with
-- the term it was reduced from — and the term it answered with, one to a line,
-- and any firing an operand took while it was being reduced, one level deeper
-- again. A name no entry answers stands
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
  | -- A 'dataize' operand of the firing: the meta it bound, the term the entry
    -- wrote under that meta, and the data it came down to, or the symbol that
    -- data was manufactured for.
    EvData Int T.Text Expression (Either Int Bytes)
  | -- A 'morph' operand of the firing: the meta it bound, the term the entry
    -- wrote under that meta, and the normal form it reached.
    EvTerm Int T.Text Expression Expression
  | -- What the firing answered with.
    EvAnswer Int Expression

type SaveEvalFunc = Evaluation -> IO ()

-- What the protocol has counted so far: how many firings the whole run has
-- opened, which is what numbers them and so tells a line of one firing from
-- the same line of any other; how many answers the whole run has given, which
-- numbers them; the name last given to each symbol, which is how a term
-- already written out is named instead of written again; and which firing is
-- open at each depth, since the operands of a firing belong to the firing it
-- was when it started and not to the one another firing has made of it since.
-- The firings are numbered across the run rather than per λ function, so no
-- two of them give an operand meta the same name and a name the protocol
-- points back to points at one line only (#1261). The order the firings come
-- in carries nothing — it is the order 𝕄 walks the term — so the symbols are
-- what the dependencies are read from: a term carrying 𝜎4 is the term the
-- line that minted 𝜎4 stood for.
data Protocol = Protocol
  { _fired :: Int
  , _answered :: Int
  , _named :: Map.Map Int T.Text
  , _open :: Map.Map Int Int
  }

-- The protocol before a single firing has been written.
emptyProtocol :: Protocol
emptyProtocol = Protocol 0 0 Map.empty Map.empty

-- What the XML protocol has counted so far: how many firings the run has
-- opened, which is what numbers them, and the elements standing open around
-- the record being written, innermost first, each with the depth it was opened
-- at and the name it closes under. The text format needs no such stack, since
-- indentation opens and closes nothing; markup does, and the depth a record
-- carries is the only thing saying which firings it stands outside of.
data Nesting = Nesting
  { _fires :: Int
  , _closing :: [(Int, String)]
  }

-- The XML protocol before a single element has been opened.
emptyNesting :: Nesting
emptyNesting = Nesting 0 []

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
            { _fired = firings
            , _open = Map.insert depth firings protocol._open
            }
        , indented depth (printf "𝔼(%s)" (T.unpack key))
        )
      where
        firings :: Int
        firings = protocol._fired + 1
    written (EvStuck depth key) protocol =
      pure (protocol, indented depth (printf "?(%s)" (T.unpack key)))
    written (EvData depth spelling operand value) protocol = do
      line <- commented (printf "%s := %s" (labelled protocol depth spelling) (spelled value)) operand
      pure (protocol, indented depth line)
      where
        spelled :: Either Int Bytes -> String
        spelled (Left symbol) = printf "𝔻(%s)" (printFunction (FnSymbol symbol))
        spelled (Right bytes) = printBytes bytes
    written (EvTerm depth spelling operand term) protocol = do
      let naming = labelled protocol depth spelling
      (protocol', value) <- valued protocol naming term
      line <- commented (printf "%s := %s" naming value) operand
      pure (protocol', indented depth line)
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
    -- The line of an operand with the term it was reduced from appended to it
    -- as a comment, since the value alone says what the meta was bound to and
    -- never what it was bound from. It is the very term the entry wrote under
    -- the meta, in the notation the calculus reads it in — '$.x' is read as
    -- 'ξ.x' — and it is rendered by the same 'render' the value is, so the
    -- whole line stays one line of 𝜑.
    commented :: String -> Expression -> IO String
    commented line operand = printf "%s  # %s" line <$> render operand
    -- The name of an operand meta on this firing of its λ function: the meta
    -- the entry spells it with and which firing of the run this is, since
    -- every entry numbers its own metas from 𝛿1 and 𝑛1 and only the firing
    -- tells two 𝛿1 apart. The number counts the firings of the whole run and
    -- not those of one λ function, so the second firing of one entry and the
    -- second of another never write the same name (#1261), and it is the very
    -- number the XML format gives the firing in its 'id'. The firing a line
    -- belongs to is the one opened one level above it.
    labelled :: Protocol -> Int -> T.Text -> String
    labelled protocol depth spelling =
      printf "%s.%d" (T.unpack spelling) (fromMaybe 0 (Map.lookup (depth - 1) protocol._open))

-- The same protocol as XML, which is what '--protocol' writes when the file it
-- names ends in '.xml' (see 'withEvalFunc'). It carries the very facts the text
-- format carries and carries them as markup rather than as a 𝜑-term a reader
-- would have to parse back: the name of an element says what its record is,
-- the symbol a term stands for stands in 'symbol', and the value the record
-- carries stands as the text of the element, so the edge from the line that
-- minted an unknown to the line that consumed it is read off the markup
-- instead of off the spelling of a term (#1245, #1257). Where the text format
-- names an earlier line, this one repeats that line's symbol, since the symbol
-- is what the two lines share and a name is only how the text format spells
-- it. The term itself stays as the text of the element, for a reader and not
-- for a program.
--
-- Nothing is buffered: an element is written the moment its record arrives,
-- and the ones it closes are written just before it, so a run firing thousands
-- of λ functions costs no more memory than one firing a single λ function and
-- the last element to reach the disk is the last one the run got to. What is
-- still open when the run ends is closed by 'endEvalXml'.
saveEvalXml :: Handle -> IORef Nesting -> (Expression -> IO String) -> SaveEvalFunc
saveEvalXml handle cursor render report = do
  written <- atomicModify cursor (elements report)
  mapM_ (hPutStrLn handle) written
  logDebug (printf "Saved %d line(s) of the XML protocol" (length written))
  where
    -- The elements a report is written as, together with what the protocol has
    -- counted once they are written. A report closes every firing it stands
    -- outside of before it opens or writes anything of its own.
    elements :: Evaluation -> Nesting -> IO (Nesting, [String])
    elements (EvRun judgment locator) nesting =
      pure
        ( nesting{_closing = (0, "protocol") : nesting._closing}
        ,
          [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
          , printf "<protocol judgment=\"%s\" of=\"%s\">" (quoted judgment) (quoted locator)
          ]
        )
    elements (EvFiring depth key) nesting =
      pure
        ( nesting{_fires = fires, _closing = (depth, "evaluate") : kept}
        , closers ++ [indented depth (printf "<evaluate λ=\"%s\" id=\"%d\">" (quoted key) fires)]
        )
      where
        (kept, closers) = closed depth nesting._closing
        fires :: Int
        fires = nesting._fires + 1
    elements (EvStuck depth key) nesting =
      pure (nesting{_closing = kept}, closers ++ [indented depth (printf "<stuck λ=\"%s\"/>" (quoted key))])
      where
        (kept, closers) = closed depth nesting._closing
    elements (EvData depth spelling _ value) nesting =
      pure (nesting{_closing = kept}, closers ++ [indented depth (stood value)])
      where
        (kept, closers) = closed depth nesting._closing
        -- A 'dataize' operand has no term of its own to show: it either came
        -- down to data, which is the data, or to the datum manufactured for an
        -- unknown, which is that unknown and never the 42 standing for it.
        -- These are two different facts, so the name of the element tells them
        -- apart the way 𝔻(…) does in the text format, rather than leaving a
        -- reader to test which of two attributes an element carries (#1257).
        stood :: Either Int Bytes -> String
        stood (Left symbol) = printf "<dataize meta=\"%s\">%s</dataize>" (quoted spelling) (sigma symbol)
        stood (Right bytes) = printf "<bind meta=\"%s\">%s</bind>" (quoted spelling) (escapeXMLText (printBytes bytes))
    elements (EvTerm depth spelling _ term) nesting = do
      body <- render term
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indented depth (printf "<bind meta=\"%s\"%s>%s</bind>" (quoted spelling) (carried term) (escapeXMLText body))])
    elements (EvAnswer depth term) nesting = do
      body <- render term
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indented depth (printf "<answer%s>%s</answer>" (carried term) (escapeXMLText body))])
    -- The unknown a term stands for, where it carries one. A term standing for
    -- nothing takes no attribute at all, the terminator ⊥ included, since it is
    -- itself and its own text already says so.
    carried :: Expression -> String
    carried term = maybe "" (printf " symbol=\"%s\"" . sigma) (denoted term)
    -- The name of a symbol, spelled the way every term carrying it is spelled,
    -- so a reader joining an attribute to a term compares two strings that
    -- look alike instead of a number against a name.
    sigma :: Int -> String
    sigma = printFunction . FnSymbol
    quoted :: T.Text -> String
    quoted = escapeXML . T.unpack

-- Close every element the run left open, innermost first, which is what makes
-- the document well-formed however the run ended. It is written on the way out
-- of 'withEvalFunc', failure included, so a run giving up half-way through a
-- derivation still leaves a file a parser can read. A run failing before it
-- opened the protocol leaves an empty file, exactly as it leaves one under the
-- text format.
endEvalXml :: Handle -> IORef Nesting -> IO ()
endEvalXml handle cursor = do
  nesting <- readIORef cursor
  mapM_ (hPutStrLn handle) (snd (closed 0 nesting._closing))
  writeIORef cursor nesting{_closing = []}

-- The elements a record standing at this depth closes, innermost first,
-- together with what stays open once they are written. A record belongs to the
-- firing opened above it, so one standing at the depth of an open element, or
-- shallower than it, is the first record after that element and ends it.
closed :: Int -> [(Int, String)] -> ([(Int, String)], [String])
closed depth open = (kept, [indented level (printf "</%s>" element) | (level, element) <- shut])
  where
    (shut, kept) = span ((>= depth) . fst) open

-- Stand a line at the depth of what it reports, which is what makes both
-- protocols a tree rather than a list: two spaces per level.
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
