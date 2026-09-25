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
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Files (overwrite)
import GHC.Clock (getMonotonicTime)
import Logger (logDebug, logInfo)
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
-- term already holds, which symbol the last datum was manufactured for,
-- since every symbol dataizes to the very same 42 and only the state can tell
-- the protocol which unknown that 42 stood for, and which λ function the last
-- reduction of it was parked on, since a run '--partial' parks answers a
-- residue and the name of what parked it would otherwise be lost with the
-- signal the residue was made of (#1288).
data State = State
  { _minted :: Int
  , _manufactured :: Maybe Int
  , _stuck :: Maybe T.Text
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

-- The judgment a run of the protocol records, which is the one thing the two
-- formats spell in two ways: the text format writes the letter the calculus
-- writes, '𝕄(Φ.x)', and the markup names the root after it, '<morph
-- at="Φ.x">', the way every record under it is named after the judgment it
-- carries (#1279). A stuck site spells it the same two ways, since it too is a
-- judgment asking and getting no answer (see 'EvStuck'); nothing else is
-- spelled twice, since nothing else of a record is a name of the calculus.
data Judgment
  = -- The Morphing function 𝕄, which the 'morph' command runs.
    Morphing
  | -- The Dataization function 𝔻, which the 'dataize' command runs.
    Dataization

-- The letter the calculus writes a judgment with, which is how the text format
-- opens a run of it.
letter :: Judgment -> String
letter Morphing = "𝕄"
letter Dataization = "𝔻"

-- The element the markup opens a run of a judgment with, and closes it under,
-- named after the judgment the way '<evaluate>' is named after 𝔼. A root
-- '<dataize>' carries the locator the run was aimed at where one inside a
-- firing carries the meta it bound, which is the very difference the text
-- format draws between '𝔻(Φ)' at the top and '𝛿1.2 := 𝔻(…)' in a block.
opened :: Judgment -> String
opened Morphing = "morph"
opened Dataization = "dataize"

-- What '--acyclic' takes for the same formation entered again, which is how
-- sure a cut is that the recursion it stops would never have stopped. 'Proven'
-- takes a formation 'alike' one a frame above entered, the same up to a
-- renaming of symbols, and a formation like that replays its round forever.
-- 'Plausible' takes one the formation a frame above entered is 'within', with
-- the same attributes and λ function and every term the earlier one bound
-- there found again in it, maybe under wrappers it gained: it cuts a
-- recursion whose accumulator grows on every round as well, and now and then
-- one that would have stopped (#1451).
data Acyclic
  = Proven
  | Plausible
  deriving (Bounded, Enum, Eq, Show)

-- The word the command line and the protocol spell a mode of '--acyclic' with.
certainty :: Acyclic -> String
certainty Proven = "proven"
certainty Plausible = "plausible"

-- One line of the protocol the '--protocol' option writes, which is a tree of
-- the firings of the Evaluation function 𝔼 rather than a list of them. The run
-- itself opens it — '𝕄(Q.φ)' for a morphing, '𝔻(Q)' for a dataization — and
-- under it stands one block per firing, '𝔼(L_number_plus)  # 𝔻(Φ.φ)', naming
-- the entry that answered, the judgment that asked for the firing and the site
-- of the program it was fired at (#1302, #1306). Inside a block stand the
-- operands the entry bound — each with the judgment that reduced it and the
-- term it was reduced from — whatever a 'symbolize' line of it knows about a
-- symbol it minted, the terms a 'join' line of it made one of and what it
-- knows about the symbol they were joined into, and the term it answered with,
-- one to a line, and any firing an operand took while it was being reduced,
-- one level deeper again. The answer takes two of those lines rather than one:
-- the term the entry wrote, then the normal form 𝕄 makes of it, so the
-- morphing between them is a step a reader watches happen rather than a shape a
-- term arrives in (#1298). A name no entry answers stands there as
-- '?(L_number_nope)', where the block of its firing would have been. A
-- formation 𝔻 gets into through its 'box' rule opens a block of its own,
-- 'formation(⟦ … ⟧)  # 𝔻(Φ.x)', and what its φ body fires stands under it
-- (#1420).
data Evaluation
  = -- The run and the term it was aimed at.
    EvRun Judgment T.Text
  | -- One firing of the entry under that key, at the depth its nesting gives
    -- it, together with the site it was fired at: the locator of the part of
    -- the program the firing belongs to, which is the aim of the run refined by
    -- the '--deep' walk as it enters a binding (see '_site' in 'Morph'). The
    -- key says which entry answered and one entry answers the same way wherever
    -- it is fired, so the site is the one thing telling two firings of it apart
    -- by something other than the order they came in, and it is written as the
    -- comment of the line the way a stuck site carries the formation it was
    -- asked about (#1302). The judgment stands beside it for the reason a stuck
    -- site carries one: 𝔼 is fired from the 'ml' rule of morphing and from the
    -- 'fire' rule of dataization, and the comment says what was running over
    -- that part of the program rather than leaving a locator to say it alone
    -- (#1306).
    EvFiring Int T.Text Judgment Expression
  | -- A formation 𝔻 got into through the 'box' rule, at the depth its nesting
    -- gives it, together with the site it was entered at (see '_site' in
    -- 'Morph'). The box rule is the one place a judgment gets into a formation
    -- without firing it: 𝕄 stops at a formation and hands it back, and a
    -- formation whose λ is fired is already an 'EvFiring'. Everything 𝔻 does
    -- inside the φ body, the firings its dataization demands above all, stands
    -- one level deeper, under this record, so a reader sees which object a
    -- firing was made on the way into rather than a flat list of firings. It
    -- opens a block the way a firing does, by indentation in the text format
    -- and by an element in the markup, but it is no firing: it counts nothing
    -- and names no meta, so the metas of the firings under it are numbered as
    -- if it were not there (#1420).
    EvFormation Int Expression Expression
  | -- A frame '--acyclic' cut as it opened, since a frame above it had already
    -- entered the formation it was about to enter, by the mode it was given:
    -- the depth the frame would have opened at, the judgment it belonged to,
    -- the mode that took the two formations for the same one, the formation
    -- the frame above entered, as that frame had it, and the site the cut was
    -- made at. It stands where the 'EvFormation' of the cut frame would have
    -- stood, and its term is the very term of the round that was kept, so a
    -- reader, or a program, pairs the two by their terms without renaming any
    -- symbol by eye, and sees the recursion cut at its site rather than
    -- reconstructing the cut from the residual. Nothing runs under a cut, so
    -- the line stands alone and no block opens under it, the way none opens
    -- under a stuck site (#1434).
    EvLooped Int Judgment Acyclic Expression Expression
  | -- A λ function no entry of the '--symbolic' file answers, at the depth the
    -- firing of it would have stood at, together with the judgment that asked
    -- for the firing and the formation 𝔼 was fired against, as it was handed
    -- it. Nothing fired, so the line stands alone and no block opens under it.
    -- It is written whether or not '--partial' goes on to park the run, since
    -- the protocol records what 𝔼 was asked for and a question it could not
    -- answer belongs there as much as one it could — and the object it was
    -- asked about is half of that question, so the record carries it the way
    -- every other one carries the term it is about, as the comment of the line
    -- in the text format and as the text of the element in the markup. The
    -- judgment stands beside it because 𝔼 is fired from two places — the 'ml'
    -- rule of morphing and the 'fire' rule of dataization — and which of them
    -- asked is what says where in the reduction the site stands (#1300).
    EvStuck Int T.Text Judgment Expression
  | -- A 'dataize' operand of the firing: the meta it bound, the term the entry
    -- wrote under that meta, and the data it came down to, or the symbol that
    -- data was manufactured for.
    EvData Int T.Text Expression (Either Int Bytes)
  | -- A 'morph' operand of the firing: the meta it bound, the term the entry
    -- wrote under that meta, and the normal form 𝕄 reached.
    EvTerm Int T.Text Expression Expression
  | -- A 'symbolize' line of the firing: the meta it bound, the meta of the
    -- entry it was told to stand the data of, and the term that standing made
    -- — the very term that meta is bound to, with the data of it standing for
    -- unknowns. It is a binding like 'EvTerm' and differs in what the line is
    -- commented with, since nothing of the calculus runs here: the line names
    -- a meta the entry bound above it, the way a 'join' line names the two it
    -- joined, where an operand line names the judgment that reduced it
    -- (#1306). A 'rewrite' line is written the same way and for the same
    -- reason: it applies the rules of the entry to a meta bound above it and
    -- reduces nothing either (#1409).
    EvSymbolize Int T.Text Expression Expression
  | -- What is known about a symbol a 'symbolize' line minted: dataizing the
    -- formation the symbol names answers these bytes. It is a fact about the
    -- symbol and no binding of it, since a 𝜎 is the name of a λ function and
    -- neither a datum nor a term, so it stands on a line of its own rather
    -- than beside a meta the firing bound (#1269).
    EvKnown Int Int Bytes
  | -- A 'join' line of the firing: the meta it bound, the two metas whose terms
    -- it joined, in the order the entry wrote them, and the term they joined
    -- into. It is a binding like 'EvTerm' and differs only in what the line is
    -- commented with, a 'join' line naming two metas of the entry where every
    -- other block names a term of the calculus (#1246).
    EvJoin Int T.Text (T.Text, T.Text) Expression
  | -- What is known about a symbol the join of two branches of a fork minted:
    -- dataizing the formation it names answers what dataizing one of the two
    -- formations the branches carried answers, and which of the two it is is
    -- the very thing nobody has worked out. The two stand in the order the
    -- entry listed the branches under '𝑛', so a reader who knows the entry
    -- knows which of them belongs to which branch. It is a fact about the
    -- symbol and no binding of it, exactly as 'EvKnown' is (#1246).
    EvJoined Int Int (Int, Int)
  | -- A 'join' line one of whose two terms is ⊥, which is how a program
    -- spells "raise unless the condition holds": what the condition of the
    -- fork came down to — the first operand the entry dataized, a symbol or
    -- data, where it dataized any — the side that raises, 'left' or 'right' in
    -- the order the entry wrote the two metas, and the meta holding the ⊥. It
    -- stands ahead of the 'join' line, which binds the other side, so a reader
    -- renders the record as a throw on that side of the condition (#1405).
    EvTerminate Int (Maybe (Either Int Bytes)) T.Text T.Text
  | -- A fresh symbol the answer of the firing asked for, one record per bare 𝜎
    -- the entry wrote it with. It is a fact about the firing and no property of
    -- any one term of it, since an answer may carry several symbols or none and
    -- no single one of them stands for the whole of it (#1280). It carries the
    -- values the 'dataize' operands of the entry came down to, in the order
    -- the entry declares them, a symbol or data each, since what the fresh
    -- symbol stands for is what the λ function makes of them, and a reader
    -- rendering the symbol back into a program reads that fact off the record
    -- rather than assembling it from the lines above it (#1421).
    EvMinted Int Int [Either Int Bytes]
  | -- The term the entry wrote as its answer, with the symbols the firing
    -- minted standing in it, before 𝕄 is asked about it. It is the first of
    -- the two records an answer is written as, and it is there because the
    -- answer of a firing is morphed (#1268) and a morphing nobody sees is a
    -- term appearing out of nothing: whatever that morphing fires opens its
    -- own block between this record and 'EvAnswer', so a reader sees the term
    -- the entry wrote, the firings reducing it took, and the normal form it
    -- came to, in that order (#1298). The line of it is commented with '𝑛',
    -- the key the entry writes its answer under, the way an operand line
    -- carries the term it was reduced from: the name on the left is minted by
    -- the protocol and says nothing about where the term was read from.
    EvBuilt Int Expression
  | -- What the firing answered with, which is the term of 'EvBuilt' as 𝕄
    -- leaves it. It is the answer every consumer reads, since it is the term
    -- the walk stands back into the program. The line of it is commented with
    -- '𝕄(𝑛.4.1)', naming the line it was morphed from, since the two may stand
    -- whole blocks apart and a value alone never says what it came from — the
    -- very reason an operand line carries the term it was reduced from.
    EvAnswer Int Expression

type SaveEvalFunc = Evaluation -> IO ()

-- The names the text protocol has given to the terms it has written out,
-- keyed by a cheap fixed-size digest of the term (see 'hashExpression') the
-- way 'Seen' keys the formations '--acyclic' has entered. A digest collision is
-- resolved by an exact structural comparison, so the common case stays O(1) on
-- the digest while a name still stands for the very term it was given to.
-- Keying on the whole term and not on the first symbol it carries is what
-- keeps the format honest: the symbolized copy of a term carries the same
-- first symbol as the term it was made of and differs deeper down, so naming
-- the copy after the original claimed nothing was replaced on the very line
-- that replaced something (#1292).
type Named = Map.Map Int [(Expression, T.Text)]

-- The name an earlier line gave this very term, if one did. The digest lookup
-- is fast; the (==) check runs only on a digest match, so two terms that differ
-- anywhere are two terms and neither is ever written as the other.
namedLookup :: Expression -> Named -> Maybe T.Text
namedLookup term names = lookup term (Map.findWithDefault [] (hashExpression term) names)

-- Remember the name a line gives a term, under the digest of that term,
-- keeping the names of any term that collides with it. A term written out
-- twice takes the name of the later line, which is the line a reader counting
-- back from the next one reaches first.
namedInsert :: Expression -> T.Text -> Named -> Named
namedInsert term naming = Map.alter renamed (hashExpression term)
  where
    renamed :: Maybe [(Expression, T.Text)] -> Maybe [(Expression, T.Text)]
    renamed entries = Just ((term, naming) : filter ((/= term) . fst) (fromMaybe [] entries))

-- What the protocol has counted so far: how many firings the whole run has
-- opened, which is what numbers them and so tells a line of one firing from
-- the same line of any other; the name last given to each term, which is how
-- a term already written out is named instead of written again; and which
-- firing is open at each depth, since the operands of a firing belong to the
-- firing it was when it started and not to the one another firing has made of
-- it since. The firings are numbered across the run rather than per λ
-- function, so no two of them give an operand meta the same name and a name
-- the protocol points back to points at one line only (#1261). The answers are
-- numbered by that same counter and no counter of their own, since a firing
-- answers once and so the two lines of its answer are told from every other
-- pair by the firing they stand in (#1298). The order the firings come in
-- carries nothing — it is the order 𝕄 walks the term — so the symbols are what
-- the dependencies are read from: a term carrying 𝜎4 is the term the line that
-- minted 𝜎4 stood for.
data Protocol = Protocol
  { _fired :: Int
  , _named :: Named
  , _open :: Map.Map Int Int
  }

-- The protocol before a single firing has been written.
emptyProtocol :: Protocol
emptyProtocol = Protocol 0 Map.empty Map.empty

-- What the XML protocol has counted so far: how many firings the whole run
-- has opened, the same single counter 'Protocol' keeps since #1261, which
-- through '_openedAt' names a meta on this firing the way the text format
-- names it and not with the bare spelling the entry's YAML gives it, an answer
-- of it included (#1298), and which no element carries on its own (#1422); and
-- the elements standing open around the record being written, innermost
-- first, each with the depth it was opened at and the name it closes under.
-- The text format needs no such stack, since indentation opens and closes
-- nothing; markup does, and the depth a record carries is the only thing
-- saying which firings it stands outside of.
data Nesting = Nesting
  { _fires :: Int
  , _openedAt :: Map.Map Int Int
  , _closing :: [(Int, String)]
  }

-- The XML protocol before a single element has been opened.
emptyNesting :: Nesting
emptyNesting = Nesting 0 Map.empty []

-- Append the line of one record to the protocol, indented by the depth of what
-- it reports and numbered by what the protocol has seen before it. The handle
-- stays open for the whole run, since a run may fire thousands of λ functions
-- and reopening the file for each of them buys nothing; the counting rides in
-- an 'IORef' next to it, since it is the cursor of the file and not a property
-- of the reduction. Expressions are rendered by the caller, which flattens
-- them, so a line never spills over more than one. There are two renderers and
-- not one because the operand a line is commented with is spelled salty while
-- the value it took is spelled the way the run prints its own answer: the
-- sweet syntax drops the ξ of 'ξ.x' and leaves a bare 'x', which is the very
-- thing the comment is there to say (see 'commented').
saveEval :: Handle -> IORef Protocol -> (Expression -> IO String) -> (Expression -> IO String) -> SaveEvalFunc
saveEval handle cursor render salted report = do
  line <- atomicModify cursor (written report)
  mapM_ saved line
  where
    -- Put one line of the protocol on the disk and say in the log what went
    -- there, the indentation of it dropped, since the log is a list of what
    -- happened and no tree.
    saved :: String -> IO ()
    saved line = do
      hPutStrLn handle line
      logDebug (printf "Saved one line of the protocol: %s" (dropWhile (== ' ') line))
    -- The line a report is written as, where it is written as one, together
    -- with what the protocol has counted once it is written. A term is looked
    -- up by the first symbol it carries and, where that symbol has a name
    -- already, written as that name; otherwise it is written out and the
    -- symbol takes the name of this line.
    --
    -- A symbol the answer of a firing minted is the one record this format
    -- keeps no line for: the answer stands spelled out on the line of it,
    -- symbols and all, so a reader ties a later 𝔻(⟦ λ ⤍ 𝜎4 ⟧) back to the
    -- firing that minted 𝜎4 by reading the very term it answered with. Only
    -- the markup, where a term is text and not a thing to be read, spells the
    -- fact out (#1280).
    written :: Evaluation -> Protocol -> IO (Protocol, Maybe String)
    written (EvRun judgment locator) protocol =
      pure (protocol, Just (printf "%s(%s)" (letter judgment) (T.unpack locator)))
    written (EvFiring depth key judgment site) protocol = do
      locator <- render site
      pure
        ( protocol
            { _fired = firings
            , _open = Map.insert depth firings protocol._open
            }
        , Just (indented depth (printf "𝔼(%s)  # %s(%s)" (T.unpack key) (letter judgment) locator))
        )
      where
        firings :: Int
        firings = protocol._fired + 1
    written (EvFormation depth self site) protocol = do
      form <- render self
      locator <- render site
      pure (protocol, Just (indented depth (printf "formation(%s)  # %s(%s)" form (letter Dataization) locator)))
    written (EvLooped depth judgment mode self site) protocol = do
      form <- render self
      locator <- render site
      pure (protocol, Just (indented depth (printf "looped(%s)  # %s(%s), %s" form (letter judgment) locator (certainty mode))))
    written (EvStuck depth key judgment self) protocol = do
      form <- render self
      pure (protocol, Just (indented depth (printf "?(%s)  # %s(%s)" (T.unpack key) (letter judgment) form)))
    written (EvData depth spelling operand value) protocol = do
      datum <- spelled value
      line <- commented (printf "%s := %s" (labelled protocol depth spelling) datum) Dataization operand
      pure (protocol, Just (indented depth line))
      where
        spelled :: Either Int Bytes -> IO String
        spelled (Left symbol) = printf "𝔻(%s)" <$> render (standing symbol)
        spelled (Right bytes) = pure (printBytes bytes)
    written (EvTerm depth spelling operand term) protocol = do
      let naming = labelled protocol depth spelling
      (protocol', value) <- valued protocol naming term
      line <- commented (printf "%s := %s" naming value) Morphing operand
      pure (protocol', Just (indented depth line))
    written (EvSymbolize depth spelling source term) protocol = do
      let naming = labelled protocol depth spelling
      (protocol', value) <- valued protocol naming term
      line <- commented' (printf "%s := %s" naming value) source
      pure (protocol', Just (indented depth line))
    written (EvKnown depth symbol bytes) protocol = do
      form <- render (standing symbol)
      pure (protocol, Just (indented depth (printf "𝔻(%s) == %s" form (printBytes bytes))))
    written (EvJoin depth spelling (left, right) term) protocol = do
      let naming = labelled protocol depth spelling
      (protocol', value) <- valued protocol naming term
      pure (protocol', Just (indented depth (printf "%s := %s  # [%s, %s]" naming value (T.unpack left) (T.unpack right))))
    written (EvJoined depth fresh (one, two)) protocol = do
      form <- render (standing fresh)
      left <- render (standing one)
      right <- render (standing two)
      pure (protocol, Just (indented depth (printf "𝔻(%s) ∈ { 𝔻(%s), 𝔻(%s) }" form left right)))
    written (EvTerminate depth condition side raised) protocol = do
      cond <- maybe (pure []) (fmap pure . spelled) condition
      pure (protocol, Just (indented depth (printf "terminate(%s)  # %s" (intercalate ", " (cond ++ [T.unpack side])) (T.unpack raised))))
      where
        spelled :: Either Int Bytes -> IO String
        spelled (Left symbol) = printf "𝔻(%s)" <$> render (standing symbol)
        spelled (Right bytes) = pure (printBytes bytes)
    written EvMinted{} protocol = pure (protocol, Nothing)
    written (EvBuilt depth term) protocol = do
      value <- borrowed protocol term
      pure (protocol, Just (indented depth (printf "%s.1 := %s  # %s" (labelled protocol depth answer) value (T.unpack answer))))
    written (EvAnswer depth term) protocol = do
      let stem :: String
          stem = labelled protocol depth answer
          naming :: String
          naming = printf "%s.2" stem
      (protocol', value) <- valued protocol naming term
      pure (protocol', Just (indented depth (printf "%s := %s  # 𝕄(%s.1)" naming value stem)))
    -- The value of a term, next to the name this line gives it: the name an
    -- earlier line gave this very term, where one did, and the term itself
    -- otherwise. Either way the term takes the name of this line, so the next
    -- line holding it points back here and not further. Only a term whose
    -- value is a symbol is named at all, since that is a term a firing
    -- answered with and every other one is worth no less written out than
    -- pointed at; the term is matched verbatim, so a term that differs from
    -- the one a name stands for is written out however deep the difference
    -- sits (#1292).
    valued :: Protocol -> String -> Expression -> IO (Protocol, String)
    valued protocol naming term = case denoted term of
      Nothing -> (,) protocol <$> render term
      Just _ -> do
        value <- maybe (render term) (pure . T.unpack) (namedLookup term protocol._named)
        pure (protocol{_named = namedInsert term (T.pack naming) protocol._named}, value)
    -- The value of a term on a line that claims no name for it: the name an
    -- earlier line gave this very term, where one did, and the term itself
    -- otherwise. The built answer of a firing stands on such a line, since the
    -- line under it holds the term 𝕄 made of that one and the two are not the
    -- same term: were the first of the pair to claim the name, the second
    -- would be written as the first and the morphing would be as invisible as
    -- it was before it had a line at all (#1298).
    borrowed :: Protocol -> Expression -> IO String
    borrowed protocol term = case namedLookup term protocol._named of
      Nothing -> render term
      Just naming -> pure (T.unpack naming)
    -- The line of an operand with the judgment that reduced it and the term it
    -- was reduced from appended to it as a comment, since the value alone says
    -- what the meta was bound to and neither what it was bound from nor what
    -- was done to it — and which of the two judgments ran is the whole
    -- difference between a line ending in data and one ending in a term
    -- (#1306). It is the very term the entry wrote under
    -- the meta, spelled the way the calculus reads it — '$.x' is read as 'ξ.x'
    -- — which is why it goes through 'salted' and not through the 'render' the
    -- value goes through: the sweet syntax writes that same term as a bare 'x',
    -- and a bare 'x' reads as a name rather than as the term it is. It is
    -- flattened like everything else, so the whole line stays one line of 𝜑.
    commented :: String -> Judgment -> Expression -> IO String
    commented line judgment operand = printf "%s  # %s(%s)" line (letter judgment) <$> salted operand
    -- The same for a line no judgment made: a 'symbolize' one, which stands
    -- the data of a term into unknowns and reduces nothing, so the comment
    -- names the meta of the entry it was told to stand rather than a judgment
    -- applied to a term of the calculus (see 'EvSymbolize').
    commented' :: String -> Expression -> IO String
    commented' line source = printf "%s  # %s" line <$> salted source
    -- The name of an operand meta on this firing of its λ function: the meta
    -- the entry spells it with and which firing of the run this is, since
    -- every entry numbers its own metas from 𝛿1 and 𝑛1 and only the firing
    -- tells two 𝛿1 apart. The number counts the firings of the whole run and
    -- not those of one λ function, so the second firing of one entry and the
    -- second of another never write the same name (#1261), and it is the very
    -- number the XML format gives the firing in its 'id'. The answer of the
    -- firing is named the same way, with a step of its own appended: a firing
    -- answers once, so '𝑛.4.1' and '𝑛.4.2' are the built term and the normal
    -- form of the one answer firing 4 gave (#1298). The firing a line
    -- belongs to is the one opened one level above it.
    labelled :: Protocol -> Int -> T.Text -> String
    labelled protocol depth spelling =
      printf "%s.%d" (T.unpack spelling) (fromMaybe 0 (Map.lookup (depth - 1) protocol._open))

-- The same protocol as XML, which is what '--protocol' writes when the file it
-- names ends in '.xml' (see 'withEvalFunc'). It carries the very facts the text
-- format carries and carries them as markup rather than as a 𝜑-term a reader
-- would have to parse back: the name of an element says what its record is and
-- the value the record carries stands as the text of the element, so the edge
-- from the firing that minted an unknown to the record that consumed it is read
-- off the markup instead of off the spelling of a term (#1245, #1257). That
-- edge is what 'minted' carries: a firing hands out one symbol per bare 𝜎 of
-- its answer and each of them stands in a record of its own, the way what is
-- known about a symbol does, since no one symbol of a term stands for the whole
-- of it and picking one would say nothing (#1280). The term itself stays as the
-- text of the element, for a reader and not for a program.
--
-- The two lines an answer stands on are two elements, and they are told apart
-- by their names for the same reason every other pair of records is: 'built'
-- holds the term the entry wrote and 'answer' the normal form 𝕄 made of it, so
-- a consumer reading 'answer' reads what it always read and one asking what the
-- entry itself wrote has an element to ask (#1298).
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
        ( nesting{_closing = (0, opened judgment) : nesting._closing}
        ,
          [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
          , printf "<%s at=\"%s\">" (opened judgment) (quoted locator)
          ]
        )
    elements (EvFiring depth key judgment site) nesting = do
      locator <- render site
      pure
        ( nesting
            { _fires = fires
            , _openedAt = Map.insert depth fires nesting._openedAt
            , _closing = (depth, "evaluate") : kept
            }
        , closers ++ [indented depth (printf "<evaluate λ=\"%s\" by=\"%s\" at=\"%s\">" (quoted key) (opened judgment) (escapeXML locator))]
        )
      where
        (kept, closers) = closed depth nesting._closing
        fires :: Int
        fires = nesting._fires + 1
    elements (EvFormation depth self site) nesting = do
      form <- render self
      locator <- render site
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = (depth, "formation") : kept}, closers ++ [indented depth (printf "<formation at=\"%s\" term=\"%s\">" (escapeXML locator) (escapeXML form))])
    elements (EvLooped depth judgment mode self site) nesting = do
      form <- render self
      locator <- render site
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indented depth (printf "<looped by=\"%s\" match=\"%s\" at=\"%s\" term=\"%s\"/>" (opened judgment) (certainty mode) (escapeXML locator) (escapeXML form))])
    elements (EvStuck depth key judgment self) nesting = do
      form <- render self
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indented depth (printf "<stuck λ=\"%s\" by=\"%s\">%s</stuck>" (quoted key) (opened judgment) (escapeXMLText form))])
    elements (EvData depth spelling _ value) nesting = do
      record <- stood value
      pure (nesting{_closing = kept}, closers ++ [indented depth record])
      where
        (kept, closers) = closed depth nesting._closing
        -- An operand of a 'dataize' line either came down to data, which is
        -- the data, or to the datum manufactured for an unknown, which is the
        -- formation that unknown names and never the 42 standing for it. These
        -- are two different facts, so the name of the element tells them apart
        -- the way 𝔻(…) does in the text format, rather than leaving a reader
        -- to test which of two attributes an element carries (#1257). What 𝔻
        -- was applied to is a term either way, and the element named after the
        -- judgment holds it as the text format holds it (#1278).
        stood :: Either Int Bytes -> IO String
        stood (Left symbol) = do
          form <- render (standing symbol)
          pure (printf "<dataize meta=\"%s\">%s</dataize>" (escapeXML (labelled nesting depth spelling)) (escapeXMLText form))
        stood (Right bytes) = pure (printf "<bind meta=\"%s\">%s</bind>" (escapeXML (labelled nesting depth spelling)) (escapeXMLText (printBytes bytes)))
    elements (EvTerm depth spelling _ term) nesting = do
      body <- render term
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indented depth (printf "<bind meta=\"%s\">%s</bind>" (escapeXML (labelled nesting depth spelling)) (escapeXMLText body))])
    -- A 'symbolize' line binds a meta to a term like every other line of a
    -- firing, and the markup holds what it was bound to and not what it was
    -- made from: the term an operand was reduced from is what the text format
    -- comments a line with and the markup has never carried, so the two lines
    -- the text now tells apart by that comment are one element here (#1306).
    elements (EvSymbolize depth spelling _ term) nesting = do
      body <- render term
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indented depth (printf "<bind meta=\"%s\">%s</bind>" (escapeXML (labelled nesting depth spelling)) (escapeXMLText body))])
    elements (EvKnown depth symbol bytes) nesting =
      pure (nesting{_closing = kept}, closers ++ [indented depth known])
      where
        (kept, closers) = closed depth nesting._closing
        known :: String
        known = printf "<known symbol=\"%s\">%s</known>" (sigma symbol) (escapeXMLText (printBytes bytes))
    elements (EvJoin depth spelling _ term) nesting = do
      body <- render term
      let (kept, closers) = closed depth nesting._closing
      pure (nesting{_closing = kept}, closers ++ [indented depth (printf "<bind meta=\"%s\">%s</bind>" (escapeXML (labelled nesting depth spelling)) (escapeXMLText body))])
    elements (EvJoined depth fresh (one, two)) nesting =
      pure (nesting{_closing = kept}, closers ++ [indented depth joint])
      where
        (kept, closers) = closed depth nesting._closing
        joint :: String
        joint = printf "<joined symbol=\"%s\">%s %s</joined>" (sigma fresh) (sigma one) (sigma two)
    elements (EvTerminate depth condition side _) nesting =
      pure (nesting{_closing = kept}, closers ++ [indented depth terminal])
      where
        (kept, closers) = closed depth nesting._closing
        -- The condition a symbol stands for is named by it, the way 'joined'
        -- names one, and data the condition came down to is the text.
        terminal :: String
        terminal = case condition of
          Just (Left symbol) -> printf "<terminate symbol=\"%s\" branch=\"%s\"/>" (sigma symbol) (quoted side)
          Just (Right bytes) -> printf "<terminate branch=\"%s\">%s</terminate>" (quoted side) (escapeXMLText (printBytes bytes))
          Nothing -> printf "<terminate branch=\"%s\"/>" (quoted side)
    elements (EvMinted depth symbol operands) nesting =
      pure (nesting{_closing = kept}, closers ++ [indented depth mint])
      where
        (kept, closers) = closed depth nesting._closing
        -- The symbol stands in 'symbol', the way 'known' and 'joined' put
        -- theirs, and the values the λ function was fired on are the text,
        -- each spelled the way its own line spells it (#1421).
        mint :: String
        mint
          | null operands = printf "<minted symbol=\"%s\"/>" (sigma symbol)
          | otherwise = printf "<minted symbol=\"%s\">%s</minted>" (sigma symbol) (escapeXMLText (unwords (map spelled operands)))
        spelled :: Either Int Bytes -> String
        spelled (Left fresh) = sigma fresh
        spelled (Right bytes) = printBytes bytes
    elements (EvBuilt depth term) nesting = do
      body <- render term
      let (kept, closers) = closed depth nesting._closing
          naming :: String
          naming = printf "%s.1" (labelled nesting depth answer)
      pure (nesting{_closing = kept}, closers ++ [indented depth (printf "<built meta=\"%s\">%s</built>" (escapeXML naming) (escapeXMLText body))])
    elements (EvAnswer depth term) nesting = do
      body <- render term
      let (kept, closers) = closed depth nesting._closing
          naming :: String
          naming = printf "%s.2" (labelled nesting depth answer)
      pure (nesting{_closing = kept}, closers ++ [indented depth (printf "<answer meta=\"%s\">%s</answer>" (escapeXML naming) (escapeXMLText body))])
    -- The name of an operand meta on this firing, spelled the way the text
    -- protocol's own 'labelled' spells it: the meta the entry names it with
    -- in the YAML, followed by which firing of the whole run this is, the
    -- very number the element's own 'id' carries, since every entry numbers
    -- its own metas from 𝛿1 and 𝑛1 and only the firing tells two 𝛿1 apart
    -- (#1261). An answer is named the same way, with the step of the pair
    -- appended (#1298). The firing a record belongs to is the one opened one
    -- level above it.
    labelled :: Nesting -> Int -> T.Text -> String
    labelled nesting depth spelling =
      printf "%s.%d" (T.unpack spelling) (fromMaybe 0 (Map.lookup (depth - 1) nesting._openedAt))
    -- The name of a symbol, spelled the way every term carrying it is spelled,
    -- so a reader joining a record to a term compares two strings that look
    -- alike instead of a number against a name.
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

-- The formation a symbol names, which is what 𝔻 brought an operand down to and
-- what a 'symbolize' line knows the data of: a 𝜎 is the name of a λ function
-- and no term of its own, so 𝔻 is applied to the formation carrying it and
-- never to the name alone (#1269). Both formats stand it where they report
-- what 𝔻 was applied to, since they carry the same facts and disagreeing about
-- this one would make a reader of the markup believe 𝔻 took a name (#1278).
standing :: Int -> Expression
standing symbol = ExFormation [BiLambda (FnSymbol symbol)]

-- How the calculus spells the meta a λ function writes its answer to, which is
-- the name the protocol writes the two lines of a firing's answer under.
answer :: T.Text
answer = "𝑛"

dontSaveEval :: SaveEvalFunc
dontSaveEval _ = pure ()

-- What '--log-level=INFO' has counted of a run so far: when the run began and
-- when a line about it last reached the console, both on the monotonic clock in
-- seconds, and how many formations it has entered and how many λ functions it
-- has fired. A long run prints nothing else until it ends, so a stuck entry and
-- a slow one look the same from outside; the counts say whether it advances
-- and the site of the latest record says where it is (#1470).
data Progress = Progress
  { _began :: Double
  , _told :: Maybe Double
  , _formations :: Int
  , _firings :: Int
  }

-- The progress of a run that began at this moment and has done nothing yet.
emptyProgress :: Double -> Progress
emptyProgress began = Progress began Nothing 0 0

-- Record every report the way the wrapped function does and count the ones
-- that carry a site, which are the firings and the formations entered. Once
-- the given number of seconds has passed since the last line, and on the first
-- such report too, one line goes to the console naming the counts, the time
-- the run has taken and the site of the report, rendered only then, since a
-- run may make hundreds of thousands of them and prints one every few seconds.
progressed :: IORef Progress -> Double -> (Expression -> IO String) -> SaveEvalFunc -> SaveEvalFunc
progressed cursor interval render record evaluation = do
  record evaluation
  mapM_ reported (sited evaluation)
  where
    sited :: Evaluation -> Maybe (Progress -> Progress, Expression)
    sited (EvFiring _ _ _ site) = Just (\progress -> progress{_firings = progress._firings + 1}, site)
    sited (EvFormation _ _ site) = Just (\progress -> progress{_formations = progress._formations + 1}, site)
    sited _ = Nothing
    reported :: (Progress -> Progress, Expression) -> IO ()
    reported (counted, site) = do
      now <- getMonotonicTime
      progress <- counted <$> readIORef cursor
      if maybe True (\told -> now - told >= interval) progress._told
        then do
          locator <- render site
          logInfo (printf "Entered %d formations and fired %d λ functions in %.0fs, now at %s" progress._formations progress._firings (now - progress._began) locator)
          writeIORef cursor progress{_told = Just now}
        else writeIORef cursor progress
