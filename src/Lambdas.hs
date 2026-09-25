{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- Which λ functions exist is a property of the object model being reduced, not
-- of the calculus. phino therefore implements none of them: it reads them from
-- the YAML file given with '--symbolic', where each is an entry answering the
-- firing with a term of the calculus:
--
-- > - λ: L_number_plus
-- >   dataize:
-- >     𝛿1: $.ρ
-- >     𝛿2: $.x
-- >   𝑛: Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ 𝜎 ⟧ ) )
--
-- The 'λ' of an entry is a regular expression over λ names, matching the whole
-- name, so a plain name means that one function while 'L_box_[0-9]+_number'
-- stands for a family of them. It is unique: the lookup answers one entry or
-- none. 'dataize' names the operands brought down through 𝔻, each binding a
-- bytes meta 𝛿1, and 'morph' the ones reduced through 𝕄, each binding an
-- expression meta 𝑛1; both are terms of the calculus, where ξ stands for the
-- formation being fired, so '$.x' is its x, and Φ for the universe. The term
-- under '𝑛' is what the firing answers with, and a bare 𝜎 in it mints a fresh
-- symbol.
--
-- 'rewrite' is the third block and reduces nothing either. Each line of it
-- names a meta the entry has bound already under 'of' and a list of ordinary
-- rules under 'rules', spelled the way a rule file spells one, and binds an
-- expression meta of its own to that term with the rules applied to it (see
-- 'rewritten' in 'Evaluate'). It is how a program brings two branches of a
-- fork to one shape before they are compared: one literal of a bool and the
-- answer of a firing are one value in two spellings, and nothing but the
-- program knows that (#1409).
--
-- 'symbolize' is the fourth block and reduces nothing at all. It takes a term
-- an earlier block of the very same entry has already bound and binds an
-- expression meta of its own to that term with every datum in it standing for
-- an unknown, so a normal form reached from a literal is written the way one
-- reached from an unknown is written and the two of them compare as
-- expressions (see 'symbolized').
--
-- 'join' is the fifth block and reduces nothing either. It takes two metas the
-- entry has bound already and binds one of its own to the two terms joined,
-- which is what a branching λ function answers with: a fork stands for either
-- of its branches and no one branch stands for both, so the shape both of them
-- have, with a fresh symbol wherever they differ, is what the answer names
-- (see 'joined').
--
-- An entry answers, it never computes: the job of these functions is symbolic
-- morphing, so the answer carries a symbol standing for a value nobody worked
-- out, and the data its 'dataize' operands came down to is not its to read.
-- An answer mentioning a bytes meta is refused where the file is read.
--
-- This module holds the entries and the four things reading one takes — the
-- lookup of a λ name, the minting of the symbols an answer asks for, the
-- standing of the data of a term into unknowns and the joining of two terms
-- into one. Firing an entry is 𝔼's business and lives in 'Morph', which alone
-- holds the judgments an entry reduces its operands with.
module Lambdas
  ( Lambda (..)
  , LambdaException (..)
  , Lambdas
  , Meta (..)
  , emptyLambdas
  , joined
  , matched
  , minted
  , readLambdas
  , symbolized
  , taken
  )
where

import AST
import Control.Exception (Exception, throwIO)
import Control.Monad (void)
import Data.Aeson (FromJSON (parseJSON), Key, Object, Value (Object), withObject, (.!=), (.:), (.:?))
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Yaml as Yaml
import Logger (logDebug)
import Metas (Metas (metas))
import Parser (parseBytes, parseExpression)
import Slots (Slots (slots))
import Text.Printf (printf)
import Text.Regex.PCRE (matchTest)
import Text.Regex.PCRE.ByteString (Regex, compUTF8, compile, execBlank)
import Yaml (referenceless)
import qualified Yaml as Y

-- One meta an entry of the file binds: the name the file spells it with, which
-- is the name the protocol of '--protocol' reports it back under, and the name
-- a substitution keeps it under, which is the one 𝜑-calculus gives it. The two
-- differ — '𝑛1' against 'n1', '𝛿1' against 'd1' — so an entry is read through
-- the very parser a rewriting rule's pattern is read through and never
-- guesses.
data Meta = Meta
  { _spelling :: Text
  , _name :: Text
  }

-- One λ function phino may fire, as the file spells it: the key it is
-- registered under, the operands it brings down to data, the operands it
-- reduces to a normal form, the terms of those it rewrites with rules of its
-- own, the terms of those it stands the data of into unknowns, the pairs of those it joins into one term and the term it answers
-- with.
data Lambda = Lambda
  { _key :: Text
  , _dataized :: [(Meta, Expression)]
  , _morphed :: [(Meta, Expression)]
  , _rewritten :: [(Meta, (Meta, [Y.Rule]))]
  , _symbolized :: [(Meta, Expression)]
  , _paired :: [(Meta, (Meta, Meta))]
  , _answer :: Expression
  }

-- Every λ function phino may fire, in the order the file lists them: each key,
-- a regular expression over λ names, paired with the entry it introduces. A
-- lookup walks them top to bottom and the first key matching the whole name
-- wins, so one entry may stand for a family of λ functions while a plain name,
-- being a regular expression matching itself, keeps meaning that one function.
newtype Lambdas = Lambdas [(Regex, Lambda)]

data LambdaException
  = -- The '--symbolic' file is not a list of λ function entries.
    BrokenLambdas FilePath String
  deriving anyclass (Exception)

instance Show LambdaException where
  show (BrokenLambdas file failure) =
    printf "The λ functions of '%s' cannot be read: %s" file failure

instance FromJSON Lambda where
  parseJSON = withObject "Lambda" $ \entry -> do
    key <- entry .: "λ"
    lambda <-
      Lambda key
        <$> operands key bytesMeta entry "dataize"
        <*> operands key expressionMeta entry "morph"
        <*> rewrites (T.unpack key) entry
        <*> operands key expressionMeta entry "symbolize"
        <*> pairs (T.unpack key) entry
        <*> entry .: "𝑛"
    sigmas (T.unpack key) lambda._answer
    dataless (T.unpack key) lambda._answer
    earlier (T.unpack key) lambda
    pure lambda
    where
      -- The metas one block of an entry binds, each paired with the term it is
      -- reduced from, ordered by the name of the meta: a YAML mapping keeps no
      -- order of its own, so numbering the metas 𝛿1, 𝛿2, … and 𝑛1, 𝑛2, … is
      -- what reduces them the way they are written.
      operands :: Text -> (Text -> Yaml.Parser Meta) -> Object -> Key -> Yaml.Parser [(Meta, Expression)]
      operands key kind entry name = do
        mapping <- entry .:? name .!= (Map.empty :: Map Text Expression)
        mapM bound (Map.toAscList mapping)
        where
          bound :: (Text, Expression) -> Yaml.Parser (Meta, Expression)
          bound (meta, term) = do
            referenceless (T.unpack key) (T.unpack meta) term
            kind meta >>= \named -> pure (named, term)
      -- The meta a 'morph' block binds: '𝑛1' is written the way 𝜑-calculus
      -- writes it and stands for the same meta a rule's 'pattern' would bind,
      -- so the parser of the calculus is what reads it here too.
      expressionMeta :: Text -> Yaml.Parser Meta
      expressionMeta meta = case parseExpression (T.unpack meta) of
        Right (ExMeta name) -> pure (Meta meta name)
        _ -> fail (printf "The operand '%s' is not an expression meta, such as '𝑛1'" (T.unpack meta))
      -- The meta a 'dataize' block binds, which is a bytes meta and not an
      -- expression one, since what 𝔻 answers is data and nothing else.
      bytesMeta :: Text -> Yaml.Parser Meta
      bytesMeta meta = case parseBytes (T.unpack meta) of
        Right (BtMeta name) -> pure (Meta meta name)
        _ -> fail (printf "The operand '%s' is not a bytes meta, such as '𝛿1'" (T.unpack meta))
      -- The metas a 'join' block binds, each paired with the two it joins,
      -- ordered by the name of the meta the way every other block is. A line
      -- joins two metas and never three: it stands for a choice between two
      -- branches, and a walk over three terms in parallel is no such choice.
      pairs :: String -> Object -> Yaml.Parser [(Meta, (Meta, Meta))]
      pairs key entry = do
        mapping <- entry .:? "join" .!= (Map.empty :: Map Text [Text])
        mapM joins (Map.toAscList mapping)
        where
          joins :: (Text, [Text]) -> Yaml.Parser (Meta, (Meta, Meta))
          joins (meta, [left, right]) = do
            named <- expressionMeta meta
            branches <- (,) <$> expressionMeta left <*> expressionMeta right
            pure (named, branches)
          joins (meta, _) =
            fail
              ( printf
                  "The operand '%s' of λ function '%s' must join exactly two metas, such as '[𝑛1, 𝑛2]'"
                  (T.unpack meta)
                  key
              )
      -- The metas a 'rewrite' block binds, each paired with the meta whose
      -- term it rewrites and the rules it rewrites that term with, ordered by
      -- the name of the meta the way every other block is. A rule is read the
      -- way a rule file reads one, and on top of that a rule writing a symbol
      -- into its result or reading a meta its match never bound is refused
      -- here: a symbol is minted by a firing and never spelled by hand, and a
      -- meta nothing bound is one the rule cannot be built with.
      rewrites :: String -> Object -> Yaml.Parser [(Meta, (Meta, [Y.Rule]))]
      rewrites key entry = do
        mapping <- entry .:? "rewrite" .!= (Map.empty :: Map Text Object)
        mapM line (Map.toAscList mapping)
        where
          line :: (Text, Object) -> Yaml.Parser (Meta, (Meta, [Y.Rule]))
          line (meta, body) = do
            named <- expressionMeta meta
            source <- body .: "of" >>= expressionMeta
            written <- body .: "rules"
            rules <- mapM rule written
            pure (named, (source, rules))
          rule :: Object -> Yaml.Parser Y.Rule
          rule body = do
            result <- body .: "result"
            symbolless result
            parsed <- parseJSON (Object body)
            bound parsed
            pure parsed
          -- A bare 𝜎 or a numbered one written into a result: the one names a
          -- symbol a rewrite has no business minting, since it is a
          -- substitution the entry vouches for and no firing, and the other
          -- one nobody minted at all.
          symbolless :: Expression -> Yaml.Parser ()
          symbolless result
            | null (symbols result) && null [kind | Slot kind _ <- slots result, kind == "S"] = pure ()
            | otherwise = fail (printf "A rule of the 'rewrite' block of λ function '%s' writes a symbol 𝜎 into its result" key)
          -- Every meta a result reads is one the pattern or a 'where'
          -- extension of the very same rule binds.
          bound :: Y.Rule -> Yaml.Parser ()
          bound parsed = case filter (`notElem` known) (metas parsed.result) of
            [] -> pure ()
            meta : _ ->
              fail
                ( printf
                    "The rule '%s' of the 'rewrite' block of λ function '%s' reads the meta '%s' it never binds"
                    parsed.name
                    key
                    (T.unpack meta)
                )
            where
              known :: [Text]
              known = metas parsed.pattern ++ concatMap (metas . (.meta)) (concat parsed.where_)
      -- Every 'rewrite', 'symbolize' and 'join' line reads terms the entry has
      -- bound already: a 'morph' operand, a line above it in its own block or
      -- a line of a block above its own, since nothing else of an entry is a
      -- normal form yet and the blocks run in the order the entry lists them
      -- here. A line naming anything else names a term nobody reduced, and the
      -- file is wrong where it is read rather than half-way through a firing.
      earlier :: String -> Lambda -> Yaml.Parser ()
      earlier key lambda = do
        rewrote <- goRewrites (map (_name . fst) lambda._morphed) lambda._rewritten
        stood <- go rewrote lambda._symbolized
        void (goJoins stood lambda._paired)
        where
          goRewrites :: [Text] -> [(Meta, (Meta, [Y.Rule]))] -> Yaml.Parser [Text]
          goRewrites reduced [] = pure reduced
          goRewrites reduced ((meta, (source, _)) : rest)
            | source._name `elem` reduced = goRewrites (meta._name : reduced) rest
            | otherwise = unbound source
          go :: [Text] -> [(Meta, Expression)] -> Yaml.Parser [Text]
          go reduced [] = pure reduced
          go reduced ((meta, term) : rest) = case term of
            ExMeta name | name `elem` reduced -> go (meta._name : reduced) rest
            _ -> unbound meta
          goJoins :: [Text] -> [(Meta, (Meta, Meta))] -> Yaml.Parser [Text]
          goJoins reduced [] = pure reduced
          goJoins reduced ((meta, (left, right)) : rest)
            | all ((`elem` reduced) . _name) [left, right] = goJoins (meta._name : reduced) rest
            | otherwise = unbound (if left._name `elem` reduced then right else left)
          unbound :: Meta -> Yaml.Parser a
          unbound meta =
            fail
              ( printf
                  "The operand '%s' of λ function '%s' names no meta bound by 'morph' or by a line above it"
                  (T.unpack meta._spelling)
                  key
              )
      -- A bare 𝜎 is the one anonymous meta an answer may carry, since minting
      -- a fresh symbol is exactly what it asks for; every other one names a
      -- match the entry never made.
      sigmas :: String -> Expression -> Yaml.Parser ()
      sigmas key answer = case [kind | Slot kind _ <- slots answer, kind /= "S"] of
        [] -> pure ()
        kind : _ -> fail (printf "The anonymous meta '!%s' cannot be referenced in the '𝑛' of λ function '%s'" (T.unpack kind) key)
      -- An entry answers a term carrying a symbol and never a value it worked
      -- out, so the data its operands came down to is not its to read.
      dataless :: String -> Expression -> Yaml.Parser ()
      dataless key answer
        | computes answer = fail (printf "The '𝑛' of λ function '%s' reads data, while a symbolic answer may mention nothing but 𝜎" key)
        | otherwise = pure ()

-- Whether a term reads data — carries a bytes meta anywhere inside it — which
-- is what tells an answer that computes from one that merely stands for an
-- unknown.
computes :: Expression -> Bool
computes = goExpr
  where
    goExpr :: Expression -> Bool
    goExpr (ExFormation bds) = any goBinding bds
    goExpr (ExApplication expr arg) = goExpr expr || goArgument arg
    goExpr (ExDispatch expr _) = goExpr expr
    goExpr (ExPhiMeet _ _ expr) = goExpr expr
    goExpr (ExPhiAgain _ _ expr) = goExpr expr
    goExpr (ExBytes bts) = goBytes bts
    goExpr _ = False
    goBinding :: Binding -> Bool
    goBinding (BiTau _ expr) = goExpr expr
    goBinding (BiDelta bts) = goBytes bts
    goBinding _ = False
    goArgument :: Argument -> Bool
    goArgument (ArTau _ expr) = goExpr expr
    goArgument (ArAlpha _ expr) = goExpr expr
    goBytes :: Bytes -> Bool
    goBytes (BtMeta _) = True
    goBytes (BtAny _) = True
    goBytes _ = False

-- No λ function at all: every one of them gets stuck. This is what a run
-- without '--symbolic' fires against.
emptyLambdas :: Lambdas
emptyLambdas = Lambdas []

-- Read the λ functions from a YAML file. A key that is no regular expression,
-- a key two entries share, an entry with no answer under '𝑛', an answer
-- reading data and malformed YAML all fail here, before any reduction starts,
-- so a run never gets half-way through a derivation to discover that one of
-- its λ functions cannot be read at all.
readLambdas :: FilePath -> IO Lambdas
readLambdas path = do
  entries <- Yaml.decodeFileEither path >>= either broken pure
  mapM_ (unique entries) entries
  registered <- mapM keyed entries
  overlaps path registered
  logDebug (printf "Loaded %d λ function(s) from '%s'" (length entries) path)
  pure (Lambdas registered)
  where
    broken :: Yaml.ParseException -> IO [Lambda]
    broken failure = throwIO (BrokenLambdas path (Yaml.prettyPrintParseException failure))
    -- Two entries under one key are one entry too many: nothing tells them
    -- apart any more, so the second is unreachable and the file is wrong
    -- rather than merely redundant.
    unique :: [Lambda] -> Lambda -> IO ()
    unique entries entry
      | length (filter ((== entry._key) . (._key)) entries) == 1 = pure ()
      | otherwise = throwIO (BrokenLambdas path (printf "the key '%s' is used by more than one entry" (T.unpack entry._key)))
    -- The key as the regular expression it is, made to match the whole name,
    -- so that a plain name means that one λ function and not every name it is
    -- a part of.
    keyed :: Lambda -> IO (Regex, Lambda)
    keyed entry = do
      compiled <- compile compUTF8 execBlank (encodeUtf8 ("^(?:" <> entry._key <> ")$"))
      either (unreadable entry._key) (\key -> pure (key, entry)) compiled
    unreadable :: Text -> (a, String) -> IO b
    unreadable key (_, failure) =
      throwIO (BrokenLambdas path (printf "the key '%s' is not a regular expression: %s" (T.unpack key) failure))

    overlaps :: FilePath -> [(Regex, Lambda)] -> IO ()
    overlaps file = check
      where
        check :: [(Regex, Lambda)] -> IO ()
        check [] = pure ()
        check ((first, left) : rest) = do
          mapM_ (pair first left) rest
          check rest
        pair :: Regex -> Lambda -> (Regex, Lambda) -> IO ()
        pair first left (second, right)
          | matchTest first (encodeUtf8 right._key)
              || matchTest second (encodeUtf8 left._key) =
              throwIO
                ( BrokenLambdas
                    file
                    (printf "the keys '%s' and '%s' match some of the same lambda names" (T.unpack left._key) (T.unpack right._key))
                )
          | otherwise = pure ()

-- The entry whose key matches the whole λ name, if any. There is at most one:
-- the keys are unique, so a name either has a λ function or has none at all.
matched :: Lambdas -> Text -> Maybe Lambda
matched (Lambdas entries) func = snd <$> find (\(key, _) -> matchTest key (encodeUtf8 func)) entries

-- The fresh symbols an answer asks for, one per bare 𝜎 it was written with,
-- each paired with the slot that asked for it, together with the count of
-- symbols the run has minted once they are taken. Uniqueness is the state's
-- job and not the file's: the state 𝑠 threaded through 𝕄, 𝔻 and 𝔼 carries how
-- many symbols the run has minted so far, so every firing takes the next names
-- and no two unknowns are ever spelled alike. The names are sequential rather
-- than random, which keeps a symbolic run reproducible.
minted :: Expression -> Int -> ([(Slot, Function)], Int)
minted answer spent = (zip fresh [FnSymbol idx | idx <- [spent + 1 ..]], spent + length fresh)
  where
    fresh :: [Slot]
    fresh = [slot | slot@(Slot kind _) <- slots answer, kind == "S"]

-- What a walk standing the data of a term into unknowns carries from one
-- sub-term to the next: how many symbols the run has minted once everything
-- left of this sub-term is standing, and what is known about each symbol
-- minted along the way, the last of them first.
type Minting = (Int, [(Int, Bytes)])

-- The term with every datum of it standing for an unknown instead: each
-- 'Δ ⤍ b' binding becomes a 'λ ⤍ 𝜎k' naming a fresh symbol, one per
-- occurrence, so '⟦ Δ ⤍ b ⟧' reads as '⟦ λ ⤍ 𝜎k ⟧' and a normal form
-- reached from a literal is written the way one reached from an unknown is
-- written, the two of them comparing as expressions. It is the binding and not
-- the formation around it that changes, since a datum carries a ρ of its own
-- and so does the unknown it is put beside. A term nobody worked a value out
-- in passes through unchanged.
--
-- Only the φ chain is walked. A term carries the value it stands for where
-- that chain ends, so a datum anywhere else says nothing about the term and is
-- left alone, the whole subtree of it (see 'denoted'). What sits under ρ
-- belongs to the object around this one, and a normal form drags the universe
-- it was reduced inside along under ρ, so a walk reaching into it would stand
-- the data of the whole program into unknowns to say one thing about one term.
-- What sits under a method is code and not data: the literals of
-- 'neg ↦ ⟦ φ ↦ ξ.ρ.times( -1 ) ⟧' are the body of something nobody has called,
-- and minting a symbol per literal of every method a carrier declares would
-- write dozens of unknowns nobody reads for one value that is read (#1293).
--
-- What is known about each fresh symbol comes back beside the term: the data
-- dataizing the formation it names answers. That is a fact about the symbol
-- and no binding of it — a 𝜎 is the name of a λ function, neither a datum
-- nor a term — which is why it travels apart from the term rather than inside
-- it. The count of symbols the run has minted once they are taken comes back
-- too, uniqueness being the state's job here exactly as it is in 'minted'.
symbolized :: Expression -> Int -> (Expression, [(Int, Bytes)], Int)
symbolized term spent = case goExpr term (spent, []) of
  (masked, (spent', known)) -> (masked, reverse known, spent')
  where
    goExpr :: Expression -> Minting -> (Expression, Minting)
    goExpr (ExFormation bds) minting =
      let (bds', minting') = goBindings bds minting
       in (ExFormation bds', minting')
    goExpr (ExApplication expr arg) minting =
      let (expr', minting') = goExpr expr minting
          (arg', minting'') = goArgument arg minting'
       in (ExApplication expr' arg', minting'')
    goExpr (ExDispatch expr attr) minting =
      let (expr', minting') = goExpr expr minting
       in (ExDispatch expr' attr, minting')
    goExpr (ExPhiMeet prefix idx expr) minting =
      let (expr', minting') = goExpr expr minting
       in (ExPhiMeet prefix idx expr', minting')
    goExpr (ExPhiAgain prefix idx expr) minting =
      let (expr', minting') = goExpr expr minting
       in (ExPhiAgain prefix idx expr', minting')
    goExpr expr minting = (expr, minting)
    goBindings :: [Binding] -> Minting -> ([Binding], Minting)
    goBindings [] minting = ([], minting)
    goBindings (bd : rest) minting =
      let (bd', minting') = goBinding bd minting
          (rest', minting'') = goBindings rest minting'
       in (bd' : rest', minting'')
    -- One binding of a formation stood into unknowns. The Δ of the formation
    -- is its value and becomes a symbol; its φ is where the value of a term
    -- that has no Δ is reached, so the walk goes on through it; every other
    -- binding is carried as it was written, the whole subtree of it.
    goBinding :: Binding -> Minting -> (Binding, Minting)
    goBinding (BiDelta bts) (spent', known) =
      (BiLambda (FnSymbol fresh), (fresh, (fresh, bts) : known))
      where
        fresh :: Int
        fresh = spent' + 1
    goBinding (BiTau AtPhi expr) minting =
      let (expr', minting') = goExpr expr minting
       in (BiTau AtPhi expr', minting')
    goBinding bd minting = (bd, minting)
    goArgument :: Argument -> Minting -> (Argument, Minting)
    goArgument (ArTau attr expr) minting =
      let (expr', minting') = goExpr expr minting
       in (ArTau attr expr', minting')
    goArgument (ArAlpha alpha expr) minting =
      let (expr', minting') = goExpr expr minting
       in (ArAlpha alpha expr', minting')

-- What a walk joining two terms carries from one sub-term to the next: how
-- many symbols the run has minted once everything left of this sub-term is
-- joined, the fresh symbol every pair of differing symbols was given, since
-- one pair met twice is one choice and not two, and those pairs in the order
-- they were met, the last of them first.
type Joining = (Int, Map (Int, Int) Int, [(Int, (Int, Int))])

-- The two terms joined into the one term standing for either of them, which is
-- what a fork of two branches answers with: neither branch is the answer, the
-- value being the one nobody has picked, and the shape both of them have is.
-- The walk goes over the two in parallel and requires them to match verbatim,
-- with one exception: where a 'λ ⤍ 𝜎A' binding meets a different 'λ ⤍ 𝜎B' one
-- it mints a fresh symbol and stands it there, and the same pair met again
-- further down gets that very symbol, since the branch it came from is one
-- choice however often the two terms differ by it. Two identical branches join
-- into that same term and nothing is minted at all. Only the φ chain is
-- compared, exactly as 'symbolized' stands only that chain into unknowns:
-- everything else is carried from the first branch, since the value of a
-- branch is where its φ chain ends and what sits under ρ or under a method is
-- none of it (see 'goBinding' below).
--
-- Any other difference — a datum against a symbol, two different data, a
-- binding one of them carries and the other does not — is no join, and nothing
-- comes back: a fork whose branches differ in structure is stuck the way a λ
-- function no entry answers is, and bringing two such branches to one shape is
-- the program's business rather than phino's, which its entry does in a
-- 'rewrite' block (#1409). This is why a datum is never joined with anything
-- and why a branch carrying one goes through 'symbolized' first (#1246).
--
-- What each fresh symbol stands for comes back beside the term, the two
-- symbols it was minted for in the order the branches were given, since
-- dataizing its formation answers what dataizing one of the two answers and
-- that is a fact about the symbol rather than a binding of it. The count of
-- symbols the run has minted once they are taken comes back too, uniqueness
-- being the state's business here exactly as it is in 'minted'.
joined :: Expression -> Expression -> Int -> Maybe (Expression, [(Int, (Int, Int))], Int)
joined left right spent = taking <$> goExpr left right (spent, Map.empty, [])
  where
    -- The term the walk built, with what it minted put back in the order the
    -- pairs were met and the count of symbols the run has spent by then.
    taking :: (Expression, Joining) -> (Expression, [(Int, (Int, Int))], Int)
    taking (term, (spent', _, made)) = (term, reverse made, spent')
    goExpr :: Expression -> Expression -> Joining -> Maybe (Expression, Joining)
    goExpr (ExFormation one) (ExFormation two) joining = do
      (bds, joining') <- goBindings one two joining
      pure (ExFormation bds, joining')
    goExpr (ExApplication one arg) (ExApplication two arg') joining = do
      (expr, joining') <- goExpr one two joining
      (applied, joining'') <- goArgument arg arg' joining'
      pure (ExApplication expr applied, joining'')
    goExpr (ExDispatch one attr) (ExDispatch two attr') joining
      | attr == attr' = do
          (expr, joining') <- goExpr one two joining
          pure (ExDispatch expr attr, joining')
    goExpr (ExPhiMeet prefix idx one) (ExPhiMeet prefix' idx' two) joining
      | prefix == prefix' && idx == idx' = do
          (expr, joining') <- goExpr one two joining
          pure (ExPhiMeet prefix idx expr, joining')
    goExpr (ExPhiAgain prefix idx one) (ExPhiAgain prefix' idx' two) joining
      | prefix == prefix' && idx == idx' = do
          (expr, joining') <- goExpr one two joining
          pure (ExPhiAgain prefix idx expr, joining')
    goExpr one two joining
      | one == two = Just (one, joining)
      | otherwise = Nothing
    goBindings :: [Binding] -> [Binding] -> Joining -> Maybe ([Binding], Joining)
    goBindings [] [] joining = Just ([], joining)
    goBindings (one : rest) (two : rest') joining = do
      (bd, joining') <- goBinding one two joining
      (bds, joining'') <- goBindings rest rest' joining'
      pure (bd : bds, joining'')
    goBindings _ _ _ = Nothing
    -- One binding of each term joined. A λ binding naming a symbol is the one
    -- place the two may differ, since a symbol is a value nobody worked out
    -- and the two branches standing one each is exactly what a fork is. The φ
    -- of a formation is walked into, that being where the value of the branch
    -- is reached; every other binding is taken from the first branch, the whole
    -- subtree of it, and never compared at all.
    --
    -- That is the rule ρ has always followed, read off what a branch is rather
    -- than off the attribute: what sits under ρ belongs to the object around
    -- this one, and the two branches of a fork are reduced in scopes of their
    -- own — each inside the universe its own operand was reduced in — so their
    -- ρ differ wherever that reduction left a trace, and a walk comparing them
    -- would refuse every fork whose branches 𝕄 reached by two different routes.
    -- A method is the same: its body is code nobody has called, so two branches
    -- differing inside one are not two values, and minting a symbol per literal
    -- of every method the carrier declares writes unknowns nobody reads
    -- (#1293). The shape the answer keeps is the first branch's, methods and
    -- all, so the program can go on dispatching on what the fork answered.
    goBinding :: Binding -> Binding -> Joining -> Maybe (Binding, Joining)
    goBinding (BiLambda (FnSymbol one)) (BiLambda (FnSymbol two)) joining
      | one /= two = case picked (one, two) joining of
          (fresh, joining') -> Just (BiLambda (FnSymbol fresh), joining')
    goBinding (BiTau AtPhi one) (BiTau AtPhi two) joining = do
      (expr, joining') <- goExpr one two joining
      pure (BiTau AtPhi expr, joining')
    goBinding bd@(BiTau attr _) (BiTau attr' _) joining
      | attr == attr' = Just (bd, joining)
    goBinding one two joining
      | one == two = Just (one, joining)
      | otherwise = Nothing
    goArgument :: Argument -> Argument -> Joining -> Maybe (Argument, Joining)
    goArgument (ArTau attr one) (ArTau attr' two) joining
      | attr == attr' = do
          (expr, joining') <- goExpr one two joining
          pure (ArTau attr expr, joining')
    goArgument (ArAlpha alpha one) (ArAlpha alpha' two) joining
      | alpha == alpha' = do
          (expr, joining') <- goExpr one two joining
          pure (ArAlpha alpha expr, joining')
    goArgument one two joining
      | one == two = Just (one, joining)
      | otherwise = Nothing
    -- The fresh symbol standing for one pair of differing symbols, and what
    -- the walk carries once it is taken: a pair met before keeps the symbol it
    -- was given already, and one met for the first time takes the next name
    -- the run has not minted.
    picked :: (Int, Int) -> Joining -> (Int, Joining)
    picked pair joining@(spent', names, made)
      | Just name <- Map.lookup pair names = (name, joining)
      | otherwise = (fresh, (fresh, Map.insert pair fresh names, (fresh, pair) : made))
      where
        fresh :: Int
        fresh = spent' + 1

-- The last symbol a program already carries, which is where minting starts: a
-- program written by an earlier run holds symbols of its own, and a fresh one
-- must never be spelled like one of them.
taken :: Expression -> Int
taken program = maximum (0 : symbols program)
