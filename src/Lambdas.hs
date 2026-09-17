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
-- An entry answers, it never computes: the job of these functions is symbolic
-- morphing, so the answer carries a symbol standing for a value nobody worked
-- out, and the data its 'dataize' operands came down to is not its to read.
-- An answer mentioning a bytes meta is refused where the file is read.
--
-- This module holds the entries and the two things reading one takes — the
-- lookup of a λ name and the minting of the symbols an answer asks for.
-- Firing an entry is 𝔼's business and lives in 'Morph', which alone holds the
-- judgments an entry reduces its operands with.
module Lambdas
  ( Lambda (..)
  , LambdaException (..)
  , Lambdas
  , Meta (..)
  , emptyLambdas
  , matched
  , minted
  , readLambdas
  , taken
  )
where

import AST
import Control.Exception (Exception, throwIO)
import Data.Aeson (FromJSON (parseJSON), Key, Object, withObject, (.!=), (.:), (.:?))
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Yaml as Yaml
import Logger (logDebug)
import Parser (parseBytes, parseExpression)
import Slots (Slots (slots))
import Text.Printf (printf)
import Text.Regex.PCRE (matchTest)
import Text.Regex.PCRE.ByteString (Regex, compUTF8, compile, execBlank)
import Yaml (referenceless)

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
-- reduces to a normal form and the term it answers with.
data Lambda = Lambda
  { _key :: Text
  , _dataized :: [(Meta, Expression)]
  , _morphed :: [(Meta, Expression)]
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
        <*> entry .: "𝑛"
    sigmas (T.unpack key) lambda._answer
    dataless (T.unpack key) lambda._answer
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
  registered <- Lambdas <$> mapM keyed entries
  logDebug (printf "Loaded %d λ function(s) from '%s'" (length entries) path)
  pure registered
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

-- The last symbol a program already carries, which is where minting starts: a
-- program written by an earlier run holds symbols of its own, and a fresh one
-- must never be spelled like one of them.
taken :: Expression -> Int
taken program = maximum (0 : symbols program)
