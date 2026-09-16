{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- Which λ functions exist is a property of the object model being dataized,
-- not of the calculus. phino therefore implements none of them: it reads them
-- from the YAML file given with '--functions', where each is a rule phino
-- answers the firing with itself, in the language its own judgments are
-- written in:
--
-- > - λ: L_bytes_eq
-- >   dataize:
-- >     δ1: ρ
-- >     δ2: x
-- >   symbols: [𝑓0]
-- >   𝑛: Φ.bool( if ↦ ⟦ guard ↦ ⟦ λ ⤍ 𝑓0 ⟧, λ ⤍ L_fork ⟧ )
--
-- The 'λ' of an entry is a regular expression over λ names, matching the whole
-- name, so a plain name means that one function while 'L_box_[0-9]+_number'
-- stands for a family of them. 'dataize' and 'morph' each name a dotted path
-- down the formation being fired and bind the meta that names it: 𝔻 answers
-- data, so a 'dataize' operand binds a bytes meta δ1, while 𝕄 answers a normal
-- form, so a 'morph' one binds an expression meta 𝑛1 — the way every rule of
-- 'resources' already spells the two premises apart. 'symbols' binds its metas
-- to fresh λ names, counted in the state 𝑠; 'where' calls a build-term
-- function, as a rewriting rule's does; 'when' guards the entry; and '𝑛' is
-- the term the firing answers with, normalized before it is handed back.
--
-- This module holds the entries and the two things reading one takes — the
-- lookup of a λ name and the walk of a dotted path. Firing one is 𝔼's
-- business and lives in 'Morph', which alone holds the judgments an entry
-- reduces its operands with.
module Lambdas
  ( Lambda (..)
  , LambdaException (..)
  , Lambdas
  , Meta (..)
  , attributeOf
  , emptyLambdas
  , matched
  , minted
  , readLambdas
  )
where

import AST
import Builder (contextualize)
import Control.Exception (Exception, throwIO)
import Control.Monad (foldM)
import Data.Aeson (FromJSON (parseJSON), Key, Object, withObject, (.!=), (.:), (.:?))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Yaml as Yaml
import Deps (State)
import Logger (logDebug)
import Parser (parseBinding, parseBytes, parseExpression)
import Printer (printAttribute)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Text.Regex.PCRE (matchTest)
import Text.Regex.PCRE.ByteString (Regex, compUTF8, compile, execBlank)
import Yaml (Condition, Extra, referenceless)

-- One meta an entry of the file binds: the name the file spells it with, which
-- is the name the protocol of '--evaluations' reports it back under, and the
-- name a substitution keeps it under, which is the one 𝜑-calculus gives it.
-- The two differ — '𝑛1' against 'n1', 'δ1' against 'd1' — so an entry is read
-- through the very parser a rewriting rule's pattern is read through and never
-- guesses.
data Meta = Meta
  { _spelling :: Text
  , _name :: Text
  }

-- One λ function phino may fire, as the file spells it: the key it is
-- registered under, the operands it reduces before it answers, the fresh
-- symbols it mints, the extra terms it builds, the guard it holds under and
-- the term it answers with.
data Lambda = Lambda
  { _key :: Text
  , _dataized :: [(Meta, Text)]
  , _morphed :: [(Meta, Text)]
  , _symbols :: [Meta]
  , _when :: Maybe Condition
  , _extras :: [Extra]
  , _answer :: Expression
  }

-- Every λ function phino may fire, in the order the file lists them: each key,
-- a regular expression over λ names, paired with the entry it introduces. A
-- lookup keeps every entry whose key matches the whole name, in that same
-- order, since one name may be answered by several entries and the guards tell
-- them apart.
newtype Lambdas = Lambdas [(Regex, Lambda)]

data LambdaException
  = -- The '--functions' file is not a list of λ function rules.
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
        <$> paths bytesMeta entry "dataize"
        <*> paths expressionMeta entry "morph"
        <*> (entry .:? "symbols" .!= [] >>= mapM functionMeta)
        <*> entry .:? "when"
        <*> entry .:? "where" .!= []
        <*> entry .: "𝑛"
    referenceless (T.unpack key) "𝑛" lambda._answer
    referenceless (T.unpack key) "when" lambda._when
    referenceless (T.unpack key) "where" lambda._extras
    pure lambda
    where
      -- The metas one block of an entry binds, each paired with the dotted
      -- path down the formation it is reduced from, ordered by the name of the
      -- meta: a YAML mapping keeps no order of its own, so numbering the metas
      -- δ1, δ2, … and 𝑛1, 𝑛2, … is what reduces them the way they are written.
      paths :: (Text -> Yaml.Parser Meta) -> Object -> Key -> Yaml.Parser [(Meta, Text)]
      paths kind entry name = do
        mapping <- entry .:? name .!= (Map.empty :: Map Text Text)
        mapM (\(meta, path) -> kind meta >>= \bound -> pure (bound, path)) (Map.toAscList mapping)
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
        _ -> fail (printf "The operand '%s' is not a bytes meta, such as 'δ1'" (T.unpack meta))
      -- The same for a function meta, which 𝜑-calculus spells only as the λ
      -- binding it stands in for.
      functionMeta :: Text -> Yaml.Parser Meta
      functionMeta meta = case parseBinding (printf "λ ⤍ %s" (T.unpack meta)) of
        Right (BiLambda (FnMeta name)) -> pure (Meta meta name)
        _ -> fail (printf "The symbol '%s' is not a function meta, such as '𝑓0'" (T.unpack meta))

-- No λ function at all: every one of them gets stuck. This is what a run
-- without '--functions' fires against.
emptyLambdas :: Lambdas
emptyLambdas = Lambdas []

-- Read the λ functions from a YAML file. A key that is no regular expression,
-- an entry with no answer under '𝑛' and malformed YAML all fail here, before
-- any dataization starts, so a run never gets half-way through a derivation to
-- discover that one of its λ functions cannot be read at all.
readLambdas :: FilePath -> IO Lambdas
readLambdas path = do
  entries <- Yaml.decodeFileEither path >>= either broken pure
  registered <- Lambdas <$> mapM keyed entries
  logDebug (printf "Loaded %d λ function(s) from '%s'" (length entries) path)
  pure registered
  where
    broken :: Yaml.ParseException -> IO [Lambda]
    broken failure = throwIO (BrokenLambdas path (Yaml.prettyPrintParseException failure))
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

-- Every entry whose key matches the whole λ name, in the order the file lists
-- them. There may be several: the guards of an entry are what tell one answer
-- of a λ function from another, so 'L_bytes_eq' answers one term when its
-- operands agree and another when they do not.
matched :: Lambdas -> Text -> [Lambda]
matched (Lambdas entries) func = [entry | (key, entry) <- entries, matchTest key (encodeUtf8 func)]

-- The node the formation being fired holds under a dotted path, in the scope
-- it is bound in. The path is read left to right and split on the dot, which
-- no attribute of 𝜑-calculus carries in its own name, so 'ρ.length' goes two
-- deep; every segment but the last has to name a formation or an application
-- to go on into. An argument of an application binds an attribute the way a τ
-- binding of a formation does, and it is the outer of the two, so it is what
-- the attribute is whatever the formation under it still says about it; a
-- positional argument names nothing, so the walk goes past it. The ξ of a node
-- bound in a formation stands for that formation, the way the 'dot' rule has
-- it when it dispatches the same attribute, so a node taken out of one carries
-- it along; an argument, written in the scope around the application, stands
-- for itself. An attribute that is void, or one nothing carries at all, has no
-- node to reduce and answers with nothing.
attributeOf :: Text -> Expression -> Maybe Expression
attributeOf path term = foldM deeper term (T.splitOn "." path)
  where
    deeper :: Expression -> Text -> Maybe Expression
    deeper form@(ExFormation bds) name = go bds
      where
        go :: [Binding] -> Maybe Expression
        go [] = Nothing
        go (BiTau attr value : rest)
          | named name attr = Just (contextualize value form)
          | otherwise = go rest
        go (_ : rest) = go rest
    deeper (ExApplication applied (ArTau attr value)) name
      | named name attr = Just value
      | otherwise = deeper applied name
    deeper (ExApplication applied _) name = deeper applied name
    deeper _ _ = Nothing
    named :: Text -> Attribute -> Bool
    named name attr = T.pack (printAttribute attr) == name

-- The fresh λ names a 'symbols' block binds its metas to, together with the
-- state that counts them. Uniqueness is the state's job and not the file's:
-- the state 𝑠 threaded through 𝕄, 𝔻 and 𝔼 carries how many symbols the run has
-- minted so far, so every firing takes the next names and no two unknowns are
-- ever spelled alike. The names are sequential rather than random, which keeps
-- a symbolic run deterministic.
minted :: [Meta] -> State -> ([(Meta, Text)], State)
minted metas state = (zip metas names, show (spent + length metas))
  where
    spent :: Int
    spent = fromMaybe 0 (readMaybe state)
    names :: [Text]
    names = [T.pack (printf "S_%d" idx) | idx <- [spent + 1 :: Int ..]]
