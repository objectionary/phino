{-# LANGUAGE OverloadedRecordDot #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- What runs the built-in rules of the calculus: the rewriting steps of
-- normalization, the answers to which of them match a term and to whether a
-- term is a normal form, the Contextualization function 𝒞 and the rules of 𝕄
-- and 𝔻. The engine of 'yaml' interprets the rules as they are written, the
-- way phino always has; 'phino compile' writes the Haskell of another one into
-- the module 'Compiled', which a build with the flag 'compiled' links in
-- (#1617, #1628, #1643). Nothing in the library reaches for either of them:
-- the command line picks one and hands it down through the contexts, the way
-- it hands down '_buildTerm'.
module Engine (Engine (..), building, current, fresh, stepOf, yaml) where

import AST
import Contextualize (contextualize)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Deps (BuildTermFunc)
import Functions (buildTerm, contextualizing)
import Inference (Inference, dataizationOf, morphingOf)
import Rewriter (every, interpreted)
import Rule (Step, normal)
import qualified Yaml as Y

-- One engine of the built-in rules: the steps normalization takes, in the
-- order of the rules; the numbers of those matching somewhere in a term, told
-- the world it stands in (see '_matching' of 'RewriteContext'); the steps of
-- any other rewriting rule it knows how to take, by the text of the rule (see
-- 'stepOf'); whether a term is a normal form; 𝒞; the rules of 𝕄 and of 𝔻, in
-- the order of their files (see 'Inference'); and the texts of the built-in
-- rules it was made from, which tell whether it still runs the rules phino
-- carries (see 'fresh').
data Engine = Engine
  { _normalization :: [Step]
  , _matching :: Maybe Expression -> Expression -> Set Int
  , _rules :: Map String Step
  , _normal :: Expression -> Bool
  , _contextualize :: Expression -> Expression -> IO Expression
  , _morphing :: [Inference Expression]
  , _dataization :: [Inference Bytes]
  , _sources :: [String]
  }

-- The engine interpreting the rules of YAML, which names every step of
-- normalization as one matching a term, so each of them is tried.
yaml :: Engine
yaml = Engine steps (every steps) Map.empty normal contextualize (map morphingOf Y.morphingRules) (map dataizationOf Y.dataizationRules) current
  where
    steps :: [Step]
    steps = map interpreted Y.normalizationRules

-- The step the engine takes for the rewriting rule: the one it was compiled
-- to, where the engine was compiled from this very rule, and the interpreted
-- one otherwise, so a rule of '--rule' changed after 'phino compile' still
-- runs as it is written.
stepOf :: Engine -> Y.Rule -> Step
stepOf engine rule = fromMaybe (interpreted rule) (Map.lookup (show rule) engine._rules)

-- The texts of the built-in rules phino carries, of all four judgments.
current :: [String]
current =
  map show Y.normalizationRules
    ++ map show Y.contextualizationRules
    ++ map show Y.morphingRules
    ++ map show Y.dataizationRules

-- Whether the engine runs the built-in rules phino carries, and not the ones
-- it carried when the engine was compiled.
fresh :: Engine -> Bool
fresh engine = engine._sources == current

-- The term builder the functions of a rule run with, whose 'contextualize' is
-- the 𝒞 of the engine.
building :: Engine -> BuildTermFunc
building engine "contextualize" = contextualizing engine._contextualize
building _ func = buildTerm func
