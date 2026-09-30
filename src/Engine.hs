{-# LANGUAGE OverloadedRecordDot #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- What runs the built-in rules of the calculus: the rewriting steps of
-- normalization, the answer to whether a term is a normal form, and the
-- Contextualization function 𝒞. The engine of 'yaml' interprets the rules as
-- they are written, the way phino always has; 'phino compile' writes the
-- Haskell of another one into the module 'Compiled', which a build with the
-- flag 'compiled' links in (#1617). Nothing in the library reaches for either
-- of them: the command line picks one and hands it down through the contexts,
-- the way it hands down '_buildTerm'.
module Engine (Engine (..), building, current, fresh, stepOf, yaml) where

import AST
import Contextualize (contextualize)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Deps (BuildTermFunc)
import Functions (buildTerm, contextualizing)
import Rewriter (interpreted)
import Rule (Step, normal)
import qualified Yaml as Y

-- One engine of the built-in rules: the steps normalization takes, in the
-- order of the rules; the steps of any other rewriting rule it knows how to
-- take, by the text of the rule (see 'stepOf'); whether a term is a normal
-- form; 𝒞; and the texts of the built-in rules it was made from, which tell
-- whether it still runs the rules phino carries (see 'fresh').
data Engine = Engine
  { _normalization :: [Step]
  , _rules :: Map String Step
  , _normal :: Expression -> Bool
  , _contextualize :: Expression -> Expression -> IO Expression
  , _sources :: [String]
  }

-- The engine interpreting the rules of YAML.
yaml :: Engine
yaml = Engine (map interpreted Y.normalizationRules) Map.empty normal contextualize current

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
