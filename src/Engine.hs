{-# LANGUAGE OverloadedRecordDot #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

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

yaml :: Engine
yaml = Engine steps (every steps) Map.empty normal contextualize (map morphingOf Y.morphingRules) (map dataizationOf Y.dataizationRules) current
  where
    steps :: [Step]
    steps = map interpreted Y.normalizationRules

stepOf :: Engine -> Y.Rule -> Step
stepOf engine rule = fromMaybe (interpreted rule) (Map.lookup (show rule) engine._rules)

current :: [String]
current =
  map show Y.normalizationRules
    ++ map show Y.contextualizationRules
    ++ map show Y.morphingRules
    ++ map show Y.dataizationRules

fresh :: Engine -> Bool
fresh engine = engine._sources == current

building :: Engine -> BuildTermFunc
building engine "contextualize" = contextualizing engine._contextualize
building _ func = buildTerm func
