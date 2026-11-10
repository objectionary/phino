{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module EngineSpec where

import AST
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Deps (Term (TeExpression))
import Engine (Engine (..), building, fresh, stepOf, yaml)
import Matcher (substEmpty)
import Rule (Step (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Yaml qualified as Y

spec :: Spec
spec = do
  describe "stepOf" $ do
    it "interprets a rule the engine was not compiled from" $
      _name (stepOf yaml (Y.Rule "wnkq" Nothing Nothing ExXi ExRoot Nothing Nothing Nothing)) `shouldBe` "wnkq"
    it "takes the step the engine compiled out of the very same rule" $
      let rule = Y.Rule "prv" Nothing Nothing ExTermination ExXi Nothing Nothing Nothing
       in _name (stepOf yaml{_rules = Map.fromList [(show rule, Step "zyx8" (\_ _ -> pure Nothing))]} rule) `shouldBe` "zyx8"
  describe "yaml" $
    it "names every rule of normalization as one matching a term" $
      _matching yaml (Just (ExFormation [BiVoid (AtLabel "ug")])) (ExDispatch ExRoot (AtLabel "yb"))
        `shouldBe` Set.fromList [0 .. length Y.normalizationRules - 1]
  describe "fresh" $ do
    it "accepts the engine interpreting the rules phino carries" $
      fresh yaml `shouldBe` True
    it "refuses an engine compiled from other rules" $
      fresh yaml{_sources = ["Rule {name = \"gone\"}"]} `shouldBe` False
  describe "building" $
    it "hands contextualization to the engine" $ do
      TeExpression term <- building yaml{_contextualize = \_ _ -> pure (ExDispatch ExRoot (AtLabel "qo"))} "contextualize" [Y.ArgExpression ExXi, Y.ArgExpression (ExFormation [])] substEmpty
      term `shouldBe` ExDispatch ExRoot (AtLabel "qo")
