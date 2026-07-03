{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module YamlSpec where

import Control.Exception (Exception (displayException), SomeException)
import Control.Monad
import Data.List (isInfixOf, nub, (\\))
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml qualified as Yaml
import Files (allPathsIn)
import System.FilePath
import Test.Hspec (Spec, describe, it, runIO, shouldBe, shouldReturn, shouldSatisfy, shouldThrow)
import Yaml (ContextualizeRule (..), DataizeRule (..), MorphRule (..), Operation (..), Premise (..), contextualizationRules, dataizationRules, morphingRules, yamlRule)

spec :: Spec
spec = do
  describe "parses yaml rule" $ do
    let resources = "test-resources/yaml-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          _ <- yamlRule pth
          pure () `shouldReturn` ()
      )

  describe "fails on yaml typos" $ do
    let resources = "test-resources/yaml-typos"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth ->
          it (makeRelative resources pth) $
            shouldThrow
              (yamlRule pth)
              ( \e ->
                  let msg = displayException (e :: SomeException)
                   in "Unknown" `isInfixOf` msg || "Exactly one" `isInfixOf` msg
              )
      )

  describe "rejects a label that equals the name" $ do
    let failsAsRedundant decoded = case decoded of
          Left err -> "redundant" `isInfixOf` Yaml.prettyPrintParseException err
          Right _ -> False
        decodeYaml :: (Yaml.FromJSON a) => String -> Either Yaml.ParseException a
        decodeYaml = Yaml.decodeEither' . encodeUtf8 . T.pack
    it "in a morphing rule" $
      (decodeYaml "name: prim\nlabel: prim\nmatch: ⟦𝐵⟧\ne-match: 𝑒\nn-result: ⟦𝐵⟧" :: Either Yaml.ParseException MorphRule)
        `shouldSatisfy` failsAsRedundant
    it "in a dataization rule" $
      (decodeYaml "name: end\nlabel: end\nmatch: ⊥\ne-match: 𝑒\nd-result: '--'" :: Either Yaml.ParseException DataizeRule)
        `shouldSatisfy` failsAsRedundant
    it "in a contextualization rule" $
      (decodeYaml "name: cxi\nlabel: cxi\nmatch: ξ\nc-match: 𝑘\nc-result: 𝑘" :: Either Yaml.ParseException ContextualizeRule)
        `shouldSatisfy` failsAsRedundant

  describe "keeps effective labels unique across rule sets" $
    -- The effective label of a rule is its 'label' when present, else its
    -- 'name'. 'explain' typesets that label as the rule's token, so two rules
    -- sharing an effective label become indistinguishable. Collect every
    -- effective label from the three embedded rule sets and assert no repeats.
    it "across morphing, dataization and contextualization rules" $ do
      let labels :: [String]
          labels =
            map (\MorphRule{name, label} -> fromMaybe name label) morphingRules
              ++ map (\DataizeRule{name, label} -> fromMaybe name label) dataizationRules
              ++ map (\ContextualizeRule{name, label} -> fromMaybe name label) contextualizationRules
      (labels \\ nub labels) `shouldBe` []

  describe "reserves 𝑛-family metas for normal forms" $
    -- 𝒞 ('contextualize') and 𝔼 ('evaluate') return an expression that is not
    -- necessarily a normal form — that is why a 'normalize' premise follows
    -- them — so binding their result to an 𝑛-reserved meta (internal prefix
    -- "n") in a morphing or dataization rule conflates the calculus's 'e'
    -- (expression) with 'n' (normal form). Such a slip is notational, not
    -- functional (the meta name is only a substitution-map key), so it is easy
    -- to miss by eye; flag it automatically instead. Contextualization is
    -- excluded on purpose: its 𝒞-valued results are the point and its header
    -- reserves nothing (see #971).
    it "no contextualize/evaluate premise in a morphing or dataization rule binds an 𝑛-reserved meta" $ do
      let expressionValued :: Operation -> Bool
          expressionValued OpContextualize{} = True
          expressionValued OpEvaluate{} = True
          expressionValued _ = False
          verbOf :: Operation -> String
          verbOf OpContextualize{} = "contextualize"
          verbOf OpEvaluate{} = "evaluate"
          verbOf _ = "?"
          premisesOf :: [(String, [Premise])]
          premisesOf =
            map (\MorphRule{name, premises} -> (name, premises)) morphingRules
              ++ map (\DataizeRule{name, premises} -> (name, premises)) dataizationRules
          offenders :: [String]
          offenders =
            [ ruleName ++ ": " ++ verbOf operation ++ " result '" ++ T.unpack result ++ "'"
            | (ruleName, premises) <- premisesOf
            , Premise{result, operation} <- premises
            , expressionValued operation
            , T.isPrefixOf (T.pack "n") result
            ]
      offenders `shouldBe` []
