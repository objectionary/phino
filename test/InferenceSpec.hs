{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module InferenceSpec where

import AST
import Data.List (find)
import Data.Maybe (fromMaybe, isNothing)
import Deps (Judgment (..))
import Engine (Engine (..), building, yaml)
import Inference (Conclusion (..), Premises (..), Way (..), dataizationOf, dataizationSpine, direct, morphingOf, morphingSpine)
import Rule (RuleContext (RuleContext))
import System.IO.Error (ioeGetErrorString)
import Test.Hspec (Spec, describe, it, shouldBe, shouldThrow)
import Text.Printf (printf)
import Yaml qualified as Y

spec :: Spec
spec = do
  describe "morphingSpine" $ do
    it "answers with the conclusion of a rule no premise produces" $
      morphingSpine (Y.MorphRule "qz" Nothing ExXi (ExMeta "e1") (ExDispatch ExRoot (AtLabel "wk")) Nothing [])
        `shouldBe` Right ([], Answered (Morphing, "qz") (ExDispatch ExRoot (AtLabel "wk")))
    it "names the universe the rule 'universe' normalizes" $
      snd <$> morphingSpine (morphingRule "universe") `shouldBe` Right (Onward (Named (Morphing, "universe")) (ExMeta "e1") (ExMeta "e1"))
    it "normalizes the term the rule 'ma' builds for its spine" $
      snd <$> morphingSpine (morphingRule "ma") `shouldBe` Right (Onward (Normalized (Morphing, "ma")) (ExApplication (ExMeta "n2") (ArTau (AtMeta "t1") (ExMeta "k1"))) (ExMeta "e1"))
    it "keeps the premise the rule 'ma' runs beside its spine" $
      fst <$> morphingSpine (morphingRule "ma") `shouldBe` Right [Y.Premise "n2" (Y.OpMorph (ExMeta "n1") (ExMeta "e1"))]
    it "takes a step to the term a rule hands straight to its 'morph'" $
      morphingSpine (Y.MorphRule "vb" Nothing ExXi (ExMeta "e1") (ExMeta "n7") Nothing [Y.Premise "n7" (Y.OpMorph ExTermination (ExMeta "e1"))])
        `shouldBe` Right ([], Onward (Taken (Morphing, "vb")) ExTermination (ExMeta "e1"))
    it "refuses a rule whose conclusion no 'morph' produces" $
      morphingSpine (Y.MorphRule "hq" Nothing ExXi (ExMeta "e1") (ExMeta "n2") Nothing [Y.Premise "n2" (Y.OpNormalize ExXi)])
        `shouldBe` Left "it concludes with no 'morph' premise"
  describe "dataizationSpine" $ do
    it "answers with the data the rule 'delta' finds" $
      snd <$> dataizationSpine (dataizationRule "delta") `shouldBe` Right (Answered (Dataization, "delta") (BtMeta "d1"))
    it "labels the step of the rule 'box' by its 'contextualize'" $
      snd <$> dataizationSpine (dataizationRule "box") `shouldBe` Right (Onward (Normalized (Contextualization, "contextualize")) (ExMeta "e3") (ExMeta "e1"))
    it "labels the step of the rule 'fire' by its 'evaluate'" $
      snd <$> dataizationSpine (dataizationRule "fire") `shouldBe` Right (Onward (Taken (Evaluation, "evaluate")) (ExMeta "n1") (ExMeta "e1"))
    it "labels the step of the rule 'none' by its 'dataize'" $
      snd <$> dataizationSpine (dataizationRule "none") `shouldBe` Right (Onward (Taken (Dataization, "dataize")) ExTermination (ExMeta "e1"))
    it "stages the morphing the rule 'norm' asks for" $
      snd <$> dataizationSpine (dataizationRule "norm") `shouldBe` Right (Onward (Staged (ExMeta "e1")) (ExMeta "n1") (ExMeta "e1"))
    it "labels blank a step normalizing with nothing beside it" $
      snd <$> dataizationSpine (Y.DataizeRule "pz" Nothing ExXi (ExMeta "e1") (BtMeta "d4") Nothing [Y.Premise "n5" (Y.OpNormalize ExXi), Y.Premise "d4" (Y.OpDataize (ExMeta "n5") (ExMeta "e1"))])
        `shouldBe` Right (Onward (Normalized (Dataization, "")) ExXi (ExMeta "e1"))
    it "refuses a rule whose data no 'dataize' produces" $
      dataizationSpine (Y.DataizeRule "jr" Nothing ExXi (ExMeta "e1") (BtMeta "d2") Nothing [Y.Premise "d2" (Y.OpNormalize ExXi)])
        `shouldBe` Left "it concludes with no 'dataize' premise"
  describe "morphingOf" $ do
    it "finds nothing where the rule does not match the term" $ do
      found <- morphingOf (morphingRule "mf") context ExXi (ExFormation [])
      isNothing found `shouldBe` True
    it "concludes the way the rule 'mf' answers a formation" $ do
      Just (Concludes conclusion) <- morphingOf (morphingRule "mf") context (ExFormation [BiTau (AtLabel "yk") (ExDispatch ExRoot (AtLabel "pw"))]) (ExFormation [])
      conclusion `shouldBe` Answered (Morphing, "mf") (ExFormation [BiTau (AtLabel "yk") (ExDispatch ExRoot (AtLabel "pw"))])
    it "asks the premise beside the spine of 'ma' about the head" $ do
      Just (Morphs term world _) <- morphingOf (morphingRule "ma") context (ExApplication (ExDispatch ExRoot (AtLabel "hm")) (ArTau (AtLabel "xw") (ExDispatch ExRoot (AtLabel "qv")))) (ExFormation [BiVoid (AtLabel "oj")])
      (term, world) `shouldBe` (ExDispatch ExRoot (AtLabel "hm"), ExFormation [BiVoid (AtLabel "oj")])
    it "builds the conclusion of 'ma' out of the answer its premise is handed" $ do
      Just (Morphs _ _ next) <- morphingOf (morphingRule "ma") context (ExApplication (ExDispatch ExRoot (AtLabel "hm")) (ArTau (AtLabel "xw") (ExDispatch ExRoot (AtLabel "qv")))) (ExFormation [BiVoid (AtLabel "oj")])
      Concludes conclusion <- next (ExFormation [BiVoid (AtLabel "uf")])
      conclusion `shouldBe` Onward (Normalized (Morphing, "ma")) (ExApplication (ExFormation [BiVoid (AtLabel "uf")]) (ArTau (AtLabel "xw") (ExDispatch ExRoot (AtLabel "qv")))) (ExFormation [BiVoid (AtLabel "oj")])
    it "refuses a rule running a 'normalize' beside its spine" $
      morphingOf (Y.MorphRule "gd" Nothing ExXi (ExMeta "e1") (ExMeta "n3") Nothing [Y.Premise "n2" (Y.OpNormalize ExXi), Y.Premise "n3" (Y.OpMorph ExTermination (ExMeta "e1"))]) context ExXi (ExFormation [])
        `shouldThrow` (\failure -> ioeGetErrorString failure == "The rule 'gd' cannot be run, since its premise 'n2' runs beside the spine, which only a 'morph', an 'evaluate' or a 'contextualize' can")
  describe "dataizationOf" $ do
    it "answers with the data a formation carries" $ do
      Just (Concludes conclusion) <- dataizationOf (dataizationRule "delta") context (ExFormation [BiDelta (BtMany ["3C", "7E"])]) (ExFormation [])
      conclusion `shouldBe` Answered (Dataization, "delta") (BtMany ["3C", "7E"])
    it "hands the rule 'fire' the formation to evaluate" $ do
      Just (Evaluates form world _) <- dataizationOf (dataizationRule "fire") context (ExFormation [BiLambda (Function "Kq")]) (ExFormation [BiVoid (AtLabel "rb")])
      (form, world) `shouldBe` (ExFormation [BiLambda (Function "Kq")], ExFormation [BiVoid (AtLabel "rb")])
    it "hands the rule 'box' the body to contextualize in its formation" $ do
      Just (Contextualizes body formation _) <- dataizationOf (dataizationRule "box") context (ExFormation [BiTau AtPhi (ExDispatch ExXi (AtLabel "zu"))]) (ExFormation [])
      (body, formation) `shouldBe` (ExDispatch ExXi (AtLabel "zu"), ExFormation [BiTau AtPhi (ExDispatch ExXi (AtLabel "zu"))])
  describe "direct" $ do
    it "takes the first way a compiled rule matches" $ do
      Just (Concludes conclusion) <- direct (\_ _ -> [Concludes (Answered (Morphing, "w1") ExXi), Concludes (Answered (Morphing, "w2") ExXi)]) context ExXi ExXi
      conclusion `shouldBe` Answered (Morphing, "w1") ExXi
    it "finds nothing where a compiled rule matches no way" $ do
      found <- direct (\_ _ -> [] :: [Premises Expression]) context ExXi ExXi
      isNothing found `shouldBe` True

-- The context a rule of the specs checks its conditions in: the functions of
-- the engine of YAML and no world.
context :: RuleContext
context = RuleContext (building yaml) Nothing yaml._normal

-- The built-in rule of 𝕄 of the given name.
morphingRule :: String -> Y.MorphRule
morphingRule name = fromMaybe (error (printf "no morphing rule is named '%s'" name)) (find (\rule -> rule.name == name) Y.morphingRules)

-- The built-in rule of 𝔻 of the given name.
dataizationRule :: String -> Y.DataizeRule
dataizationRule name = fromMaybe (error (printf "no dataization rule is named '%s'" name)) (find (\rule -> rule.name == name) Y.dataizationRules)
