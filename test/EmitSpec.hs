{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module EmitSpec where

import AST
import Control.Monad (forM_)
import Data.Either (fromRight)
import Data.List (isInfixOf)
import Emit (emitted)
import Engine (current)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Yaml qualified as Y

spec :: Spec
spec = do
  describe "emitted" $ do
    it "writes the module the flag 'compiled' links in" $
      fromRight "" (emitted Y.normalizationRules [] Y.contextualizationRules current)
        `shouldSatisfy` ("module Compiled (compiled) where" `isInfixOf`)
    it "writes a function for every built-in rule of normalization" $
      fromRight "" (emitted Y.normalizationRules [] Y.contextualizationRules current)
        `shouldSatisfy` (\source -> all (\rule -> ("R.direct " ++ show rule.name) `isInfixOf` source) Y.normalizationRules)
    it "writes an equation for every rule of contextualization" $
      fromRight "" (emitted Y.normalizationRules [] Y.contextualizationRules current)
        `shouldSatisfy` (\source -> all (\rule -> ("(" ++ show rule.name ++ ", ") `isInfixOf` source) Y.contextualizationRules)
    it "writes a rule of '--rule' beside the built-in ones" $
      fromRight "" (emitted [] [Y.Rule "kwpr" Nothing Nothing (ExDispatch ExXi (AtLabel "hq")) ExRoot Nothing Nothing Nothing] [] [])
        `shouldSatisfy` ("R.direct \"kwpr\" False rewriteKwpr" `isInfixOf`)
    it "imports the texts a label of a rule is spelled with" $
      fromRight "" (emitted [] [Y.Rule "lb" Nothing Nothing (ExDispatch ExXi (AtLabel "hq")) ExRoot Nothing Nothing Nothing] [] [])
        `shouldSatisfy` ("import qualified Data.Text as T" `isInfixOf`)
    it "tells two rules of the same name apart" $
      fromRight "" (emitted [] [Y.Rule "ov" Nothing Nothing ExXi ExRoot Nothing Nothing Nothing, Y.Rule "ov" Nothing Nothing ExRoot ExXi Nothing Nothing Nothing] [] [])
        `shouldSatisfy` (\source -> all (`isInfixOf` source) ["stepOv0 ::", "stepOv1 ::"])
    it "compares a meta met twice in the pattern" $
      fromRight "" (emitted [] [Y.Rule "tw" Nothing Nothing (ExApplication (ExMeta "e1") (ArTau (AtLabel "g") (ExMeta "e1"))) ExRoot Nothing Nothing Nothing] [] [])
        `shouldSatisfy` ("x1 == x3" `isInfixOf`)
  describe "emitted refuses a rule of contextualization" $
    it "with a premise that is no contextualization" $
      emitted [] [] [Y.ContextualizeRule "cm" Nothing (ExMeta "n1") (ExMeta "k1") (ExMeta "n2") [Y.Premise "n2" (Y.OpNormalize (ExMeta "n1"))]] []
        `shouldBe` Left "The contextualization rule 'cm' cannot be compiled, since its premise 'n2' is not a contextualization"
  describe "emitted refuses" $
    forM_
      [
        ( "a rule with a 'having' condition"
        , Y.Rule "hv" Nothing Nothing ExXi ExRoot Nothing Nothing (Just (Y.NF (ExMeta "e1")))
        , "The rule 'hv' cannot be compiled, since it has a 'having' condition"
        )
      ,
        ( "a rule rewriting a formation into a formation the fast way"
        , Y.Rule "fs" Nothing Nothing (ExFormation [BiMeta "B1", BiVoid (AtLabel "x"), BiMeta "B2"]) (ExFormation [BiMeta "B1", BiVoid (AtLabel "y"), BiMeta "B2"]) Nothing Nothing Nothing
        , "The rule 'fs' cannot be compiled, since it rewrites a formation into a formation the fast way"
        )
      ,
        ( "a pattern applying Φ to a ρ"
        , Y.Rule "rt" Nothing Nothing (ExDispatch (ExApplication ExRoot (ArTau AtRho ExXi)) (AtLabel "k")) ExXi Nothing Nothing Nothing
        , "The rule 'rt' cannot be compiled, since its pattern applies Φ to a ρ"
        )
      ,
        ( "a function of 'where' other than 'contextualize' and 'named'"
        , Y.Rule "rw" Nothing Nothing ExXi (ExMeta "e1") Nothing (Just [Y.Extra (Y.ArgExpression (ExMeta "e1")) "random-tau" []]) Nothing
        , "The rule 'rw' cannot be compiled, since its 'where' calls the function 'random-tau', which only 'contextualize' and 'named' can be"
        )
      ,
        ( "a condition 'matches'"
        , Y.Rule "mt" Nothing Nothing (ExMeta "e1") ExRoot (Just (Y.Matches "^a$" (ExMeta "e1"))) Nothing Nothing
        , "The rule 'mt' cannot be compiled, since its condition 'matches' needs a run of dataization"
        )
      ,
        ( "a condition 'part-of'"
        , Y.Rule "po" Nothing Nothing (ExMeta "e1") ExRoot (Just (Y.PartOf (ExMeta "e1") (BiMeta "B1"))) Nothing Nothing
        , "The rule 'po' cannot be compiled, since its condition 'part-of' is not compiled yet"
        )
      ,
        ( "a normal form asked of a term that is no meta"
        , Y.Rule "nx" Nothing Nothing (ExMeta "e1") ExRoot (Just (Y.NF ExXi)) Nothing Nothing
        , "The rule 'nx' cannot be compiled, since its condition asks about a term that is no meta"
        )
      ,
        ( "a result naming a meta the pattern does not bind"
        , Y.Rule "ub" Nothing Nothing ExXi (ExMeta "e7") Nothing Nothing Nothing
        , "The rule 'ub' cannot be compiled, since its result names a meta its pattern does not bind"
        )
      ,
        ( "a pattern holding a fresh symbol"
        , Y.Rule "fr" Nothing Nothing (ExFormation [BiLambda (FnFresh (Slot "S" 3))]) ExRoot Nothing Nothing Nothing
        , "The rule 'fr' cannot be compiled, since its pattern asks for a fresh symbol"
        )
      ]
      ( \(desc, rule, reason) ->
          it desc (emitted [] [rule] [] [] `shouldBe` Left reason)
      )
