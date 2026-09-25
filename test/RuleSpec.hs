{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module RuleSpec where

import AST (Argument (..), Attribute (..), Binding (..), Bytes (..), Expression (..), Function (..), inert)
import Builder (buildExpressionThrows)
import Control.Monad
import Data.Aeson
import Data.Yaml qualified as Y
import Files (allPathsIn)
import Functions (buildTerm)
import GHC.Generics
import Matcher
import Parser (parseExpressionThrows)
import Printer (printSubsts)
import Rule (RuleContext (RuleContext), isNF, matchExpressionWithRule, meetCondition, redex)
import System.FilePath
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe, shouldReturn, shouldSatisfy)
import Yaml qualified

data ConditionPack = ConditionPack
  { failure :: Maybe Bool
  , expression :: Expression
  , pattern :: Expression
  , condition :: Yaml.Condition
  }
  deriving (Generic, FromJSON, Show)

spec :: Spec
spec = do
  describe "check conditions" $ do
    let resources = "test-resources/condition-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- Y.decodeFileThrow pth :: IO ConditionPack
          let expr = expression pack
          let matched = matchExpression (pattern pack) expr
          unless (matched /= []) (expectationFailure "List of matched substitutions is empty which is not expected")
          met <- meetCondition (condition pack) matched (RuleContext buildTerm Nothing)
          case failure pack of
            Just True ->
              unless
                (null met)
                ( expectationFailure $
                    "List of substitutions after condition check must be empty, but got:\n"
                      ++ printSubsts matched
                )
            _ ->
              when
                (null met)
                ( expectationFailure $
                    "List of substitution after condition check must be not empty\nOriginal substitutions:\n"
                      ++ printSubsts matched
                )
      )
  describe "isNF determines normal form" $ do
    let ctx = RuleContext buildTerm Nothing
    forM_
      [ ("returns true for ExXi", ExXi, True)
      , ("returns true for ExRoot", ExRoot, True)
      , ("returns true for ExTermination", ExTermination, True)
      , ("returns true for dispatch on ExXi", ExDispatch ExXi (AtLabel "foo"), True)
      , ("returns true for dispatch on ExRoot", ExDispatch ExRoot (AtLabel "bar"), True)
      , ("returns false for dispatch on ExTermination", ExDispatch ExTermination (AtLabel "x"), False)
      , ("returns false for application on ExTermination", ExApplication ExTermination (ArTau (AtLabel "y") ExRoot), False)
      , ("returns true for empty formation", ExFormation [], True)
      , ("returns true for formation with only delta binding", ExFormation [BiDelta (BtMany ["00", "01"])], True)
      , ("returns true for formation with only void binding", ExFormation [BiVoid (AtLabel "x")], True)
      , ("returns true for formation with only lambda binding", ExFormation [BiLambda (Function "Func")], True)
      , ("returns true for formation with delta void and lambda", ExFormation [BiDelta (BtOne "FF"), BiVoid (AtLabel "y"), BiLambda (Function "G")], True)
      , ("returns true for a formation with a tau binding whose expression is already normal", ExFormation [BiTau (AtLabel "x") ExRoot], True)
      , ("returns false for a formation with a tau binding matching a normalization rule", ExFormation [BiTau (AtLabel "x") (ExDispatch ExTermination (AtLabel "y"))], False)
      ]
      (\(desc, expr, expected) -> it desc $ isNF expr ctx `shouldBe` expected)

  describe "matchExpressionWithRule via a 'where' extension or a φ-marker meta" $ do
    let ctx :: RuleContext
        ctx = RuleContext buildTerm Nothing

        joinRule :: Yaml.Rule
        joinRule =
          Yaml.Rule
            "extra-join-test"
            Nothing
            Nothing
            (ExFormation [BiMeta "B"])
            (ExMeta "B")
            Nothing
            (Just [Yaml.Extra (Yaml.ArgBinding (BiMeta "J")) "join" [Yaml.ArgBinding (BiMeta "B")]])
            Nothing

        cascadeRule :: Yaml.Rule
        cascadeRule =
          Yaml.Rule
            "extra-cascade-test"
            Nothing
            Nothing
            (ExMeta "e")
            (ExMeta "e")
            Nothing
            ( Just
                [ Yaml.Extra (Yaml.ArgExpression ExRoot) "random-tau" []
                , Yaml.Extra (Yaml.ArgAttribute (AtMeta "J")) "random-tau" []
                ]
            )
            Nothing

        phiMeetRule :: Yaml.Rule
        phiMeetRule =
          Yaml.Rule
            "phimeet-n-test"
            Nothing
            Nothing
            (ExFormation [BiTau (AtLabel "x") (ExPhiMeet Nothing 0 (ExMeta "n1")), BiVoid AtRho])
            (ExMeta "n1")
            Nothing
            Nothing
            Nothing

        phiAgainRule :: Yaml.Rule
        phiAgainRule =
          Yaml.Rule
            "phiagain-n-test"
            Nothing
            Nothing
            (ExFormation [BiTau (AtLabel "x") (ExPhiAgain Nothing 0 (ExMeta "n2")), BiVoid AtRho])
            (ExMeta "n2")
            Nothing
            Nothing
            Nothing

    forM_
      [
        ( "binds a fresh meta from a bindings-list built by join()"
        , ExFormation [BiTau (AtLabel "x") ExRoot, BiVoid AtRho]
        , joinRule
        , True
        )
      , ("drops a substitution once an earlier extension fails to name a meta", ExRoot, cascadeRule, False)
      ,
        ( "finds an 'n' meta nested inside a φ-meet marker"
        , ExFormation [BiTau (AtLabel "x") (ExPhiMeet Nothing 0 ExRoot), BiVoid AtRho]
        , phiMeetRule
        , True
        )
      ,
        ( "finds an 'n' meta nested inside a φ-again marker"
        , ExFormation [BiTau (AtLabel "x") (ExPhiAgain Nothing 0 ExRoot), BiVoid AtRho]
        , phiAgainRule
        , True
        )
      ]
      ( \(desc, expr, rule, expectNonEmpty) -> it desc $ do
          matched <- matchExpressionWithRule expr rule ctx
          if expectNonEmpty
            then matched `shouldSatisfy` (not . null)
            else matched `shouldBe` []
      )

  describe "matchExpressionWithRule names a formation in the universe of its context" $ do
    let namingRule :: Yaml.Rule
        namingRule =
          Yaml.Rule
            "named-test"
            Nothing
            Nothing
            (ExFormation [BiMeta "B", BiVoid AtRho])
            (ExMeta "e2")
            Nothing
            (Just [Yaml.Extra (Yaml.ArgExpression (ExMeta "e2")) "named" [Yaml.ArgExpression (ExFormation [BiMeta "B", BiVoid AtRho])]])
            Nothing
        world :: Expression
        world = ExFormation [BiTau (AtLabel "qwj") (ExFormation [BiDelta (BtOne "7C")]), BiVoid AtRho]
    it "writes Φ for the whole program the context stands in" $ do
      (mapM (buildExpressionThrows (ExMeta "e2")) =<< matchExpressionWithRule world namingRule (RuleContext buildTerm (Just world)))
        `shouldReturn` [ExRoot]
    it "writes the formation itself where the context knows no universe" $ do
      (mapM (buildExpressionThrows (ExMeta "e2")) =<< matchExpressionWithRule world namingRule (RuleContext buildTerm Nothing))
        `shouldReturn` [world]

  describe "redex" $ do
    it "takes every normalization rule for a redex" $
      all redex Yaml.normalizationRules `shouldBe` True
    it "does not take a rule of a formation holding a λ alone for a redex" $
      redex (Yaml.Rule "lone" Nothing Nothing (ExFormation [BiMeta "B1", BiLambda (FnMeta "f1")]) ExTermination Nothing Nothing Nothing) `shouldBe` False
    it "does not take a rule demanding a Δ under 'not' for a redex" $
      redex (Yaml.Rule "negated" Nothing Nothing (ExFormation [BiMeta "B1", BiLambda (FnMeta "f1")]) ExTermination (Just (Yaml.Not (Yaml.In [AtDelta] [BiMeta "B1"]))) Nothing Nothing) `shouldBe` False
    it "does not take a rule dispatching on a meta for a redex" $
      redex (Yaml.Rule "loose" Nothing Nothing (ExDispatch (ExMeta "e1") (AtMeta "t1")) ExTermination Nothing Nothing Nothing) `shouldBe` False

  describe "normalization rules over inert terms" $ do
    let samples :: [String]
        samples =
          [ "⟦ kx ↦ ξ.ow( jy ↦ Φ.ya ), λ ⤍ L_ok, b ↦ ∅ ⟧"
          , "⟦ φ ↦ Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ 𝜎3 ⟧ ) ), ρ ↦ ∅, qe ↦ ⟦ Δ ⤍ 1F- ⟧ ⟧"
          , "Φ.hz( ⟦ ab ↦ ⟦ ba ↦ ξ.ρ, λ ⤍ L_z ⟧ ⟧ ).uv( α0 ↦ ⊥ )"
          , "⟦ x ↦ ∅, dd ↦ ⟦ λ ⤍ L_dd, ρ ↦ ∅ ⟧, m1 ↦ ⟦ b ↦ ∅, φ ↦ ξ.ρ.dd( b ↦ ξ.b ), ρ ↦ ∅ ⟧ ⟧"
          ]
        context :: RuleContext
        context = RuleContext buildTerm Nothing
        matches :: Expression -> Yaml.Rule -> IO [Subst]
        matches term rule = maybe pure (\cond substs -> meetCondition cond substs context) rule.when (matchExpressionDeep rule.pattern term)
    it "takes every sample for inert" $ do
      terms <- mapM parseExpressionThrows samples
      all inert terms `shouldBe` True
    it "does not find a normalization rule matching anywhere in an inert term" $ do
      terms <- mapM parseExpressionThrows samples
      found <- concat <$> sequence [matches term rule | term <- terms, rule <- Yaml.normalizationRules]
      found `shouldBe` []
