{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module RuleSpec where

import AST (Argument (..), Attribute (..), Binding (..), Bytes (..), Expression (..), Function (..))
import Control.Monad
import Data.Aeson
import Data.Yaml qualified as Y
import Files (allPathsIn)
import Functions (buildTerm)
import GHC.Generics
import Matcher
import Printer (printSubsts)
import Rule (RuleContext (RuleContext), isNF, matchExpressionWithRule, meetCondition)
import System.FilePath
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe, shouldSatisfy)
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
          met <- meetCondition (condition pack) matched (RuleContext buildTerm)
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
    let ctx = RuleContext buildTerm
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
        ctx = RuleContext buildTerm

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
