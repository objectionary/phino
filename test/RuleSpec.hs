{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module RuleSpec where

import AST (Attribute (..), Binding (..), Bytes (..), Expression (..), Program (Program))
import Control.Monad
import Data.Aeson
import Data.Yaml qualified as Y
import Functions (buildTerm)
import GHC.Generics
import Matcher
import Misc
import Printer (printSubsts)
import Rule (RuleContext (RuleContext), isNF, matchProgramWithRule, meetCondition)
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
          let prog = Program (expression pack)
          let matched = matchProgram (pattern pack) prog
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
      [ ("returns true for ExThis", ExThis, True)
      , ("returns true for ExGlobal", ExGlobal, True)
      , ("returns true for ExTermination", ExTermination, True)
      , ("returns true for dispatch on ExThis", ExDispatch ExThis (AtLabel "foo"), True)
      , ("returns true for dispatch on ExGlobal", ExDispatch ExGlobal (AtLabel "bar"), True)
      , ("returns false for dispatch on ExTermination", ExDispatch ExTermination (AtLabel "x"), False)
      , ("returns false for application on ExTermination", ExApplication ExTermination (BiTau (AtLabel "y") ExGlobal), False)
      , ("returns true for empty formation", ExFormation [], True)
      , ("returns true for formation with only delta binding", ExFormation [BiDelta (BtMany ["00", "01"])], True)
      , ("returns true for formation with only void binding", ExFormation [BiVoid (AtLabel "x")], True)
      , ("returns true for formation with only lambda binding", ExFormation [BiLambda "Func"], True)
      , ("returns true for formation with delta void and lambda", ExFormation [BiDelta (BtOne "FF"), BiVoid (AtLabel "y"), BiLambda "G"], True)
      ]
      (\(desc, expr, expected) -> it desc $ isNF expr ctx `shouldBe` expected)
  describe "matchProgramWithRule matches programs" $ do
    let ctx = RuleContext buildTerm
    it "returns non-empty substitutions for matching pattern" $ do
      rule <- Y.decodeFileThrow "resources/copy.yaml" :: IO Yaml.Rule
      let prog = Program (ExApplication (ExFormation [BiVoid (AtLabel "x")]) (BiTau (AtLabel "x") ExGlobal))
      matched <- matchProgramWithRule prog rule ctx
      matched `shouldSatisfy` (not . null)
    it "returns empty substitutions when pattern does not match" $ do
      rule <- Y.decodeFileThrow "resources/copy.yaml" :: IO Yaml.Rule
      let prog = Program ExGlobal
      matched <- matchProgramWithRule prog rule ctx
      matched `shouldSatisfy` null
    it "returns empty when condition fails" $ do
      rule <- Y.decodeFileThrow "resources/copy.yaml" :: IO Yaml.Rule
      let prog = Program (ExApplication (ExFormation [BiVoid (AtLabel "x")]) (BiTau (AtLabel "x") ExThis))
      matched <- matchProgramWithRule prog rule ctx
      matched `shouldSatisfy` null
