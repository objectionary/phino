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
import Rule (RuleContext (RuleContext), isNF, meetCondition)
import System.FilePath
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)
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
          let prog = expression pack
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
      ]
      (\(desc, expr, expected) -> it desc $ isNF expr ctx `shouldBe` expected)
