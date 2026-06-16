{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module FunctionsSpec where

import AST
import Data.Map.Strict qualified as Map
import Deps (Term (TeBindings))
import Functions (buildTerm)
import Logger (logDebug)
import Matcher (MetaValue (MvBindings), Subst (Subst))
import Misc (uniqueBindings')
import Printer (printExpression)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Printf (printf)
import Yaml (ExtraArgument (ArgBinding))

spec :: Spec
spec = describe "Functions" $
  it "contains only unique bindings after 'join'" $ do
    let first = ("B1", MvBindings [BiVoid AtRho, BiDelta BtEmpty, BiTau (AtLabel "x") ExRoot, BiVoid (AtLabel "a0")])
        second = ("B2", MvBindings [BiTau AtRho ExXi, BiLambda (Function "Func"), BiDelta (BtOne "00"), BiVoid (AtLabel "a1")])
        third = ("B3", MvBindings [BiLambda (Function "Some"), BiTau (AtLabel "y") ExXi, BiTau (AtLabel "x") ExXi, BiVoid (AtLabel "a0")])
        subst = Subst (Map.fromList [first, second, third])
    TeBindings bds <- buildTerm "join" [ArgBinding (BiMeta "B1"), ArgBinding (BiMeta "B2"), ArgBinding (BiMeta "B3")] subst
    bds' <- uniqueBindings' bds
    logDebug (printf "Joined bindings:\n%s" (printExpression (ExFormation bds')))
    length bds' `shouldBe` 9
