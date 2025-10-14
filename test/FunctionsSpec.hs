-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module FunctionsSpec where

import Ast
import Data.Map.Strict qualified as Map
import Functions (buildTerm)
import Logger (logDebug)
import Matcher (MetaValue (MvBindings), Subst (Subst))
import Misc (uniqueBindings')
import Pretty (prettyExpression')
import Deps (Term (TeBindings))
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Printf (printf)
import Yaml (ExtraArgument (ArgBinding))

spec :: Test.Hspec.Spec
spec = describe "Functions" $
  Test.Hspec.it "contains only unique bindings after 'join'" $ do
    let first = ("B1", MvBindings [BiVoid AtRho, BiDelta BtEmpty, BiTau (AtLabel "x") ExGlobal, BiVoid (AtAlpha 0)])
        second = ("B2", MvBindings [BiTau AtRho ExThis, BiLambda "Func", BiDelta (BtOne "00"), BiVoid (AtAlpha 1)])
        third = ("B3", MvBindings [BiLambda "Some", BiTau (AtLabel "y") ExThis, BiTau (AtLabel "x") ExThis, BiVoid (AtAlpha 0)])
        subst = Subst (Map.fromList [first, second, third])
    TeBindings bds <- buildTerm "join" [ArgBinding (BiMeta "B1"), ArgBinding (BiMeta "B2"), ArgBinding (BiMeta "B3")] subst (Program ExGlobal)
    bds' <- uniqueBindings' bds
    logDebug (printf "Joined bindings:\n%s" (prettyExpression' (ExFormation bds')))
    length bds' `shouldBe` 9
