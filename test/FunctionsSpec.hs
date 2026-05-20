{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module FunctionsSpec where

import AST
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Deps (Term (TeBindings))
import Functions (buildTerm)
import Logger (logDebug)
import Matcher (MetaValue (MvBindings), Subst (Subst))
import Misc (uniqueBindings')
import Printer (printExpression)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Printf (printf)
import Yaml (ExtraArgument (ArgBinding, ArgExpression))

emit :: Expression
emit = ExDispatch (ExDispatch ExGlobal (AtLabel "hone")) (AtLabel "emit")

phiBinding :: T.Text -> Expression -> Binding
phiBinding name body = BiTau (AtLabel name) (ExFormation [BiTau AtPhi body])

spec :: Test.Hspec.Spec
spec = describe "Functions" $ do
  Test.Hspec.it "contains only unique bindings after 'join'" $ do
    let first = ("B1", MvBindings [BiVoid AtRho, BiDelta BtEmpty, BiTau (AtLabel "x") ExGlobal, BiVoid (AtAlpha 0)])
        second = ("B2", MvBindings [BiTau AtRho ExThis, BiLambda "Func", BiDelta (BtOne "00"), BiVoid (AtAlpha 1)])
        third = ("B3", MvBindings [BiLambda "Some", BiTau (AtLabel "y") ExThis, BiTau (AtLabel "x") ExThis, BiVoid (AtAlpha 0)])
        subst = Subst (Map.fromList [first, second, third])
    TeBindings bds <- buildTerm "join" [ArgBinding (BiMeta "B1"), ArgBinding (BiMeta "B2"), ArgBinding (BiMeta "B3")] subst
    bds' <- uniqueBindings' bds
    logDebug (printf "Joined bindings:\n%s" (printExpression (ExFormation bds')))
    length bds' `shouldBe` 9
  Test.Hspec.it "splices replacement before every sentinel match" $ do
    let foo = ExDispatch ExGlobal (AtLabel "foo")
        bar = ExDispatch ExGlobal (AtLabel "bar")
        cpsBody =
          [ phiBinding "x" foo
          , phiBinding "emit1" emit
          , phiBinding "y" bar
          , phiBinding "emit2" emit
          ]
        autoBody =
          [ phiBinding "add1" foo
          , phiBinding "add2" bar
          ]
        subst =
          Subst
            ( Map.fromList
                [ ("B-in", MvBindings cpsBody)
                , ("B-rep", MvBindings autoBody)
                ]
            )
    TeBindings result <-
      buildTerm
        "splice"
        [ ArgBinding (BiMeta "B-in")
        , ArgExpression emit
        , ArgBinding (BiMeta "B-rep")
        ]
        subst
    bds <- uniqueBindings' result
    logDebug (printf "Spliced bindings:\n%s" (printExpression (ExFormation bds)))
    length bds `shouldBe` length cpsBody + 2 * length autoBody
  Test.Hspec.it "returns the input unchanged when no sentinel match is found" $ do
    let foo = ExDispatch ExGlobal (AtLabel "foo")
        body = [phiBinding "x" foo, phiBinding "y" foo]
        subst =
          Subst
            ( Map.fromList
                [ ("B-in", MvBindings body)
                , ("B-rep", MvBindings [phiBinding "z" foo])
                ]
            )
    TeBindings result <-
      buildTerm
        "splice"
        [ ArgBinding (BiMeta "B-in")
        , ArgExpression emit
        , ArgBinding (BiMeta "B-rep")
        ]
        subst
    result `shouldBe` body
  Test.Hspec.it "produces only unique attributes for many splice positions" $ do
    let foo = ExDispatch ExGlobal (AtLabel "foo")
        body = [phiBinding (T.pack ('e' : show i)) emit | i <- [1 .. 5 :: Int]]
        rep = [phiBinding "a" foo, phiBinding "b" foo]
        subst =
          Subst
            ( Map.fromList
                [ ("B-in", MvBindings body)
                , ("B-rep", MvBindings rep)
                ]
            )
    TeBindings result <-
      buildTerm
        "splice"
        [ ArgBinding (BiMeta "B-in")
        , ArgExpression emit
        , ArgBinding (BiMeta "B-rep")
        ]
        subst
    bds <- uniqueBindings' result
    bds `shouldSatisfy` ((== length body + 5 * length rep) . length)
