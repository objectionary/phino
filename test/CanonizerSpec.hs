{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CanonizerSpec where

import AST
import Canonizer (canonize, canonizeExpr)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "canonizeExpr" $ do
    it "leaves an expression with no lambdas unchanged" $ do
      let expr = ExFormation [BiTau (AtLabel "x") ExRoot, BiVoid AtRho]
      canonizeExpr expr `shouldBe` expr

    it "renames a single top level lambda to Fn1" $
      canonizeExpr (ExFormation [BiLambda (Function "Foo")])
        `shouldBe` ExFormation [BiLambda (Function "Fn1")]

    it "leaves a meta lambda binding untouched" $ do
      let expr = ExFormation [BiLambda (FnMeta "F")]
      canonizeExpr expr `shouldBe` expr

    it "numbers several lambdas at different nesting depths in document order" $ do
      let expr =
            ExFormation
              [ BiLambda (Function "First")
              , BiTau
                  (AtLabel "child")
                  (ExFormation [BiLambda (Function "Second")])
              , BiTau
                  (AtLabel "app")
                  ( ExApplication
                      (ExFormation [BiLambda (Function "Third")])
                      (ArTau (AtLabel "y") ExRoot)
                  )
              ]
          expected =
            ExFormation
              [ BiLambda (Function "Fn1")
              , BiTau
                  (AtLabel "child")
                  (ExFormation [BiLambda (Function "Fn2")])
              , BiTau
                  (AtLabel "app")
                  ( ExApplication
                      (ExFormation [BiLambda (Function "Fn3")])
                      (ArTau (AtLabel "y") ExRoot)
                  )
              ]
      canonizeExpr expr `shouldBe` expected

    it "recurses through ExDispatch" $
      canonizeExpr (ExDispatch (ExFormation [BiLambda (Function "Wrapped")]) (AtLabel "attr"))
        `shouldBe` ExDispatch (ExFormation [BiLambda (Function "Fn1")]) (AtLabel "attr")

    it "recurses through ExApplication's alpha argument" $
      canonizeExpr (ExApplication ExRoot (ArAlpha (Alpha 0) (ExFormation [BiLambda (Function "Wrapped")])))
        `shouldBe` ExApplication ExRoot (ArAlpha (Alpha 0) (ExFormation [BiLambda (Function "Fn1")]))

    it "recurses through ExPhiMeet" $
      canonizeExpr (ExPhiMeet (Just "p") 1 (ExFormation [BiLambda (Function "Meet")]))
        `shouldBe` ExPhiMeet (Just "p") 1 (ExFormation [BiLambda (Function "Fn1")])

    it "recurses through ExPhiAgain" $
      canonizeExpr (ExPhiAgain Nothing 2 (ExFormation [BiLambda (Function "Again")]))
        `shouldBe` ExPhiAgain Nothing 2 (ExFormation [BiLambda (Function "Fn1")])

  describe "canonize" $ do
    it "returns an empty list for an empty input" $
      canonize [] `shouldBe` []

    it "restarts the counter independently for each Rewritten in the chain" $ do
      let first = ExFormation [BiLambda (Function "X")]
          second = ExFormation [BiLambda (Function "Y")]
          expected = ExFormation [BiLambda (Function "Fn1")]
      canonize [(first, Just "rule-1"), (second, Just "rule-2")]
        `shouldBe` [(expected, Just "rule-1"), (expected, Just "rule-2")]

    it "preserves the rule tag alongside the canonized expression" $
      canonize [(ExFormation [BiLambda (Function "Foo")], Nothing)]
        `shouldBe` [(ExFormation [BiLambda (Function "Fn1")], Nothing)]
