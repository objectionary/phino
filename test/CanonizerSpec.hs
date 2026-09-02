{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CanonizerSpec where

import AST
import Canonizer (canonize, canonizeExpr)
import Control.Monad (forM_)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "canonizeExpr" $ do
    let nestedInput :: Expression
        nestedInput =
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
        nestedExpected :: Expression
        nestedExpected =
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
        unchanged :: Expression
        unchanged = ExFormation [BiTau (AtLabel "x") ExRoot, BiVoid AtRho]
        metaLambda :: Expression
        metaLambda = ExFormation [BiLambda (FnMeta "F")]
    forM_
      [ ("leaves an expression with no lambdas unchanged", unchanged, unchanged)
      ,
        ( "renames a single top level lambda to Fn1"
        , ExFormation [BiLambda (Function "Foo")]
        , ExFormation [BiLambda (Function "Fn1")]
        )
      , ("leaves a meta lambda binding untouched", metaLambda, metaLambda)
      , ("numbers several lambdas at different nesting depths in document order", nestedInput, nestedExpected)
      ,
        ( "recurses through ExDispatch"
        , ExDispatch (ExFormation [BiLambda (Function "Wrapped")]) (AtLabel "attr")
        , ExDispatch (ExFormation [BiLambda (Function "Fn1")]) (AtLabel "attr")
        )
      ,
        ( "recurses through ExApplication's alpha argument"
        , ExApplication ExRoot (ArAlpha (Alpha 0) (ExFormation [BiLambda (Function "Wrapped")]))
        , ExApplication ExRoot (ArAlpha (Alpha 0) (ExFormation [BiLambda (Function "Fn1")]))
        )
      ,
        ( "recurses through ExPhiMeet"
        , ExPhiMeet (Just "p") 1 (ExFormation [BiLambda (Function "Meet")])
        , ExPhiMeet (Just "p") 1 (ExFormation [BiLambda (Function "Fn1")])
        )
      ,
        ( "recurses through ExPhiAgain"
        , ExPhiAgain Nothing 2 (ExFormation [BiLambda (Function "Again")])
        , ExPhiAgain Nothing 2 (ExFormation [BiLambda (Function "Fn1")])
        )
      ]
      (\(desc, input, expected) -> it desc (canonizeExpr input `shouldBe` expected))

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
