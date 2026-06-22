{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module BuilderSpec where

import AST
import Builder
import Control.Monad
import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Matcher
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, anyException, describe, it, shouldBe, shouldSatisfy, shouldThrow)

test :: (Show a, Eq a) => (a -> Subst -> Either String a) -> [(String, a, [(T.Text, MetaValue)], Either String a)] -> SpecWith (Arg Expectation)
test function useCases =
  forM_ useCases $ \(desc, expr, mp, res) ->
    it desc $ function expr (Subst (Map.fromList mp)) `shouldBe` res

spec :: Spec
spec = do
  describe "buildExpression" $
    test
      buildExpression
      [
        ( "Q.!a => (!a >> x) => Q.x"
        , ExDispatch ExRoot (AtMeta "a")
        , [("a", MvAttribute (AtLabel "x"))]
        , Right (ExDispatch ExRoot (AtLabel "x"))
        )
      ,
        ( "Q.c(!a -> !e) => (!a >> x, !e >> $.y.z) => Q.c(x -> $.y.z)"
        , ExApplication (ExDispatch ExRoot (AtLabel "c")) (ArTau (AtMeta "a") (ExMeta "e"))
        , [("a", MvAttribute (AtLabel "x")), ("e", MvExpression (ExDispatch (ExDispatch ExXi (AtLabel "y")) (AtLabel "z")))]
        , Right (ExApplication (ExDispatch ExRoot (AtLabel "c")) (ArTau (AtLabel "x") (ExDispatch (ExDispatch ExXi (AtLabel "y")) (AtLabel "z"))))
        )
      ,
        ( "[[!a -> $.x, !B]] => (!a >> y, !B >> [[b -> ?, L> Func]]) => [[y -> $.x, b -> ?, L> Func]]"
        , ExFormation [BiTau (AtMeta "a") (ExDispatch ExXi (AtLabel "x")), BiMeta "B"]
        , [("a", MvAttribute (AtLabel "y")), ("B", MvBindings [BiVoid (AtLabel "b"), BiLambda (Function "Func")])]
        , Right
            ( ExFormation
                [ BiTau (AtLabel "y") (ExDispatch ExXi (AtLabel "x"))
                , BiVoid (AtLabel "b")
                , BiLambda (Function "Func")
                ]
            )
        )
      ,
        ( "Q.!a => () => X"
        , ExDispatch ExRoot (AtMeta "a")
        , []
        , Left "meta 'a' is either does not exist or refers to an inappropriate term"
        )
      ,
        ( "!e0(!a1 -> !e1, !a2 => !e2) => (!e0 >> [[]], !a1 >> x, !e1 >> Q, !a2 >> y, !e2 >> $) => [[]](x -> Q, y -> $)"
        , ExApplication (ExApplication (ExMeta "e0") (ArTau (AtMeta "a1") (ExMeta "e1"))) (ArTau (AtMeta "a2") (ExMeta "e2"))
        ,
          [ ("e0", MvExpression (ExFormation []))
          , ("a1", MvAttribute (AtLabel "x"))
          , ("e1", MvExpression ExRoot)
          , ("a2", MvAttribute (AtLabel "y"))
          , ("e2", MvExpression ExXi)
          ]
        , Right (ExApplication (ExApplication (ExFormation []) (ArTau (AtLabel "x") ExRoot)) (ArTau (AtLabel "y") ExXi))
        )
      ,
        ( "⟦!a ↦ ∅, !B⟧.!a => (!a >> t, !B >> ⟦ x ↦ ξ.t ⟧ ) => ⟦ t ↦ ∅, x ↦ ξ.t ⟧.t"
        , ExDispatch (ExFormation [BiVoid (AtMeta "a"), BiMeta "B"]) (AtMeta "a")
        ,
          [ ("a", MvAttribute (AtLabel "t"))
          , ("B", MvBindings [BiTau (AtLabel "x") (ExDispatch ExXi (AtLabel "t"))])
          ]
        , Right
            ( ExDispatch
                ( ExFormation
                    [ BiVoid (AtLabel "t")
                    , BiTau (AtLabel "x") (ExDispatch ExXi (AtLabel "t"))
                    ]
                )
                (AtLabel "t")
            )
        )
      ]

  describe "buildExpressions" $ do
    it "!e => [(!e >> Q.x), (!e >> $.y)] => [Q.x, $.y]" $ do
      built <-
        buildExpressionsThrows
          (ExMeta "e")
          [ substSingle "e" (MvExpression (ExDispatch ExRoot (AtLabel "x")))
          , substSingle "e" (MvExpression (ExDispatch ExXi (AtLabel "y")))
          ]
      built `shouldBe` [ExDispatch ExRoot (AtLabel "x"), ExDispatch ExXi (AtLabel "y")]
    it "!e => [(!e1 >> Q.x)] => X" $
      buildExpressionsThrows
        (ExMeta "e")
        [substSingle "e1" (MvExpression (ExDispatch ExRoot (AtLabel "x")))]
        `shouldThrow` anyException

  describe "build with duplicate attributes in bindings" $ do
    it "build binding with duplicates" $
      buildBinding (BiMeta "B") (substSingle "B" (MvBindings [BiVoid AtRho, BiVoid AtRho])) `shouldSatisfy` isLeft
    it "build formation with duplicates" $
      buildExpression (ExMeta "e") (substSingle "e" (MvExpression (ExFormation [BiVoid AtRho, BiVoid AtRho]))) `shouldSatisfy` isLeft
