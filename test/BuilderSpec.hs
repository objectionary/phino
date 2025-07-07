-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module BuilderSpec where

import Ast
import Builder
import Control.Monad
import Data.Map.Strict qualified as Map
import Matcher
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, describe, it, shouldBe)

test :: (Show a, Eq a) => (a -> Subst -> Maybe (a, a)) -> [(String, a, [(String, MetaValue)], Maybe (a, a))] -> SpecWith (Arg Expectation)
test function useCases =
  forM_ useCases $ \(desc, expr, mp, res) ->
    it desc $ function expr (Subst (Map.fromList mp)) `shouldBe` res

spec :: Spec
spec = do
  describe "buildExpression" $
    test
      buildExpression
      [ ( "Q.!a => (!a >> x) => Q.x",
          ExDispatch ExGlobal (AtMeta "a"),
          [("a", MvAttribute (AtLabel "x"))],
          Just (ExDispatch ExGlobal (AtLabel "x"), ExFormation [])
        ),
        ( "Q.c(!a -> !e) => (!a >> x, !e >> $.y.z) => Q.c(x -> $.y.z)",
          ExApplication (ExDispatch ExGlobal (AtLabel "c")) (BiTau (AtMeta "a") (ExMeta "e")),
          [("a", MvAttribute (AtLabel "x")), ("e", MvExpression (ExDispatch (ExDispatch ExThis (AtLabel "y")) (AtLabel "z")) (ExFormation []))],
          Just (ExApplication (ExDispatch ExGlobal (AtLabel "c")) (BiTau (AtLabel "x") (ExDispatch (ExDispatch ExThis (AtLabel "y")) (AtLabel "z"))), ExFormation [])
        ),
        ( "[[!a -> $.x, !B]] => (!a >> y, !B >> [[b -> ?, L> Func]]) => [[y -> $.x, b -> ?, L> Func]]",
          ExFormation [BiTau (AtMeta "a") (ExDispatch ExThis (AtLabel "x")), BiMeta "B"],
          [("a", MvAttribute (AtLabel "y")), ("B", MvBindings [BiVoid (AtLabel "b"), BiLambda "Func"])],
          Just
            ( ExFormation
                [ BiTau (AtLabel "y") (ExDispatch ExThis (AtLabel "x")),
                  BiVoid (AtLabel "b"),
                  BiLambda "Func"
                ],
              ExFormation []
            )
        ),
        ( "Q * !t => (!t >> [.a, .b, (~1 -> $.x)]) => Q.a.b(~1 -> $.x)",
          ExMetaTail ExGlobal "t",
          [("t", MvTail [TaDispatch (AtLabel "a"), TaDispatch (AtLabel "b"), TaApplication (BiTau (AtAlpha 1) (ExDispatch ExThis (AtLabel "x")))])],
          Just (ExApplication (ExDispatch (ExDispatch ExGlobal (AtLabel "a")) (AtLabel "b")) (BiTau (AtAlpha 1) (ExDispatch ExThis (AtLabel "x"))), ExFormation [])
        ),
        ( "Q.!a => () => X",
          ExDispatch ExGlobal (AtMeta "a"),
          [],
          Nothing
        ),
        ( "!e0(!a1 -> !e1, !a2 => !e2) => (!e0 >> [[]], !a1 >> x, !e1 >> Q, !a2 >> y, !e2 >> $) => [[]](x -> Q, y -> $)",
          ExApplication (ExApplication (ExMeta "e0") (BiTau (AtMeta "a1") (ExMeta "e1"))) (BiTau (AtMeta "a2") (ExMeta "e2")),
          [ ("e0", MvExpression (ExFormation []) (ExFormation [])),
            ("a1", MvAttribute (AtLabel "x")),
            ("e1", MvExpression ExGlobal (ExFormation [])),
            ("a2", MvAttribute (AtLabel "y")),
            ("e2", MvExpression ExThis (ExFormation []))
          ],
          Just (ExApplication (ExApplication (ExFormation []) (BiTau (AtLabel "x") ExGlobal)) (BiTau (AtLabel "y") ExThis), ExFormation [])
        ),
        ( "⟦!a ↦ ∅, !B⟧.!a => (!a >> t, !B >> ⟦ x ↦ ξ.t ⟧ ) => ⟦ t ↦ ∅, x ↦ ξ.t ⟧.t",
          ExDispatch (ExFormation [BiVoid (AtMeta "a"), BiMeta "B"]) (AtMeta "a"),
          [ ("a", MvAttribute (AtLabel "t")),
            ("B", MvBindings [BiTau (AtLabel "x") (ExDispatch ExThis (AtLabel "t"))])
          ],
          Just
            ( ExDispatch
                ( ExFormation
                    [ BiVoid (AtLabel "t"),
                      BiTau (AtLabel "x") (ExDispatch ExThis (AtLabel "t"))
                    ]
                )
                (AtLabel "t"),
              ExFormation []
            )
        )
      ]

  describe "buildExpressions" $ do
    it "!e => [(!e >> Q.x), (!e >> $.y)] => [Q.x, $.y]" $
      buildExpressions
        (ExMeta "e")
        [ substSingle "e" (MvExpression (ExDispatch ExGlobal (AtLabel "x")) (ExFormation [])),
          substSingle "e" (MvExpression (ExDispatch ExThis (AtLabel "y")) (ExFormation []))
        ]
        `shouldBe` Just [(ExDispatch ExGlobal (AtLabel "x"), ExFormation []), (ExDispatch ExThis (AtLabel "y"), ExFormation [])]
    it "!e => [(!e1 >> Q.x)] => X" $
      buildExpressions
        (ExMeta "e")
        [substSingle "e1" (MvExpression (ExDispatch ExGlobal (AtLabel "x")) (ExFormation []))]
        `shouldBe` Nothing
