-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module BuilderSpec where

import Ast
import Builder
import Control.Monad
import Data.Map.Strict qualified as Map
import Matcher
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, anyException, describe, it, shouldBe, shouldThrow)

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
          Just (ExDispatch ExGlobal (AtLabel "x"), defaultScope)
        ),
        ( "Q.c(!a -> !e) => (!a >> x, !e >> $.y.z) => Q.c(x -> $.y.z)",
          ExApplication (ExDispatch ExGlobal (AtLabel "c")) (BiTau (AtMeta "a") (ExMeta "e")),
          [("a", MvAttribute (AtLabel "x")), ("e", MvExpression (ExDispatch (ExDispatch ExThis (AtLabel "y")) (AtLabel "z")) defaultScope)],
          Just (ExApplication (ExDispatch ExGlobal (AtLabel "c")) (BiTau (AtLabel "x") (ExDispatch (ExDispatch ExThis (AtLabel "y")) (AtLabel "z"))), defaultScope)
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
              defaultScope
            )
        ),
        ( "Q * !t => (!t >> [.a, .b, (~1 -> $.x)]) => Q.a.b(~1 -> $.x)",
          ExMetaTail ExGlobal "t",
          [("t", MvTail [TaDispatch (AtLabel "a"), TaDispatch (AtLabel "b"), TaApplication (BiTau (AtAlpha 1) (ExDispatch ExThis (AtLabel "x")))])],
          Just (ExApplication (ExDispatch (ExDispatch ExGlobal (AtLabel "a")) (AtLabel "b")) (BiTau (AtAlpha 1) (ExDispatch ExThis (AtLabel "x"))), defaultScope)
        ),
        ( "Q.!a => () => X",
          ExDispatch ExGlobal (AtMeta "a"),
          [],
          Nothing
        ),
        ( "!e0(!a1 -> !e1, !a2 => !e2) => (!e0 >> [[]], !a1 >> x, !e1 >> Q, !a2 >> y, !e2 >> $) => [[]](x -> Q, y -> $)",
          ExApplication (ExApplication (ExMeta "e0") (BiTau (AtMeta "a1") (ExMeta "e1"))) (BiTau (AtMeta "a2") (ExMeta "e2")),
          [ ("e0", MvExpression (ExFormation []) defaultScope),
            ("a1", MvAttribute (AtLabel "x")),
            ("e1", MvExpression ExGlobal defaultScope),
            ("a2", MvAttribute (AtLabel "y")),
            ("e2", MvExpression ExThis defaultScope)
          ],
          Just (ExApplication (ExApplication (ExFormation []) (BiTau (AtLabel "x") ExGlobal)) (BiTau (AtLabel "y") ExThis), defaultScope)
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
              defaultScope
            )
        )
      ]

  describe "buildExpressions" $ do
    it "!e => [(!e >> Q.x), (!e >> $.y)] => [Q.x, $.y]" $ do
      built <-
        buildExpressions
          (ExMeta "e")
          [ substSingle "e" (MvExpression (ExDispatch ExGlobal (AtLabel "x")) defaultScope),
            substSingle "e" (MvExpression (ExDispatch ExThis (AtLabel "y")) defaultScope)
          ]
      built `shouldBe` [(ExDispatch ExGlobal (AtLabel "x"), defaultScope), (ExDispatch ExThis (AtLabel "y"), defaultScope)]
    it "!e => [(!e1 >> Q.x)] => X" $
      buildExpressions
        (ExMeta "e")
        [substSingle "e1" (MvExpression (ExDispatch ExGlobal (AtLabel "x")) defaultScope)]
        `shouldThrow` anyException
