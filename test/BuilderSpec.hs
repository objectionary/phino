-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module BuilderSpec where

import Ast
import Builder
import Control.Monad
import Data.Map.Strict qualified as Map
import Matcher
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, describe, it, shouldBe)

test :: (Show a, Eq a) => (a -> Subst -> Maybe a) -> [(String, a, [(String, MetaValue)], Maybe a)] -> SpecWith (Arg Expectation)
test function useCases =
  forM_ useCases $ \(desc, expr, mp, res) ->
    it desc $ function expr (Subst (Map.fromList mp)) `shouldBe` res

spec :: Spec
spec = do
  describe "buildExpression" $ do
    test
      buildExpression
      [ ( "Q.!a => [!a -> x] => Q.x",
          ExDispatch ExGlobal (AtMeta "a"),
          [("a", MvAttribute (AtLabel "x"))],
          Just (ExDispatch ExGlobal (AtLabel "x"))
        ),
        ( "Q.c(!a -> !e) => [!a -> x, !e -> $.y.z] => Q.c(x -> $.y.z)",
          ExApplication (ExDispatch ExGlobal (AtLabel "c")) [BiTau (AtMeta "a") (ExMeta "e")],
          [("a", MvAttribute (AtLabel "x")), ("e", MvExpression (ExDispatch (ExDispatch ExThis (AtLabel "y")) (AtLabel "z")))],
          Just (ExApplication (ExDispatch ExGlobal (AtLabel "c")) [BiTau (AtLabel "x") (ExDispatch (ExDispatch ExThis (AtLabel "y")) (AtLabel "z"))])
        ),
        ( "[!a -> $.x, !B] => [!a -> y, !B -> [b -> ?, L> Func]] => [y -> $.x, b -> ?, L> Func]",
          ExFormation [BiTau (AtMeta "a") (ExDispatch ExThis (AtLabel "x")), BiMeta "B"],
          [("a", MvAttribute (AtLabel "y")), ("B", MvBindings [BiVoid (AtLabel "b"), BiLambda "Func"])],
          Just
            ( ExFormation
                [ BiTau (AtLabel "y") (ExDispatch ExThis (AtLabel "x")),
                  BiVoid (AtLabel "b"),
                  BiLambda "Func"
                ]
            )
        ),
        ( "Q * !t => [!t -> [.a, .b, (~1 -> $.x)]] => Q.a.b(~1 -> $.x)",
          ExMetaTail ExGlobal "t",
          [("t", MvTail [TaDispatch (AtLabel "a"), TaDispatch (AtLabel "b"), TaApplication [BiTau (AtAlpha 1) (ExDispatch ExThis (AtLabel "x"))]])],
          Just (ExApplication (ExDispatch (ExDispatch ExGlobal (AtLabel "a")) (AtLabel "b")) [BiTau (AtAlpha 1) (ExDispatch ExThis (AtLabel "x"))])
        ),
        ( "Q.!a => [] => X",
          ExDispatch ExGlobal (AtMeta "a"),
          [],
          Nothing
        )
      ]

  describe "buildExpressions" $ do
    it "!e => [[!e -> Q.x], [!e -> $.y]] => [Q.x, $.y]" $
      do
        buildExpressions
          (ExMeta "e")
          [ substSingle "e" (MvExpression (ExDispatch ExGlobal (AtLabel "x"))),
            substSingle "e" (MvExpression (ExDispatch ExThis (AtLabel "y")))
          ]
        `shouldBe` [Just (ExDispatch ExGlobal (AtLabel "x")), Just (ExDispatch ExThis (AtLabel "y"))]
