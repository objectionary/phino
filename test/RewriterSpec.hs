-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module RewriterSpec where

import Ast
import Control.Monad (forM_)
import Rewriter
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, describe, it, shouldBe)

test ::
  (Program -> [Expression] -> [Expression] -> Maybe Program) ->
  [(String, Program, [Expression], [Expression], Maybe Program)] ->
  SpecWith (Arg Expectation)
test function useCases =
  forM_ useCases $ \(desc, prog, ptns, repls, res) ->
    it desc $ function prog ptns repls `shouldBe` res

spec :: Spec
spec = do
  describe "rewriteProgram" $ do
    test
      rewriteProgram
      [ ( "Q -> Q.y.x() => ([Q.y], [$]) => Q -> $.x()",
          Program (ExApplication (ExDispatch (ExDispatch ExGlobal (AtLabel "y")) (AtLabel "x")) []),
          [ExDispatch ExGlobal (AtLabel "y")],
          [ExThis],
          Just (Program (ExApplication (ExDispatch ExThis (AtLabel "x")) []))
        ),
        ( "Q -> [x -> [y -> $], z -> [w -> $]] => ([[y -> $], [w -> $]], [Q.y, Q.w]) => Q -> [x -> Q.y, z -> Q.w]",
          Program
            ( ExFormation
                [ BiTau (TauBinding (AtLabel "x") (ExFormation [BiTau (TauBinding (AtLabel "y") ExThis)])),
                  BiTau (TauBinding (AtLabel "z") (ExFormation [BiTau (TauBinding (AtLabel "w") ExThis)]))
                ]
            ),
          [ExFormation [BiTau (TauBinding (AtLabel "y") ExThis)], ExFormation [BiTau (TauBinding (AtLabel "w") ExThis)]],
          [ExDispatch ExGlobal (AtLabel "y"), ExDispatch ExGlobal (AtLabel "w")],
          Just
            ( Program
                ( ExFormation
                    [ BiTau (TauBinding (AtLabel "x") (ExDispatch ExGlobal (AtLabel "y"))),
                      BiTau (TauBinding (AtLabel "z") (ExDispatch ExGlobal (AtLabel "w")))
                    ]
                )
            )
        ),
        ("Q -> [] => ([], [$]) => X", Program $ ExFormation [], [], [ExThis], Nothing)
      ]
