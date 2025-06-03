-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module ReplacerSpec where

import Ast
import Control.Monad (forM_)
import Replacer
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
  describe "replaceProgram: program => ([expression], [expression]) => program" $ do
    test
      replaceProgram
      [ ( "Q -> Q.y.x() => ([Q.y], [$]) => Q -> $.x()",
          Program (ExApplication (ExDispatch (ExDispatch ExGlobal (AtLabel "y")) (AtLabel "x")) []),
          [ExDispatch ExGlobal (AtLabel "y")],
          [ExThis],
          Just (Program (ExApplication (ExDispatch ExThis (AtLabel "x")) []))
        ),
        ( "Q -> [[x -> [[y -> $]], z -> [[w -> $]] ]] => ([[y -> $], [w -> $]], [Q.y, Q.w]) => Q -> [[x -> Q.y, z -> Q.w]]",
          Program
            ( ExFormation
                [ BiTau (AtLabel "x") (ExFormation [BiTau (AtLabel "y") ExThis]),
                  BiTau (AtLabel "z") (ExFormation [BiTau (AtLabel "w") ExThis])
                ]
            ),
          [ExFormation [BiTau (AtLabel "y") ExThis], ExFormation [BiTau (AtLabel "w") ExThis]],
          [ExDispatch ExGlobal (AtLabel "y"), ExDispatch ExGlobal (AtLabel "w")],
          Just
            ( Program
                ( ExFormation
                    [ BiTau (AtLabel "x") (ExDispatch ExGlobal (AtLabel "y")),
                      BiTau (AtLabel "z") (ExDispatch ExGlobal (AtLabel "w"))
                    ]
                )
            )
        ),
        ("Q -> [[]] => ([], [$]) => X", Program (ExFormation []), [], [ExThis], Nothing),
        ( "Q -> [[L> Func, D> 00-]] => ([ [[L> Func, D> 00-]] ], [Q]) => Q -> Q",
          Program (ExFormation [BiLambda "Func", BiDelta "00-"]),
          [ExFormation [BiLambda "Func", BiDelta "00-"]],
          [ExGlobal],
          Just (Program ExGlobal)
        ),
        ("Q -> Q.org.eolang => ([Q.org.eolang, Q.org], [$, $]) => $",
          Program (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")),
          [ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang"), ExDispatch ExGlobal (AtLabel "org")],
          [ExThis, ExThis],
          Just (Program ExThis)
        )
      ]
