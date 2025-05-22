-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module PrinterSpec where

import Ast
import Control.Monad (forM_)
import Prettyprinter
import Printer
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, describe, it, shouldBe)
import Matcher (substEmpty, substSingle, MetaValue (MvExpression, MvAttribute))

test :: (Pretty a) => (a -> String) -> [(String, a)] -> SpecWith (Arg Expectation)
test function useCases =
  forM_ useCases $ \(desc, input) ->
    it desc $ function input `shouldBe` desc

spec :: Spec
spec = do
  describe "printProgram" $
    test
      printProgram
      [ ("Q -> $", Program ExThis),
        ("Q -> [[\n  x -> $.q\n]]", Program (ExFormation [BiTau (AtLabel "x") (ExDispatch ExThis (AtLabel "q"))])),
        ("Q -> Q.org.x", Program (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "x"))),
        ("Q -> [[]]", Program (ExFormation [])),
        ("Q -> [[\n  @ -> ?\n]](\n  ~1 -> Q.x\n)", Program (ExApplication (ExFormation [BiVoid AtPhi]) [BiTau (AtAlpha 1) (ExDispatch ExGlobal (AtLabel "x"))])),
        ("Q -> !e * !t", Program (ExMetaTail (ExMeta "e") "t")),
        ( "Q -> [[\n  D> 00-,\n  L> F,\n  ^ -> ?,\n  !B,\n  @ -> [[\n    y -> ?\n  ]]\n]]",
          Program
            ( ExFormation
                [ BiDelta "00-", BiLambda "F", BiVoid AtRho, BiMeta "B", BiTau AtPhi (ExFormation [BiVoid (AtLabel "y")])]
            )
        )
      ]
  describe "printSubstitution" $
    test
      printSubstitutions
      [ ("[\n  \n]", [substEmpty]),
        ("[\n  !e >> Q.x\n]", [substSingle "e" (MvExpression (ExDispatch ExGlobal (AtLabel "x")))]),
        ("[\n  !a >> x\n]", [substSingle "a" (MvAttribute (AtLabel "x"))])
      ]
