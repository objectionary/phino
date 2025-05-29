-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module PrinterSpec where

import Ast
import Control.Monad (forM_)
import Matcher (MetaValue (MvAttribute, MvExpression), substEmpty, substSingle)
import Parser (parseProgram, parseProgramThrows)
import Prettyprinter
import Printer
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, describe, it, runIO, shouldBe)

test :: (Pretty a) => (a -> String) -> [(String, String, a)] -> SpecWith (Arg Expectation)
test function useCases =
  forM_ useCases $ \(input, output, arg) ->
    it input $ function arg `shouldBe` output

spec :: Spec
spec = do
  describe "print program" $ do
    useCases <-
      runIO $
        mapM
          ( \(input, output) -> do
              prog <- parseProgramThrows input
              return (input, output, prog)
          )
          [ ("Q -> $", "Φ ↦ ξ"),
            ("Q -> Q.org.x", "Φ ↦ Φ.org.x"),
            ("Q -> [[]]", "Φ ↦ ⟦⟧"),
            ("Q -> [[@ -> ?]](~1 -> Q.x)", "Φ ↦ ⟦\n  φ ↦ ∅\n⟧(\n  α1 ↦ Φ.x\n)"),
            ("Q -> !e * !t", "Φ ↦ !e * !t"),
            ( "Q -> [[D> 00-,L> F,^ -> ?,!B,@ -> [[y -> ?]]]]",
              "Φ ↦ ⟦\n  Δ ⤍ 00-,\n  λ ⤍ F,\n  ρ ↦ ∅,\n  !B,\n  φ ↦ ⟦\n    y ↦ ∅\n  ⟧\n⟧"
            )
          ]
    test printProgram useCases

-- describe "printSubstitution" $
--   test
--     printSubstitutions
--     [ ("[\n  (\n    \n  )\n]", [substEmpty]),
--       ("[\n  (\n    !e >> Q.x\n  )\n]", [substSingle "e" (MvExpression (ExDispatch ExGlobal (AtLabel "x")))]),
--       ("[\n  (\n    !a >> x\n  )\n]", [substSingle "a" (MvAttribute (AtLabel "x"))])
--     ]
