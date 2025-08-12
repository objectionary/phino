-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module PrettySpec where

import Ast
import Control.Monad (forM_)
import Matcher (MetaValue (MvAttribute, MvExpression), substEmpty, substSingle, defaultScope)
import Parser (parseProgramThrows)
import Pretty
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, describe, it, runIO, shouldBe)

test :: (a -> PrintMode -> String) -> [(String, String, a, PrintMode)] -> SpecWith (Arg Expectation)
test function useCases =
  forM_ useCases $ \(input, output, arg, mode) ->
    it input $ function arg mode `shouldBe` output

prep :: PrintMode -> (String, String) -> IO (String, String, Program, PrintMode)
prep mode (input, output) = do
  prog <- parseProgramThrows input
  return (input, output, prog, mode)

spec :: Spec
spec = do
  describe "saltify program" $ do
    useCases <-
      runIO $
        mapM
          (prep SALTY)
          [ ("Q -> $", "Φ ↦ ξ"),
            ("Q -> Q.org.x", "Φ ↦ Φ.org.x"),
            ("Q -> [[]]", "Φ ↦ ⟦ ρ ↦ ∅ ⟧"),
            ("Q -> [[@ -> ?]](~1 -> Q.x)", "Φ ↦ ⟦\n  φ ↦ ∅,\n  ρ ↦ ∅\n⟧(\n  α1 ↦ Φ.x\n)"),
            ("Q -> !e * !t", "Φ ↦ !e * !t"),
            ( "Q -> [[D> 00-,L> F,^ -> ?,!B,@ -> [[y -> ?]]]]",
              "Φ ↦ ⟦\n  Δ ⤍ 00-,\n  λ ⤍ F,\n  ρ ↦ ∅,\n  !B,\n  φ ↦ ⟦\n    y ↦ ∅,\n    ρ ↦ ∅\n  ⟧\n⟧"
            )
          ]
    test prettyProgram' useCases

  describe "sweetify program" $ do
    useCases <-
      runIO $
        mapM
          (prep SWEET)
          [ ("Q -> $", "{ξ}"),
            ("Q -> Q.org.eolang(x -> Q.x)", "{Φ̇(\n  x ↦ Φ.x\n)}"),
            ("Q -> [[ x -> [[ y -> ?, z -> ? ]] ]]", "{⟦\n  x(y, z) ↦ ⟦⟧\n⟧}"),
            ("Q -> 5", "{5}"),
            ("Q -> [[ x -> \"hello\"]]", "{⟦\n  x ↦ \"hello\"\n⟧}"),
            ("Q -> [[ x -> \"hello\", y -> 5]]", "{⟦\n  x ↦ \"hello\",\n  y ↦ 5\n⟧}"),
            ("Q -> Q.x(x -> 1)(y -> 2)(z -> 3)", "{Φ.x(\n  x ↦ 1,\n  y ↦ 2,\n  z ↦ 3\n)}"),
            ("Q -> Q.x(~0 -> Q.y)", "{Φ.x(\n  Φ.y\n)}"),
            ("Q -> Q.x(~0 -> 1)(~1 -> 2)(~2 -> 3)", "{Φ.x(\n  1,\n  2,\n  3\n)}"),
            ("Q -> Q.x(~0 -> 1)(~2 -> 2)(~1 -> 3)", "{Φ.x(\n  α0 ↦ 1,\n  α2 ↦ 2,\n  α1 ↦ 3\n)}"),
            ("Q -> Φ.jeo.opcode.ldc(18, \"Reading \\\"\")", "{Φ.jeo.opcode.ldc(\n  18,\n  \"Reading \\\"\"\n)}"),
            ("Q -> [[ k -> [[]] ]]", "{⟦\n  k ↦ ⟦⟧\n⟧}"),
            ("Q -> [[ ]]", "{⟦⟧}")
          ]
    test prettyProgram' useCases

  describe "prettify substitution" $ do
    let useCases =
          map
            (\(desc, output, substs) -> (desc, output, substs, SALTY))
            [ ("[()]", "[\n  (\n    \n  )\n]", [substEmpty]),
              ("[(!e >> Q.x)]", "[\n  (\n    !e >> Φ.x\n  )\n]", [substSingle "e" (MvExpression (ExDispatch ExGlobal (AtLabel "x")) defaultScope)]),
              ("[(!a >> x)]", "[\n  (\n    !a >> x\n  )\n]", [substSingle "a" (MvAttribute (AtLabel "x"))])
            ]
    test prettySubsts' useCases
