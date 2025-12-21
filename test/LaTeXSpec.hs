-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

{- | Tests for the LaTeX module that provides conversion of phi-calculus
programs and rules to LaTeX format for academic documents.
-}
module LaTeXSpec where

import Control.Monad (forM_)
import LaTeX (meetInProgram)
import Parser (parseExpressionThrows, parseProgramThrows)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "meet program in program" $
    Control.Monad.forM_
      [ ("Q.x.y", "{Q.x.y}", "{[[ x -> Q.x.y ]]}", ["Q.x.y"])
      , ("Q.x.y twice", "{Q.x.y}", "{[[ x -> Q.x.y, y -> Q.x.y.z ]]}", ["Q.x.y", "Q.x.y"])
      , ("Q.x.y.z.a and Q.x.y", "{Q.x.y.z.a}", "{[[ x -> Q.x.y, y -> Q.x.y.z ]]}", ["Q.x.y.z", "Q.x.y", "Q.x.y"])
      , ("Ignore data objects", "{[[ x -> \"foo\" ]]}", "{Q.x( y -> \"foo\" )}", [])
      , ("Not found [[ t -> 42 ]]", "{⟦ ex ↦ ⟦ x ↦ ⟦ t ↦ 42 ⟧.t ⟧.x ⟧}", "{⟦ ex ↦ ⟦ x ↦ 42 ⟧.x ⟧}", [])
      , ("Missed [[ t -> 42 ]]", "{⟦ ex ↦ ⟦ x ↦ ⟦ t ↦ 42 ⟧.t ⟧.x ⟧}", "{⟦ ex ↦ 42 ⟧}", [])
      ]
      ( \(desc, first, second, exprs) -> it desc $ do
          ptn <- parseProgramThrows first
          tgt <- parseProgramThrows second
          res <- traverse parseExpressionThrows exprs
          meetInProgram ptn tgt `shouldBe` res
      )
