{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

{- | Tests for the LaTeX module that provides conversion of phi-calculus
expressions and rules to LaTeX format for academic documents.
-}
module LaTeXSpec where

import AST (Expression (ExMeta))
import Control.Monad (forM_)
import LaTeX (conditionToLatex, meetInExpression)
import Parser (parseExpressionThrows)
import Test.Hspec (Spec, describe, it, shouldBe)
import Yaml qualified as Y

spec :: Spec
spec = do
  describe "meet expression in expression" $
    forM_
      [ ("Q.x.y", "Q.x.y", "[[ x -> Q.x.y ]]", ["Q.x.y"])
      , ("Q.x.y twice", "Q.x.y", "[[ x -> Q.x.y, y -> Q.x.y.z ]]", ["Q.x.y", "Q.x.y"])
      , ("Q.x.y.z.a and Q.x.y", "Q.x.y.z.a", "[[ x -> Q.x.y, y -> Q.x.y.z ]]", ["Q.x.y.z", "Q.x.y", "Q.x.y"])
      , ("Ignore data objects", "[[ x -> \"foo\" ]]", "Q.x( y -> \"foo\" )", [])
      , ("Not found [[ t -> 42 ]]", "⟦ ex ↦ ⟦ x ↦ ⟦ t ↦ 42 ⟧.t ⟧.x ⟧", "⟦ ex ↦ ⟦ x ↦ 42 ⟧.x ⟧", [])
      , ("Missed [[ t -> 42 ]]", "⟦ ex ↦ ⟦ x ↦ ⟦ t ↦ 42 ⟧.t ⟧.x ⟧", "⟦ ex ↦ 42 ⟧", [])
      ]
      ( \(desc, first, second, exprs) -> it desc $ do
          ptn <- parseExpressionThrows first
          tgt <- parseExpressionThrows second
          res <- traverse parseExpressionThrows exprs
          meetInExpression ptn 4 tgt `shouldBe` res
      )

  describe "renders the 'formation' condition" $
    forM_
      [ ("formation", Y.IsFormation (ExMeta "n"), "{ \\phinoIsFormation{ n } }")
      , ("not formation", Y.Not (Y.IsFormation (ExMeta "n")), "{ \\phinoNotFormation{ n } }")
      ]
      (\(desc, cond, expected) -> it desc (conditionToLatex (Just cond) `shouldBe` expected))
