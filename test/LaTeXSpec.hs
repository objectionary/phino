{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

{- | Tests for the LaTeX module that provides conversion of phi-calculus
expressions and rules to LaTeX format for academic documents.
-}
module LaTeXSpec where

import AST (Expression (ExMeta))
import Control.Monad (forM_)
import Data.List (intercalate)
import Data.Text qualified as T
import LaTeX (LatexContext (..), conditionToLatex, defaultLatexContext, meetInExpression, meetInExpressions, rewrittensToLatex)
import Lining (LineFormat (MULTILINE))
import Parser (parseExpressionThrows)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldContain)
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

  describe "meets several sub-expressions in a single step" $
    -- A step routinely carries several independent recurring sub-expressions.
    -- The first step here holds two distinct recurring formations
    -- ([[ p -> Q.a ]] and [[ q -> Q.b ]]); both must be factored, so the first
    -- rendered step ends up with two \phinoMeet{}s, not just the single most
    -- frequent one (see #976).
    it "factors every recurring sub-expression, not only one" $ do
      let step :: String -> String
          step lastAttr = "[[ r -> [[ p -> Q.a ]], s -> [[ q -> Q.b ]], tag -> Q." <> lastAttr <> " ]]"
      exprs <- traverse parseExpressionThrows [step "one", step "two", step "three"]
      let ctx = defaultLatexContext{_compress = True, _meetLength = 6, _meetPopularity = 1}
      case meetInExpressions exprs ctx of
        (firstStep : _) -> T.count "ExPhiMeet" (T.pack (show firstStep)) `shouldBe` 2
        [] -> expectationFailure "meetInExpressions returned no expressions"

  describe "indents wrapped continuation steps in a --sequence (#981)" $
    it "nests a wrapped step's members below its two-space \\leadsto line and aligns the closing bracket with it, rather than laying the step out from column 0" $ do
      start <- parseExpressionThrows "[[ x -> Q.y ]]"
      wrapped <- parseExpressionThrows "[[ a -> Q.b, c -> Q.d ]]"
      let ctx = defaultLatexContext{_line = MULTILINE, _margin = 20}
      latex <- rewrittensToLatex ([(start, Just "first"), (wrapped, Just "second")], False) ctx
      latex
        `shouldContain` intercalate
          "\n"
          [ "  \\leadsto [["
          , "    |a| -> Q . |b|,"
          , "    |c| -> Q . |d|"
          , "  ]] \\leadsto_{\\nameref{r:second}}"
          ]

  describe "renders the 'formation' condition" $
    forM_
      [ ("formation", Y.IsFormation (ExMeta "n"), "{ \\phinoIsFormation{ n } }")
      , ("not formation", Y.Not (Y.IsFormation (ExMeta "n")), "{ \\phinoNotFormation{ n } }")
      ]
      (\(desc, cond, expected) -> it desc (conditionToLatex (Just cond) `shouldBe` expected))
