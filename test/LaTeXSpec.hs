{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

{- | Tests for the LaTeX module that provides conversion of phi-calculus
expressions and rules to LaTeX format for academic documents.
-}
module LaTeXSpec where

import AST (Attribute (AtLabel, AtPhi, AtRho), Binding (BiTau, BiVoid), Bytes (BtOne), Expression (ExFormation, ExMeta, ExPhiAgain, ExPhiMeet, ExRoot))
import Control.Monad (forM_)
import Data.List (intercalate)
import Data.Text qualified as T
import LaTeX
  ( LatexContext (..)
  , conditionToLatex
  , defaultLatexContext
  , explainContextualizeRules
  , explainDataizeRules
  , explainMorphRules
  , explainRules
  , expressionToLaTeX
  , meetInExpression
  , meetInExpressions
  , rewrittensToLatex
  )
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
      , ("empty (And [])", Y.And [], "{ }")
      , ("empty (Or [])", Y.Or [], "{ }")
      , ("normal form", Y.NF (ExMeta "n"), "{ \\isnormal{ n } }")
      , ("matches", Y.Matches "abc" (ExMeta "n"), "{ matches\\lparen abc, n \\rparen }")
      , ("part-of", Y.PartOf (ExMeta "n") (BiVoid AtRho), "{ part-of\\lparen n, \\phiTerminal{\\rho} -> ? \\rparen }")
      , ("compare equal", Y.Eq (Y.CmpAttr AtRho) (Y.CmpAttr AtPhi), "{ \\phiTerminal{\\rho} = @ }")
      , ("compare greater", Y.Gt (Y.CmpNum (Y.Literal 3)) (Y.CmpNum (Y.Literal 4)), "{ 3 > 4 }")
      , ("not normal form", Y.Not (Y.NF (ExMeta "n")), "{ not\\lparen \\isnormal{ n } \\rparen }")
      , ("disjoint", Y.Disjoint [AtRho] [BiVoid AtRho], "{ [ \\phiTerminal{\\rho} ] \\cap \\phiTerminal{\\rho} -> ? = \\emptyset }")
      , ("absolute", Y.Absolute (ExMeta "n"), "{ \\phinoAbsolute{ n } }")
      ]
      (\(desc, cond, expected) -> it desc (conditionToLatex (Just cond) `shouldBe` expected))

  describe "expressionToLaTeX" $ do
    it "renders '\\phiquation*' (unnumbered) when '_nonumber' is set" $ do
      expr <- parseExpressionThrows "[[ x -> Q.y ]]"
      expressionToLaTeX expr defaultLatexContext{_nonumber = True}
        `shouldBe` "\\begin{phiquation*}\n[[ |x| -> Q . |y| ]]{.}\n\\end{phiquation*}"

    it "renders a '\\label{}' when '_label' is set" $ do
      expr <- parseExpressionThrows "[[ x -> Q.y ]]"
      expressionToLaTeX expr defaultLatexContext{_label = Just "eq:one"}
        `shouldBe` "\\begin{phiquation}\n\\label{eq:one}\n[[ |x| -> Q . |y| ]]{.}\n\\end{phiquation}"

    it "renders a '\\phiExpression{}' prefix when '_expression' is set" $ do
      expr <- parseExpressionThrows "[[ x -> Q.y ]]"
      expressionToLaTeX expr defaultLatexContext{_expression = Just "e"}
        `shouldBe` "\\begin{phiquation}\n\\phiExpression{e} [[ |x| -> Q . |y| ]]{.}\n\\end{phiquation}"

    it "escapes '@' and '^' in an attribute label, same as '$' and '_'" $ do
      let weird = ExFormation [BiTau (AtLabel "a@b^c") ExRoot, BiVoid AtRho]
      expressionToLaTeX weird defaultLatexContext
        `shouldBe` "\\begin{phiquation}\n[[ |a\\char64{}b\\char94{}c| -> Q ]]{.}\n\\end{phiquation}"

    it "renders a \\phinoMeet{} marker with its prefix" $ do
      inner <- parseExpressionThrows "Q.y"
      expressionToLaTeX (ExPhiMeet (Just "pfx") 2 inner) defaultLatexContext
        `shouldBe` "\\begin{phiquation}\n\\phinoMeet{pfx:2}{ Q . |y| }{.}\n\\end{phiquation}"

    it "renders a \\phinoAgain{} marker without a prefix" $ do
      inner <- parseExpressionThrows "Q.y"
      expressionToLaTeX (ExPhiAgain Nothing 3 inner) defaultLatexContext
        `shouldBe` "\\begin{phiquation}\n\\phinoAgain{3}{.}\n\\end{phiquation}"

  describe "rewrittensToLatex" $ do
    it "renders the ellipsis ending when the chain exceeded its bound" $ do
      step1 <- parseExpressionThrows "[[ x -> Q.y ]]"
      latex <- rewrittensToLatex ([(step1, Nothing)], True) defaultLatexContext
      latex `shouldBe` "\\begin{phiquation}\n[[ |x| -> Q . |y| ]] \\leadsto\n  \\leadsto \\dots\n\\end{phiquation}"

    it "prefixes each step with a '% === Step' header when '_headers' is set" $ do
      step1 <- parseExpressionThrows "[[ x -> Q.y ]]"
      step2 <- parseExpressionThrows "[[ x -> Q.z ]]"
      latex <- rewrittensToLatex ([(step1, Nothing), (step2, Just "myrule")], False) defaultLatexContext{_headers = True}
      latex
        `shouldBe` intercalate
          "\n"
          [ "\\begin{phiquation}"
          , "% === Step #1"
          , "[[ |x| -> Q . |y| ]]"
          , "% === Step #2, Rule '?', 11t -> 11t"
          , "  \\leadsto [[ |x| -> Q . |z| ]] \\leadsto_{\\nameref{r:myrule}}{.}"
          , "\\end{phiquation}"
          ]

    it "locates the focused sub-expression at every step" $ do
      step1 <- parseExpressionThrows "[[ x -> Q.aaa.bbb.ccc.ddd ]]"
      step2 <- parseExpressionThrows "[[ x -> Q.aaa.bbb.ccc.ddd.eee ]]"
      focus <- parseExpressionThrows "Q.x"
      latex <- rewrittensToLatex ([(step1, Nothing), (step2, Just "r")], False) defaultLatexContext{_focus = focus}
      latex
        `shouldBe` intercalate
          "\n"
          [ "\\begin{phiquation}"
          , "Q . |aaa| . |bbb| . |ccc| . |ddd|"
          , "  \\leadsto Q . |aaa| . |bbb| . |ccc| . |ddd| . |eee| \\leadsto_{\\nameref{r:r}}{.}"
          , "\\end{phiquation}"
          ]

    it "compresses and canonizes a --sequence of full expressions above the default meet threshold" $ do
      step1 <- parseExpressionThrows "[[ x -> Q.a.b.c.d ]]"
      step2 <- parseExpressionThrows "[[ y -> Q.a.b.c.d ]]"
      step3 <- parseExpressionThrows "[[ z -> Q.a.b.c.d ]]"
      latex <-
        rewrittensToLatex
          ([(step1, Nothing), (step2, Just "r1"), (step3, Just "r2")], False)
          defaultLatexContext{_compress = True, _canonize = True}
      latex
        `shouldBe` intercalate
          "\n"
          [ "\\begin{phiquation}"
          , "[[ |x| -> \\phinoMeet{1}{ Q . |a| . |b| . |c| . |d| } ]]"
          , "  \\leadsto [[ |y| -> \\phinoAgain{1} ]] \\leadsto_{\\nameref{r:r1}}"
          , "  \\leadsto [[ |z| -> \\phinoAgain{1} ]] \\leadsto_{\\nameref{r:r2}}{.}"
          , "\\end{phiquation}"
          ]

    it "compresses and canonizes a --sequence of focused sub-expressions above the default meet threshold" $ do
      focus <- parseExpressionThrows "Q.x"
      step1 <- parseExpressionThrows "[[ x -> [[ w -> Q.a.b.c.d ]] ]]"
      step2 <- parseExpressionThrows "[[ x -> [[ w -> Q.a.b.c.d ]] ]]"
      step3 <- parseExpressionThrows "[[ x -> [[ w -> Q.a.b.c.d ]] ]]"
      latex <-
        rewrittensToLatex
          ([(step1, Nothing), (step2, Just "r1"), (step3, Just "r2")], False)
          defaultLatexContext{_focus = focus, _compress = True, _canonize = True}
      latex
        `shouldBe` intercalate
          "\n"
          [ "\\begin{phiquation}"
          , "\\phinoMeet{1}{ [[ |w| -> Q . |a| . |b| . |c| . |d| ]] }"
          , "  \\leadsto \\phinoAgain{1} \\leadsto_{\\nameref{r:r1}}"
          , "  \\leadsto \\phinoAgain{1} \\leadsto_{\\nameref{r:r2}}{.}"
          , "\\end{phiquation}"
          ]

  describe "meetInExpressions" $ do
    it "returns an empty list for an empty sequence" $
      meetInExpressions [] defaultLatexContext `shouldBe` []

    it "finds a meet using the default meet length and popularity, without overriding them" $ do
      step1 <- parseExpressionThrows "[[ x -> Q.a.b.c.d ]]"
      step2 <- parseExpressionThrows "[[ y -> Q.a.b.c.d ]]"
      step3 <- parseExpressionThrows "[[ z -> Q.a.b.c.d ]]"
      let compressed = meetInExpressions [step1, step2, step3] defaultLatexContext{_compress = True}
      length compressed `shouldBe` 3
      case compressed of
        (firstStep : _) -> T.count "ExPhiMeet" (T.pack (show firstStep)) `shouldBe` 1
        [] -> expectationFailure "meetInExpressions returned no expressions"

  describe "explainRules" $ do
    it "renders a rule's label, combined 'when'/'having' condition and 'where' extras" $ do
      let rule =
            Y.Rule
              { name = "myrule"
              , label = Just "disp"
              , description = Nothing
              , pattern = ExMeta "n"
              , result = ExMeta "n"
              , when = Just (Y.NF (ExMeta "n"))
              , having = Just (Y.IsFormation (ExMeta "n"))
              , where_ =
                  Just
                    [ Y.Extra
                        { meta = Y.ArgAttribute AtRho
                        , function = "foo"
                        , args = [Y.ArgExpression (ExMeta "n"), Y.ArgBinding (BiVoid AtRho), Y.ArgBytes (BtOne "01-02")]
                        }
                    , Y.Extra
                        { meta = Y.ArgAttribute AtPhi
                        , function = "bar"
                        , args = [Y.ArgExpression (ExMeta "n")]
                        }
                    ]
              }
      explainRules [rule]
        `shouldBe` intercalate
          "\n  "
          [ "\\phinoNormalizationRule[disp]{myrule}"
          , "{ n }"
          , "{ n }"
          , "{ \\isnormal{ n } \\;\\text{and}\\; \\phinoIsFormation{ n } }"
          , "{ \\phiTerminal{\\rho} \\coloneqq \\foo{ n, \\phiTerminal{\\rho} -> ?, 01-02- } and @ \\coloneqq \\bar{ n } }"
          ]

    it "omits the label and the 'if'/'where' clauses when they are absent" $ do
      let rule =
            Y.Rule
              { name = "myrule2"
              , label = Nothing
              , description = Nothing
              , pattern = ExMeta "n"
              , result = ExMeta "n"
              , when = Nothing
              , having = Nothing
              , where_ = Nothing
              }
      explainRules [rule]
        `shouldBe` intercalate
          "\n  "
          [ "\\phinoNormalizationRule{myrule2}"
          , "{ n }"
          , "{ n }"
          , "{ }"
          , "{ }"
          ]

    it "keeps just the 'having' condition when 'when' is absent" $ do
      let rule =
            Y.Rule
              { name = "myrule3"
              , label = Nothing
              , description = Nothing
              , pattern = ExMeta "n"
              , result = ExMeta "n"
              , when = Nothing
              , having = Just (Y.IsFormation (ExMeta "n"))
              , where_ = Nothing
              }
      explainRules [rule]
        `shouldBe` intercalate
          "\n  "
          [ "\\phinoNormalizationRule{myrule3}"
          , "{ n }"
          , "{ n }"
          , "{ \\phinoIsFormation{ n } }"
          , "{ }"
          ]

  describe "explainMorphRules" $
    it "threads the state through every premise operation (morph, normalize, evaluate, contextualize, dataize)" $ do
      let rule =
            Y.MorphRule
              { name = "morph1"
              , label = Just "mlbl"
              , match = ExMeta "n"
              , ematch = ExMeta "e"
              , nresult = ExMeta "n1"
              , when = Just (Y.NF (ExMeta "n"))
              , premises =
                  [ Y.Premise{result = "n1", operation = Y.OpMorph (ExMeta "n")}
                  , Y.Premise{result = "n2", operation = Y.OpNormalize (ExMeta "n1")}
                  , Y.Premise{result = "n3", operation = Y.OpEvaluate (ExMeta "n2") (ExMeta "e")}
                  , Y.Premise{result = "n4", operation = Y.OpContextualize (ExMeta "n3") (ExMeta "e")}
                  , Y.Premise{result = "n5", operation = Y.OpDataize (ExMeta "n4")}
                  ]
              }
      explainMorphRules [rule]
        `shouldBe` intercalate
          "\n"
          [ "\\begin{phinoMorphingInference}"
          , "  \\phinoName{morph1}"
          , "  \\phinoLabel{mlbl}"
          , "  \\phinoCondition{ \\isnormal{ n } }"
          , "  \\phinoPremise{ \\phinoMorph{ n }{ e }{ s_1 }{ n_1 }{ s_2 } }"
          , "  \\phinoPremise{ \\phinoNormalize{ n_1 }{ n_2 } }"
          , "  \\phinoPremise{ \\phinoEvaluate{ n_2 }{ e }{ s_2 }{ n_3 }{ s_3 } }"
          , "  \\phinoPremise{ \\phinoContextualize{ n_3 }{ e }{ n_4 } }"
          , "  \\phinoPremise{ \\phinoDataize{ n_4 }{ e }{ s_3 }{ \\delta_5 }{ s_4 } }"
          , "  \\phinoConclusion{ \\phinoMorph{ n }{ e }{ s_1 }{ n_1 }{ s_4 } }"
          , "\\end{phinoMorphingInference}"
          ]

  describe "explainDataizeRules" $
    it "renders a bare 's' in the conclusion and omits an empty 'when' condition" $ do
      let rule =
            Y.DataizeRule
              { name = "dataize1"
              , label = Nothing
              , match = ExMeta "n"
              , ematch = ExMeta "e"
              , dresult = BtOne "05"
              , when = Just (Y.And [])
              , premises = []
              }
      explainDataizeRules [rule]
        `shouldBe` intercalate
          "\n"
          [ "\\begin{phinoDataizationInference}"
          , "  \\phinoName{dataize1}"
          , "  \\phinoConclusion{ \\phinoDataize{ n }{ e }{ s }{ |05-| }{ s } }"
          , "\\end{phinoDataizationInference}"
          ]

  describe "explainContextualizeRules" $
    it "threads a morph premise through the rule's own 'e' universe" $ do
      let rule =
            Y.ContextualizeRule
              { name = "ctx1"
              , label = Nothing
              , match = ExMeta "n"
              , cmatch = ExMeta "c"
              , cresult = ExMeta "n1"
              , premises = [Y.Premise{result = "n1", operation = Y.OpMorph (ExMeta "n")}]
              }
      explainContextualizeRules [rule]
        `shouldBe` intercalate
          "\n"
          [ "\\begin{phinoContextualizationInference}"
          , "  \\phinoName{ctx1}"
          , "  \\phinoPremise{ \\phinoMorph{ n }{ e }{ s_1 }{ n_1 }{ s_2 } }"
          , "  \\phinoConclusion{ \\phinoContextualize{ n }{ e }{ n_1 } }"
          , "\\end{phinoContextualizationInference}"
          ]
