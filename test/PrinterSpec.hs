{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

{- | Tests for the Printer module that converts AST to string representation.
The module provides functions to print phi-calculus expressions with
various configurations for sugar, encoding, and line format.
-}
module PrinterSpec where

import AST
import Control.Monad (forM_)
import Data.Map.Strict qualified as Map
import Encoding (Encoding (..))
import Lining (LineFormat (..))
import Margin (defaultMargin)
import Matcher (MetaValue (..), Subst (Subst))
import Parser (parseExpression)
import Printer
import Sugar (SugarType (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain)
import Yaml (ExtraArgument (..))

spec :: Spec
spec = do
  describe "printExpression with ASCII singleline renders primitives" $
    forM_
      [ ("ξ renders as $", ExXi, "$")
      , ("Φ renders as Q", ExRoot, "Q")
      , ("⊥ renders as T", ExTermination, "T")
      , ("ρ void becomes empty", ExFormation [BiVoid AtRho], "[[]]")
      , ("φ void", ExFormation [BiVoid AtPhi], "[[ @ -> ? ]]")
      , ("label void", ExFormation [BiVoid (AtLabel "名前")], "[[ 名前 -> ? ]]")
      , ("x to Φ", ExFormation [BiTau (AtLabel "x") ExRoot], "[[ x -> Q ]]")
      , ("ρ to ⊥", ExFormation [BiTau AtRho ExTermination], "[[ ^ -> T ]]")
      , ("empty delta", ExFormation [BiDelta BtEmpty], "[[ D> -- ]]")
      , ("single byte", ExFormation [BiDelta (BtOne "1F")], "[[ D> 1F- ]]")
      , ("multiple bytes", ExFormation [BiDelta (BtMany ["00", "01", "02"])], "[[ D> 00-01-02 ]]")
      , ("función lambda", ExFormation [BiLambda (Function "Función")], "[[ L> Función ]]")
      , ("クラス lambda", ExFormation [BiLambda (Function "クラス")], "[[ L> クラス ]]")
      , ("Φ.org", ExDispatch ExRoot (AtLabel "org"), "Q.org")
      , ("ξ.ρ as sugar", ExDispatch ExXi AtRho, "^")
      , ("ξ.φ as sugar", ExDispatch ExXi AtPhi, "@")
      , ("chained dispatch", ExDispatch (ExDispatch ExRoot (AtLabel "org")) (AtLabel "éolang"), "Q.org.éolang")
      ,
        ( "dispatch with app"
        , ExApplication (ExDispatch ExRoot (AtLabel "x")) (ArTau (AtLabel "y") ExXi)
        , "Q.x( y -> $ )"
        )
      ,
        ( "formation with app"
        , ExApplication (ExFormation [BiVoid AtRho]) (ArAlpha (Alpha 0) ExRoot)
        , "[[]]( Q )"
        )
      , ("meta expr", ExMeta "e", "!e")
      , ("meta binding", ExFormation [BiMeta "B"], "[[ !B ]]")
      , ("meta lambda", ExFormation [BiLambda (FnMeta "F")], "[[ L> !F ]]")
      , ("meta attr tau", ExFormation [BiTau (AtMeta "t") ExXi], "[[ !t -> $ ]]")
      ]
      ( \(desc, expr, expected) ->
          it desc (printExpression' expr (SWEET, ASCII, SINGLELINE, defaultMargin) `shouldBe` expected)
      )

  describe "printExpression with SWEET UNICODE renders the pretty function meta" $
    it "meta lambda becomes 𝑓" $
      printExpression' (ExFormation [BiLambda (FnMeta "F")]) (SWEET, UNICODE, SINGLELINE, defaultMargin) `shouldBe` "⟦ λ ⤍ 𝑓 ⟧"

  describe "printExpression keeps special double values in byte form so they re-parse" $
    forM_
      [ ("NaN", BtMany ["7F", "F8", "00", "00", "00", "00", "00", "00"])
      , ("positive infinity", BtMany ["7F", "F0", "00", "00", "00", "00", "00", "00"])
      , ("negative infinity", BtMany ["FF", "F0", "00", "00", "00", "00", "00", "00"])
      ]
      ( \(desc, bts) ->
          it desc $ do
            let expr = DataNumber bts
                printed = printExpression' expr (SWEET, ASCII, SINGLELINE, defaultMargin)
            -- rendered as Q.number( Q.bytes( [[ D> .. ]] ) ), not a bare literal
            printed `shouldContain` "number"
            printed `shouldContain` "bytes"
            parseExpression printed `shouldBe` Right expr
      )

  describe "printExpression keeps a compressed meet atomic under a narrow margin" $
    -- A \phinoMeet is a single \overbracket visual unit, so its body must stay
    -- on one line even when the surrounding margin forces the outer formation to
    -- wrap. A newline inside the braced argument would raise "! Missing }
    -- inserted" in an aligned/gathered LaTeX context (see #978).
    it "renders the meet body on a single line even when the margin wraps its parent" $ do
      let body = ExFormation [BiTau (AtLabel "alpha") ExRoot, BiTau (AtLabel "beta") ExRoot, BiTau (AtLabel "gamma") ExRoot]
          expr = ExFormation [BiTau (AtLabel "x") (ExPhiMeet Nothing 5 body)]
          printed = printExpression' expr (SWEET, UNICODE, MULTILINE, 30)
      printed `shouldContain` "\\phinoMeet{5}{ ⟦ alpha ↦ Φ, beta ↦ Φ, gamma ↦ Φ ⟧ }"

  describe "printExpression with default config" $
    forM_
      [ ("empty formation", ExFormation [BiVoid AtRho], "⟦⟧")
      , ("dispatch", ExDispatch ExRoot (AtLabel "org"), "Φ.org")
      ]
      ( \(desc, expr, expected) ->
          it desc (printExpression expr `shouldBe` expected)
      )

  describe "printExpression in salty does not inject a duplicate void rho when rho is already present" $
    forM_
      [
        ( "rho bound to an empty formation"
        , ExFormation [BiTau AtRho (ExFormation [BiVoid AtRho])]
        , "⟦ ρ ↦ ⟦ ρ ↦ ∅ ⟧ ⟧"
        )
      ,
        ( "rho bound to a non empty formation"
        , ExFormation [BiTau AtRho (ExFormation [BiVoid (AtLabel "名前"), BiVoid AtRho])]
        , "⟦ ρ ↦ ⟦ 名前 ↦ ∅, ρ ↦ ∅ ⟧ ⟧"
        )
      ,
        ( "rho binding placed after another binding"
        , ExFormation [BiTau (AtLabel "café") ExRoot, BiTau AtRho (ExFormation [BiVoid AtRho])]
        , "⟦ café ↦ Φ, ρ ↦ ⟦ ρ ↦ ∅ ⟧ ⟧"
        )
      ]
      ( \(desc, expr, expected) ->
          it desc (printExpression' expr (SALTY, UNICODE, SINGLELINE, defaultMargin) `shouldBe` expected)
      )

  describe "printExpressionHidingRho strips every rho binding for --hide-rho" $ do
    let issueExpr =
          ExFormation
            [ BiTau
                (AtLabel "foo")
                ( ExFormation
                    [ BiTau (AtLabel "x") (ExFormation [BiVoid AtRho])
                    , BiTau AtRho (ExDispatch ExXi (AtLabel "y"))
                    ]
                )
            , BiTau (AtLabel "y") (ExFormation [BiVoid AtRho])
            , BiVoid AtRho
            ]
    forM_
      [ ("salty clears both void and dispatch-valued rho", SALTY, issueExpr, "⟦ foo ↦ ⟦ x ↦ ⟦⟧ ⟧, y ↦ ⟦⟧ ⟧")
      , ("sweet also drops the rho that --sweet keeps", SWEET, issueExpr, "⟦ foo ↦ ⟦ x ↦ ⟦⟧ ⟧, y ↦ ⟦⟧ ⟧")
      ,
        ( "a rho bound to an expression is removed"
        , SWEET
        , ExFormation [BiTau (AtLabel "a") ExRoot, BiTau AtRho (ExDispatch ExXi (AtLabel "y"))]
        , "⟦ a ↦ Φ ⟧"
        )
      , ("a formation holding only rho collapses to empty", SALTY, ExFormation [BiVoid AtRho], "⟦⟧")
      ,
        ( "a ξ.ρ dispatch value is left untouched"
        , SALTY
        , ExFormation [BiTau (AtLabel "a") (ExDispatch ExXi AtRho)]
        , "⟦ a ↦ ξ.ρ ⟧"
        )
      ,
        ( "an application whose only argument is rho collapses to its callee"
        , SWEET
        , ExApplication (ExDispatch ExRoot (AtLabel "x")) (ArTau AtRho (ExFormation [BiVoid AtRho]))
        , "Φ.x"
        )
      ,
        ( "the same collapse happens on the salty path, without leftover parens"
        , SALTY
        , ExApplication (ExDispatch ExRoot (AtLabel "x")) (ArTau AtRho (ExFormation [BiVoid AtRho]))
        , "Φ.x"
        )
      ,
        ( "an application keeps its other arguments and drops only rho"
        , SWEET
        , ExApplication (ExApplication (ExDispatch ExRoot (AtLabel "e")) (ArTau (AtLabel "a") ExRoot)) (ArTau AtRho ExXi)
        , "Φ.e( a ↦ Φ )"
        )
      ]
      ( \(desc, sugar, expr, expected) ->
          it desc (printExpressionHidingRho' expr (sugar, UNICODE, SINGLELINE, defaultMargin) `shouldBe` expected)
      )

  describe "printExpressionHidingRho keeps primitives that carry no visible rho" $
    it "renders a sweet numeric literal exactly as printExpression' does" $ do
      let number = DataNumber (BtMany ["40", "45", "00", "00", "00", "00", "00", "00"])
          config = (SWEET, UNICODE, SINGLELINE, defaultMargin)
      printExpressionHidingRho' number config `shouldBe` printExpression' number config

  describe "printAttribute with default encoding" $
    forM_
      [ ("label", AtLabel "attr", "attr")
      , ("ρ", AtRho, "ρ")
      , ("φ", AtPhi, "φ")
      , ("λ", AtLambda, "λ")
      , ("Δ", AtDelta, "Δ")
      , ("meta", AtMeta "t", "𝜏")
      ]
      ( \(desc, attr, expected) ->
          it desc (printAttribute attr `shouldBe` expected)
      )

  describe "printAlpha with default encoding" $
    forM_
      [ ("α42", Alpha 42, "α42")
      , ("meta alpha", AlMeta "i", "α𝑖")
      ]
      (\(desc, alpha, expected) -> it desc (printAlpha alpha `shouldBe` expected))

  describe "printBinding renders as formation" $
    forM_
      [ ("tau binding", BiTau (AtLabel "x") ExRoot, "x ↦ Φ")
      , ("void binding", BiVoid (AtLabel "y"), "y ↦ ∅")
      , ("delta binding", BiDelta (BtOne "00"), "Δ ⤍ 00-")
      , ("lambda binding", BiLambda (Function "Func"), "λ ⤍ Func")
      , ("meta binding", BiMeta "B", "𝐵")
      ]
      ( \(desc, bd, expected) ->
          it desc (printBinding bd `shouldContain` expected)
      )

  describe "printBytes renders bytes" $
    forM_
      [ ("empty bytes", BtEmpty, "--")
      , ("single byte", BtOne "1F", "1F-")
      , ("multiple bytes", BtMany ["00", "01", "02"], "00-01-02")
      , ("meta bytes", BtMeta "D", "δ")
      ]
      ( \(desc, bts, expected) ->
          it desc (printBytes bts `shouldBe` expected)
      )

  describe "printExtraArg renders arguments" $
    forM_
      [ ("attribute arg", ArgAttribute (AtLabel "tëst"), "tëst")
      , ("binding arg", ArgBinding (BiVoid (AtLabel "βind")), "βind ↦ ∅")
      , ("expression arg", ArgExpression ExRoot, "Φ")
      , ("bytes arg", ArgBytes (BtOne "FF"), "FF-")
      ]
      ( \(desc, arg, expected) ->
          it desc (printExtraArg arg `shouldContain` expected)
      )

  describe "printSubsts and printSubsts' render substitutions" $
    forM_
      [ ("MvAttribute", [Subst (Map.singleton "t" (MvAttribute (AtLabel "x")))], (SWEET, UNICODE, MULTILINE, defaultMargin), "t >> x")
      , ("MvIndex", [Subst (Map.singleton "i" (MvIndex 3))], (SWEET, UNICODE, MULTILINE, defaultMargin), "i >> 3")
      , ("MvExpression", [Subst (Map.singleton "e" (MvExpression ExRoot))], (SWEET, UNICODE, MULTILINE, defaultMargin), "e >> Φ")
      , ("MvBytes", [Subst (Map.singleton "b" (MvBytes (BtOne "1F")))], (SWEET, UNICODE, MULTILINE, defaultMargin), "b >> 1F-")
      , ("MvBindings", [Subst (Map.singleton "bnd" (MvBindings [BiVoid (AtLabel "y")]))], (SWEET, UNICODE, MULTILINE, defaultMargin), "bnd >> ⟦ y ↦ ∅ ⟧")
      , ("MvFunction", [Subst (Map.singleton "f" (MvFunction "func"))], (SWEET, UNICODE, MULTILINE, defaultMargin), "f >> func")
      ,
        ( "keys of a multi-entry substitution are sorted and each is on its own line"
        , [Subst (Map.fromList [("a", MvIndex 1), ("b", MvIndex 2)])]
        , (SWEET, UNICODE, MULTILINE, defaultMargin)
        , "a >> 1\nb >> 2"
        )
      ,
        ( "multiple substitutions are separated with a dashed line"
        , [Subst (Map.singleton "a" (MvIndex 1)), Subst (Map.singleton "b" (MvIndex 2))]
        , (SWEET, UNICODE, MULTILINE, defaultMargin)
        , "a >> 1\n------\nb >> 2"
        )
      , ("an empty substitution list renders the dashed placeholder", [], (SWEET, UNICODE, SINGLELINE, defaultMargin), "------")
      ,
        ( "picks the encoding from its PrintConfig for an attribute meta value (ASCII rho)"
        , [Subst (Map.singleton "t" (MvAttribute AtRho))]
        , (SWEET, ASCII, SINGLELINE, defaultMargin)
        , "t >> ^"
        )
      ]
      ( \(desc, substs, config, expected) ->
          it desc (printSubsts' substs config `shouldBe` expected)
      )

  describe "printExpressionHidingRho strips rho at every nesting depth" $
    it "three nested formations, each with its own rho" $ do
      let deep =
            ExFormation
              [ BiTau
                  (AtLabel "a")
                  ( ExFormation
                      [ BiTau (AtLabel "b") (ExFormation [BiVoid AtRho])
                      , BiTau AtRho ExXi
                      ]
                  )
              , BiTau AtRho ExRoot
              ]
      printExpressionHidingRho' deep (SWEET, UNICODE, SINGLELINE, defaultMargin) `shouldBe` "⟦ a ↦ ⟦ b ↦ ⟦⟧ ⟧ ⟧"

  describe "logPrintConfig" $
    it "is a fixed SWEET/UNICODE/SINGLELINE config at the default margin" $
      logPrintConfig `shouldBe` (SWEET, UNICODE, SINGLELINE, defaultMargin)
