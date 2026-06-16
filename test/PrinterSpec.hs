{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

{- | Tests for the Printer module that converts AST to string representation.
The module provides functions to print phi-calculus expressions with
various configurations for sugar, encoding, and line format.
-}
module PrinterSpec where

import AST
import Control.Monad (forM_)
import Encoding (Encoding (..))
import Lining (LineFormat (..))
import Margin (defaultMargin)
import Misc (pattern DataNumber)
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
      , ("meta tail", ExMetaTail ExRoot "t", "Q * !t")
      , ("meta binding", ExFormation [BiMeta "B"], "[[ !B ]]")
      , ("meta lambda", ExFormation [BiLambda (FnMeta "F")], "[[ L> !F ]]")
      , ("meta attr tau", ExFormation [BiTau (AtMeta "a") ExXi], "[[ !a -> $ ]]")
      ]
      ( \(desc, expr, expected) ->
          it desc (printExpression' expr (SWEET, ASCII, SINGLELINE, defaultMargin) `shouldBe` expected)
      )

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

  describe "printProgram with default config" $
    forM_
      [ ("empty formation", Program (ExFormation [BiVoid AtRho]), "{⟦⟧}")
      , ("dispatch", Program (ExDispatch ExRoot (AtLabel "org")), "{Φ.org}")
      ]
      ( \(desc, prog, expected) ->
          it desc (printProgram prog `shouldBe` expected)
      )

  describe "printProgram in salty does not inject a duplicate void rho when rho is already present" $
    forM_
      [
        ( "rho bound to an empty formation"
        , Program (ExFormation [BiTau AtRho (ExFormation [BiVoid AtRho])])
        , "Φ ↦ ⟦ ρ ↦ ⟦ ρ ↦ ∅ ⟧ ⟧"
        )
      ,
        ( "rho bound to a non empty formation"
        , Program (ExFormation [BiTau AtRho (ExFormation [BiVoid (AtLabel "名前"), BiVoid AtRho])])
        , "Φ ↦ ⟦ ρ ↦ ⟦ 名前 ↦ ∅, ρ ↦ ∅ ⟧ ⟧"
        )
      ,
        ( "rho binding placed after another binding"
        , Program (ExFormation [BiTau (AtLabel "café") ExRoot, BiTau AtRho (ExFormation [BiVoid AtRho])])
        , "Φ ↦ ⟦ café ↦ Φ, ρ ↦ ⟦ ρ ↦ ∅ ⟧ ⟧"
        )
      ]
      ( \(desc, prog, expected) ->
          it desc (printProgram' prog (SALTY, UNICODE, SINGLELINE, defaultMargin) `shouldBe` expected)
      )

  describe "printAttribute with default encoding" $
    forM_
      [ ("label", AtLabel "attr", "attr")
      , ("ρ", AtRho, "ρ")
      , ("φ", AtPhi, "φ")
      , ("λ", AtLambda, "λ")
      , ("Δ", AtDelta, "Δ")
      ]
      ( \(desc, attr, expected) ->
          it desc (printAttribute attr `shouldBe` expected)
      )

  describe "printAlpha with default encoding" $
    it "α42" (printAlpha (Alpha 42) `shouldBe` "α42")

  describe "printBinding renders as formation" $
    forM_
      [ ("tau binding", BiTau (AtLabel "x") ExRoot, "x ↦ Φ")
      , ("void binding", BiVoid (AtLabel "y"), "y ↦ ∅")
      , ("delta binding", BiDelta (BtOne "00"), "Δ ⤍ 00-")
      , ("lambda binding", BiLambda (Function "Func"), "λ ⤍ Func")
      ]
      ( \(desc, bd, expected) ->
          it desc (printBinding bd `shouldContain` expected)
      )

  describe "printBytes renders bytes" $
    forM_
      [ ("empty bytes", BtEmpty, "--")
      , ("single byte", BtOne "1F", "1F-")
      , ("multiple bytes", BtMany ["00", "01", "02"], "00-01-02")
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
