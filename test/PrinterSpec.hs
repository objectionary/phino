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
import Printer
import Sugar (SugarType (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain)
import Yaml (ExtraArgument (..))

spec :: Spec
spec = do
  describe "printExpression with ASCII singleline renders primitives" $
    forM_
      [ ("ξ renders as $", ExThis, "$")
      , ("Φ renders as Q", ExGlobal, "Q")
      , ("⊥ renders as T", ExTermination, "T")
      , ("ρ void becomes empty", ExFormation [BiVoid AtRho], "[[]]")
      , ("φ void", ExFormation [BiVoid AtPhi], "[[ @ -> ? ]]")
      , ("label void", ExFormation [BiVoid (AtLabel "名前")], "[[ 名前 -> ? ]]")
      , ("x to Φ", ExFormation [BiTau (AtLabel "x") ExGlobal], "[[ x -> Q ]]")
      , ("α0 to ξ", ExFormation [BiTau (AtAlpha 0) ExThis], "[[ ~0 -> $ ]]")
      , ("ρ to ⊥", ExFormation [BiTau AtRho ExTermination], "[[ ^ -> T ]]")
      , ("empty delta", ExFormation [BiDelta BtEmpty], "[[ D> -- ]]")
      , ("single byte", ExFormation [BiDelta (BtOne "1F")], "[[ D> 1F- ]]")
      , ("multiple bytes", ExFormation [BiDelta (BtMany ["00", "01", "02"])], "[[ D> 00-01-02 ]]")
      , ("función lambda", ExFormation [BiLambda "Función"], "[[ L> Función ]]")
      , ("クラス lambda", ExFormation [BiLambda "クラス"], "[[ L> クラス ]]")
      , ("Φ.org", ExDispatch ExGlobal (AtLabel "org"), "Q.org")
      , ("ξ.ρ as sugar", ExDispatch ExThis AtRho, "^")
      , ("ξ.φ as sugar", ExDispatch ExThis AtPhi, "@")
      , ("chained dispatch", ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "éolang"), "Q.org.éolang")
      , ("ξ.α0 as sugar", ExDispatch ExThis (AtAlpha 0), "~0")
      ,
        ( "dispatch with app"
        , ExApplication (ExDispatch ExGlobal (AtLabel "x")) (BiTau (AtLabel "y") ExThis)
        , "Q.x( y -> $ )"
        )
      ,
        ( "formation with app"
        , ExApplication (ExFormation [BiVoid AtRho]) (BiTau (AtAlpha 0) ExGlobal)
        , "[[]]( Q )"
        )
      , ("meta expr", ExMeta "e", "!e")
      , ("meta tail", ExMetaTail ExGlobal "t", "Q * !t")
      , ("meta binding", ExFormation [BiMeta "B"], "[[ !B ]]")
      , ("meta lambda", ExFormation [BiMetaLambda "F"], "[[ L> !F ]]")
      , ("meta attr tau", ExFormation [BiTau (AtMeta "a") ExThis], "[[ !a -> $ ]]")
      ]
      ( \(desc, expr, expected) ->
          it desc (printExpression' expr (SWEET, ASCII, SINGLELINE, defaultMargin) `shouldBe` expected)
      )

  describe "printProgram with default config" $
    forM_
      [ ("empty formation", Program (ExFormation [BiVoid AtRho]), "{⟦⟧}")
      , ("dispatch", Program (ExDispatch ExGlobal (AtLabel "org")), "{Φ.org}")
      ]
      ( \(desc, prog, expected) ->
          it desc (printProgram prog `shouldBe` expected)
      )

  describe "printAttribute with default encoding" $
    forM_
      [ ("label", AtLabel "attr", "attr")
      , ("ρ", AtRho, "ρ")
      , ("φ", AtPhi, "φ")
      , ("α42", AtAlpha 42, "α42")
      , ("λ", AtLambda, "λ")
      , ("Δ", AtDelta, "Δ")
      ]
      ( \(desc, attr, expected) ->
          it desc (printAttribute attr `shouldBe` expected)
      )

  describe "printBinding renders as formation" $
    forM_
      [ ("tau binding", BiTau (AtLabel "x") ExGlobal, "x ↦ Φ")
      , ("void binding", BiVoid (AtLabel "y"), "y ↦ ∅")
      , ("delta binding", BiDelta (BtOne "00"), "Δ ⤍ 00-")
      , ("lambda binding", BiLambda "Func", "λ ⤍ Func")
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
      , ("expression arg", ArgExpression ExGlobal, "Φ")
      , ("bytes arg", ArgBytes (BtOne "FF"), "FF-")
      ]
      ( \(desc, arg, expected) ->
          it desc (printExtraArg arg `shouldContain` expected)
      )
