-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

{- | Tests for the Printer module that converts AST to string representation.
The module provides functions to print phi-calculus expressions with
various configurations for sugar, encoding, and line format.
-}
module PrinterSpec where

import AST
import Control.Monad (forM_)
import qualified Data.Map.Strict as Map
import Encoding (Encoding (..))
import Lining (LineFormat (..))
import Matcher (MetaValue (..), Subst (..), Tail (..), defaultScope)
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
      ]
      ( \(desc, expr, expected) ->
          it desc (printExpression' expr (SWEET, ASCII, SINGLELINE) `shouldBe` expected)
      )

  describe "printExpression with ASCII singleline renders formation with void" $
    forM_
      [ ("ρ void becomes empty", ExFormation [BiVoid AtRho], "[[]]")
      , ("φ void", ExFormation [BiVoid AtPhi], "[[ @ -> ? ]]")
      , ("label void", ExFormation [BiVoid (AtLabel "名前")], "[[ 名前 -> ? ]]")
      ]
      ( \(desc, expr, expected) ->
          it desc (printExpression' expr (SWEET, ASCII, SINGLELINE) `shouldBe` expected)
      )

  describe "printExpression with ASCII singleline renders formation with tau" $
    forM_
      [ ("x to Φ", ExFormation [BiTau (AtLabel "x") ExGlobal], "[[ x -> Q ]]")
      , ("α0 to ξ", ExFormation [BiTau (AtAlpha 0) ExThis], "[[ ~0 -> $ ]]")
      , ("ρ to ⊥", ExFormation [BiTau AtRho ExTermination], "[[ ^ -> T ]]")
      ]
      ( \(desc, expr, expected) ->
          it desc (printExpression' expr (SWEET, ASCII, SINGLELINE) `shouldBe` expected)
      )

  describe "printExpression with ASCII singleline renders formation with delta" $
    forM_
      [ ("empty delta", ExFormation [BiDelta BtEmpty], "[[ D> -- ]]")
      , ("single byte", ExFormation [BiDelta (BtOne "1F")], "[[ D> 1F- ]]")
      , ("multiple bytes", ExFormation [BiDelta (BtMany ["00", "01", "02"])], "[[ D> 00-01-02 ]]")
      ]
      ( \(desc, expr, expected) ->
          it desc (printExpression' expr (SWEET, ASCII, SINGLELINE) `shouldBe` expected)
      )

  describe "printExpression with ASCII singleline renders formation with lambda" $
    forM_
      [ ("función lambda", ExFormation [BiLambda "Función"], "[[ L> Función ]]")
      , ("クラス lambda", ExFormation [BiLambda "クラス"], "[[ L> クラス ]]")
      ]
      ( \(desc, expr, expected) ->
          it desc (printExpression' expr (SWEET, ASCII, SINGLELINE) `shouldBe` expected)
      )

  describe "printExpression with ASCII singleline renders dispatch" $
    forM_
      [ ("Φ.org", ExDispatch ExGlobal (AtLabel "org"), "Q.org")
      , ("ξ.ρ as sugar", ExDispatch ExThis AtRho, "^")
      , ("ξ.φ as sugar", ExDispatch ExThis AtPhi, "@")
      , ("chained dispatch", ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "éolang"), "Q.org.éolang")
      , ("ξ.α0 as sugar", ExDispatch ExThis (AtAlpha 0), "~0")
      ]
      ( \(desc, expr, expected) ->
          it desc (printExpression' expr (SWEET, ASCII, SINGLELINE) `shouldBe` expected)
      )

  describe "printExpression with ASCII singleline renders application" $
    forM_
      [
        ( "dispatch with app"
        , ExApplication (ExDispatch ExGlobal (AtLabel "x")) (BiTau (AtLabel "y") ExThis)
        , "Q.x( y -> $ )"
        )
      ,
        ( "formation with app"
        , ExApplication (ExFormation [BiVoid AtRho]) (BiTau (AtAlpha 0) ExGlobal)
        , "[[]]( Q )"
        )
      ]
      ( \(desc, expr, expected) ->
          it desc (printExpression' expr (SWEET, ASCII, SINGLELINE) `shouldBe` expected)
      )

  describe "printExpression with ASCII singleline renders meta expressions" $
    forM_
      [ ("meta expr", ExMeta "e", "!e")
      , ("meta tail", ExMetaTail ExGlobal "t", "Q * !t")
      ]
      ( \(desc, expr, expected) ->
          it desc (printExpression' expr (SWEET, ASCII, SINGLELINE) `shouldBe` expected)
      )

  describe "printExpression with ASCII singleline renders meta bindings" $
    forM_
      [ ("meta binding", ExFormation [BiMeta "B"], "[[ !B ]]")
      , ("meta lambda", ExFormation [BiMetaLambda "F"], "[[ L> !F ]]")
      , ("meta attr tau", ExFormation [BiTau (AtMeta "a") ExThis], "[[ !a -> $ ]]")
      ]
      ( \(desc, expr, expected) ->
          it desc (printExpression' expr (SWEET, ASCII, SINGLELINE) `shouldBe` expected)
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

  describe "printAttribute in ASCII dispatch expression with sugar" $
    forM_
      [ ("ρ as caret", AtRho, "^")
      , ("φ as at", AtPhi, "@")
      , ("αN as tildeN", AtAlpha 42, "~42")
      ]
      ( \(desc, attr, expected) ->
          it desc (printExpression' (ExDispatch ExThis attr) (SWEET, ASCII, SINGLELINE) `shouldBe` expected)
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

  describe "printSubsts renders empty list" $ do
    it "returns separator" $
      printSubsts [] `shouldBe` "------"

  describe "printSubsts renders attribute substitution" $ do
    it "contains key and value" $
      printSubsts [Subst (Map.singleton "α" (MvAttribute (AtLabel "ατρ")))]
        `shouldContain` "α >> ατρ"

  describe "printSubsts renders multiple substitutions" $ do
    it "separates with dashed line" $ do
      let substs =
            [ Subst (Map.singleton "a" (MvAttribute AtRho))
            , Subst (Map.singleton "b" (MvAttribute AtPhi))
            ]
      printSubsts substs `shouldContain` "------"

  describe "printSubsts renders expression value" $ do
    it "contains expression" $
      printSubsts [Subst (Map.singleton "e" (MvExpression ExGlobal defaultScope))]
        `shouldContain` "e >> Φ"

  describe "printSubsts renders bindings value" $ do
    it "contains bindings header" $
      printSubsts [Subst (Map.singleton "B" (MvBindings [BiVoid (AtLabel "x")]))]
        `shouldContain` "B >> ⟦"

  describe "printSubsts renders bytes value" $ do
    it "contains bytes" $
      printSubsts [Subst (Map.singleton "d" (MvBytes (BtMany ["AB", "CD"])))]
        `shouldContain` "d >> AB-CD"

  describe "printSubsts renders function value" $ do
    it "contains function name" $
      printSubsts [Subst (Map.singleton "F" (MvFunction "MyFunc"))]
        `shouldContain` "F >> MyFunc"

  describe "printSubsts renders tail value with dispatch" $ do
    it "contains dispatch" $
      printSubsts [Subst (Map.singleton "t" (MvTail [TaDispatch (AtLabel "attr")]))]
        `shouldContain` "t >> .attr"

  describe "printSubsts renders tail value with application" $ do
    it "contains application" $
      printSubsts [Subst (Map.singleton "t" (MvTail [TaApplication (BiTau (AtLabel "x") ExThis)]))]
        `shouldContain` "(⟦"

  describe "printExpression with salty config" $ do
    it "adds explicit rho binding" $
      printExpression' (ExFormation [BiVoid (AtLabel "x")]) (SALTY, UNICODE, SINGLELINE)
        `shouldContain` "ρ ↦ ∅"

  describe "printExpression with multiline format" $ do
    it "adds newlines in formation" $ do
      let expr = ExFormation [BiTau (AtLabel "x") ExGlobal, BiVoid (AtLabel "y")]
      let result = printExpression' expr (SWEET, UNICODE, MULTILINE)
      result `shouldContain` "\n"

  describe "logPrintConfig" $ do
    it "is sweet unicode singleline" $
      logPrintConfig `shouldBe` (SWEET, UNICODE, SINGLELINE)
