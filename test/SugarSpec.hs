-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

{- | Tests for the Sugar module that provides conversion between sweet
(sugared) and salty (desugared) syntax representations of phi-calculus programs.
-}
module SugarSpec where

import AST
import CST
import Control.Monad (forM_)
import Parser (parseProgramThrows)
import Render (Render (render))
import Sugar (SugarType (..), toSalty, withSugarType)
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain, shouldSatisfy)

spec :: Spec
spec = do
  describe "SugarType Eq instance compares types" $
    forM_
      [ ("sweet equals sweet", SWEET, SWEET, True)
      , ("salty equals salty", SALTY, SALTY, True)
      , ("sweet differs from salty", SWEET, SALTY, False)
      , ("salty differs from sweet", SALTY, SWEET, False)
      ]
      ( \(desc, lhs, rhs, expected) ->
          it desc $ (lhs == rhs) `shouldBe` expected
      )

  describe "SugarType Show instance renders types" $
    forM_
      [ ("shows sweet", SWEET, "SWEET")
      , ("shows salty", SALTY, "SALTY")
      ]
      ( \(desc, sugar, expected) ->
          it desc $ show sugar `shouldBe` expected
      )

  describe "withSugarType SWEET returns unchanged program" $
    it "preserves sweet CST" $ do
      prog <- parseProgramThrows "{Q}"
      let cst = programToCST prog
          result = withSugarType SWEET cst
      result `shouldBe` cst

  describe "withSugarType SALTY converts to salty" $
    it "transforms sweet CST to salty" $ do
      prog <- parseProgramThrows "{Q}"
      let cst = programToCST prog
          result = withSugarType SALTY cst
          isSalty PR_SALTY{} = True
          isSalty _ = False
      result `shouldSatisfy` isSalty

  describe "toSalty PROGRAM converts sweet to salty" $
    it "converts PR_SWEET to PR_SALTY" $ do
      prog <- parseProgramThrows "{Q}"
      let cst = programToCST prog
          salty = toSalty cst
          isSalty PR_SALTY{} = True
          isSalty _ = False
      salty `shouldSatisfy` isSalty

  describe "toSalty PROGRAM leaves salty unchanged" $
    it "preserves PR_SALTY" $ do
      prog <- parseProgramThrows "{Q}"
      let cst = programToCST prog
          salty = toSalty cst
          twice = toSalty salty
      twice `shouldBe` salty

  describe "toSalty EXPRESSION converts default package" $
    it "expands QQ to Q.org.eolang" $ do
      prog <- parseProgramThrows "{QQ}"
      let cst = programToCST prog
          salty = toSalty cst
      render salty `shouldContain` "org"

  describe "toSalty EXPRESSION converts attribute sugar" $
    it "expands x to $.x" $ do
      prog <- parseProgramThrows "{[[ @ -> x ]]}"
      let cst = programToCST prog
          salty = toSalty cst
      render salty `shouldContain` "ξ"

  describe "toSalty EXPRESSION converts empty formation" $
    it "adds void rho to empty formation" $ do
      prog <- parseProgramThrows "{[[]]}"
      let cst = programToCST prog
          salty = toSalty cst
      render salty `shouldContain` "ρ"

  describe "toSalty EXPRESSION converts formation with bindings" $
    it "adds void rho when missing" $ do
      prog <- parseProgramThrows "{[[ x -> Q ]]}"
      let cst = programToCST prog
          salty = toSalty cst
      render salty `shouldContain` "ρ"

  describe "toSalty EXPRESSION preserves existing rho void" $
    it "keeps void rho binding" $ do
      prog <- parseProgramThrows "{[[ ^ -> ?, x -> Q ]]}"
      let cst = programToCST prog
          salty = toSalty cst
          rendered = render salty
          count = length (filter (== 'ρ') rendered)
      count `shouldBe` 1

  describe "toSalty EXPRESSION preserves existing rho tau" $
    it "keeps tau rho binding" $ do
      prog <- parseProgramThrows "{[[ ^ -> Q, x -> $ ]]}"
      let cst = programToCST prog
          salty = toSalty cst
          rendered = render salty
          count = length (filter (== 'ρ') rendered)
      count `shouldBe` 1

  describe "toSalty EXPRESSION converts dispatch" $
    it "recursively processes dispatch expression" $ do
      prog <- parseProgramThrows "{QQ.x}"
      let cst = programToCST prog
          salty = toSalty cst
      render salty `shouldContain` "org"

  describe "toSalty EXPRESSION converts application" $
    it "processes single application" $ do
      prog <- parseProgramThrows "{Q.x(y -> $)}"
      let cst = programToCST prog
          salty = toSalty cst
      render salty `shouldContain` "y"

  describe "toSalty EXPRESSION converts multiple applications" $
    it "processes chained applications" $ do
      prog <- parseProgramThrows "{Q.x(a -> $, b -> Q)}"
      let cst = programToCST prog
          salty = toSalty cst
          rendered = render salty
      rendered `shouldContain` "a"

  describe "toSalty EXPRESSION converts expression arguments" $
    it "converts positional args to alpha bindings" $ do
      prog <- parseProgramThrows "{Q.x($, Q)}"
      let cst = programToCST prog
          salty = toSalty cst
      render salty `shouldContain` "α0"

  describe "toSalty EXPRESSION converts number literal" $
    it "expands number to bytes" $ do
      prog <- parseProgramThrows "{[[ x -> 42 ]]}"
      let cst = programToCST prog
          salty = toSalty cst
      render salty `shouldContain` "number"

  describe "toSalty EXPRESSION converts string literal" $
    it "expands string to bytes" $ do
      prog <- parseProgramThrows "{[[ x -> \"hello\" ]]}"
      let cst = programToCST prog
          salty = toSalty cst
      render salty `shouldContain` "string"

  describe "toSalty EXPRESSION leaves global unchanged" $
    it "preserves Q" $ do
      let expr = EX_GLOBAL Φ
          salty = toSalty expr
      salty `shouldBe` expr

  describe "toSalty EXPRESSION leaves xi unchanged" $
    it "preserves $" $ do
      let expr = EX_XI XI
          salty = toSalty expr
      salty `shouldBe` expr

  describe "toSalty EXPRESSION leaves termination unchanged" $
    it "preserves T" $ do
      let expr = EX_TERMINATION DEAD
          salty = toSalty expr
      salty `shouldBe` expr

  describe "toSalty EXPRESSION leaves meta unchanged" $
    it "preserves meta expression" $ do
      let expr = EX_META (MT_EXPRESSION "e")
          salty = toSalty expr
      salty `shouldBe` expr

  describe "toSalty BINDING converts pair" $
    it "recursively processes binding pair" $ do
      prog <- parseProgramThrows "{[[ x -> QQ ]]}"
      let cst = programToCST prog
          salty = toSalty cst
      render salty `shouldContain` "org"

  describe "toSalty BINDING leaves empty unchanged" $
    it "preserves empty binding" $ do
      let binding = BI_EMPTY (TAB 0)
          salty = toSalty binding
      salty `shouldBe` binding

  describe "toSalty BINDINGS converts pair" $
    it "recursively processes bindings" $ do
      prog <- parseProgramThrows "{[[ x -> Q, y -> QQ ]]}"
      let cst = programToCST prog
          salty = toSalty cst
      render salty `shouldContain` "org"

  describe "toSalty BINDINGS leaves empty unchanged" $
    it "preserves empty bindings" $ do
      let bindings = BDS_EMPTY (TAB 0)
          salty = toSalty bindings
      salty `shouldBe` bindings

  describe "toSalty PAIR converts tau" $
    it "recursively processes tau pair" $ do
      prog <- parseProgramThrows "{[[ x -> QQ ]]}"
      let cst = programToCST prog
          salty = toSalty cst
      render salty `shouldContain` "org"

  describe "toSalty PAIR converts formation with voids" $
    it "expands void parameters into formation" $ do
      prog <- parseProgramThrows "{[[ f(a, b) -> [[]] ]]}"
      let cst = programToCST prog
          salty = toSalty cst
          rendered = render salty
      rendered `shouldContain` "a"

  describe "toSalty PAIR leaves void unchanged" $
    it "preserves void pair" $ do
      let pair = PA_VOID (AT_LABEL "x") ARROW EMPTY
          salty = toSalty pair
      salty `shouldBe` pair

  describe "toSalty PAIR leaves lambda unchanged" $
    it "preserves lambda pair" $ do
      let pair = PA_LAMBDA "Func"
          salty = toSalty pair
      salty `shouldBe` pair

  describe "toSalty PAIR leaves delta unchanged" $
    it "preserves delta pair" $ do
      let pair = PA_DELTA BT_EMPTY
          salty = toSalty pair
      salty `shouldBe` pair

  describe "toSalty APP_BINDING converts pair" $
    it "recursively processes app binding" $ do
      prog <- parseProgramThrows "{Q.x(y -> QQ)}"
      let cst = programToCST prog
          salty = toSalty cst
      render salty `shouldContain` "org"

  describe "toSalty handles nested formations" $
    it "adds rho to nested formations" $ do
      prog <- parseProgramThrows "{[[ x -> [[ y -> Q ]] ]]}"
      let cst = programToCST prog
          salty = toSalty cst
          rendered = render salty
          count = length (filter (== 'ρ') rendered)
      count `shouldBe` 2

  describe "toSalty handles complex program" $
    it "processes fibonacci example" $ do
      prog <- parseProgramThrows "{[[ fac(n) -> [[ @ -> n.eq(1, n.times(^.fac(n.plus(-1)))) ]] ]]}"
      let cst = programToCST prog
          salty = toSalty cst
          rendered = render salty
      rendered `shouldContain` "ρ"

  describe "toSalty handles mixed case identifiers" $
    it "preserves case in labels" $ do
      prog <- parseProgramThrows "{[[ myLabel -> Q ]]}"
      let cst = programToCST prog
          salty = toSalty cst
      render salty `shouldContain` "myLabel"

  describe "toSalty handles deep dispatch chain" $
    it "processes Q.a.b.c.d" $ do
      prog <- parseProgramThrows "{Q.a.b.c.d}"
      let cst = programToCST prog
          salty = toSalty cst
          rendered = render salty
      rendered `shouldContain` "d"

  describe "toSalty handles multiple expression arguments" $
    it "converts all positional args" $ do
      prog <- parseProgramThrows "{Q.f($, Q, $)}"
      let cst = programToCST prog
          salty = toSalty cst
          rendered = render salty
      rendered `shouldContain` "α2"
