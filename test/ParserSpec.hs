-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module ParserSpec where

import Ast
import Data.Either (isLeft)
import Parser
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  describe "parseProgram" $ do
    it "parses a simple program" $
      parseProgram "Q -> []" `shouldBe` Right (Program (ExFormation []))
    it "parses application" $
      parseProgram "Q -> T(x -> Q)" `shouldBe` Right (Program (ExApplication ExTermination [TauBinding (AtLabel "x") ExGlobal]))
    it "parses dispatches" $
      parseProgram "Q -> Q.org.eolang" `shouldBe` Right (Program (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")))
    it "parses bindings" $
      shouldBe
        (parseProgram "Q -> [x -> $, ~1 -> ?]")
        (Right (Program (ExFormation [BiTau (TauBinding (AtLabel "x") ExThis), BiVoid (AtAlpha 1)])))
  describe "parseExpression" $ do
    it "parses meta attribute in dispatch" $
      parseExpression "Q.!a" `shouldBe` Right (ExDispatch ExGlobal (AtMeta "a"))
    it "parses meta attribute in application" $
      parseExpression "[](!a1 -> $)" `shouldBe` Right (ExApplication (ExFormation []) [TauBinding (AtMeta "a1") ExThis])
    it "parses sequence of applications" $
      parseExpression "[](~0 -> $)(~1 -> Q)"
        `shouldBe` Right
          ( ExApplication
              ( ExApplication
                  (ExFormation [])
                  [TauBinding (AtAlpha 0) ExThis]
              )
              [TauBinding (AtAlpha 1) ExGlobal]
          )
    it "parses many bindings in application" $
      parseExpression "[](x -> $, y -> Q)" `shouldBe` Right (ExApplication (ExFormation []) [TauBinding (AtLabel "x") ExThis, TauBinding (AtLabel "y") ExGlobal])
    it "parses meta bindings" $
      parseExpression "[!B, !B1]" `shouldBe` Right (ExFormation [BiMeta "B", BiMeta "B1"])
    it "parses meta bindings with meta attributes in bindings" $
      parseExpression "[!B2, !a2 -> $]" `shouldBe` Right (ExFormation [BiMeta "B2", BiTau (TauBinding (AtMeta "a2") ExThis)])
    it "parses simple meta expression" $
      parseExpression "!e0" `shouldBe` Right (ExMeta "e0")
    it "parses meta expression inside" $
      parseExpression "[x -> !e]" `shouldBe` Right (ExFormation [BiTau (TauBinding (AtLabel "x") (ExMeta "e"))])
    it "parses meta attribute with meta expression" $
      parseExpression "[!a -> !e1]" `shouldBe` Right (ExFormation [BiTau (TauBinding (AtMeta "a") (ExMeta "e1"))])
    it "parses expression meta tail" $
      parseExpression "Q * !t" `shouldBe` Right (ExMetaTail ExGlobal "t")
    it "parses complex expression meta tail" $
      parseExpression "[](x -> $) * !t1" `shouldBe` Right (ExMetaTail (ExApplication (ExFormation []) [TauBinding (AtLabel "x") ExThis]) "t1")
    it "parses empty delta binding" $
      parseExpression "[D> --]" `shouldBe` Right (ExFormation [BiDelta "--"])
    it "parses one byte delta binding" $
      parseExpression "[D> 1F-]" `shouldBe` Right (ExFormation [BiDelta "1F-"])
    it "parses several bytes delta binding" $
      parseExpression "[D> 1F-2A-00]" `shouldBe` Right (ExFormation [BiDelta "1F-2A-00"])
    it "parses delta meta binding" $
      parseExpression "[D> !b0]" `shouldBe` Right (ExFormation [BiMetaDelta "b0"])
    it "parses lambda binding" $
      parseExpression "[L> Function]" `shouldBe` Right (ExFormation [BiLambda "Function"])
    it "parses meta labmda binding" $
      parseExpression "[L> !F3]" `shouldBe` Right (ExFormation [BiMetaLambda "F3"])
    it "prohibits parsing empty application" $
      parseExpression "Q.x()" `shouldSatisfy` isLeft
    it "prohibits parsing two meta tails" $
      parseExpression "Q * !t1 * !t2" `shouldSatisfy` isLeft
    it "prohibits Q() application" $
      parseExpression "Q(x -> [])" `shouldSatisfy` isLeft
    it "prohibits $() application" $
      parseExpression "$(x -> [])" `shouldSatisfy` isLeft
