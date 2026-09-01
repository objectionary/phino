{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module EncodingSpec where

import CST
import Encoding (Encoding (..), toASCII, withEncoding)
import Test.Hspec (Spec, describe, it, shouldBe)

leafExpr :: EXPRESSION
leafExpr = EX_GLOBAL Φ

leafExprASCII :: EXPRESSION
leafExprASCII = EX_GLOBAL Q

biPair :: BINDING
biPair = BI_PAIR (PA_TAU (AT_PHI PHI) ARROW leafExpr) (BDS_EMPTY (TAB 1)) (TAB 1)

biPairASCII :: BINDING
biPairASCII = BI_PAIR (PA_TAU (AT_PHI AT) ARROW' leafExprASCII) (BDS_EMPTY (TAB 1)) (TAB 1)

spec :: Spec
spec = do
  describe "toASCII on EXPRESSION" $ do
    it "EX_GLOBAL becomes Q" $ toASCII (EX_GLOBAL Φ) `shouldBe` EX_GLOBAL Q
    it "EX_XI becomes $" $ toASCII (EX_XI XI) `shouldBe` EX_XI DOLLAR
    it "EX_ATTR recurses into its attribute" $ toASCII (EX_ATTR (AT_PHI PHI)) `shouldBe` EX_ATTR (AT_PHI AT)
    it "EX_TERMINATION becomes T" $ toASCII (EX_TERMINATION DEAD) `shouldBe` EX_TERMINATION T

    it "EX_FORMATION recurses into its binding and forces LSB'/RSB'" $
      toASCII (EX_FORMATION LSB EOL (TAB 1) biPair EOL (TAB 0) RSB)
        `shouldBe` EX_FORMATION LSB' EOL (TAB 1) biPairASCII EOL (TAB 0) RSB'

    it "EX_DISPATCH recurses into its expression and attribute" $
      toASCII (EX_DISPATCH leafExpr NO_SPACE (AT_RHO RHO)) `shouldBe` EX_DISPATCH leafExprASCII NO_SPACE (AT_RHO CARET)

    it "EX_APPLICATION recurses into its expression and argument" $
      toASCII (EX_APPLICATION leafExpr NO_SPACE EOL (TAB 1) (AA_TAUS biPair) EOL (TAB 0) 1)
        `shouldBe` EX_APPLICATION leafExprASCII NO_SPACE EOL (TAB 1) (AA_TAUS biPairASCII) EOL (TAB 0) 1

    it "EX_META with an 'n'-headed meta becomes N'" $
      toASCII (EX_META (META NO_EXCL N "abc")) `shouldBe` EX_META (META EXCL N' "abc")
    it "EX_META with a 'k'-headed meta becomes K'" $
      toASCII (EX_META (META NO_EXCL K "abc")) `shouldBe` EX_META (META EXCL K' "abc")
    it "EX_META with any other head becomes E'" $
      toASCII (EX_META (META NO_EXCL E "abc")) `shouldBe` EX_META (META EXCL E' "abc")

    it "EX_PHI_MEET recurses into its expression" $
      toASCII (EX_PHI_MEET (Just "p") 3 leafExpr) `shouldBe` EX_PHI_MEET (Just "p") 3 leafExprASCII
    it "EX_PHI_AGAIN recurses into its expression" $
      toASCII (EX_PHI_AGAIN Nothing 4 leafExpr) `shouldBe` EX_PHI_AGAIN Nothing 4 leafExprASCII

    it "leaves every other constructor untouched" $ do
      toASCII (EX_STRING "hi" (TAB 1) []) `shouldBe` EX_STRING "hi" (TAB 1) []
      toASCII (EX_NUMBER (Left 5) (TAB 1) []) `shouldBe` EX_NUMBER (Left 5) (TAB 1) []
      toASCII (EX_BYTES BT_EMPTY) `shouldBe` EX_BYTES BT_EMPTY

  describe "toASCII on APP_BINDING" $
    it "recurses into the pair" $
      toASCII (APP_BINDING (PA_TAU (AT_PHI PHI) ARROW leafExpr)) `shouldBe` APP_BINDING (PA_TAU (AT_PHI AT) ARROW' leafExprASCII)

  describe "toASCII on BINDING" $ do
    it "recurses through BI_PAIR" $ toASCII biPair `shouldBe` biPairASCII

    it "recurses through BI_META, forcing the meta head to B'" $ do
      let biMeta = BI_META (META NO_EXCL B "X") (BDS_EMPTY (TAB 1)) (TAB 1)
          expected = BI_META (META EXCL B' "X") (BDS_EMPTY (TAB 1)) (TAB 1)
      toASCII biMeta `shouldBe` expected

    it "leaves BI_EMPTY untouched" $ do
      let biEmpty = BI_EMPTY (TAB 1)
      toASCII biEmpty `shouldBe` biEmpty

  describe "toASCII on BINDINGS" $ do
    it "recurses through BDS_PAIR" $ do
      let bdsPair = BDS_PAIR EOL (TAB 1) (PA_TAU (AT_PHI PHI) ARROW leafExpr) (BDS_EMPTY (TAB 1))
          expected = BDS_PAIR EOL (TAB 1) (PA_TAU (AT_PHI AT) ARROW' leafExprASCII) (BDS_EMPTY (TAB 1))
      toASCII bdsPair `shouldBe` expected

    it "recurses through BDS_META, forcing the meta head to B'" $ do
      let bdsMeta = BDS_META EOL (TAB 1) (META NO_EXCL B "X") (BDS_EMPTY (TAB 1))
          expected = BDS_META EOL (TAB 1) (META EXCL B' "X") (BDS_EMPTY (TAB 1))
      toASCII bdsMeta `shouldBe` expected

    it "leaves BDS_EMPTY untouched" $ do
      let bdsEmpty = BDS_EMPTY (TAB 1)
      toASCII bdsEmpty `shouldBe` bdsEmpty

  describe "toASCII on APP_ARGUMENT" $ do
    it "recurses through AA_TAU" $
      toASCII (AA_TAU (APP_BINDING (PA_TAU (AT_PHI PHI) ARROW leafExpr)))
        `shouldBe` AA_TAU (APP_BINDING (PA_TAU (AT_PHI AT) ARROW' leafExprASCII))
    it "recurses through AA_TAUS" $
      toASCII (AA_TAUS biPair) `shouldBe` AA_TAUS biPairASCII
    it "recurses through AA_EXPRS" $
      toASCII (AA_EXPRS (APP_ARG leafExpr AAS_EMPTY)) `shouldBe` AA_EXPRS (APP_ARG leafExprASCII AAS_EMPTY)

  describe "toASCII on APP_ARG" $
    it "recurses through both fields" $
      toASCII (APP_ARG leafExpr (AAS_EXPR EOL (TAB 1) leafExpr AAS_EMPTY))
        `shouldBe` APP_ARG leafExprASCII (AAS_EXPR EOL (TAB 1) leafExprASCII AAS_EMPTY)

  describe "toASCII on APP_ARGS" $ do
    it "recurses through AAS_EXPR" $
      toASCII (AAS_EXPR EOL (TAB 1) leafExpr AAS_EMPTY) `shouldBe` AAS_EXPR EOL (TAB 1) leafExprASCII AAS_EMPTY
    it "leaves AAS_EMPTY untouched" $
      toASCII AAS_EMPTY `shouldBe` AAS_EMPTY

  describe "toASCII on PAIR" $ do
    it "recurses through PA_TAU, forcing the arrow to ARROW'" $
      toASCII (PA_TAU (AT_PHI PHI) ARROW leafExpr) `shouldBe` PA_TAU (AT_PHI AT) ARROW' leafExprASCII
    it "recurses through PA_ALPHA, forcing the arrow to ARROW'" $
      toASCII (PA_ALPHA (AL_IDX ALPHA 0) ARROW leafExpr) `shouldBe` PA_ALPHA (AL_IDX ALPHA' 0) ARROW' leafExprASCII
    it "recurses through PA_FORMATION, forcing the arrow to ARROW'" $
      toASCII (PA_FORMATION (AT_PHI PHI) [AT_RHO RHO] ARROW leafExpr)
        `shouldBe` PA_FORMATION (AT_PHI AT) [AT_RHO CARET] ARROW' leafExprASCII
    it "PA_VOID forces the arrow and the void marker" $
      toASCII (PA_VOID (AT_PHI PHI) ARROW EMPTY) `shouldBe` PA_VOID (AT_PHI AT) ARROW' QUESTION
    it "PA_LAMBDA becomes PA_LAMBDA'" $ toASCII (PA_LAMBDA "Func") `shouldBe` PA_LAMBDA' "Func"
    it "PA_DELTA becomes PA_DELTA'" $ toASCII (PA_DELTA BT_EMPTY) `shouldBe` PA_DELTA' BT_EMPTY
    it "PA_META_LAMBDA becomes PA_META_LAMBDA' with head F'" $
      toASCII (PA_META_LAMBDA (META NO_EXCL F "fn")) `shouldBe` PA_META_LAMBDA' (META EXCL F' "fn")
    it "PA_META_DELTA becomes PA_META_DELTA' with head D'" $
      toASCII (PA_META_DELTA (META NO_EXCL D "dl")) `shouldBe` PA_META_DELTA' (META EXCL D' "dl")
    it "leaves already-ASCII pairs untouched" $ do
      toASCII (PA_LAMBDA' "Func") `shouldBe` PA_LAMBDA' "Func"
      toASCII (PA_DELTA' BT_EMPTY) `shouldBe` PA_DELTA' BT_EMPTY

  describe "toASCII on ALPHA" $ do
    it "recurses through AL_IDX" $ toASCII (AL_IDX ALPHA 7) `shouldBe` AL_IDX ALPHA' 7
    it "recurses through AL_META" $ toASCII (AL_META ALPHA (META NO_EXCL I "abc")) `shouldBe` AL_META ALPHA' (META EXCL I' "abc")

  describe "toASCII on ATTRIBUTE" $ do
    it "AT_PHI becomes AT" $ toASCII (AT_PHI PHI) `shouldBe` AT_PHI AT
    it "AT_RHO becomes CARET" $ toASCII (AT_RHO RHO) `shouldBe` AT_RHO CARET
    it "AT_META forces the head to A" $ toASCII (AT_META (META NO_EXCL TAU "abc")) `shouldBe` AT_META (META EXCL A "abc")
    it "leaves every other attribute untouched" $ do
      toASCII (AT_LABEL "x") `shouldBe` AT_LABEL "x"
      toASCII (AT_LAMBDA LAMBDA) `shouldBe` AT_LAMBDA LAMBDA
      toASCII (AT_DELTA DELTA) `shouldBe` AT_DELTA DELTA

  describe "toASCII on SET" $ do
    it "recurses through ST_BINDING" $ toASCII (ST_BINDING biPair) `shouldBe` ST_BINDING biPairASCII
    it "maps toASCII over ST_ATTRIBUTES" $
      toASCII (ST_ATTRIBUTES [AT_PHI PHI, AT_RHO RHO]) `shouldBe` ST_ATTRIBUTES [AT_PHI AT, AT_RHO CARET]

  describe "toASCII on NUMBER" $ do
    it "IDX_META forces the head to I'" $ toASCII (IDX_META (META NO_EXCL I "abc")) `shouldBe` IDX_META (META EXCL I' "abc")
    it "recurses through LENGTH" $ toASCII (LENGTH biPair) `shouldBe` LENGTH biPairASCII
    it "recurses through DOMAIN" $ toASCII (DOMAIN biPair) `shouldBe` DOMAIN biPairASCII
    it "leaves LITERAL untouched" $ toASCII (LITERAL 5) `shouldBe` LITERAL 5

  describe "toASCII on COMPARABLE" $ do
    it "recurses through CMP_ATTR" $ toASCII (CMP_ATTR (AT_PHI PHI)) `shouldBe` CMP_ATTR (AT_PHI AT)
    it "recurses through CMP_EXPR" $ toASCII (CMP_EXPR leafExpr) `shouldBe` CMP_EXPR leafExprASCII
    it "recurses through CMP_NUM" $ toASCII (CMP_NUM (LENGTH biPair)) `shouldBe` CMP_NUM (LENGTH biPairASCII)

  describe "toASCII on CONDITION" $ do
    it "recurses through CO_BELONGS" $
      toASCII (CO_BELONGS (AT_PHI PHI) IN (ST_BINDING biPair)) `shouldBe` CO_BELONGS (AT_PHI AT) IN (ST_BINDING biPairASCII)
    it "maps toASCII over CO_LOGIC" $
      toASCII (CO_LOGIC [CO_NF leafExpr, CO_EMPTY] AND) `shouldBe` CO_LOGIC [CO_NF leafExprASCII, CO_EMPTY] AND
    it "recurses through CO_NF" $ toASCII (CO_NF leafExpr) `shouldBe` CO_NF leafExprASCII
    it "recurses through CO_ABSOLUTE" $ toASCII (CO_ABSOLUTE leafExpr IN) `shouldBe` CO_ABSOLUTE leafExprASCII IN
    it "recurses through CO_NOT" $ toASCII (CO_NOT (CO_NF leafExpr)) `shouldBe` CO_NOT (CO_NF leafExprASCII)
    it "recurses through CO_COMPARE" $
      toASCII (CO_COMPARE (CMP_ATTR (AT_PHI PHI)) EQUAL (CMP_EXPR leafExpr))
        `shouldBe` CO_COMPARE (CMP_ATTR (AT_PHI AT)) EQUAL (CMP_EXPR leafExprASCII)
    it "recurses through CO_MATCHES" $ toASCII (CO_MATCHES "abc" leafExpr) `shouldBe` CO_MATCHES "abc" leafExprASCII
    it "recurses through CO_PART_OF" $ toASCII (CO_PART_OF leafExpr biPair) `shouldBe` CO_PART_OF leafExprASCII biPairASCII
    it "maps toASCII over CO_DISJOINT" $
      toASCII (CO_DISJOINT [AT_PHI PHI] [biPair]) `shouldBe` CO_DISJOINT [AT_PHI AT] [biPairASCII]
    it "recurses through CO_FORMATION" $ toASCII (CO_FORMATION leafExpr) `shouldBe` CO_FORMATION leafExprASCII
    it "leaves CO_EMPTY untouched" $ toASCII CO_EMPTY `shouldBe` CO_EMPTY

  describe "toASCII on EXTRA_ARG" $ do
    it "recurses through ARG_ATTR" $ toASCII (ARG_ATTR (AT_PHI PHI)) `shouldBe` ARG_ATTR (AT_PHI AT)
    it "recurses through ARG_EXPR" $ toASCII (ARG_EXPR leafExpr) `shouldBe` ARG_EXPR leafExprASCII
    it "recurses through ARG_BINDING" $ toASCII (ARG_BINDING biPair) `shouldBe` ARG_BINDING biPairASCII
    it "leaves ARG_BYTES untouched" $ toASCII (ARG_BYTES BT_EMPTY) `shouldBe` ARG_BYTES BT_EMPTY

  describe "toASCII on EXTRA" $
    it "recurses through meta and every arg, keeping func untouched" $
      toASCII (EXTRA (ARG_ATTR (AT_PHI PHI)) "func" [ARG_EXPR leafExpr, ARG_BYTES BT_EMPTY])
        `shouldBe` EXTRA (ARG_ATTR (AT_PHI AT)) "func" [ARG_EXPR leafExprASCII, ARG_BYTES BT_EMPTY]

  describe "withEncoding" $ do
    it "UNICODE is the identity" $ withEncoding UNICODE biPair `shouldBe` biPair
    it "ASCII is toASCII" $ withEncoding ASCII biPair `shouldBe` toASCII biPair
