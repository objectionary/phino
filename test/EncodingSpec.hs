{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module EncodingSpec where

import CST
import Control.Monad (forM_)
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
    forM_
      [ ("EX_GLOBAL becomes Q", toASCII (EX_GLOBAL Φ), EX_GLOBAL Q)
      , ("EX_XI becomes $", toASCII (EX_XI XI), EX_XI DOLLAR)
      , ("EX_ATTR recurses into its attribute", toASCII (EX_ATTR (AT_PHI PHI)), EX_ATTR (AT_PHI AT))
      , ("EX_TERMINATION becomes T", toASCII (EX_TERMINATION DEAD), EX_TERMINATION T)
      ,
        ( "EX_FORMATION recurses into its binding and forces LSB'/RSB'"
        , toASCII (EX_FORMATION LSB EOL (TAB 1) biPair EOL (TAB 0) RSB)
        , EX_FORMATION LSB' EOL (TAB 1) biPairASCII EOL (TAB 0) RSB'
        )
      ,
        ( "EX_DISPATCH recurses into its expression and attribute"
        , toASCII (EX_DISPATCH leafExpr NO_SPACE (AT_RHO RHO))
        , EX_DISPATCH leafExprASCII NO_SPACE (AT_RHO CARET)
        )
      ,
        ( "EX_APPLICATION recurses into its expression and argument"
        , toASCII (EX_APPLICATION leafExpr NO_SPACE EOL (TAB 1) (AA_TAUS biPair) EOL (TAB 0) 1)
        , EX_APPLICATION leafExprASCII NO_SPACE EOL (TAB 1) (AA_TAUS biPairASCII) EOL (TAB 0) 1
        )
      ,
        ( "EX_META with an 'n'-headed meta becomes N'"
        , toASCII (EX_META (META NO_EXCL N "abc"))
        , EX_META (META EXCL N' "abc")
        )
      ,
        ( "EX_META with a 'k'-headed meta becomes K'"
        , toASCII (EX_META (META NO_EXCL K "abc"))
        , EX_META (META EXCL K' "abc")
        )
      ,
        ( "EX_META with any other head becomes E'"
        , toASCII (EX_META (META NO_EXCL E "abc"))
        , EX_META (META EXCL E' "abc")
        )
      ,
        ( "EX_PHI_MEET recurses into its expression"
        , toASCII (EX_PHI_MEET (Just "p") 3 leafExpr)
        , EX_PHI_MEET (Just "p") 3 leafExprASCII
        )
      ,
        ( "EX_PHI_AGAIN recurses into its expression"
        , toASCII (EX_PHI_AGAIN Nothing 4 leafExpr)
        , EX_PHI_AGAIN Nothing 4 leafExprASCII
        )
      ]
      (\(desc, actual, expected) -> it desc (actual `shouldBe` expected))

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

  describe "toASCII on APP_ARGUMENT" $
    forM_
      [
        ( "recurses through AA_TAU"
        , toASCII (AA_TAU (APP_BINDING (PA_TAU (AT_PHI PHI) ARROW leafExpr)))
        , AA_TAU (APP_BINDING (PA_TAU (AT_PHI AT) ARROW' leafExprASCII))
        )
      , ("recurses through AA_TAUS", toASCII (AA_TAUS biPair), AA_TAUS biPairASCII)
      ,
        ( "recurses through AA_EXPRS"
        , toASCII (AA_EXPRS (APP_ARG leafExpr AAS_EMPTY))
        , AA_EXPRS (APP_ARG leafExprASCII AAS_EMPTY)
        )
      ]
      (\(desc, actual, expected) -> it desc (actual `shouldBe` expected))

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
    forM_
      [
        ( "recurses through PA_TAU, forcing the arrow to ARROW'"
        , toASCII (PA_TAU (AT_PHI PHI) ARROW leafExpr)
        , PA_TAU (AT_PHI AT) ARROW' leafExprASCII
        )
      ,
        ( "recurses through PA_ALPHA, forcing the arrow to ARROW'"
        , toASCII (PA_ALPHA (AL_IDX ALPHA 0) ARROW leafExpr)
        , PA_ALPHA (AL_IDX ALPHA' 0) ARROW' leafExprASCII
        )
      ,
        ( "recurses through PA_FORMATION, forcing the arrow to ARROW'"
        , toASCII (PA_FORMATION (AT_PHI PHI) [AT_RHO RHO] ARROW leafExpr)
        , PA_FORMATION (AT_PHI AT) [AT_RHO CARET] ARROW' leafExprASCII
        )
      ,
        ( "PA_VOID forces the arrow and the void marker"
        , toASCII (PA_VOID (AT_PHI PHI) ARROW EMPTY)
        , PA_VOID (AT_PHI AT) ARROW' QUESTION
        )
      , ("PA_LAMBDA becomes PA_LAMBDA'", toASCII (PA_LAMBDA "Func"), PA_LAMBDA' "Func")
      , ("PA_DELTA becomes PA_DELTA'", toASCII (PA_DELTA BT_EMPTY), PA_DELTA' BT_EMPTY)
      ,
        ( "PA_META_LAMBDA becomes PA_META_LAMBDA' with head F'"
        , toASCII (PA_META_LAMBDA (META NO_EXCL F "fn"))
        , PA_META_LAMBDA' (META EXCL F' "fn")
        )
      ,
        ( "PA_META_DELTA becomes PA_META_DELTA' with head D'"
        , toASCII (PA_META_DELTA (META NO_EXCL D "dl"))
        , PA_META_DELTA' (META EXCL D' "dl")
        )
      ]
      (\(desc, actual, expected) -> it desc (actual `shouldBe` expected))

    it "leaves already-ASCII pairs untouched" $ do
      toASCII (PA_LAMBDA' "Func") `shouldBe` PA_LAMBDA' "Func"
      toASCII (PA_DELTA' BT_EMPTY) `shouldBe` PA_DELTA' BT_EMPTY

  describe "toASCII on ALPHA" $ do
    it "recurses through AL_IDX" $ toASCII (AL_IDX ALPHA 7) `shouldBe` AL_IDX ALPHA' 7
    it "recurses through AL_META" $ toASCII (AL_META ALPHA (META NO_EXCL I "abc")) `shouldBe` AL_META ALPHA' (META EXCL I' "abc")

  describe "toASCII on ATTRIBUTE" $ do
    forM_
      [ ("AT_PHI becomes AT", toASCII (AT_PHI PHI), AT_PHI AT)
      , ("AT_RHO becomes CARET", toASCII (AT_RHO RHO), AT_RHO CARET)
      , ("AT_META forces the head to A", toASCII (AT_META (META NO_EXCL TAU "abc")), AT_META (META EXCL A "abc"))
      ]
      (\(desc, actual, expected) -> it desc (actual `shouldBe` expected))

    it "leaves every other attribute untouched" $ do
      toASCII (AT_LABEL "x") `shouldBe` AT_LABEL "x"
      toASCII (AT_LAMBDA LAMBDA) `shouldBe` AT_LAMBDA LAMBDA
      toASCII (AT_DELTA DELTA) `shouldBe` AT_DELTA DELTA

  describe "toASCII on SET" $ do
    it "recurses through ST_BINDING" $ toASCII (ST_BINDING biPair) `shouldBe` ST_BINDING biPairASCII
    it "maps toASCII over ST_ATTRIBUTES" $
      toASCII (ST_ATTRIBUTES [AT_PHI PHI, AT_RHO RHO]) `shouldBe` ST_ATTRIBUTES [AT_PHI AT, AT_RHO CARET]

  describe "toASCII on NUMBER" $
    forM_
      [ ("IDX_META forces the head to I'", toASCII (IDX_META (META NO_EXCL I "abc")), IDX_META (META EXCL I' "abc"))
      , ("recurses through LENGTH", toASCII (LENGTH biPair), LENGTH biPairASCII)
      , ("recurses through DOMAIN", toASCII (DOMAIN biPair), DOMAIN biPairASCII)
      , ("leaves LITERAL untouched", toASCII (LITERAL 5), LITERAL 5)
      ]
      (\(desc, actual, expected) -> it desc (actual `shouldBe` expected))

  describe "toASCII on COMPARABLE" $
    forM_
      [ ("recurses through CMP_ATTR", toASCII (CMP_ATTR (AT_PHI PHI)), CMP_ATTR (AT_PHI AT))
      , ("recurses through CMP_EXPR", toASCII (CMP_EXPR leafExpr), CMP_EXPR leafExprASCII)
      , ("recurses through CMP_NUM", toASCII (CMP_NUM (LENGTH biPair)), CMP_NUM (LENGTH biPairASCII))
      ]
      (\(desc, actual, expected) -> it desc (actual `shouldBe` expected))

  describe "toASCII on CONDITION" $
    forM_
      [
        ( "recurses through CO_BELONGS"
        , toASCII (CO_BELONGS (AT_PHI PHI) IN (ST_BINDING biPair))
        , CO_BELONGS (AT_PHI AT) IN (ST_BINDING biPairASCII)
        )
      ,
        ( "maps toASCII over CO_LOGIC"
        , toASCII (CO_LOGIC [CO_NF leafExpr, CO_EMPTY] AND)
        , CO_LOGIC [CO_NF leafExprASCII, CO_EMPTY] AND
        )
      , ("recurses through CO_NF", toASCII (CO_NF leafExpr), CO_NF leafExprASCII)
      , ("recurses through CO_ABSOLUTE", toASCII (CO_ABSOLUTE leafExpr IN), CO_ABSOLUTE leafExprASCII IN)
      , ("recurses through CO_NOT", toASCII (CO_NOT (CO_NF leafExpr)), CO_NOT (CO_NF leafExprASCII))
      ,
        ( "recurses through CO_COMPARE"
        , toASCII (CO_COMPARE (CMP_ATTR (AT_PHI PHI)) EQUAL (CMP_EXPR leafExpr))
        , CO_COMPARE (CMP_ATTR (AT_PHI AT)) EQUAL (CMP_EXPR leafExprASCII)
        )
      , ("recurses through CO_MATCHES", toASCII (CO_MATCHES "abc" leafExpr), CO_MATCHES "abc" leafExprASCII)
      , ("recurses through CO_PART_OF", toASCII (CO_PART_OF leafExpr biPair), CO_PART_OF leafExprASCII biPairASCII)
      ,
        ( "maps toASCII over CO_DISJOINT"
        , toASCII (CO_DISJOINT [AT_PHI PHI] [biPair])
        , CO_DISJOINT [AT_PHI AT] [biPairASCII]
        )
      , ("recurses through CO_FORMATION", toASCII (CO_FORMATION leafExpr), CO_FORMATION leafExprASCII)
      , ("leaves CO_EMPTY untouched", toASCII CO_EMPTY, CO_EMPTY)
      ]
      (\(desc, actual, expected) -> it desc (actual `shouldBe` expected))

  describe "toASCII on EXTRA_ARG" $
    forM_
      [ ("recurses through ARG_ATTR", toASCII (ARG_ATTR (AT_PHI PHI)), ARG_ATTR (AT_PHI AT))
      , ("recurses through ARG_EXPR", toASCII (ARG_EXPR leafExpr), ARG_EXPR leafExprASCII)
      , ("recurses through ARG_BINDING", toASCII (ARG_BINDING biPair), ARG_BINDING biPairASCII)
      , ("leaves ARG_BYTES untouched", toASCII (ARG_BYTES BT_EMPTY), ARG_BYTES BT_EMPTY)
      ]
      (\(desc, actual, expected) -> it desc (actual `shouldBe` expected))

  describe "toASCII on EXTRA" $
    it "recurses through meta and every arg, keeping func untouched" $
      toASCII (EXTRA (ARG_ATTR (AT_PHI PHI)) "func" [ARG_EXPR leafExpr, ARG_BYTES BT_EMPTY])
        `shouldBe` EXTRA (ARG_ATTR (AT_PHI AT)) "func" [ARG_EXPR leafExprASCII, ARG_BYTES BT_EMPTY]

  describe "withEncoding" $ do
    it "UNICODE is the identity" $ withEncoding UNICODE biPair `shouldBe` biPair
    it "ASCII is toASCII" $ withEncoding ASCII biPair `shouldBe` toASCII biPair
