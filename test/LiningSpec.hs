{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module LiningSpec where

import CST
import Control.Monad (forM_)
import Lining (LineFormat (..), toSingleLine, withLineFormat)
import Test.Hspec (Spec, describe, it, shouldBe)

leafExpr :: EXPRESSION
leafExpr = EX_GLOBAL Φ

multilineFormation :: EXPRESSION
multilineFormation =
  EX_FORMATION LSB EOL (TAB 1) (BI_PAIR (PA_TAU (AT_LABEL "x") ARROW leafExpr) (BDS_EMPTY (TAB 1)) (TAB 1)) EOL (TAB 0) RSB

singlelineFormation :: EXPRESSION
singlelineFormation =
  EX_FORMATION LSB NO_EOL TAB' (BI_PAIR (PA_TAU (AT_LABEL "x") ARROW leafExpr) (BDS_EMPTY (TAB 1)) TAB') NO_EOL TAB' RSB

biPair :: BINDING
biPair = BI_PAIR (PA_TAU (AT_LABEL "x") ARROW leafExpr) (BDS_PAIR EOL (TAB 1) (PA_TAU (AT_LABEL "y") ARROW leafExpr) (BDS_EMPTY (TAB 1))) (TAB 1)

expectedBiPair :: BINDING
expectedBiPair = BI_PAIR (PA_TAU (AT_LABEL "x") ARROW leafExpr) (BDS_PAIR NO_EOL TAB' (PA_TAU (AT_LABEL "y") ARROW leafExpr) (BDS_EMPTY (TAB 1))) TAB'

spec :: Spec
spec = do
  describe "toSingleLine on EXPRESSION" $ do
    forM_
      [ ("collapses a multiline formation into one line", multilineFormation, singlelineFormation)
      ,
        ( "keeps the BI_EMPTY special case, only touching the outer tabs"
        , EX_FORMATION LSB EOL (TAB 1) (BI_EMPTY (TAB 1)) EOL (TAB 0) RSB
        , EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY (TAB 1)) NO_EOL NO_TAB RSB
        )
      ,
        ( "recurses through EX_DISPATCH"
        , EX_DISPATCH multilineFormation NO_SPACE (AT_LABEL "y")
        , EX_DISPATCH singlelineFormation NO_SPACE (AT_LABEL "y")
        )
      ,
        ( "recurses through EX_APPLICATION"
        , EX_APPLICATION multilineFormation NO_SPACE EOL (TAB 1) (AA_TAUS biPair) EOL (TAB 0) 1
        , EX_APPLICATION singlelineFormation NO_SPACE NO_EOL TAB' (AA_TAUS expectedBiPair) NO_EOL TAB' 1
        )
      , ("recurses through EX_PHI_MEET", EX_PHI_MEET (Just "p") 3 multilineFormation, EX_PHI_MEET (Just "p") 3 singlelineFormation)
      , ("recurses through EX_PHI_AGAIN", EX_PHI_AGAIN Nothing 4 multilineFormation, EX_PHI_AGAIN Nothing 4 singlelineFormation)
      ]
      (\(desc, node, expected) -> it desc (toSingleLine node `shouldBe` expected))

    it "leaves every other constructor untouched" $
      forM_
        [ EX_GLOBAL Φ
        , EX_XI XI
        , EX_ATTR (AT_LABEL "z")
        , EX_TERMINATION DEAD
        , EX_STRING "hi" (TAB 2) []
        , EX_NUMBER (Left 5) (TAB 2) []
        , EX_META (META NO_EXCL E "x")
        , EX_BYTES BT_EMPTY
        ]
        (\node -> toSingleLine node `shouldBe` node)

  describe "toSingleLine on BINDING" $
    forM_
      [ ("recurses through BI_PAIR, forcing TAB'", biPair, expectedBiPair)
      ,
        ( "recurses through BI_META, keeping the meta untouched"
        , BI_META (META NO_EXCL B "X") (BDS_EMPTY (TAB 1)) (TAB 1)
        , BI_META (META NO_EXCL B "X") (BDS_EMPTY (TAB 1)) TAB'
        )
      , ("leaves BI_EMPTY untouched", BI_EMPTY (TAB 1), BI_EMPTY (TAB 1))
      ]
      (\(desc, node, expected) -> it desc (toSingleLine node `shouldBe` expected))

  describe "toSingleLine on BINDINGS" $
    forM_
      [
        ( "recurses through BDS_PAIR, forcing TAB'"
        , BDS_PAIR EOL (TAB 1) (PA_TAU (AT_LABEL "x") ARROW leafExpr) (BDS_EMPTY (TAB 1))
        , BDS_PAIR NO_EOL TAB' (PA_TAU (AT_LABEL "x") ARROW leafExpr) (BDS_EMPTY (TAB 1))
        )
      ,
        ( "recurses through BDS_META, keeping the meta untouched"
        , BDS_META EOL (TAB 1) (META NO_EXCL B "X") (BDS_EMPTY (TAB 1))
        , BDS_META NO_EOL TAB' (META NO_EXCL B "X") (BDS_EMPTY (TAB 1))
        )
      , ("leaves BDS_EMPTY untouched", BDS_EMPTY (TAB 1), BDS_EMPTY (TAB 1))
      ]
      (\(desc, node, expected) -> it desc (toSingleLine node `shouldBe` expected))

  describe "toSingleLine on PAIR" $
    forM_
      [
        ( "recurses through PA_TAU"
        , PA_TAU (AT_LABEL "x") ARROW multilineFormation
        , PA_TAU (AT_LABEL "x") ARROW singlelineFormation
        )
      ,
        ( "recurses through PA_ALPHA"
        , PA_ALPHA (AL_IDX ALPHA 0) ARROW multilineFormation
        , PA_ALPHA (AL_IDX ALPHA 0) ARROW singlelineFormation
        )
      ,
        ( "recurses through PA_FORMATION"
        , PA_FORMATION (AT_LABEL "x") [AT_RHO RHO] ARROW multilineFormation
        , PA_FORMATION (AT_LABEL "x") [AT_RHO RHO] ARROW singlelineFormation
        )
      , ("leaves every other constructor untouched", PA_VOID (AT_LABEL "x") ARROW EMPTY, PA_VOID (AT_LABEL "x") ARROW EMPTY)
      ]
      (\(desc, node, expected) -> it desc (toSingleLine node `shouldBe` expected))

  describe "toSingleLine on APP_BINDING" $
    it "recurses into the pair" $
      toSingleLine (APP_BINDING (PA_TAU (AT_LABEL "x") ARROW multilineFormation))
        `shouldBe` APP_BINDING (PA_TAU (AT_LABEL "x") ARROW singlelineFormation)

  describe "toSingleLine on APP_ARGUMENT" $
    forM_
      [
        ( "recurses through AA_TAU"
        , AA_TAU (APP_BINDING (PA_TAU (AT_LABEL "x") ARROW multilineFormation))
        , AA_TAU (APP_BINDING (PA_TAU (AT_LABEL "x") ARROW singlelineFormation))
        )
      , ("recurses through AA_TAUS", AA_TAUS biPair, AA_TAUS expectedBiPair)
      ,
        ( "recurses through AA_EXPRS"
        , AA_EXPRS (APP_ARG multilineFormation AAS_EMPTY)
        , AA_EXPRS (APP_ARG singlelineFormation AAS_EMPTY)
        )
      ]
      (\(desc, node, expected) -> it desc (toSingleLine node `shouldBe` expected))

  describe "toSingleLine on APP_ARG" $
    it "recurses through both fields" $ do
      let appArg = APP_ARG multilineFormation (AAS_EXPR EOL (TAB 1) leafExpr AAS_EMPTY)
          expected = APP_ARG singlelineFormation (AAS_EXPR NO_EOL TAB' leafExpr AAS_EMPTY)
      toSingleLine appArg `shouldBe` expected

  describe "toSingleLine on APP_ARGS" $
    forM_
      [
        ( "recurses through AAS_EXPR"
        , AAS_EXPR EOL (TAB 1) multilineFormation AAS_EMPTY
        , AAS_EXPR NO_EOL TAB' singlelineFormation AAS_EMPTY
        )
      , ("leaves AAS_EMPTY untouched", AAS_EMPTY, AAS_EMPTY)
      ]
      (\(desc, node, expected) -> it desc (toSingleLine node `shouldBe` expected))

  describe "toSingleLine on SET" $
    forM_
      [ ("recurses through ST_BINDING", ST_BINDING biPair, ST_BINDING expectedBiPair)
      , ("leaves ST_ATTRIBUTES untouched", ST_ATTRIBUTES [AT_LABEL "x"], ST_ATTRIBUTES [AT_LABEL "x"])
      ]
      (\(desc, node, expected) -> it desc (toSingleLine node `shouldBe` expected))

  describe "toSingleLine on NUMBER" $ do
    forM_
      [ ("recurses through LENGTH", LENGTH biPair, LENGTH expectedBiPair)
      , ("recurses through DOMAIN", DOMAIN biPair, DOMAIN expectedBiPair)
      ]
      (\(desc, node, expected) -> it desc (toSingleLine node `shouldBe` expected))

    it "leaves IDX_META and LITERAL untouched" $ do
      let idxMeta = IDX_META (META NO_EXCL I "x")
          literalNum = LITERAL 5
      toSingleLine idxMeta `shouldBe` idxMeta
      toSingleLine literalNum `shouldBe` literalNum

  describe "toSingleLine on COMPARABLE" $
    forM_
      [ ("leaves CMP_ATTR untouched", CMP_ATTR (AT_LABEL "x"), CMP_ATTR (AT_LABEL "x"))
      , ("recurses through CMP_EXPR", CMP_EXPR multilineFormation, CMP_EXPR singlelineFormation)
      , ("recurses through CMP_NUM", CMP_NUM (LENGTH biPair), CMP_NUM (LENGTH expectedBiPair))
      ]
      (\(desc, node, expected) -> it desc (toSingleLine node `shouldBe` expected))

  describe "toSingleLine on CONDITION" $ do
    let stBinding = ST_BINDING biPair
        coBelongs = CO_BELONGS (AT_LABEL "x") IN stBinding
    forM_
      [ ("recurses through CO_BELONGS", coBelongs, CO_BELONGS (AT_LABEL "x") IN (ST_BINDING expectedBiPair))
      ,
        ( "recurses through every condition in CO_LOGIC"
        , CO_LOGIC [coBelongs, CO_EMPTY] AND
        , CO_LOGIC [CO_BELONGS (AT_LABEL "x") IN (ST_BINDING expectedBiPair), CO_EMPTY] AND
        )
      , ("recurses through CO_NF", CO_NF multilineFormation, CO_NF singlelineFormation)
      , ("recurses through CO_ABSOLUTE", CO_ABSOLUTE multilineFormation IN, CO_ABSOLUTE singlelineFormation IN)
      , ("recurses through CO_NOT", CO_NOT coBelongs, CO_NOT (CO_BELONGS (AT_LABEL "x") IN (ST_BINDING expectedBiPair)))
      ,
        ( "recurses through CO_COMPARE"
        , CO_COMPARE (CMP_ATTR (AT_LABEL "x")) EQUAL (CMP_EXPR multilineFormation)
        , CO_COMPARE (CMP_ATTR (AT_LABEL "x")) EQUAL (CMP_EXPR singlelineFormation)
        )
      , ("recurses through CO_MATCHES", CO_MATCHES "abc" multilineFormation, CO_MATCHES "abc" singlelineFormation)
      , ("recurses through CO_PART_OF", CO_PART_OF multilineFormation biPair, CO_PART_OF singlelineFormation expectedBiPair)
      ,
        ( "recurses through every group in CO_DISJOINT"
        , CO_DISJOINT [AT_LABEL "x"] [biPair]
        , CO_DISJOINT [AT_LABEL "x"] [expectedBiPair]
        )
      , ("recurses through CO_FORMATION", CO_FORMATION multilineFormation, CO_FORMATION singlelineFormation)
      , ("leaves CO_EMPTY untouched", CO_EMPTY, CO_EMPTY)
      ]
      (\(desc, node, expected) -> it desc (toSingleLine node `shouldBe` expected))

  describe "toSingleLine on EXTRA_ARG" $
    forM_
      [ ("recurses through ARG_EXPR", ARG_EXPR multilineFormation, ARG_EXPR singlelineFormation)
      , ("recurses through ARG_BINDING", ARG_BINDING biPair, ARG_BINDING expectedBiPair)
      , ("leaves ARG_ATTR untouched", ARG_ATTR (AT_LABEL "x"), ARG_ATTR (AT_LABEL "x"))
      , ("leaves ARG_BYTES untouched", ARG_BYTES BT_EMPTY, ARG_BYTES BT_EMPTY)
      ]
      (\(desc, node, expected) -> it desc (toSingleLine node `shouldBe` expected))

  describe "toSingleLine on EXTRA" $
    it "recurses through meta and every arg, keeping func untouched" $ do
      let extra = EXTRA (ARG_EXPR multilineFormation) "func" [ARG_ATTR (AT_LABEL "x"), ARG_BYTES BT_EMPTY]
          expected = EXTRA (ARG_EXPR singlelineFormation) "func" [ARG_ATTR (AT_LABEL "x"), ARG_BYTES BT_EMPTY]
      toSingleLine extra `shouldBe` expected

  describe "withLineFormat" $ do
    it "MULTILINE is the identity" $
      withLineFormat MULTILINE multilineFormation `shouldBe` multilineFormation
    it "SINGLELINE is toSingleLine" $
      withLineFormat SINGLELINE multilineFormation `shouldBe` toSingleLine multilineFormation
