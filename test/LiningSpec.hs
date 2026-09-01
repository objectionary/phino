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
    it "collapses a multiline formation into one line" $
      toSingleLine multilineFormation `shouldBe` singlelineFormation

    it "keeps the BI_EMPTY special case, only touching the outer tabs" $ do
      let emptyFormation = EX_FORMATION LSB EOL (TAB 1) (BI_EMPTY (TAB 1)) EOL (TAB 0) RSB
          expected = EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY (TAB 1)) NO_EOL NO_TAB RSB
      toSingleLine emptyFormation `shouldBe` expected

    it "recurses through EX_DISPATCH" $
      toSingleLine (EX_DISPATCH multilineFormation NO_SPACE (AT_LABEL "y"))
        `shouldBe` EX_DISPATCH singlelineFormation NO_SPACE (AT_LABEL "y")

    it "recurses through EX_APPLICATION" $ do
      let app = EX_APPLICATION multilineFormation NO_SPACE EOL (TAB 1) (AA_TAUS biPair) EOL (TAB 0) 1
          expected = EX_APPLICATION singlelineFormation NO_SPACE NO_EOL TAB' (AA_TAUS expectedBiPair) NO_EOL TAB' 1
      toSingleLine app `shouldBe` expected

    it "recurses through EX_PHI_MEET" $
      toSingleLine (EX_PHI_MEET (Just "p") 3 multilineFormation) `shouldBe` EX_PHI_MEET (Just "p") 3 singlelineFormation

    it "recurses through EX_PHI_AGAIN" $
      toSingleLine (EX_PHI_AGAIN Nothing 4 multilineFormation) `shouldBe` EX_PHI_AGAIN Nothing 4 singlelineFormation

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

  describe "toSingleLine on BINDING" $ do
    it "recurses through BI_PAIR, forcing TAB'" $
      toSingleLine biPair `shouldBe` expectedBiPair

    it "recurses through BI_META, keeping the meta untouched" $ do
      let biMeta = BI_META (META NO_EXCL B "X") (BDS_EMPTY (TAB 1)) (TAB 1)
          expected = BI_META (META NO_EXCL B "X") (BDS_EMPTY (TAB 1)) TAB'
      toSingleLine biMeta `shouldBe` expected

    it "leaves BI_EMPTY untouched" $ do
      let biEmpty = BI_EMPTY (TAB 1)
      toSingleLine biEmpty `shouldBe` biEmpty

  describe "toSingleLine on BINDINGS" $ do
    it "recurses through BDS_PAIR, forcing TAB'" $ do
      let bdsPair = BDS_PAIR EOL (TAB 1) (PA_TAU (AT_LABEL "x") ARROW leafExpr) (BDS_EMPTY (TAB 1))
          expected = BDS_PAIR NO_EOL TAB' (PA_TAU (AT_LABEL "x") ARROW leafExpr) (BDS_EMPTY (TAB 1))
      toSingleLine bdsPair `shouldBe` expected

    it "recurses through BDS_META, keeping the meta untouched" $ do
      let bdsMeta = BDS_META EOL (TAB 1) (META NO_EXCL B "X") (BDS_EMPTY (TAB 1))
          expected = BDS_META NO_EOL TAB' (META NO_EXCL B "X") (BDS_EMPTY (TAB 1))
      toSingleLine bdsMeta `shouldBe` expected

    it "leaves BDS_EMPTY untouched" $ do
      let bdsEmpty = BDS_EMPTY (TAB 1)
      toSingleLine bdsEmpty `shouldBe` bdsEmpty

  describe "toSingleLine on PAIR" $ do
    it "recurses through PA_TAU" $
      toSingleLine (PA_TAU (AT_LABEL "x") ARROW multilineFormation) `shouldBe` PA_TAU (AT_LABEL "x") ARROW singlelineFormation

    it "recurses through PA_ALPHA" $
      toSingleLine (PA_ALPHA (AL_IDX ALPHA 0) ARROW multilineFormation)
        `shouldBe` PA_ALPHA (AL_IDX ALPHA 0) ARROW singlelineFormation

    it "recurses through PA_FORMATION" $
      toSingleLine (PA_FORMATION (AT_LABEL "x") [AT_RHO RHO] ARROW multilineFormation)
        `shouldBe` PA_FORMATION (AT_LABEL "x") [AT_RHO RHO] ARROW singlelineFormation

    it "leaves every other constructor untouched" $ do
      let paVoid = PA_VOID (AT_LABEL "x") ARROW EMPTY
      toSingleLine paVoid `shouldBe` paVoid

  describe "toSingleLine on APP_BINDING" $
    it "recurses into the pair" $
      toSingleLine (APP_BINDING (PA_TAU (AT_LABEL "x") ARROW multilineFormation))
        `shouldBe` APP_BINDING (PA_TAU (AT_LABEL "x") ARROW singlelineFormation)

  describe "toSingleLine on APP_ARGUMENT" $ do
    it "recurses through AA_TAU" $
      toSingleLine (AA_TAU (APP_BINDING (PA_TAU (AT_LABEL "x") ARROW multilineFormation)))
        `shouldBe` AA_TAU (APP_BINDING (PA_TAU (AT_LABEL "x") ARROW singlelineFormation))

    it "recurses through AA_TAUS" $
      toSingleLine (AA_TAUS biPair) `shouldBe` AA_TAUS expectedBiPair

    it "recurses through AA_EXPRS" $
      toSingleLine (AA_EXPRS (APP_ARG multilineFormation AAS_EMPTY))
        `shouldBe` AA_EXPRS (APP_ARG singlelineFormation AAS_EMPTY)

  describe "toSingleLine on APP_ARG" $
    it "recurses through both fields" $ do
      let appArg = APP_ARG multilineFormation (AAS_EXPR EOL (TAB 1) leafExpr AAS_EMPTY)
          expected = APP_ARG singlelineFormation (AAS_EXPR NO_EOL TAB' leafExpr AAS_EMPTY)
      toSingleLine appArg `shouldBe` expected

  describe "toSingleLine on APP_ARGS" $ do
    it "recurses through AAS_EXPR" $
      toSingleLine (AAS_EXPR EOL (TAB 1) multilineFormation AAS_EMPTY)
        `shouldBe` AAS_EXPR NO_EOL TAB' singlelineFormation AAS_EMPTY

    it "leaves AAS_EMPTY untouched" $
      toSingleLine AAS_EMPTY `shouldBe` AAS_EMPTY

  describe "toSingleLine on SET" $ do
    it "recurses through ST_BINDING" $
      toSingleLine (ST_BINDING biPair) `shouldBe` ST_BINDING expectedBiPair

    it "leaves ST_ATTRIBUTES untouched" $ do
      let stAttrs = ST_ATTRIBUTES [AT_LABEL "x"]
      toSingleLine stAttrs `shouldBe` stAttrs

  describe "toSingleLine on NUMBER" $ do
    it "recurses through LENGTH" $
      toSingleLine (LENGTH biPair) `shouldBe` LENGTH expectedBiPair

    it "recurses through DOMAIN" $
      toSingleLine (DOMAIN biPair) `shouldBe` DOMAIN expectedBiPair

    it "leaves IDX_META and LITERAL untouched" $ do
      let idxMeta = IDX_META (META NO_EXCL I "x")
          literalNum = LITERAL 5
      toSingleLine idxMeta `shouldBe` idxMeta
      toSingleLine literalNum `shouldBe` literalNum

  describe "toSingleLine on COMPARABLE" $ do
    it "leaves CMP_ATTR untouched" $ do
      let cmpAttr = CMP_ATTR (AT_LABEL "x")
      toSingleLine cmpAttr `shouldBe` cmpAttr

    it "recurses through CMP_EXPR" $
      toSingleLine (CMP_EXPR multilineFormation) `shouldBe` CMP_EXPR singlelineFormation

    it "recurses through CMP_NUM" $
      toSingleLine (CMP_NUM (LENGTH biPair)) `shouldBe` CMP_NUM (LENGTH expectedBiPair)

  describe "toSingleLine on CONDITION" $ do
    let stBinding = ST_BINDING biPair
        coBelongs = CO_BELONGS (AT_LABEL "x") IN stBinding
    it "recurses through CO_BELONGS" $
      toSingleLine coBelongs `shouldBe` CO_BELONGS (AT_LABEL "x") IN (ST_BINDING expectedBiPair)

    it "recurses through every condition in CO_LOGIC" $
      toSingleLine (CO_LOGIC [coBelongs, CO_EMPTY] AND)
        `shouldBe` CO_LOGIC [CO_BELONGS (AT_LABEL "x") IN (ST_BINDING expectedBiPair), CO_EMPTY] AND

    it "recurses through CO_NF" $
      toSingleLine (CO_NF multilineFormation) `shouldBe` CO_NF singlelineFormation

    it "recurses through CO_ABSOLUTE" $
      toSingleLine (CO_ABSOLUTE multilineFormation IN) `shouldBe` CO_ABSOLUTE singlelineFormation IN

    it "recurses through CO_NOT" $
      toSingleLine (CO_NOT coBelongs) `shouldBe` CO_NOT (CO_BELONGS (AT_LABEL "x") IN (ST_BINDING expectedBiPair))

    it "recurses through CO_COMPARE" $
      toSingleLine (CO_COMPARE (CMP_ATTR (AT_LABEL "x")) EQUAL (CMP_EXPR multilineFormation))
        `shouldBe` CO_COMPARE (CMP_ATTR (AT_LABEL "x")) EQUAL (CMP_EXPR singlelineFormation)

    it "recurses through CO_MATCHES" $
      toSingleLine (CO_MATCHES "abc" multilineFormation) `shouldBe` CO_MATCHES "abc" singlelineFormation

    it "recurses through CO_PART_OF" $
      toSingleLine (CO_PART_OF multilineFormation biPair) `shouldBe` CO_PART_OF singlelineFormation expectedBiPair

    it "recurses through every group in CO_DISJOINT" $
      toSingleLine (CO_DISJOINT [AT_LABEL "x"] [biPair]) `shouldBe` CO_DISJOINT [AT_LABEL "x"] [expectedBiPair]

    it "recurses through CO_FORMATION" $
      toSingleLine (CO_FORMATION multilineFormation) `shouldBe` CO_FORMATION singlelineFormation

    it "leaves CO_EMPTY untouched" $
      toSingleLine CO_EMPTY `shouldBe` CO_EMPTY

  describe "toSingleLine on EXTRA_ARG" $ do
    it "recurses through ARG_EXPR" $
      toSingleLine (ARG_EXPR multilineFormation) `shouldBe` ARG_EXPR singlelineFormation

    it "recurses through ARG_BINDING" $
      toSingleLine (ARG_BINDING biPair) `shouldBe` ARG_BINDING expectedBiPair

    it "leaves ARG_ATTR untouched" $ do
      let argAttr = ARG_ATTR (AT_LABEL "x")
      toSingleLine argAttr `shouldBe` argAttr

    it "leaves ARG_BYTES untouched" $ do
      let argBytes = ARG_BYTES BT_EMPTY
      toSingleLine argBytes `shouldBe` argBytes

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
