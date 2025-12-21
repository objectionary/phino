-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

{- | Tests for the Lining module that converts CST to single-line format.
The module provides functions to remove line breaks and indentation from
phi-calculus concrete syntax trees while preserving structural semantics.
-}
module LiningSpec (spec) where

import CST
import Control.Monad (forM_)
import Lining (LineFormat (..), toSingleLine, withLineFormat)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

spec :: Spec
spec = do
  describe "withLineFormat preserves multiline programs" $
    it "returns program unchanged" $
      let prog = PR_SWEET LCB (EX_GLOBAL Φ) RCB
       in withLineFormat MULTILINE prog `shouldBe` prog

  describe "withLineFormat converts to singleline" $
    it "applies toSingleLine transformation" $
      let prog = PR_SWEET LCB (EX_FORMATION LSB EOL (TAB 1) (BI_EMPTY (TAB 1)) EOL (TAB 0) RSB) RCB
          result = withLineFormat SINGLELINE prog
       in result `shouldBe` PR_SWEET LCB (EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY (TAB 1)) NO_EOL NO_TAB RSB) RCB

  describe "LineFormat Eq instance" $
    forM_
      [ ("SINGLELINE equals SINGLELINE", SINGLELINE, SINGLELINE, True)
      , ("MULTILINE equals MULTILINE", MULTILINE, MULTILINE, True)
      , ("SINGLELINE differs from MULTILINE", SINGLELINE, MULTILINE, False)
      ]
      ( \(desc, fmt1, fmt2, expected) ->
          it desc $
            if expected
              then fmt1 `shouldBe` fmt2
              else fmt1 `shouldNotBe` fmt2
      )

  describe "LineFormat Show instance" $
    forM_
      [ ("SINGLELINE shows correctly", SINGLELINE, "SINGLELINE")
      , ("MULTILINE shows correctly", MULTILINE, "MULTILINE")
      ]
      ( \(desc, fmt, expected) ->
          it desc (show fmt `shouldBe` expected)
      )

  describe "toSingleLine PROGRAM for PR_SWEET" $
    it "removes newlines from expression" $
      let prog = PR_SWEET LCB (EX_FORMATION LSB EOL (TAB 1) (BI_EMPTY (TAB 1)) EOL (TAB 0) RSB) RCB
       in toSingleLine prog `shouldBe` PR_SWEET LCB (EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY (TAB 1)) NO_EOL NO_TAB RSB) RCB

  describe "toSingleLine PROGRAM for PR_SALTY" $
    it "converts salty program to singleline" $
      let prog = PR_SALTY Φ ARROW (EX_FORMATION LSB EOL (TAB 1) (BI_EMPTY (TAB 1)) EOL (TAB 0) RSB)
       in toSingleLine prog `shouldBe` PR_SALTY Φ ARROW (EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY (TAB 1)) NO_EOL NO_TAB RSB)

  describe "toSingleLine EXPRESSION for EX_FORMATION with empty binding" $
    it "removes tabs and newlines" $
      let ex = EX_FORMATION LSB EOL (TAB 1) (BI_EMPTY (TAB 1)) EOL (TAB 0) RSB
       in toSingleLine ex `shouldBe` EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY (TAB 1)) NO_EOL NO_TAB RSB

  describe "toSingleLine EXPRESSION for EX_FORMATION with bindings" $
    it "converts to singleline with TAB markers" $
      let bd = BI_PAIR (PA_TAU (AT_LABEL "x") ARROW (EX_GLOBAL Φ)) (BDS_EMPTY (TAB 1)) (TAB 1)
          ex = EX_FORMATION LSB EOL (TAB 1) bd EOL (TAB 0) RSB
          result = toSingleLine ex
       in result `shouldBe` EX_FORMATION LSB NO_EOL TAB' (BI_PAIR (PA_TAU (AT_LABEL "x") ARROW (EX_GLOBAL Φ)) (BDS_EMPTY (TAB 1)) TAB') NO_EOL TAB' RSB

  describe "toSingleLine EXPRESSION for EX_DISPATCH" $
    it "converts nested expression" $
      let ex = EX_DISPATCH (EX_FORMATION LSB EOL (TAB 1) (BI_EMPTY (TAB 1)) EOL (TAB 0) RSB) (AT_LABEL "attr")
       in toSingleLine ex `shouldBe` EX_DISPATCH (EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY (TAB 1)) NO_EOL NO_TAB RSB) (AT_LABEL "attr")

  describe "toSingleLine EXPRESSION for EX_APPLICATION" $
    it "converts with proper spacing" $
      let t = APP_BINDING (PA_TAU (AT_LABEL "y") ARROW (EX_GLOBAL Φ))
          ex = EX_APPLICATION (EX_GLOBAL Φ) EOL (TAB 1) t EOL (TAB 0) 1
       in toSingleLine ex `shouldBe` EX_APPLICATION (EX_GLOBAL Φ) NO_EOL TAB' (APP_BINDING (PA_TAU (AT_LABEL "y") ARROW (EX_GLOBAL Φ))) NO_EOL TAB' 1

  describe "toSingleLine EXPRESSION for EX_APPLICATION_TAUS" $
    it "converts taus application" $
      let ts = BI_PAIR (PA_TAU (AT_LABEL "z") ARROW (EX_GLOBAL Φ)) (BDS_EMPTY (TAB 1)) (TAB 1)
          ex = EX_APPLICATION_TAUS (EX_GLOBAL Φ) EOL (TAB 1) ts EOL (TAB 0) 1
       in toSingleLine ex `shouldBe` EX_APPLICATION_TAUS (EX_GLOBAL Φ) NO_EOL TAB' (BI_PAIR (PA_TAU (AT_LABEL "z") ARROW (EX_GLOBAL Φ)) (BDS_EMPTY (TAB 1)) TAB') NO_EOL TAB' 1

  describe "toSingleLine EXPRESSION for EX_APPLICATION_EXPRS" $
    it "converts expressions application" $
      let as = APP_ARG (EX_GLOBAL Φ) AAS_EMPTY
          ex = EX_APPLICATION_EXPRS (EX_GLOBAL Φ) EOL (TAB 1) as EOL (TAB 0) 1
       in toSingleLine ex `shouldBe` EX_APPLICATION_EXPRS (EX_GLOBAL Φ) NO_EOL TAB' (APP_ARG (EX_GLOBAL Φ) AAS_EMPTY) NO_EOL TAB' 1

  describe "toSingleLine EXPRESSION for EX_PHI_MEET" $
    it "converts meet expression" $
      let ex = EX_PHI_MEET Nothing 0 (EX_FORMATION LSB EOL (TAB 1) (BI_EMPTY (TAB 1)) EOL (TAB 0) RSB)
       in toSingleLine ex `shouldBe` EX_PHI_MEET Nothing 0 (EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY (TAB 1)) NO_EOL NO_TAB RSB)

  describe "toSingleLine EXPRESSION for EX_PHI_AGAIN" $
    it "converts again expression" $
      let ex = EX_PHI_AGAIN Nothing 1 (EX_FORMATION LSB EOL (TAB 1) (BI_EMPTY (TAB 1)) EOL (TAB 0) RSB)
       in toSingleLine ex `shouldBe` EX_PHI_AGAIN Nothing 1 (EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY (TAB 1)) NO_EOL NO_TAB RSB)

  describe "toSingleLine EXPRESSION leaves primitives unchanged" $
    forM_
      [ ("EX_GLOBAL", EX_GLOBAL Φ)
      , ("EX_XI", EX_XI XI)
      , ("EX_ATTR", EX_ATTR (AT_LABEL "attr"))
      , ("EX_TERMINATION", EX_TERMINATION DEAD)
      , ("EX_STRING", EX_STRING "тест" (TAB 0) [])
      , ("EX_NUMBER", EX_NUMBER (Left 42) (TAB 0) [])
      , ("EX_META", EX_META (MT_EXPRESSION "e"))
      , ("EX_META_TAIL", EX_META_TAIL (EX_GLOBAL Φ) (MT_TAIL "t"))
      ]
      ( \(desc, ex) ->
          it desc (toSingleLine ex `shouldBe` ex)
      )

  describe "toSingleLine APP_BINDING" $
    it "converts nested pair expression" $
      let bd = APP_BINDING (PA_TAU (AT_LABEL "x") ARROW (EX_FORMATION LSB EOL (TAB 1) (BI_EMPTY (TAB 1)) EOL (TAB 0) RSB))
       in toSingleLine bd `shouldBe` APP_BINDING (PA_TAU (AT_LABEL "x") ARROW (EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY (TAB 1)) NO_EOL NO_TAB RSB))

  describe "toSingleLine BINDING for BI_PAIR" $
    it "converts to singleline" $
      let bd = BI_PAIR (PA_TAU (AT_LABEL "x") ARROW (EX_GLOBAL Φ)) (BDS_EMPTY (TAB 1)) (TAB 1)
       in toSingleLine bd `shouldBe` BI_PAIR (PA_TAU (AT_LABEL "x") ARROW (EX_GLOBAL Φ)) (BDS_EMPTY (TAB 1)) TAB'

  describe "toSingleLine BINDING for BI_META" $
    it "converts meta binding" $
      let bd = BI_META (MT_BINDING "B") (BDS_EMPTY (TAB 1)) (TAB 1)
       in toSingleLine bd `shouldBe` BI_META (MT_BINDING "B") (BDS_EMPTY (TAB 1)) TAB'

  describe "toSingleLine BINDING for BI_EMPTY" $
    it "returns binding unchanged" $
      let bd = BI_EMPTY (TAB 1)
       in toSingleLine bd `shouldBe` bd

  describe "toSingleLine BINDINGS for BDS_PAIR" $
    it "converts to singleline" $
      let bds = BDS_PAIR EOL (TAB 1) (PA_TAU (AT_LABEL "y") ARROW (EX_GLOBAL Φ)) (BDS_EMPTY (TAB 1))
       in toSingleLine bds `shouldBe` BDS_PAIR NO_EOL TAB' (PA_TAU (AT_LABEL "y") ARROW (EX_GLOBAL Φ)) (BDS_EMPTY (TAB 1))

  describe "toSingleLine BINDINGS for BDS_META" $
    it "converts meta bindings" $
      let bds = BDS_META EOL (TAB 1) (MT_BINDING "B") (BDS_EMPTY (TAB 1))
       in toSingleLine bds `shouldBe` BDS_META NO_EOL TAB' (MT_BINDING "B") (BDS_EMPTY (TAB 1))

  describe "toSingleLine BINDINGS for BDS_EMPTY" $
    it "returns bindings unchanged" $
      let bds = BDS_EMPTY (TAB 1)
       in toSingleLine bds `shouldBe` bds

  describe "toSingleLine PAIR for PA_TAU" $
    it "converts expression" $
      let pr = PA_TAU (AT_LABEL "x") ARROW (EX_FORMATION LSB EOL (TAB 1) (BI_EMPTY (TAB 1)) EOL (TAB 0) RSB)
       in toSingleLine pr `shouldBe` PA_TAU (AT_LABEL "x") ARROW (EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY (TAB 1)) NO_EOL NO_TAB RSB)

  describe "toSingleLine PAIR for PA_FORMATION" $
    it "converts formation pair" $
      let pr = PA_FORMATION (AT_LABEL "f") [AT_LABEL "v"] ARROW (EX_FORMATION LSB EOL (TAB 1) (BI_EMPTY (TAB 1)) EOL (TAB 0) RSB)
       in toSingleLine pr `shouldBe` PA_FORMATION (AT_LABEL "f") [AT_LABEL "v"] ARROW (EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY (TAB 1)) NO_EOL NO_TAB RSB)

  describe "toSingleLine PAIR leaves non-expression pairs unchanged" $
    forM_
      [ ("PA_VOID", PA_VOID (AT_LABEL "v") ARROW EMPTY)
      , ("PA_LAMBDA", PA_LAMBDA "λфункция")
      , ("PA_LAMBDA'", PA_LAMBDA' "Function")
      , ("PA_META_LAMBDA", PA_META_LAMBDA (MT_FUNCTION "F"))
      , ("PA_META_LAMBDA'", PA_META_LAMBDA' (MT_FUNCTION "F"))
      , ("PA_DELTA", PA_DELTA (BT_ONE "FF"))
      , ("PA_DELTA'", PA_DELTA' (BT_MANY ["00", "01"]))
      , ("PA_META_DELTA", PA_META_DELTA (MT_BYTES "d"))
      , ("PA_META_DELTA'", PA_META_DELTA' (MT_BYTES "d"))
      ]
      ( \(desc, pr) ->
          it desc (toSingleLine pr `shouldBe` pr)
      )

  describe "toSingleLine APP_ARG" $
    it "converts expression and args" $
      let arg = APP_ARG (EX_FORMATION LSB EOL (TAB 1) (BI_EMPTY (TAB 1)) EOL (TAB 0) RSB) AAS_EMPTY
       in toSingleLine arg `shouldBe` APP_ARG (EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY (TAB 1)) NO_EOL NO_TAB RSB) AAS_EMPTY

  describe "toSingleLine APP_ARGS for AAS_EXPR" $
    it "converts nested expression" $
      let as = AAS_EXPR EOL (TAB 1) (EX_FORMATION LSB EOL (TAB 1) (BI_EMPTY (TAB 1)) EOL (TAB 0) RSB) AAS_EMPTY
       in toSingleLine as `shouldBe` AAS_EXPR NO_EOL TAB' (EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY (TAB 1)) NO_EOL NO_TAB RSB) AAS_EMPTY

  describe "toSingleLine APP_ARGS for AAS_EMPTY" $
    it "returns unchanged" $
      toSingleLine AAS_EMPTY `shouldBe` AAS_EMPTY
