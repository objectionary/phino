{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module RenderSpec (spec) where

import CST
import Control.Monad (forM_)
import qualified Data.Text as T
import Render (Render (render))
import Test.Hspec

xiExpr :: EXPRESSION
xiExpr = EX_XI XI

rootExpr :: EXPRESSION
rootExpr = EX_GLOBAL Φ

pairXi :: T.Text -> PAIR
pairXi attrLabel = PA_TAU (AT_LABEL attrLabel) ARROW xiExpr

bindingXi :: T.Text -> BINDING
bindingXi attrLabel = BI_PAIR (pairXi attrLabel) (BDS_EMPTY NO_TAB) NO_TAB

spec :: Spec
spec = do
  describe "render primitive wrapper instances" $ do
    it "String" (render ("hi" :: String) `shouldBe` "hi")
    it "Char" (render 'z' `shouldBe` "z")
    it "Int" (render (7 :: Int) `shouldBe` "7")

  describe "render braces, comma and arrows" $ do
    it "LCB" (render LCB `shouldBe` "{")
    it "BIG_LCB" (render BIG_LCB `shouldBe` "\\Big\\{")
    it "RCB" (render RCB `shouldBe` "}")
    it "BIG_RCB" (render BIG_RCB `shouldBe` "\\Big\\}")
    it "LSB'" (render LSB' `shouldBe` "[[")
    it "RSB'" (render RSB' `shouldBe` "]]")
    it "COMMA" (render COMMA `shouldBe` ",")
    it "NO_COMMA" (render NO_COMMA `shouldBe` "")
    it "ARROW'" (render ARROW' `shouldBe` "->")
    it "DASHED_ARROW" (render DASHED_ARROW `shouldBe` "⤍")
    it "QUESTION" (render QUESTION `shouldBe` "?")
    it "AT" (render AT `shouldBe` "@")
    it "CARET" (render CARET `shouldBe` "^")
    it "RHO'" (render RHO' `shouldBe` "\\phiTerminal{\\rho}")
    it "DELTA'" (render DELTA' `shouldBe` "D")
    it "LAMBDA'" (render LAMBDA' `shouldBe` "L")
    it "Q" (render Q `shouldBe` "Q")
    it "DEAD" (render DEAD `shouldBe` "⊥")
    it "SPACE" (render SPACE `shouldBe` " ")
    it "DOTS" (render DOTS `shouldBe` "...")
    it "DOTS'" (render DOTS' `shouldBe` "\\dots")

  describe "render BYTES" $
    forM_
      [ ("empty", BT_EMPTY, "--")
      , ("one", BT_ONE "1F", "1F-")
      , ("many", BT_MANY ["00", "01", "02"], "00-01-02")
      , ("meta", BT_META (META NO_EXCL D "1"), "δ1")
      , ("piped", BT_PIPED (BT_ONE "1F"), "|1F-|")
      ]
      (\(desc, bts, expected) -> it desc (render bts `shouldBe` expected))

  describe "render every META_HEAD" $
    forM_
      [ (E, "𝑒")
      , (E', "e")
      , (N, "𝑛")
      , (N', "n")
      , (K, "𝑘")
      , (K', "k")
      , (A, "t")
      , (TAU, "𝜏")
      , (TAU', "\\tau")
      , (I, "𝑖")
      , (I', "i")
      , (B, "𝐵")
      , (B', "B")
      , (D, "δ")
      , (D', "\\delta")
      , (F, "𝑓")
      , (F', "F")
      ]
      (\(metaHead, expected) -> it (show metaHead) (render (META NO_EXCL metaHead "") `shouldBe` expected))

  describe "render META" $ do
    it "without exclamation" (render (META NO_EXCL E "x") `shouldBe` "𝑒x")
    it "with exclamation" (render (META EXCL E' "42") `shouldBe` "!e42")

  describe "render ALPHA'" $ do
    it "ALPHA" (render ALPHA `shouldBe` "α")
    it "ALPHA'" (render ALPHA' `shouldBe` "~")

  describe "render ALPHA" $ do
    it "AL_IDX unicode" (render (AL_IDX ALPHA 3) `shouldBe` "α3")
    it "AL_IDX ascii" (render (AL_IDX ALPHA' 3) `shouldBe` "~3")
    it "AL_META unicode" (render (AL_META ALPHA (META NO_EXCL I "5")) `shouldBe` "α𝑖5")
    it "AL_META ascii" (render (AL_META ALPHA' (META EXCL I' "5")) `shouldBe` "~!i5")

  describe "render TAB" $ do
    it "no indent" (render (TAB 0) `shouldBe` "")
    it "two levels" (render (TAB 2) `shouldBe` "    ")
    it "TAB'" (render TAB' `shouldBe` " ")
    it "NO_TAB" (render NO_TAB `shouldBe` "")

  describe "render PAIR" $ do
    it "PA_TAU" (render (PA_TAU (AT_LABEL "x") ARROW xiExpr) `shouldBe` "x ↦ ξ")
    it "PA_ALPHA" (render (PA_ALPHA (AL_IDX ALPHA 0) ARROW xiExpr) `shouldBe` "α0 ↦ ξ")
    it "PA_FORMATION with no voids delegates to PA_TAU" (render (PA_FORMATION (AT_LABEL "f") [] ARROW xiExpr) `shouldBe` "f ↦ ξ")
    it
      "PA_FORMATION with voids"
      (render (PA_FORMATION (AT_LABEL "f") [AT_LABEL "p", AT_LABEL "q"] ARROW xiExpr) `shouldBe` "f(p, q) ↦ ξ")
    it "PA_LAMBDA" (render (PA_LAMBDA "Func") `shouldBe` "λ ⤍ Func")
    it "PA_LAMBDA'" (render (PA_LAMBDA' "Func") `shouldBe` "L> Func")
    it "PA_VOID question" (render (PA_VOID (AT_LABEL "x") ARROW QUESTION) `shouldBe` "x ↦ ?")
    it "PA_VOID empty" (render (PA_VOID (AT_LABEL "x") ARROW EMPTY) `shouldBe` "x ↦ ∅")
    it "PA_DELTA" (render (PA_DELTA (BT_ONE "1F")) `shouldBe` "Δ ⤍ 1F-")
    it "PA_DELTA'" (render (PA_DELTA' (BT_ONE "1F")) `shouldBe` "D> 1F-")
    it "PA_META_LAMBDA" (render (PA_META_LAMBDA (META NO_EXCL F "n")) `shouldBe` "λ ⤍ 𝑓n")
    it "PA_META_LAMBDA'" (render (PA_META_LAMBDA' (META EXCL F' "n")) `shouldBe` "L> !Fn")
    it "PA_META_DELTA" (render (PA_META_DELTA (META NO_EXCL D "n")) `shouldBe` "Δ ⤍ δn")
    it "PA_META_DELTA'" (render (PA_META_DELTA' (META EXCL D' "n")) `shouldBe` "D> !\\deltan")

  describe "render BINDINGS" $ do
    it "empty" (render (BDS_EMPTY (TAB 1)) `shouldBe` "")
    it
      "two pairs"
      ( render
          ( BDS_PAIR
              EOL
              (TAB 1)
              (pairXi "a")
              (BDS_PAIR EOL (TAB 1) (pairXi "b") (BDS_EMPTY (TAB 1)))
          )
          `shouldBe` ",\n  a ↦ ξ,\n  b ↦ ξ"
      )
    it
      "meta tail"
      (render (BDS_META EOL (TAB 1) (META NO_EXCL B "X") (BDS_EMPTY (TAB 1))) `shouldBe` ",\n  𝐵X")

  describe "render APP_BINDING" $
    it "delegates to its pair" (render (APP_BINDING (pairXi "a")) `shouldBe` "a ↦ ξ")

  describe "render BINDING" $ do
    it "BI_EMPTY" (render (BI_EMPTY (TAB 0)) `shouldBe` "")
    it "BI_PAIR" (render (bindingXi "a") `shouldBe` "a ↦ ξ")
    it "BI_META" (render (BI_META (META NO_EXCL B "X") (BDS_EMPTY (TAB 0)) (TAB 0)) `shouldBe` "𝐵X")

  describe "render APP_ARGUMENT" $ do
    it "AA_TAU" (render (AA_TAU (APP_BINDING (pairXi "a"))) `shouldBe` "a ↦ ξ")
    it "AA_TAUS" (render (AA_TAUS (bindingXi "a")) `shouldBe` "a ↦ ξ")
    it "AA_EXPRS" (render (AA_EXPRS (APP_ARG xiExpr AAS_EMPTY)) `shouldBe` "ξ")

  describe "render APP_ARG and APP_ARGS" $ do
    it "APP_ARG with trailing arg" (render (APP_ARG xiExpr (AAS_EXPR EOL (TAB 1) xiExpr AAS_EMPTY)) `shouldBe` "ξ,\n  ξ")
    it "APP_ARGS empty" (render AAS_EMPTY `shouldBe` "")
    it "APP_ARGS one" (render (AAS_EXPR EOL (TAB 1) xiExpr AAS_EMPTY) `shouldBe` ",\n  ξ")

  describe "render EXPRESSION" $ do
    it "EX_GLOBAL Φ" (render rootExpr `shouldBe` "Φ")
    it "EX_GLOBAL Q" (render (EX_GLOBAL Q) `shouldBe` "Q")
    it "EX_XI" (render xiExpr `shouldBe` "ξ")
    it "EX_XI DOLLAR" (render (EX_XI DOLLAR) `shouldBe` "$")
    it "EX_XI XI'" (render (EX_XI XI') `shouldBe` "\\phiTerminal{\\xi}")
    it "EX_ATTR" (render (EX_ATTR (AT_LABEL "x")) `shouldBe` "x")
    it "EX_TERMINATION DEAD" (render (EX_TERMINATION DEAD) `shouldBe` "⊥")
    it "EX_TERMINATION T" (render (EX_TERMINATION T) `shouldBe` "T")
    it
      "EX_FORMATION multiline"
      ( render
          (EX_FORMATION LSB EOL (TAB 1) (bindingXi "x") EOL (TAB 0) RSB)
          `shouldBe` "⟦\n  x ↦ ξ\n⟧"
      )
    it "EX_DISPATCH no space" (render (EX_DISPATCH xiExpr NO_SPACE (AT_LABEL "y")) `shouldBe` "ξ.y")
    it "EX_DISPATCH with space" (render (EX_DISPATCH xiExpr SPACE (AT_LABEL "y")) `shouldBe` "ξ . y")
    it
      "EX_APPLICATION"
      ( render
          (EX_APPLICATION xiExpr NO_SPACE EOL (TAB 1) (AA_TAU (APP_BINDING (pairXi "a"))) EOL (TAB 0) 1)
          `shouldBe` "ξ(\n  a ↦ ξ\n)"
      )
    it "EX_STRING" (render (EX_STRING "hi" (TAB 0) []) `shouldBe` "\"hi\"")
    it "EX_NUMBER integer" (render (EX_NUMBER (Left 42) (TAB 0) []) `shouldBe` "42")
    it "EX_NUMBER double" (render (EX_NUMBER (Right 3.5) (TAB 0) []) `shouldBe` "3.5")
    it "EX_META" (render (EX_META (META NO_EXCL E "x")) `shouldBe` "𝑒x")
    it "EX_PHI_MEET without prefix" (render (EX_PHI_MEET Nothing 5 xiExpr) `shouldBe` "\\phinoMeet{5}{ ξ }")
    it "EX_PHI_MEET with prefix" (render (EX_PHI_MEET (Just "p") 5 xiExpr) `shouldBe` "\\phinoMeet{p:5}{ ξ }")
    it "EX_PHI_AGAIN without prefix" (render (EX_PHI_AGAIN Nothing 3 xiExpr) `shouldBe` "\\phinoAgain{3}")
    it "EX_PHI_AGAIN with prefix" (render (EX_PHI_AGAIN (Just "p") 3 xiExpr) `shouldBe` "\\phinoAgain{p:3}")
    it "EX_BYTES" (render (EX_BYTES (BT_ONE "1F")) `shouldBe` "1F-")

  describe "render [ATTRIBUTE]" $
    it "joins with comma space" (render [AT_LABEL "a", AT_LABEL "b"] `shouldBe` "a, b")

  describe "render ATTRIBUTE" $
    forM_
      [ ("label", AT_LABEL "x", "x")
      , ("rho", AT_RHO RHO, "ρ")
      , ("phi", AT_PHI PHI, "φ")
      , ("lambda", AT_LAMBDA LAMBDA, "λ")
      , ("delta", AT_DELTA DELTA, "Δ")
      , ("meta", AT_META (META NO_EXCL TAU "x"), "𝜏x")
      , ("dots", AT_REST DOTS, "...")
      , ("dots ascii", AT_REST DOTS', "\\dots")
      ]
      (\(desc, attribute, expected) -> it desc (render attribute `shouldBe` expected))

  describe "render BELONGING" $ do
    it "IN" (render IN `shouldBe` "\\in")
    it "NOT_IN" (render NOT_IN `shouldBe` "\\notin")

  describe "render SET" $ do
    it "ST_BINDING" (render (ST_BINDING (bindingXi "x")) `shouldBe` "x ↦ ξ")
    it
      "ST_ATTRIBUTES"
      (render (ST_ATTRIBUTES [AT_LABEL "a", AT_LABEL "b"]) `shouldBe` "[ a \\char44{} b ]")

  describe "render LOGIC_OPERATOR" $ do
    it "AND" (render AND `shouldBe` "\\;\\text{and}\\;")
    it "OR" (render OR `shouldBe` "\\;\\text{or}\\;")

  describe "render NUMBER" $ do
    it "IDX_META" (render (IDX_META (META NO_EXCL I "x")) `shouldBe` "𝑖x")
    it "LENGTH" (render (LENGTH (bindingXi "x")) `shouldBe` "\\vert x ↦ ξ \\vert")
    it "DOMAIN" (render (DOMAIN (bindingXi "x")) `shouldBe` "\\vert \\overline{ x ↦ ξ } \\vert")
    it "LITERAL" (render (LITERAL 7) `shouldBe` "7")

  describe "render COMPARABLE" $ do
    it "CMP_ATTR" (render (CMP_ATTR (AT_LABEL "x")) `shouldBe` "x")
    it "CMP_EXPR" (render (CMP_EXPR xiExpr) `shouldBe` "ξ")
    it "CMP_NUM" (render (CMP_NUM (LITERAL 3)) `shouldBe` "3")

  describe "render EQUAL" $
    forM_
      [ (EQUAL, "=")
      , (NOT_EQUAL, "\\not=")
      , (GREATER, ">")
      , (NOT_GREATER, "\\leq")
      ]
      (\(eq, expected) -> it (show eq) (render eq `shouldBe` expected))

  describe "render CONDITION" $ do
    it
      "CO_BELONGS"
      (render (CO_BELONGS (AT_LABEL "x") IN (ST_BINDING (bindingXi "y"))) `shouldBe` "x \\in y ↦ ξ")
    it
      "CO_LOGIC single condition is unwrapped"
      (render (CO_LOGIC [CO_NF xiExpr] AND) `shouldBe` "\\isnormal{ ξ }")
    it
      "CO_LOGIC joins multiple conditions"
      ( render (CO_LOGIC [CO_NF xiExpr, CO_NF rootExpr] AND)
          `shouldBe` "\\isnormal{ ξ } \\;\\text{and}\\; \\isnormal{ Φ }"
      )
    it
      "CO_LOGIC wraps a nested non-singleton CO_LOGIC in parens"
      ( render (CO_LOGIC [CO_LOGIC [CO_NF xiExpr, CO_NF rootExpr] OR, CO_NF xiExpr] AND)
          `shouldBe` "\\lparen \\isnormal{ ξ } \\;\\text{or}\\; \\isnormal{ Φ } \\rparen \\;\\text{and}\\; \\isnormal{ ξ }"
      )
    it "CO_NF" (render (CO_NF xiExpr) `shouldBe` "\\isnormal{ ξ }")
    it "CO_ABSOLUTE in" (render (CO_ABSOLUTE xiExpr IN) `shouldBe` "\\phinoAbsolute{ ξ }")
    it "CO_ABSOLUTE not in" (render (CO_ABSOLUTE xiExpr NOT_IN) `shouldBe` "\\phinoNotAbsolute{ ξ }")
    it "CO_NOT wrapping CO_FORMATION" (render (CO_NOT (CO_FORMATION xiExpr)) `shouldBe` "\\phinoNotFormation{ ξ }")
    it "CO_NOT wrapping a generic condition" (render (CO_NOT (CO_NF xiExpr)) `shouldBe` "not\\lparen \\isnormal{ ξ } \\rparen")
    it
      "CO_COMPARE"
      (render (CO_COMPARE (CMP_ATTR (AT_LABEL "x")) EQUAL (CMP_NUM (LITERAL 3))) `shouldBe` "x = 3")
    it
      "CO_MATCHES"
      (render (CO_MATCHES "^a+$" xiExpr) `shouldBe` "matches\\lparen ^a+$, ξ \\rparen")
    it
      "CO_PART_OF"
      (render (CO_PART_OF xiExpr (bindingXi "y")) `shouldBe` "part-of\\lparen ξ, y ↦ ξ \\rparen")
    it "CO_FORMATION" (render (CO_FORMATION xiExpr) `shouldBe` "\\phinoIsFormation{ ξ }")
    it
      "CO_DISJOINT single group"
      (render (CO_DISJOINT [AT_LABEL "a"] [bindingXi "x"]) `shouldBe` "[ a ] \\cap x ↦ ξ = \\emptyset")
    it
      "CO_DISJOINT multiple groups"
      ( render (CO_DISJOINT [AT_LABEL "a", AT_LABEL "b"] [bindingXi "x", bindingXi "y"])
          `shouldBe` "[ a \\char44{} b ] \\cap \\lparen x ↦ ξ \\cup y ↦ ξ \\rparen = \\emptyset"
      )
    it "CO_EMPTY" (render CO_EMPTY `shouldBe` "")

  describe "render EXTRA_ARG" $ do
    it "ARG_ATTR" (render (ARG_ATTR (AT_LABEL "x")) `shouldBe` "x")
    it "ARG_EXPR" (render (ARG_EXPR xiExpr) `shouldBe` "ξ")
    it "ARG_BINDING" (render (ARG_BINDING (bindingXi "x")) `shouldBe` "x ↦ ξ")
    it "ARG_BYTES" (render (ARG_BYTES (BT_ONE "1F")) `shouldBe` "1F-")

  describe "render EXTRA" $ do
    it
      "contextualize wraps the first arg, the rest, and the meta separately"
      ( render (EXTRA (ARG_EXPR xiExpr) "contextualize" [ARG_ATTR (AT_LABEL "a"), ARG_ATTR (AT_LABEL "b")])
          `shouldBe` "\\phinoContextualize{ a }{ b }{ ξ }"
      )
    it
      "morph renders the fixed universe and state arguments"
      ( render (EXTRA (ARG_EXPR xiExpr) "morph" [ARG_ATTR (AT_LABEL "n")])
          `shouldBe` "ξ \\coloneqq \\phinoMorph{ n }{ e }{ s_1 }"
      )
    it
      "evaluate uses its dedicated macro name"
      ( render (EXTRA (ARG_EXPR xiExpr) "evaluate" [ARG_ATTR (AT_LABEL "n")])
          `shouldBe` "ξ \\coloneqq \\phinoEvaluate{ n }"
      )
    it
      "any other function becomes its own backslash macro"
      ( render (EXTRA (ARG_EXPR xiExpr) "dataize" [ARG_ATTR (AT_LABEL "n")])
          `shouldBe` "ξ \\coloneqq \\dataize{ n }"
      )
