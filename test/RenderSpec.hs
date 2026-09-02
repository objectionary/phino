{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module RenderSpec (spec) where

import CST
import Control.Monad (forM_)
import Data.Text qualified as T
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
  describe "render primitive wrapper instances" $
    forM_
      [ ("String", render ("hi" :: String), "hi")
      , ("Char", render 'z', "z")
      , ("Int", render (7 :: Int), "7")
      ]
      (\(desc, actual, expected) -> it desc (actual `shouldBe` expected))

  describe "render braces, comma and arrows" $
    forM_
      [ ("LCB", render LCB, "{")
      , ("BIG_LCB", render BIG_LCB, "\\Big\\{")
      , ("RCB", render RCB, "}")
      , ("BIG_RCB", render BIG_RCB, "\\Big\\}")
      , ("LSB'", render LSB', "[[")
      , ("RSB'", render RSB', "]]")
      , ("COMMA", render COMMA, ",")
      , ("NO_COMMA", render NO_COMMA, "")
      , ("ARROW'", render ARROW', "->")
      , ("DASHED_ARROW", render DASHED_ARROW, "⤍")
      , ("QUESTION", render QUESTION, "?")
      , ("AT", render AT, "@")
      , ("CARET", render CARET, "^")
      , ("RHO'", render RHO', "\\phiTerminal{\\rho}")
      , ("DELTA'", render DELTA', "D")
      , ("LAMBDA'", render LAMBDA', "L")
      , ("Q", render Q, "Q")
      , ("DEAD", render DEAD, "⊥")
      , ("SPACE", render SPACE, " ")
      , ("DOTS", render DOTS, "...")
      , ("DOTS'", render DOTS', "\\dots")
      ]
      (\(desc, actual, expected) -> it desc (actual `shouldBe` expected))

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

  describe "render META" $
    forM_
      [ ("without exclamation", render (META NO_EXCL E "x"), "𝑒x")
      , ("with exclamation", render (META EXCL E' "42"), "!e42")
      ]
      (\(desc, actual, expected) -> it desc (actual `shouldBe` expected))

  describe "render ALPHA'" $
    forM_
      [ ("ALPHA", ALPHA, "α")
      , ("ALPHA'", ALPHA', "~")
      ]
      (\(desc, node, expected) -> it desc (render node `shouldBe` expected))

  describe "render ALPHA" $
    forM_
      [ ("AL_IDX unicode", AL_IDX ALPHA 3, "α3")
      , ("AL_IDX ascii", AL_IDX ALPHA' 3, "~3")
      , ("AL_META unicode", AL_META ALPHA (META NO_EXCL I "5"), "α𝑖5")
      , ("AL_META ascii", AL_META ALPHA' (META EXCL I' "5"), "~!i5")
      ]
      (\(desc, node, expected) -> it desc (render node `shouldBe` expected))

  describe "render TAB" $
    forM_
      [ ("no indent", TAB 0, "")
      , ("two levels", TAB 2, "    ")
      , ("TAB'", TAB', " ")
      , ("NO_TAB", NO_TAB, "")
      ]
      (\(desc, node, expected) -> it desc (render node `shouldBe` expected))

  describe "render PAIR" $
    forM_
      [ ("PA_TAU", PA_TAU (AT_LABEL "x") ARROW xiExpr, "x ↦ ξ")
      , ("PA_ALPHA", PA_ALPHA (AL_IDX ALPHA 0) ARROW xiExpr, "α0 ↦ ξ")
      , ("PA_FORMATION with no voids delegates to PA_TAU", PA_FORMATION (AT_LABEL "f") [] ARROW xiExpr, "f ↦ ξ")
      ,
        ( "PA_FORMATION with voids"
        , PA_FORMATION (AT_LABEL "f") [AT_LABEL "p", AT_LABEL "q"] ARROW xiExpr
        , "f(p, q) ↦ ξ"
        )
      , ("PA_LAMBDA", PA_LAMBDA "Func", "λ ⤍ Func")
      , ("PA_LAMBDA'", PA_LAMBDA' "Func", "L> Func")
      , ("PA_VOID question", PA_VOID (AT_LABEL "x") ARROW QUESTION, "x ↦ ?")
      , ("PA_VOID empty", PA_VOID (AT_LABEL "x") ARROW EMPTY, "x ↦ ∅")
      , ("PA_DELTA", PA_DELTA (BT_ONE "1F"), "Δ ⤍ 1F-")
      , ("PA_DELTA'", PA_DELTA' (BT_ONE "1F"), "D> 1F-")
      , ("PA_META_LAMBDA", PA_META_LAMBDA (META NO_EXCL F "n"), "λ ⤍ 𝑓n")
      , ("PA_META_LAMBDA'", PA_META_LAMBDA' (META EXCL F' "n"), "L> !Fn")
      , ("PA_META_DELTA", PA_META_DELTA (META NO_EXCL D "n"), "Δ ⤍ δn")
      , ("PA_META_DELTA'", PA_META_DELTA' (META EXCL D' "n"), "D> !\\deltan")
      ]
      (\(desc, node, expected) -> it desc (render node `shouldBe` expected))

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

  describe "render BINDING" $
    forM_
      [ ("BI_EMPTY", BI_EMPTY (TAB 0), "")
      , ("BI_PAIR", bindingXi "a", "a ↦ ξ")
      , ("BI_META", BI_META (META NO_EXCL B "X") (BDS_EMPTY (TAB 0)) (TAB 0), "𝐵X")
      ]
      (\(desc, node, expected) -> it desc (render node `shouldBe` expected))

  describe "render APP_ARGUMENT" $
    forM_
      [ ("AA_TAU", AA_TAU (APP_BINDING (pairXi "a")), "a ↦ ξ")
      , ("AA_TAUS", AA_TAUS (bindingXi "a"), "a ↦ ξ")
      , ("AA_EXPRS", AA_EXPRS (APP_ARG xiExpr AAS_EMPTY), "ξ")
      ]
      (\(desc, node, expected) -> it desc (render node `shouldBe` expected))

  describe "render APP_ARG and APP_ARGS" $
    forM_
      [ ("APP_ARG with trailing arg", render (APP_ARG xiExpr (AAS_EXPR EOL (TAB 1) xiExpr AAS_EMPTY)), "ξ,\n  ξ")
      , ("APP_ARGS empty", render AAS_EMPTY, "")
      , ("APP_ARGS one", render (AAS_EXPR EOL (TAB 1) xiExpr AAS_EMPTY), ",\n  ξ")
      ]
      (\(desc, actual, expected) -> it desc (actual `shouldBe` expected))

  describe "render EXPRESSION" $
    forM_
      [ ("EX_GLOBAL Φ", rootExpr, "Φ")
      , ("EX_GLOBAL Q", EX_GLOBAL Q, "Q")
      , ("EX_XI", xiExpr, "ξ")
      , ("EX_XI DOLLAR", EX_XI DOLLAR, "$")
      , ("EX_XI XI'", EX_XI XI', "\\phiTerminal{\\xi}")
      , ("EX_ATTR", EX_ATTR (AT_LABEL "x"), "x")
      , ("EX_TERMINATION DEAD", EX_TERMINATION DEAD, "⊥")
      , ("EX_TERMINATION T", EX_TERMINATION T, "T")
      ,
        ( "EX_FORMATION multiline"
        , EX_FORMATION LSB EOL (TAB 1) (bindingXi "x") EOL (TAB 0) RSB
        , "⟦\n  x ↦ ξ\n⟧"
        )
      , ("EX_DISPATCH no space", EX_DISPATCH xiExpr NO_SPACE (AT_LABEL "y"), "ξ.y")
      , ("EX_DISPATCH with space", EX_DISPATCH xiExpr SPACE (AT_LABEL "y"), "ξ . y")
      ,
        ( "EX_APPLICATION"
        , EX_APPLICATION xiExpr NO_SPACE EOL (TAB 1) (AA_TAU (APP_BINDING (pairXi "a"))) EOL (TAB 0) 1
        , "ξ(\n  a ↦ ξ\n)"
        )
      , ("EX_STRING", EX_STRING "hi" (TAB 0) [], "\"hi\"")
      , ("EX_NUMBER integer", EX_NUMBER (Left 42) (TAB 0) [], "42")
      , ("EX_NUMBER double", EX_NUMBER (Right 3.5) (TAB 0) [], "3.5")
      , ("EX_META", EX_META (META NO_EXCL E "x"), "𝑒x")
      , ("EX_PHI_MEET without prefix", EX_PHI_MEET Nothing 5 xiExpr, "\\phinoMeet{5}{ ξ }")
      , ("EX_PHI_MEET with prefix", EX_PHI_MEET (Just "p") 5 xiExpr, "\\phinoMeet{p:5}{ ξ }")
      , ("EX_PHI_AGAIN without prefix", EX_PHI_AGAIN Nothing 3 xiExpr, "\\phinoAgain{3}")
      , ("EX_PHI_AGAIN with prefix", EX_PHI_AGAIN (Just "p") 3 xiExpr, "\\phinoAgain{p:3}")
      , ("EX_BYTES", EX_BYTES (BT_ONE "1F"), "1F-")
      ]
      (\(desc, node, expected) -> it desc (render node `shouldBe` expected))

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

  describe "render BELONGING" $
    forM_
      [ ("IN", IN, "\\in")
      , ("NOT_IN", NOT_IN, "\\notin")
      ]
      (\(desc, node, expected) -> it desc (render node `shouldBe` expected))

  describe "render SET" $
    forM_
      [ ("ST_BINDING", ST_BINDING (bindingXi "x"), "x ↦ ξ")
      , ("ST_ATTRIBUTES", ST_ATTRIBUTES [AT_LABEL "a", AT_LABEL "b"], "[ a \\char44{} b ]")
      ]
      (\(desc, node, expected) -> it desc (render node `shouldBe` expected))

  describe "render LOGIC_OPERATOR" $
    forM_
      [ ("AND", AND, "\\;\\text{and}\\;")
      , ("OR", OR, "\\;\\text{or}\\;")
      ]
      (\(desc, node, expected) -> it desc (render node `shouldBe` expected))

  describe "render NUMBER" $
    forM_
      [ ("IDX_META", IDX_META (META NO_EXCL I "x"), "𝑖x")
      , ("LENGTH", LENGTH (bindingXi "x"), "\\vert x ↦ ξ \\vert")
      , ("DOMAIN", DOMAIN (bindingXi "x"), "\\vert \\overline{ x ↦ ξ } \\vert")
      , ("LITERAL", LITERAL 7, "7")
      ]
      (\(desc, node, expected) -> it desc (render node `shouldBe` expected))

  describe "render COMPARABLE" $
    forM_
      [ ("CMP_ATTR", CMP_ATTR (AT_LABEL "x"), "x")
      , ("CMP_EXPR", CMP_EXPR xiExpr, "ξ")
      , ("CMP_NUM", CMP_NUM (LITERAL 3), "3")
      ]
      (\(desc, node, expected) -> it desc (render node `shouldBe` expected))

  describe "render EQUAL" $
    forM_
      [ (EQUAL, "=")
      , (NOT_EQUAL, "\\not=")
      , (GREATER, ">")
      , (NOT_GREATER, "\\leq")
      ]
      (\(eq, expected) -> it (show eq) (render eq `shouldBe` expected))

  describe "render CONDITION" $
    forM_
      [ ("CO_BELONGS", CO_BELONGS (AT_LABEL "x") IN (ST_BINDING (bindingXi "y")), "x \\in y ↦ ξ")
      , ("CO_LOGIC single condition is unwrapped", CO_LOGIC [CO_NF xiExpr] AND, "\\isnormal{ ξ }")
      ,
        ( "CO_LOGIC joins multiple conditions"
        , CO_LOGIC [CO_NF xiExpr, CO_NF rootExpr] AND
        , "\\isnormal{ ξ } \\;\\text{and}\\; \\isnormal{ Φ }"
        )
      ,
        ( "CO_LOGIC wraps a nested non-singleton CO_LOGIC in parens"
        , CO_LOGIC [CO_LOGIC [CO_NF xiExpr, CO_NF rootExpr] OR, CO_NF xiExpr] AND
        , "\\lparen \\isnormal{ ξ } \\;\\text{or}\\; \\isnormal{ Φ } \\rparen \\;\\text{and}\\; \\isnormal{ ξ }"
        )
      , ("CO_NF", CO_NF xiExpr, "\\isnormal{ ξ }")
      , ("CO_ABSOLUTE in", CO_ABSOLUTE xiExpr IN, "\\phinoAbsolute{ ξ }")
      , ("CO_ABSOLUTE not in", CO_ABSOLUTE xiExpr NOT_IN, "\\phinoNotAbsolute{ ξ }")
      , ("CO_NOT wrapping CO_FORMATION", CO_NOT (CO_FORMATION xiExpr), "\\phinoNotFormation{ ξ }")
      ,
        ( "CO_NOT wrapping a generic condition"
        , CO_NOT (CO_NF xiExpr)
        , "not\\lparen \\isnormal{ ξ } \\rparen"
        )
      , ("CO_COMPARE", CO_COMPARE (CMP_ATTR (AT_LABEL "x")) EQUAL (CMP_NUM (LITERAL 3)), "x = 3")
      , ("CO_MATCHES", CO_MATCHES "^a+$" xiExpr, "matches\\lparen ^a+$, ξ \\rparen")
      , ("CO_PART_OF", CO_PART_OF xiExpr (bindingXi "y"), "part-of\\lparen ξ, y ↦ ξ \\rparen")
      , ("CO_FORMATION", CO_FORMATION xiExpr, "\\phinoIsFormation{ ξ }")
      , ("CO_DISJOINT single group", CO_DISJOINT [AT_LABEL "a"] [bindingXi "x"], "[ a ] \\cap x ↦ ξ = \\emptyset")
      ,
        ( "CO_DISJOINT multiple groups"
        , CO_DISJOINT [AT_LABEL "a", AT_LABEL "b"] [bindingXi "x", bindingXi "y"]
        , "[ a \\char44{} b ] \\cap \\lparen x ↦ ξ \\cup y ↦ ξ \\rparen = \\emptyset"
        )
      , ("CO_EMPTY", CO_EMPTY, "")
      ]
      (\(desc, node, expected) -> it desc (render node `shouldBe` expected))

  describe "render EXTRA_ARG" $
    forM_
      [ ("ARG_ATTR", ARG_ATTR (AT_LABEL "x"), "x")
      , ("ARG_EXPR", ARG_EXPR xiExpr, "ξ")
      , ("ARG_BINDING", ARG_BINDING (bindingXi "x"), "x ↦ ξ")
      , ("ARG_BYTES", ARG_BYTES (BT_ONE "1F"), "1F-")
      ]
      (\(desc, node, expected) -> it desc (render node `shouldBe` expected))

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
