{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module SugarSpec (spec) where

import AST
import CST
import Control.Monad (forM_)
import Encoding (Encoding (UNICODE))
import Lining (LineFormat (SINGLELINE))
import Margin (defaultMargin)
import Printer (printExpression')
import Render (Render (render))
import Sugar
import Test.Hspec

xiExpr :: EXPRESSION
xiExpr = EX_XI XI

rootExpr :: EXPRESSION
rootExpr = EX_GLOBAL Φ

-- `x` dispatched off the root, used as a representative attribute-valued
-- callee for the application collapse cases.
rootDotX :: EXPRESSION
rootDotX = EX_DISPATCH rootExpr NO_SPACE (AT_LABEL "x")

-- `$.y`, the salty desugaring of the bare attribute `y`.
dottedY :: EXPRESSION
dottedY = EX_DISPATCH (EX_XI XI) NO_SPACE (AT_LABEL "y")

-- The bare attribute `y`, sugar for `$.y`.
exYAttr :: EXPRESSION
exYAttr = EX_ATTR (AT_LABEL "y")

spec :: Spec
spec = do
  describe "withSugarType" $ do
    it "SWEET leaves a CST node untouched" (withSugarType SWEET xiExpr `shouldBe` xiExpr)
    it "SALTY dispatches to toSalty" (withSugarType SALTY (EX_ATTR (AT_LABEL "x")) `shouldBe` toSalty (EX_ATTR (AT_LABEL "x")))

  describe "toSalty EXPRESSION" $ do
    it
      "EX_ATTR sugars $.x into an explicit xi dispatch"
      (toSalty (EX_ATTR (AT_LABEL "x")) `shouldBe` EX_DISPATCH (EX_XI XI) NO_SPACE (AT_LABEL "x"))

    it
      "EX_DISPATCH recurses into its callee"
      ( toSalty (EX_DISPATCH (EX_ATTR (AT_LABEL "y")) NO_SPACE (AT_LABEL "x"))
          `shouldBe` EX_DISPATCH (EX_DISPATCH (EX_XI XI) NO_SPACE (AT_LABEL "y")) NO_SPACE (AT_LABEL "x")
      )

    it
      "EX_FORMATION with an empty binding collapses to the TAB' layout and gains a void rho"
      ( toSalty (EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY NO_TAB) NO_EOL NO_TAB RSB)
          `shouldBe` EX_FORMATION
            LSB
            NO_EOL
            TAB'
            (BI_PAIR (PA_VOID (AT_RHO RHO) ARROW EMPTY) (BDS_EMPTY NO_TAB) NO_TAB)
            NO_EOL
            TAB'
            RSB
      )

    it
      "EX_FORMATION with a real binding keeps its layout and appends a trailing void rho"
      ( toSalty
          ( EX_FORMATION
              LSB
              EOL
              (TAB 1)
              (BI_PAIR (PA_TAU (AT_LABEL "x") ARROW xiExpr) (BDS_EMPTY (TAB 1)) (TAB 1))
              EOL
              (TAB 0)
              RSB
          )
          `shouldBe` EX_FORMATION
            LSB
            EOL
            (TAB 1)
            ( BI_PAIR
                (PA_TAU (AT_LABEL "x") ARROW xiExpr)
                (BDS_PAIR EOL (TAB 1) (PA_VOID (AT_RHO RHO) ARROW EMPTY) (BDS_EMPTY (TAB 1)))
                (TAB 1)
            )
            EOL
            (TAB 0)
            RSB
      )

    it
      "EX_FORMATION already ending in a void rho is left with that one tail unchanged"
      ( toSalty
          ( EX_FORMATION
              LSB
              EOL
              (TAB 1)
              ( BI_PAIR
                  (PA_TAU (AT_LABEL "x") ARROW xiExpr)
                  (BDS_PAIR EOL (TAB 1) (PA_VOID (AT_RHO RHO) ARROW EMPTY) (BDS_EMPTY (TAB 1)))
                  (TAB 1)
              )
              EOL
              (TAB 0)
              RSB
          )
          `shouldBe` EX_FORMATION
            LSB
            EOL
            (TAB 1)
            ( BI_PAIR
                (PA_TAU (AT_LABEL "x") ARROW xiExpr)
                (BDS_PAIR EOL (TAB 1) (PA_VOID (AT_RHO RHO) ARROW EMPTY) (BDS_EMPTY (TAB 1)))
                (TAB 1)
            )
            EOL
            (TAB 0)
            RSB
      )

    it
      "EX_FORMATION whose head binding is itself the void rho is left unchanged"
      ( toSalty (EX_FORMATION LSB EOL (TAB 1) (BI_PAIR (PA_VOID (AT_RHO RHO) ARROW EMPTY) (BDS_EMPTY (TAB 1)) (TAB 1)) EOL (TAB 0) RSB)
          `shouldBe` EX_FORMATION LSB EOL (TAB 1) (BI_PAIR (PA_VOID (AT_RHO RHO) ARROW EMPTY) (BDS_EMPTY (TAB 1)) (TAB 1)) EOL (TAB 0) RSB
      )

    it
      "EX_FORMATION whose head binding is a tau bound to rho is left unchanged"
      ( toSalty (EX_FORMATION LSB EOL (TAB 1) (BI_PAIR (PA_TAU (AT_RHO RHO) ARROW xiExpr) (BDS_EMPTY (TAB 1)) (TAB 1)) EOL (TAB 0) RSB)
          `shouldBe` EX_FORMATION LSB EOL (TAB 1) (BI_PAIR (PA_TAU (AT_RHO RHO) ARROW xiExpr) (BDS_EMPTY (TAB 1)) (TAB 1)) EOL (TAB 0) RSB
      )

    it
      "EX_FORMATION whose head binding is an object-with-params rho is left unchanged"
      ( toSalty (EX_FORMATION LSB EOL (TAB 1) (BI_PAIR (PA_FORMATION (AT_RHO RHO) [] ARROW xiExpr) (BDS_EMPTY (TAB 1)) (TAB 1)) EOL (TAB 0) RSB)
          `shouldBe` EX_FORMATION LSB EOL (TAB 1) (BI_PAIR (PA_FORMATION (AT_RHO RHO) [] ARROW xiExpr) (BDS_EMPTY (TAB 1)) (TAB 1)) EOL (TAB 0) RSB
      )

    it
      "EX_FORMATION with a meta binding at the head is left unchanged"
      ( toSalty (EX_FORMATION LSB EOL (TAB 1) (BI_META (META NO_EXCL B "X") (BDS_EMPTY (TAB 1)) (TAB 1)) EOL (TAB 0) RSB)
          `shouldBe` EX_FORMATION LSB EOL (TAB 1) (BI_META (META NO_EXCL B "X") (BDS_EMPTY (TAB 1)) (TAB 1)) EOL (TAB 0) RSB
      )

    it
      "EX_APPLICATION with a single tau argument (AA_TAU) recurses into callee and argument"
      ( toSalty (EX_APPLICATION xiExpr NO_SPACE EOL (TAB 1) (AA_TAU (APP_BINDING (PA_TAU (AT_LABEL "y") ARROW xiExpr))) EOL (TAB 0) 1)
          `shouldBe` EX_APPLICATION xiExpr NO_SPACE EOL (TAB 1) (AA_TAU (APP_BINDING (PA_TAU (AT_LABEL "y") ARROW xiExpr))) EOL (TAB 0) 1
      )

    it
      "EX_APPLICATION with several tau bindings (AA_TAUS) unrolls into a chain of applications"
      ( render
          ( toSalty
              ( EX_APPLICATION
                  xiExpr
                  NO_SPACE
                  EOL
                  (TAB 1)
                  (AA_TAUS (BI_PAIR (PA_TAU (AT_LABEL "a") ARROW xiExpr) (BDS_PAIR EOL (TAB 1) (PA_TAU (AT_LABEL "b") ARROW xiExpr) (BDS_EMPTY (TAB 1))) (TAB 1)))
                  EOL
                  (TAB 0)
                  1
              )
          )
          `shouldBe` "ξ(\n  a ↦ ξ\n)(\n  b ↦ ξ\n)"
      )

    it
      "EX_APPLICATION with positional arguments (AA_EXPRS) sugars them into alpha-indexed applications"
      ( render
          ( toSalty
              ( EX_APPLICATION
                  xiExpr
                  NO_SPACE
                  EOL
                  (TAB 1)
                  (AA_EXPRS (APP_ARG xiExpr (AAS_EXPR EOL (TAB 1) rootExpr AAS_EMPTY)))
                  EOL
                  (TAB 0)
                  1
              )
          )
          `shouldBe` "ξ(\n  α0 ↦ ξ\n)(\n  α1 ↦ Φ\n)"
      )

    it
      "EX_NUMBER with no extra rho expands into the Q.number(Q.bytes(...)) form"
      ( render (toSalty (EX_NUMBER (Left 42) (TAB 1) []))
          `shouldBe` "Φ.number(\n    as-bytes ↦ Φ.bytes(\n      data ↦ ⟦\n        Δ ⤍ 40-45-00-00-00-00-00-00,\n        ρ ↦ ∅\n      ⟧\n    )\n  )"
      )

    it
      "EX_NUMBER preserves an extra rho argument carried alongside the primitive"
      ( render (toSalty (EX_NUMBER (Left 42) (TAB 1) [ArTau AtRho (ExDispatch ExXi (AtLabel "y"))]))
          `shouldBe` "Φ.number(\n    as-bytes ↦ Φ.bytes(\n      data ↦ ⟦\n        Δ ⤍ 40-45-00-00-00-00-00-00,\n        ρ ↦ ∅\n      ⟧\n    )\n  )(\n    ρ ↦ ξ.y\n  )"
      )

    it
      "EX_STRING expands into the Q.string(Q.bytes(...)) form"
      ( render (toSalty (EX_STRING "hi" (TAB 1) []))
          `shouldBe` "Φ.string(\n    as-bytes ↦ Φ.bytes(\n      data ↦ ⟦\n        Δ ⤍ 68-69,\n        ρ ↦ ∅\n      ⟧\n    )\n  )"
      )

    it
      "EX_FORMATION with a meta tail (no head rho) leaves the meta tail untouched"
      ( toSalty
          ( EX_FORMATION
              LSB
              EOL
              (TAB 1)
              (BI_PAIR (PA_TAU (AT_LABEL "x") ARROW xiExpr) (BDS_META EOL (TAB 1) (META NO_EXCL B "X") (BDS_EMPTY (TAB 1))) (TAB 1))
              EOL
              (TAB 0)
              RSB
          )
          `shouldBe` EX_FORMATION
            LSB
            EOL
            (TAB 1)
            (BI_PAIR (PA_TAU (AT_LABEL "x") ARROW xiExpr) (BDS_META EOL (TAB 1) (META NO_EXCL B "X") (BDS_EMPTY (TAB 1))) (TAB 1))
            EOL
            (TAB 0)
            RSB
      )

    it
      "EX_APPLICATION with an empty AA_TAUS binding collapses to its bare callee"
      ( toSalty (EX_APPLICATION rootDotX NO_SPACE EOL (TAB 1) (AA_TAUS (BI_EMPTY (TAB 1))) EOL (TAB 0) 1)
          `shouldBe` rootDotX
      )

    it
      "EX_PHI_MEET recurses into its wrapped expression"
      (toSalty (EX_PHI_MEET Nothing 3 (EX_ATTR (AT_LABEL "x"))) `shouldBe` EX_PHI_MEET Nothing 3 (EX_DISPATCH (EX_XI XI) NO_SPACE (AT_LABEL "x")))

    it
      "EX_PHI_AGAIN recurses into its wrapped expression"
      (toSalty (EX_PHI_AGAIN (Just "p") 2 (EX_ATTR (AT_LABEL "x"))) `shouldBe` EX_PHI_AGAIN (Just "p") 2 (EX_DISPATCH (EX_XI XI) NO_SPACE (AT_LABEL "x")))

    it
      "default clause leaves terminals untouched"
      $ forM_
        [ rootExpr
        , EX_TERMINATION DEAD
        , EX_META (META NO_EXCL E "x")
        , EX_BYTES (BT_ONE "1F")
        ]
        (\terminal -> toSalty terminal `shouldBe` terminal)

  describe "toSalty BINDING" $ do
    it
      "BI_PAIR recurses into its pair and tail"
      ( toSalty (BI_PAIR (PA_TAU (AT_LABEL "x") ARROW (EX_ATTR (AT_LABEL "y"))) (BDS_EMPTY (TAB 1)) (TAB 1))
          `shouldBe` BI_PAIR (PA_TAU (AT_LABEL "x") ARROW dottedY) (BDS_EMPTY (TAB 1)) (TAB 1)
      )
    it "BI_EMPTY is unchanged" (toSalty (BI_EMPTY (TAB 1)) `shouldBe` BI_EMPTY (TAB 1))
    it
      "BI_META is unchanged"
      (toSalty (BI_META (META NO_EXCL B "X") (BDS_EMPTY (TAB 1)) (TAB 1)) `shouldBe` BI_META (META NO_EXCL B "X") (BDS_EMPTY (TAB 1)) (TAB 1))

  describe "toSalty APP_BINDING" $
    it
      "delegates to the pair instance"
      (toSalty (APP_BINDING (PA_TAU (AT_LABEL "x") ARROW (EX_ATTR (AT_LABEL "y")))) `shouldBe` APP_BINDING (PA_TAU (AT_LABEL "x") ARROW (EX_DISPATCH (EX_XI XI) NO_SPACE (AT_LABEL "y"))))

  describe "toSalty BINDINGS" $ do
    it
      "BDS_PAIR recurses into its pair and tail"
      ( toSalty (BDS_PAIR EOL (TAB 1) (PA_TAU (AT_LABEL "x") ARROW (EX_ATTR (AT_LABEL "y"))) (BDS_EMPTY (TAB 1)))
          `shouldBe` BDS_PAIR EOL (TAB 1) (PA_TAU (AT_LABEL "x") ARROW (EX_DISPATCH (EX_XI XI) NO_SPACE (AT_LABEL "y"))) (BDS_EMPTY (TAB 1))
      )
    it "BDS_EMPTY is unchanged" (toSalty (BDS_EMPTY (TAB 1)) `shouldBe` BDS_EMPTY (TAB 1))

  describe "toSalty PAIR" $ do
    it
      "PA_TAU recurses into its value"
      (toSalty (PA_TAU (AT_LABEL "x") ARROW (EX_ATTR (AT_LABEL "y"))) `shouldBe` PA_TAU (AT_LABEL "x") ARROW (EX_DISPATCH (EX_XI XI) NO_SPACE (AT_LABEL "y")))
    it
      "PA_ALPHA recurses into its value"
      (toSalty (PA_ALPHA (AL_IDX ALPHA 0) ARROW (EX_ATTR (AT_LABEL "y"))) `shouldBe` PA_ALPHA (AL_IDX ALPHA 0) ARROW (EX_DISPATCH (EX_XI XI) NO_SPACE (AT_LABEL "y")))
    it
      "PA_FORMATION with an empty object body joins its void params ahead of the body and gains a trailing rho"
      ( toSalty (PA_FORMATION (AT_LABEL "f") [AT_LABEL "p"] ARROW (EX_FORMATION LSB EOL (TAB 2) (BI_EMPTY (TAB 2)) EOL (TAB 1) RSB))
          `shouldBe` PA_TAU
            (AT_LABEL "f")
            ARROW
            ( EX_FORMATION
                LSB
                EOL
                (TAB 2)
                ( BI_PAIR
                    (PA_VOID (AT_LABEL "p") ARROW EMPTY)
                    (BDS_PAIR EOL (TAB 2) (PA_VOID (AT_RHO RHO) ARROW EMPTY) (BDS_EMPTY (TAB 2)))
                    (TAB 2)
                )
                EOL
                (TAB 1)
                RSB
            )
      )
    it
      "PA_FORMATION with a non-empty object body joins several void params ahead of the existing bindings"
      ( toSalty
          ( PA_FORMATION
              (AT_LABEL "f")
              [AT_LABEL "p", AT_LABEL "q"]
              ARROW
              (EX_FORMATION LSB EOL (TAB 2) (BI_PAIR (PA_TAU (AT_LABEL "z") ARROW xiExpr) (BDS_EMPTY (TAB 2)) (TAB 2)) EOL (TAB 1) RSB)
          )
          `shouldBe` PA_TAU
            (AT_LABEL "f")
            ARROW
            ( EX_FORMATION
                LSB
                EOL
                (TAB 2)
                ( BI_PAIR
                    (PA_VOID (AT_LABEL "p") ARROW EMPTY)
                    ( BDS_PAIR
                        EOL
                        (TAB 2)
                        (PA_VOID (AT_LABEL "q") ARROW EMPTY)
                        ( BDS_PAIR
                            EOL
                            (TAB 2)
                            (PA_TAU (AT_LABEL "z") ARROW xiExpr)
                            (BDS_PAIR EOL (TAB 2) (PA_VOID (AT_RHO RHO) ARROW EMPTY) (BDS_EMPTY (TAB 2)))
                        )
                    )
                    (TAB 2)
                )
                EOL
                (TAB 1)
                RSB
            )
      )
    it
      "default clause leaves a PA_VOID pair untouched"
      (toSalty (PA_VOID (AT_LABEL "x") ARROW QUESTION) `shouldBe` PA_VOID (AT_LABEL "x") ARROW QUESTION)

  describe "toSalty SET" $ do
    it
      "ST_BINDING recurses into its binding"
      ( toSalty (ST_BINDING (BI_PAIR (PA_TAU (AT_LABEL "x") ARROW (EX_ATTR (AT_LABEL "y"))) (BDS_EMPTY (TAB 1)) (TAB 1)))
          `shouldBe` ST_BINDING (BI_PAIR (PA_TAU (AT_LABEL "x") ARROW (EX_DISPATCH (EX_XI XI) NO_SPACE (AT_LABEL "y"))) (BDS_EMPTY (TAB 1)) (TAB 1))
      )
    it "ST_ATTRIBUTES is unchanged" (toSalty (ST_ATTRIBUTES [AT_LABEL "a"]) `shouldBe` ST_ATTRIBUTES [AT_LABEL "a"])

  describe "toSalty NUMBER" $ do
    it
      "LENGTH recurses into its binding"
      ( toSalty (LENGTH (BI_PAIR (PA_TAU (AT_LABEL "x") ARROW (EX_ATTR (AT_LABEL "y"))) (BDS_EMPTY (TAB 1)) (TAB 1)))
          `shouldBe` LENGTH (BI_PAIR (PA_TAU (AT_LABEL "x") ARROW (EX_DISPATCH (EX_XI XI) NO_SPACE (AT_LABEL "y"))) (BDS_EMPTY (TAB 1)) (TAB 1))
      )
    it "DOMAIN recurses into its binding" (toSalty (DOMAIN (BI_EMPTY (TAB 1))) `shouldBe` DOMAIN (BI_EMPTY (TAB 1)))
    it "LITERAL is unchanged" (toSalty (LITERAL 3) `shouldBe` LITERAL 3)

  describe "toSalty COMPARABLE" $ do
    it "CMP_ATTR is unchanged" (toSalty (CMP_ATTR (AT_LABEL "x")) `shouldBe` CMP_ATTR (AT_LABEL "x"))
    it
      "CMP_EXPR recurses into its expression"
      (toSalty (CMP_EXPR (EX_ATTR (AT_LABEL "y"))) `shouldBe` CMP_EXPR (EX_DISPATCH (EX_XI XI) NO_SPACE (AT_LABEL "y")))
    it "CMP_NUM recurses into its number" (toSalty (CMP_NUM (LITERAL 4)) `shouldBe` CMP_NUM (LITERAL 4))

  describe "toSalty CONDITION" $ do
    it
      "CO_BELONGS recurses into its set"
      (toSalty (CO_BELONGS (AT_LABEL "x") IN (ST_BINDING (BI_EMPTY (TAB 1)))) `shouldBe` CO_BELONGS (AT_LABEL "x") IN (ST_BINDING (BI_EMPTY (TAB 1))))
    it
      "CO_LOGIC recurses into every condition"
      (toSalty (CO_LOGIC [CO_NF exYAttr] AND) `shouldBe` CO_LOGIC [CO_NF dottedY] AND)
    it "CO_NF recurses into its expression" (toSalty (CO_NF exYAttr) `shouldBe` CO_NF dottedY)
    it "CO_ABSOLUTE recurses into its expression" (toSalty (CO_ABSOLUTE exYAttr IN) `shouldBe` CO_ABSOLUTE dottedY IN)
    it "CO_NOT recurses into its condition" (toSalty (CO_NOT (CO_NF exYAttr)) `shouldBe` CO_NOT (CO_NF dottedY))
    it
      "CO_COMPARE recurses into both sides"
      (toSalty (CO_COMPARE (CMP_ATTR (AT_LABEL "x")) EQUAL (CMP_EXPR exYAttr)) `shouldBe` CO_COMPARE (CMP_ATTR (AT_LABEL "x")) EQUAL (CMP_EXPR dottedY))
    it "CO_MATCHES recurses into its expression" (toSalty (CO_MATCHES "abc" exYAttr) `shouldBe` CO_MATCHES "abc" dottedY)
    it
      "CO_PART_OF recurses into its expression"
      (toSalty (CO_PART_OF exYAttr (BI_EMPTY (TAB 1))) `shouldBe` CO_PART_OF dottedY (BI_EMPTY (TAB 1)))
    it
      "CO_DISJOINT recurses into every group"
      (toSalty (CO_DISJOINT [AT_LABEL "a"] [BI_EMPTY (TAB 1)]) `shouldBe` CO_DISJOINT [AT_LABEL "a"] [BI_EMPTY (TAB 1)])
    it "CO_FORMATION recurses into its expression" (toSalty (CO_FORMATION exYAttr) `shouldBe` CO_FORMATION dottedY)
    it "CO_EMPTY is unchanged" (toSalty CO_EMPTY `shouldBe` CO_EMPTY)

  describe "toSalty EXTRA_ARG" $ do
    it "ARG_EXPR recurses" (toSalty (ARG_EXPR exYAttr) `shouldBe` ARG_EXPR dottedY)
    it "ARG_BINDING recurses" (toSalty (ARG_BINDING (BI_EMPTY (TAB 1))) `shouldBe` ARG_BINDING (BI_EMPTY (TAB 1)))
    it "ARG_ATTR is unchanged" (toSalty (ARG_ATTR (AT_LABEL "x")) `shouldBe` ARG_ATTR (AT_LABEL "x"))
    it "ARG_BYTES is unchanged" (toSalty (ARG_BYTES (BT_ONE "1F")) `shouldBe` ARG_BYTES (BT_ONE "1F"))

  describe "toSalty EXTRA" $
    it
      "recurses into its meta and every argument"
      ( toSalty (EXTRA (ARG_EXPR (EX_ATTR (AT_LABEL "y"))) "f" [ARG_ATTR (AT_LABEL "n")])
          `shouldBe` EXTRA (ARG_EXPR (EX_DISPATCH (EX_XI XI) NO_SPACE (AT_LABEL "y"))) "f" [ARG_ATTR (AT_LABEL "n")]
      )

  describe "withoutRho" $ do
    it
      "a formation whose only binding is rho collapses to the compact empty layout"
      ( withoutRho (EX_FORMATION LSB EOL (TAB 1) (BI_PAIR (PA_VOID (AT_RHO RHO) ARROW EMPTY) (BDS_EMPTY (TAB 1)) (TAB 1)) EOL (TAB 0) RSB)
          `shouldBe` EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY (TAB 1)) NO_EOL NO_TAB RSB
      )
    it
      "a formation whose head binding is a meta is left with its own layout"
      ( withoutRho (EX_FORMATION LSB EOL (TAB 1) (BI_META (META NO_EXCL B "X") (BDS_EMPTY (TAB 1)) (TAB 1)) EOL (TAB 0) RSB)
          `shouldBe` EX_FORMATION LSB EOL (TAB 1) (BI_META (META NO_EXCL B "X") (BDS_EMPTY (TAB 1)) (TAB 1)) EOL (TAB 0) RSB
      )
    it
      "dropping a leading rho promotes a following meta tail into the head binding"
      ( withoutRho
          (EX_FORMATION LSB EOL (TAB 1) (BI_PAIR (PA_VOID (AT_RHO RHO) ARROW EMPTY) (BDS_META EOL (TAB 1) (META NO_EXCL B "X") (BDS_EMPTY (TAB 1))) (TAB 1)) EOL (TAB 0) RSB)
          `shouldBe` EX_FORMATION LSB EOL (TAB 1) (BI_META (META NO_EXCL B "X") (BDS_EMPTY (TAB 1)) (TAB 1)) EOL (TAB 0) RSB
      )
    it
      "dropping a leading rho promotes a following pair tail into the head binding"
      ( withoutRho
          (EX_FORMATION LSB EOL (TAB 1) (BI_PAIR (PA_VOID (AT_RHO RHO) ARROW EMPTY) (BDS_PAIR EOL (TAB 1) (PA_TAU (AT_LABEL "x") ARROW xiExpr) (BDS_EMPTY (TAB 1))) (TAB 1)) EOL (TAB 0) RSB)
          `shouldBe` EX_FORMATION LSB EOL (TAB 1) (BI_PAIR (PA_TAU (AT_LABEL "x") ARROW xiExpr) (BDS_EMPTY (TAB 1)) (TAB 1)) EOL (TAB 0) RSB
      )
    it
      "a rho binding in the middle of a chain is dropped, its neighbours kept"
      ( withoutRho
          ( EX_FORMATION
              LSB
              EOL
              (TAB 1)
              ( BI_PAIR
                  (PA_TAU (AT_LABEL "x") ARROW xiExpr)
                  (BDS_PAIR EOL (TAB 1) (PA_VOID (AT_RHO RHO) ARROW EMPTY) (BDS_PAIR EOL (TAB 1) (PA_TAU (AT_LABEL "y") ARROW xiExpr) (BDS_EMPTY (TAB 1))))
                  (TAB 1)
              )
              EOL
              (TAB 0)
              RSB
          )
          `shouldBe` EX_FORMATION
            LSB
            EOL
            (TAB 1)
            (BI_PAIR (PA_TAU (AT_LABEL "x") ARROW xiExpr) (BDS_PAIR EOL (TAB 1) (PA_TAU (AT_LABEL "y") ARROW xiExpr) (BDS_EMPTY (TAB 1))) (TAB 1))
            EOL
            (TAB 0)
            RSB
      )
    it
      "a positional alpha binding is not rho and is kept, recursing through goPair"
      ( withoutRho (EX_APPLICATION xiExpr NO_SPACE EOL (TAB 1) (AA_TAUS (BI_PAIR (PA_ALPHA (AL_IDX ALPHA 0) ARROW xiExpr) (BDS_EMPTY (TAB 1)) (TAB 1))) EOL (TAB 0) 1)
          `shouldBe` EX_APPLICATION xiExpr NO_SPACE EOL (TAB 1) (AA_TAUS (BI_PAIR (PA_ALPHA (AL_IDX ALPHA 0) ARROW xiExpr) (BDS_EMPTY (TAB 1)) (TAB 1))) EOL (TAB 0) 1
      )
    it
      "an application whose only tau argument (AA_TAU) is rho collapses to its bare callee"
      ( withoutRho (EX_APPLICATION rootDotX NO_SPACE EOL (TAB 1) (AA_TAU (APP_BINDING (PA_TAU (AT_RHO RHO) ARROW xiExpr))) EOL (TAB 0) 1)
          `shouldBe` rootDotX
      )
    it
      "an application whose only tau argument (AA_TAU) is not rho is kept"
      ( withoutRho (EX_APPLICATION rootDotX NO_SPACE EOL (TAB 1) (AA_TAU (APP_BINDING (PA_TAU (AT_LABEL "y") ARROW xiExpr))) EOL (TAB 0) 1)
          `shouldBe` EX_APPLICATION rootDotX NO_SPACE EOL (TAB 1) (AA_TAU (APP_BINDING (PA_TAU (AT_LABEL "y") ARROW xiExpr))) EOL (TAB 0) 1
      )
    it
      "an application argument chain (AA_TAUS) that strips down to nothing collapses to the bare callee"
      ( withoutRho (EX_APPLICATION rootDotX NO_SPACE EOL (TAB 1) (AA_TAUS (BI_PAIR (PA_TAU (AT_RHO RHO) ARROW xiExpr) (BDS_EMPTY (TAB 1)) (TAB 1))) EOL (TAB 0) 1)
          `shouldBe` rootDotX
      )
    it
      "an application argument chain (AA_TAUS) keeps whatever remains after dropping rho"
      ( withoutRho
          ( EX_APPLICATION
              rootDotX
              NO_SPACE
              EOL
              (TAB 1)
              (AA_TAUS (BI_PAIR (PA_TAU (AT_RHO RHO) ARROW xiExpr) (BDS_PAIR EOL (TAB 1) (PA_TAU (AT_LABEL "z") ARROW xiExpr) (BDS_EMPTY (TAB 1))) (TAB 1)))
              EOL
              (TAB 0)
              1
          )
          `shouldBe` EX_APPLICATION
            rootDotX
            NO_SPACE
            EOL
            (TAB 1)
            (AA_TAUS (BI_PAIR (PA_TAU (AT_LABEL "z") ARROW xiExpr) (BDS_EMPTY (TAB 1)) (TAB 1)))
            EOL
            (TAB 0)
            1
      )
    it
      "an application argument chain (AA_TAUS) headed by a meta is kept"
      ( withoutRho (EX_APPLICATION rootDotX NO_SPACE EOL (TAB 1) (AA_TAUS (BI_META (META NO_EXCL B "X") (BDS_EMPTY (TAB 1)) (TAB 1))) EOL (TAB 0) 1)
          `shouldBe` EX_APPLICATION rootDotX NO_SPACE EOL (TAB 1) (AA_TAUS (BI_META (META NO_EXCL B "X") (BDS_EMPTY (TAB 1)) (TAB 1))) EOL (TAB 0) 1
      )
    it
      "an application argument chain (AA_TAUS) promotes a meta tail after dropping the leading rho"
      ( withoutRho
          ( EX_APPLICATION
              rootDotX
              NO_SPACE
              EOL
              (TAB 1)
              (AA_TAUS (BI_PAIR (PA_TAU (AT_RHO RHO) ARROW xiExpr) (BDS_META EOL (TAB 1) (META NO_EXCL B "X") (BDS_EMPTY (TAB 1))) (TAB 1)))
              EOL
              (TAB 0)
              1
          )
          `shouldBe` EX_APPLICATION rootDotX NO_SPACE EOL (TAB 1) (AA_TAUS (BI_META (META NO_EXCL B "X") (BDS_EMPTY (TAB 1)) (TAB 1))) EOL (TAB 0) 1
      )
    it
      "an application argument chain (AA_TAUS) drops a rho pair in its tail, keeping the head"
      ( withoutRho
          ( EX_APPLICATION
              rootDotX
              NO_SPACE
              EOL
              (TAB 1)
              (AA_TAUS (BI_PAIR (PA_TAU (AT_LABEL "a") ARROW xiExpr) (BDS_PAIR EOL (TAB 1) (PA_TAU (AT_RHO RHO) ARROW xiExpr) (BDS_EMPTY (TAB 1))) (TAB 1)))
              EOL
              (TAB 0)
              1
          )
          `shouldBe` EX_APPLICATION
            rootDotX
            NO_SPACE
            EOL
            (TAB 1)
            (AA_TAUS (BI_PAIR (PA_TAU (AT_LABEL "a") ARROW xiExpr) (BDS_EMPTY (TAB 1)) (TAB 1)))
            EOL
            (TAB 0)
            1
      )
    it
      "an application argument list (AA_EXPRS) is always kept and recurses"
      ( withoutRho (EX_APPLICATION xiExpr NO_SPACE EOL (TAB 1) (AA_EXPRS (APP_ARG xiExpr (AAS_EXPR EOL (TAB 1) rootExpr AAS_EMPTY))) EOL (TAB 0) 1)
          `shouldBe` EX_APPLICATION xiExpr NO_SPACE EOL (TAB 1) (AA_EXPRS (APP_ARG xiExpr (AAS_EXPR EOL (TAB 1) rootExpr AAS_EMPTY))) EOL (TAB 0) 1
      )
    it
      "a xi.rho dispatch value is left untouched (only bindings and app arguments are stripped)"
      (withoutRho (EX_DISPATCH xiExpr NO_SPACE (AT_RHO RHO)) `shouldBe` EX_DISPATCH xiExpr NO_SPACE (AT_RHO RHO))
    it
      "a phi-meet wrapper recurses into its wrapped expression via goExpr"
      ( withoutRho (EX_PHI_MEET Nothing 2 (EX_FORMATION LSB EOL (TAB 1) (BI_PAIR (PA_VOID (AT_RHO RHO) ARROW EMPTY) (BDS_EMPTY (TAB 1)) (TAB 1)) EOL (TAB 0) RSB))
          `shouldBe` EX_PHI_MEET Nothing 2 (EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY (TAB 1)) NO_EOL NO_TAB RSB)
      )
    it
      "a phi-again wrapper recurses into its wrapped expression via goExpr"
      ( withoutRho (EX_PHI_AGAIN (Just "a") 1 (EX_FORMATION LSB EOL (TAB 1) (BI_PAIR (PA_VOID (AT_RHO RHO) ARROW EMPTY) (BDS_EMPTY (TAB 1)) (TAB 1)) EOL (TAB 0) RSB))
          `shouldBe` EX_PHI_AGAIN (Just "a") 1 (EX_FORMATION LSB NO_EOL NO_TAB (BI_EMPTY (TAB 1)) NO_EOL NO_TAB RSB)
      )
    it
      "a lambda pair is not rho and is kept via the goPair catch-all"
      ( withoutRho (EX_FORMATION LSB EOL (TAB 1) (BI_PAIR (PA_LAMBDA "some.func") (BDS_EMPTY (TAB 1)) (TAB 1)) EOL (TAB 0) RSB)
          `shouldBe` EX_FORMATION LSB EOL (TAB 1) (BI_PAIR (PA_LAMBDA "some.func") (BDS_EMPTY (TAB 1)) (TAB 1)) EOL (TAB 0) RSB
      )

  describe "full pipeline round trips, SWEET vs SALTY" $ do
    let config :: SugarType -> (SugarType, Encoding, LineFormat, Int)
        config sugar = (sugar, UNICODE, SINGLELINE, defaultMargin)
    it
      "a sweet numeric literal expands into Q.number(Q.bytes(...)) when salted"
      $ do
        let number = DataNumber (BtMany ["40", "45", "00", "00", "00", "00", "00", "00"])
        printExpression' number (config SWEET) `shouldBe` "42"
        printExpression' number (config SALTY) `shouldBe` "Φ.number( as-bytes ↦ Φ.bytes( data ↦ ⟦ Δ ⤍ 40-45-00-00-00-00-00-00, ρ ↦ ∅ ⟧ ) )"
    it
      "a sweet string literal expands into Q.string(Q.bytes(...)) when salted"
      $ do
        let string = DataString (BtMany ["68", "69"])
        printExpression' string (config SWEET) `shouldBe` "\"hi\""
        printExpression' string (config SALTY) `shouldBe` "Φ.string( as-bytes ↦ Φ.bytes( data ↦ ⟦ Δ ⤍ 68-69, ρ ↦ ∅ ⟧ ) )"
    it
      "an application with multiple positional arguments sugars/salts between e(e0, e1) and e(α0 ↦ e0)(α1 ↦ e1)"
      $ do
        let multiArgApp = ExApplication (ExApplication (ExDispatch ExRoot (AtLabel "e")) (ArAlpha (Alpha 0) ExRoot)) (ArAlpha (Alpha 1) ExXi)
        printExpression' multiArgApp (config SWEET) `shouldBe` "Φ.e( Φ, ξ )"
        printExpression' multiArgApp (config SALTY) `shouldBe` "Φ.e( α0 ↦ Φ )( α1 ↦ ξ )"
    it
      "a nested object-with-params formation sugars/salts between obj(p, q) -> [[..]] and its expanded void bindings"
      $ do
        let nestedForm = ExFormation [BiTau (AtLabel "obj") (ExFormation [BiVoid (AtLabel "p"), BiVoid (AtLabel "q"), BiTau (AtLabel "z") ExXi])]
        printExpression' nestedForm (config SWEET) `shouldBe` "⟦ obj(p, q) ↦ ⟦ z ↦ ξ ⟧ ⟧"
        printExpression' nestedForm (config SALTY) `shouldBe` "⟦ obj ↦ ⟦ p ↦ ∅, q ↦ ∅, z ↦ ξ, ρ ↦ ∅ ⟧, ρ ↦ ∅ ⟧"
    it
      "a phi-meet/phi-again chain renders identically under both sugar types"
      $ do
        let meetChain = ExFormation [BiTau (AtLabel "x") (ExPhiMeet Nothing 2 (ExPhiAgain (Just "a") 1 (ExDispatch ExXi (AtLabel "y"))))]
        printExpression' meetChain (config SWEET) `shouldBe` "⟦ x ↦ \\phinoMeet{2}{ \\phinoAgain{a:1} } ⟧"
        printExpression' meetChain (config SALTY) `shouldBe` "⟦ x ↦ \\phinoMeet{2}{ \\phinoAgain{a:1} }, ρ ↦ ∅ ⟧"
