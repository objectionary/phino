-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module ConditionSpec where

import AST (Attribute (AtLabel, AtMeta), Binding (BiMeta), Expression (ExDispatch, ExGlobal, ExMeta))
import Condition
import Control.Monad (forM_)
import Data.Either (isLeft, isRight)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Yaml qualified as Y

spec :: Spec
spec = do
  describe "just parses" $
    forM_
      [ "in (!a, !B)"
      , " not   (in (!a1,   !B))   "
      , "alpha(x)"
      , "eq(1, 1)"
      , "or(eq(index(a),1),eq(length(!B),-2),eq(!e1,!e2),eq(!a1,x),eq(Q.org.eolang,[[ x -> 2 ]]))"
      , "and(alpha(q),eq(-5,21))"
      , "nf([[ x -> !e ]].x)"
      , "xi(!e1)"
      , "matches(\"hello(\\\"\\u0000)\", !e)"
      , "part-of ( [[ x -> 1 ]] , !B ) "
      , "and(not(alpha(!a)),eq(!a,x))"
      ]
      (\expr -> it expr (parseCondition expr `shouldSatisfy` isRight))

  describe "parses correctly" $
    forM_
      [ ("in(!a, !B)", Y.In (AtMeta "a") (BiMeta "B"))
      , ("not(in(!a,!B))", Y.Not (Y.In (AtMeta "a") (BiMeta "B")))
      , ("alpha(y)", Y.Alpha (AtLabel "y"))
      , ("eq(1,-2)", Y.Eq (Y.CmpNum (Y.Literal 1)) (Y.CmpNum (Y.Literal (-2))))
      , ("eq(index(z),length(!B1))", Y.Eq (Y.CmpNum (Y.Index (AtLabel "z"))) (Y.CmpNum (Y.Length (BiMeta "B1"))))
      , ("eq(!a1, !e2)", Y.Eq (Y.CmpAttr (AtMeta "a1")) (Y.CmpExpr (ExMeta "e2")))
      , ("or(xi(!e1), nf(Q.x))", Y.Or [Y.Xi (ExMeta "e1"), Y.NF (ExDispatch ExGlobal (AtLabel "x"))])
      , ("and(matches(\"hi\", !e),part-of(!e, !B))", Y.And [Y.Matches "hi" (ExMeta "e"), Y.PartOf (ExMeta "e") (BiMeta "B")])
      ]
      (\(expr, res) -> it expr (parseCondition expr `shouldBe` Right res))

  describe "does not parse" $
    forM_
      [ "some()"
      , "in(!a, !a)"
      , "alpha(!B)"
      , "or(or(), or())"
      ]
      (\expr -> it expr (parseCondition expr `shouldSatisfy` isLeft))
