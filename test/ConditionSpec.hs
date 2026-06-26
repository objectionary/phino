{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module ConditionSpec where

import AST (Attribute (AtLabel, AtMeta), Binding (BiMeta), Expression (ExDispatch, ExMeta, ExRoot))
import Condition
import Control.Monad (forM_)
import Data.Either (isLeft, isRight)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Yaml qualified as Y

spec :: Spec
spec = do
  describe "just parses" $
    forM_
      [ "in (!t, !B)"
      , " not   (in (!t1,   !B))   "
      , "eq(1, 1)"
      , "gt(domain(!B), !i)"
      , "or(eq(!i,1),eq(length(!B),-2),eq(!e1,!e2),eq(!t1,x),eq(Q.org.eolang,[[ x -> 2 ]]))"
      , "nf([[ x -> !e ]].x)"
      , "absolute(!e1)"
      , "matches(\"hello(\\\"\\u0000)\", !e)"
      , "part-of ( [[ x -> 1 ]] , !B ) "
      , "not(formation(!n))"
      ]
      (\expr -> it expr (parseCondition expr `shouldSatisfy` isRight))

  describe "parses correctly" $
    forM_
      [ ("in(!t, !B)", Y.In (AtMeta "t") (BiMeta "B"))
      , ("not(in(!t,!B))", Y.Not (Y.In (AtMeta "t") (BiMeta "B")))
      , ("eq(1,-2)", Y.Eq (Y.CmpNum (Y.Literal 1)) (Y.CmpNum (Y.Literal (-2))))
      , ("eq(!i,length(!B1))", Y.Eq (Y.CmpNum (Y.MetaIndex "i")) (Y.CmpNum (Y.Length (BiMeta "B1"))))
      , ("eq(!i2,domain(!B1))", Y.Eq (Y.CmpNum (Y.MetaIndex "i2")) (Y.CmpNum (Y.Domain (BiMeta "B1"))))
      , ("gt(domain(!B1),!i)", Y.Gt (Y.CmpNum (Y.Domain (BiMeta "B1"))) (Y.CmpNum (Y.MetaIndex "i")))
      , ("eq(!t1, !e2)", Y.Eq (Y.CmpAttr (AtMeta "t1")) (Y.CmpExpr (ExMeta "e2")))
      , ("or(absolute(!e1), nf(Q.x))", Y.Or [Y.Absolute (ExMeta "e1"), Y.NF (ExDispatch ExRoot (AtLabel "x"))])
      , ("and(matches(\"hi\", !e),part-of(!e, !B))", Y.And [Y.Matches "hi" (ExMeta "e"), Y.PartOf (ExMeta "e") (BiMeta "B")])
      , ("not(formation(!n))", Y.Not (Y.IsFormation (ExMeta "n")))
      ]
      (\(expr, res) -> it expr (parseCondition expr `shouldBe` Right res))

  describe "does not parse" $
    forM_
      [ "some()"
      , "in(!t, !t)"
      , "or(or(), or())"
      ]
      (\expr -> it expr (parseCondition expr `shouldSatisfy` isLeft))
