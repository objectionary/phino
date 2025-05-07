-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module RewriterSpec where

import Ast
import Rewriter
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "rewriteProgram" $ do
    it "rewrites" $ do
      shouldBe
        ( rewriteProgram
            (Program (ExApplication (ExDispatch (ExDispatch ExGlobal (AtLabel "y")) (AtLabel "x")) []))
            (ExDispatch ExGlobal (AtLabel "y"))
            [ExThis]
        )
        (Program (ExApplication (ExDispatch ExThis (AtLabel "x")) []))
