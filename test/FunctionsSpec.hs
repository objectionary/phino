{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module FunctionsSpec where

import AST
import Data.Map.Strict qualified as Map
import Deps (Term (TeAttribute, TeBindings, TeBytes, TeExpression))
import Functions (buildTerm)
import Logger (logDebug)
import Matcher (MetaValue (MvBindings, MvBytes, MvExpression), Subst (Subst), defaultScope, substEmpty)
import Misc (numToBts, strToBts, uniqueBindings', pattern DataNumber, pattern DataString)
import Printer (printExpression)
import Test.Hspec (Spec, anyException, describe, it, shouldBe, shouldSatisfy, shouldThrow)
import Text.Printf (printf)
import Yaml (ExtraArgument (ArgAttribute, ArgBinding, ArgBytes, ArgExpression))

spec :: Test.Hspec.Spec
spec = do
  describe "join function" $ do
    Test.Hspec.it "returns only unique bindings after join" $ do
      let first = ("B1", MvBindings [BiVoid AtRho, BiDelta BtEmpty, BiTau (AtLabel "x") ExGlobal, BiVoid (AtAlpha 0)])
          second = ("B2", MvBindings [BiTau AtRho ExThis, BiLambda "Func", BiDelta (BtOne "00"), BiVoid (AtAlpha 1)])
          third = ("B3", MvBindings [BiLambda "Some", BiTau (AtLabel "y") ExThis, BiTau (AtLabel "x") ExThis, BiVoid (AtAlpha 0)])
          subst = Subst (Map.fromList [first, second, third])
      TeBindings bds <- buildTerm "join" [ArgBinding (BiMeta "B1"), ArgBinding (BiMeta "B2"), ArgBinding (BiMeta "B3")] subst
      bds' <- uniqueBindings' bds
      logDebug (printf "Joined bindings:\n%s" (printExpression (ExFormation bds')))
      length bds' `shouldBe` 9

    Test.Hspec.it "returns empty bindings for empty arguments" $ do
      TeBindings bds <- buildTerm "join" [] substEmpty
      bds `shouldBe` []

    Test.Hspec.it "preserves order of first occurrence in joined bindings" $ do
      let first = ("B1", MvBindings [BiVoid (AtLabel "a")])
          second = ("B2", MvBindings [BiVoid (AtLabel "b")])
          subst = Subst (Map.fromList [first, second])
      TeBindings bds <- buildTerm "join" [ArgBinding (BiMeta "B1"), ArgBinding (BiMeta "B2")] subst
      bds `shouldBe` [BiVoid (AtLabel "a"), BiVoid (AtLabel "b")]

    Test.Hspec.it "fails with non-binding argument" $
      buildTerm "join" [ArgExpression ExGlobal] substEmpty `shouldThrow` anyException

  describe "contextualize function" $ do
    Test.Hspec.it "replaces this with context expression" $ do
      let expr = ("e", MvExpression ExThis defaultScope)
          context = ("c", MvExpression (ExDispatch ExGlobal (AtLabel "ctx")) defaultScope)
          subst = Subst (Map.fromList [expr, context])
      TeExpression result <- buildTerm "contextualize" [ArgExpression (ExMeta "e"), ArgExpression (ExMeta "c")] subst
      result `shouldBe` ExDispatch ExGlobal (AtLabel "ctx")

    Test.Hspec.it "preserves global in contextualized expression" $ do
      let expr = ("e", MvExpression ExGlobal defaultScope)
          context = ("c", MvExpression ExThis defaultScope)
          subst = Subst (Map.fromList [expr, context])
      TeExpression result <- buildTerm "contextualize" [ArgExpression (ExMeta "e"), ArgExpression (ExMeta "c")] subst
      result `shouldBe` ExGlobal

    Test.Hspec.it "preserves termination in contextualized expression" $ do
      let expr = ("e", MvExpression ExTermination defaultScope)
          context = ("c", MvExpression ExThis defaultScope)
          subst = Subst (Map.fromList [expr, context])
      TeExpression result <- buildTerm "contextualize" [ArgExpression (ExMeta "e"), ArgExpression (ExMeta "c")] subst
      result `shouldBe` ExTermination

    Test.Hspec.it "contextualizes dispatch expression" $ do
      let expr = ("e", MvExpression (ExDispatch ExThis (AtLabel "attr")) defaultScope)
          context = ("c", MvExpression (ExFormation []) defaultScope)
          subst = Subst (Map.fromList [expr, context])
      TeExpression result <- buildTerm "contextualize" [ArgExpression (ExMeta "e"), ArgExpression (ExMeta "c")] subst
      result `shouldBe` ExDispatch (ExFormation []) (AtLabel "attr")

    Test.Hspec.it "fails with single argument" $
      buildTerm "contextualize" [ArgExpression ExGlobal] substEmpty `shouldThrow` anyException

    Test.Hspec.it "fails with non-expression arguments" $
      buildTerm "contextualize" [ArgAttribute (AtLabel "a"), ArgAttribute (AtLabel "b")] substEmpty `shouldThrow` anyException

  describe "scope function" $ do
    Test.Hspec.it "extracts scope from expression" $ do
      let scope = ExFormation [BiVoid (AtLabel "scoped")]
          expr = ("e", MvExpression ExThis scope)
          subst = Subst (Map.fromList [expr])
      TeExpression result <- buildTerm "scope" [ArgExpression (ExMeta "e")] subst
      result `shouldBe` scope

    Test.Hspec.it "fails with multiple arguments" $
      buildTerm "scope" [ArgExpression ExGlobal, ArgExpression ExThis] substEmpty `shouldThrow` anyException

    Test.Hspec.it "fails with no arguments" $
      buildTerm "scope" [] substEmpty `shouldThrow` anyException

  describe "dataize function" $ do
    Test.Hspec.it "returns bytes from bytes argument" $ do
      let bytes = BtMany ["01", "02", "03"]
          subst = Subst (Map.fromList [("b", MvBytes bytes)])
      TeBytes result <- buildTerm "dataize" [ArgBytes (BtMeta "b")] subst
      result `shouldBe` bytes

    Test.Hspec.it "extracts bytes from data object" $ do
      let bytes = strToBts "héllo"
          expr = ("e", MvExpression (DataString bytes) defaultScope)
          subst = Subst (Map.fromList [expr])
      TeBytes result <- buildTerm "dataize" [ArgExpression (ExMeta "e")] subst
      result `shouldBe` bytes

    Test.Hspec.it "fails with non-data expression" $ do
      let expr = ("e", MvExpression ExGlobal defaultScope)
          subst = Subst (Map.fromList [expr])
      buildTerm "dataize" [ArgExpression (ExMeta "e")] subst `shouldThrow` anyException

    Test.Hspec.it "fails with multiple arguments" $
      buildTerm "dataize" [ArgBytes BtEmpty, ArgBytes BtEmpty] substEmpty `shouldThrow` anyException

  describe "concat function" $ do
    Test.Hspec.it "concatenates two string values" $ do
      let e1 = ("e1", MvExpression (DataString (strToBts "hello")) defaultScope)
          e2 = ("e2", MvExpression (DataString (strToBts "world")) defaultScope)
          subst = Subst (Map.fromList [e1, e2])
      TeExpression (DataString result) <- buildTerm "concat" [ArgExpression (ExMeta "e1"), ArgExpression (ExMeta "e2")] subst
      result `shouldBe` strToBts "helloworld"

    Test.Hspec.it "concatenates unicode strings" $ do
      let e1 = ("e1", MvExpression (DataString (strToBts "привет")) defaultScope)
          e2 = ("e2", MvExpression (DataString (strToBts "мир")) defaultScope)
          subst = Subst (Map.fromList [e1, e2])
      TeExpression (DataString result) <- buildTerm "concat" [ArgExpression (ExMeta "e1"), ArgExpression (ExMeta "e2")] subst
      result `shouldBe` strToBts "приветмир"

    Test.Hspec.it "returns empty string for empty arguments" $ do
      TeExpression (DataString result) <- buildTerm "concat" [] substEmpty
      result `shouldBe` BtEmpty

  describe "sed function" $ do
    Test.Hspec.it "replaces first occurrence in string" $ do
      let str = ("s", MvExpression (DataString (strToBts "hello world")) defaultScope)
          pat = ("p", MvExpression (DataString (strToBts "s/world/there/")) defaultScope)
          subst = Subst (Map.fromList [str, pat])
      TeExpression (DataString result) <- buildTerm "sed" [ArgExpression (ExMeta "s"), ArgExpression (ExMeta "p")] subst
      result `shouldBe` strToBts "hello there"

    Test.Hspec.it "replaces all occurrences with global flag" $ do
      let str = ("s", MvExpression (DataString (strToBts "aa bb aa")) defaultScope)
          pat = ("p", MvExpression (DataString (strToBts "s/aa/xx/g")) defaultScope)
          subst = Subst (Map.fromList [str, pat])
      TeExpression (DataString result) <- buildTerm "sed" [ArgExpression (ExMeta "s"), ArgExpression (ExMeta "p")] subst
      result `shouldBe` strToBts "xx bb xx"

    Test.Hspec.it "applies multiple substitution patterns" $ do
      let str = ("s", MvExpression (DataString (strToBts "foo bar")) defaultScope)
          p1 = ("p1", MvExpression (DataString (strToBts "s/foo/baz/")) defaultScope)
          p2 = ("p2", MvExpression (DataString (strToBts "s/bar/qux/")) defaultScope)
          subst = Subst (Map.fromList [str, p1, p2])
      TeExpression (DataString result) <- buildTerm "sed" [ArgExpression (ExMeta "s"), ArgExpression (ExMeta "p1"), ArgExpression (ExMeta "p2")] subst
      result `shouldBe` strToBts "baz qux"

    Test.Hspec.it "fails with less than two arguments" $
      buildTerm "sed" [ArgExpression (ExMeta "s")] substEmpty `shouldThrow` anyException

    Test.Hspec.it "fails with invalid pattern format" $ do
      let str = ("s", MvExpression (DataString (strToBts "test")) defaultScope)
          pat = ("p", MvExpression (DataString (strToBts "invalid")) defaultScope)
          subst = Subst (Map.fromList [str, pat])
      buildTerm "sed" [ArgExpression (ExMeta "s"), ArgExpression (ExMeta "p")] subst `shouldThrow` anyException

  describe "random-string function" $ do
    Test.Hspec.it "generates string matching pattern" $ do
      let pat = ("p", MvExpression (DataString (strToBts "test")) defaultScope)
          subst = Subst (Map.fromList [pat])
      TeExpression (DataString result) <- buildTerm "random-string" [ArgExpression (ExMeta "p")] subst
      result `shouldBe` strToBts "test"

    Test.Hspec.it "generates unique random digit strings" $ do
      let pat = ("p", MvExpression (DataString (strToBts "n%d")) defaultScope)
          subst = Subst (Map.fromList [pat])
      TeExpression (DataString r1) <- buildTerm "random-string" [ArgExpression (ExMeta "p")] subst
      TeExpression (DataString r2) <- buildTerm "random-string" [ArgExpression (ExMeta "p")] subst
      (r1 /= r2) `shouldBe` True

    Test.Hspec.it "fails with multiple arguments" $
      buildTerm "random-string" [ArgExpression ExGlobal, ArgExpression ExThis] substEmpty `shouldThrow` anyException

  describe "size function" $ do
    Test.Hspec.it "returns size of binding list" $ do
      let bds = ("B", MvBindings [BiVoid (AtLabel "a"), BiVoid (AtLabel "b"), BiVoid (AtLabel "c")])
          subst = Subst (Map.fromList [bds])
      TeExpression (DataNumber result) <- buildTerm "size" [ArgBinding (BiMeta "B")] subst
      result `shouldBe` numToBts 3

    Test.Hspec.it "returns zero for empty binding list" $ do
      let bds = ("B", MvBindings [])
          subst = Subst (Map.fromList [bds])
      TeExpression (DataNumber result) <- buildTerm "size" [ArgBinding (BiMeta "B")] subst
      result `shouldBe` numToBts 0

    Test.Hspec.it "fails with non-meta binding" $
      buildTerm "size" [ArgBinding (BiVoid (AtLabel "x"))] substEmpty `shouldThrow` anyException

  describe "tau function" $ do
    Test.Hspec.it "converts string to attribute" $ do
      let expr = ("e", MvExpression (DataString (strToBts "myattr")) defaultScope)
          subst = Subst (Map.fromList [expr])
      TeAttribute result <- buildTerm "tau" [ArgExpression (ExMeta "e")] subst
      result `shouldBe` AtLabel "myattr"

    Test.Hspec.it "converts string to alpha attribute" $ do
      let expr = ("e", MvExpression (DataString (strToBts "α42")) defaultScope)
          subst = Subst (Map.fromList [expr])
      TeAttribute result <- buildTerm "tau" [ArgExpression (ExMeta "e")] subst
      result `shouldBe` AtAlpha 42

    Test.Hspec.it "converts string to rho attribute" $ do
      let expr = ("e", MvExpression (DataString (strToBts "ρ")) defaultScope)
          subst = Subst (Map.fromList [expr])
      TeAttribute result <- buildTerm "tau" [ArgExpression (ExMeta "e")] subst
      result `shouldBe` AtRho

    Test.Hspec.it "fails with multiple arguments" $
      buildTerm "tau" [ArgExpression ExGlobal, ArgExpression ExThis] substEmpty `shouldThrow` anyException

  describe "string function" $ do
    Test.Hspec.it "converts number to string" $ do
      let expr = ("e", MvExpression (DataNumber (numToBts 42)) defaultScope)
          subst = Subst (Map.fromList [expr])
      TeExpression (DataString result) <- buildTerm "string" [ArgExpression (ExMeta "e")] subst
      result `shouldBe` strToBts "42"

    Test.Hspec.it "preserves string value" $ do
      let expr = ("e", MvExpression (DataString (strToBts "Ω")) defaultScope)
          subst = Subst (Map.fromList [expr])
      TeExpression (DataString result) <- buildTerm "string" [ArgExpression (ExMeta "e")] subst
      result `shouldBe` strToBts "Ω"

    Test.Hspec.it "converts attribute to string" $ do
      TeExpression (DataString result) <- buildTerm "string" [ArgAttribute (AtLabel "attr")] substEmpty
      result `shouldBe` strToBts "attr"

    Test.Hspec.it "converts rho attribute to string" $ do
      TeExpression (DataString result) <- buildTerm "string" [ArgAttribute AtRho] substEmpty
      result `shouldBe` strToBts "ρ"

    Test.Hspec.it "fails with non-data expression" $ do
      let expr = ("e", MvExpression ExGlobal defaultScope)
          subst = Subst (Map.fromList [expr])
      buildTerm "string" [ArgExpression (ExMeta "e")] subst `shouldThrow` anyException

    Test.Hspec.it "fails with no arguments" $
      buildTerm "string" [] substEmpty `shouldThrow` anyException

  describe "number function" $ do
    Test.Hspec.it "parses integer from string" $ do
      let expr = ("e", MvExpression (DataString (strToBts "123")) defaultScope)
          subst = Subst (Map.fromList [expr])
      TeExpression (DataNumber result) <- buildTerm "number" [ArgExpression (ExMeta "e")] subst
      result `shouldBe` numToBts 123

    Test.Hspec.it "parses float from string" $ do
      let expr = ("e", MvExpression (DataString (strToBts "3.14")) defaultScope)
          subst = Subst (Map.fromList [expr])
      TeExpression (DataNumber result) <- buildTerm "number" [ArgExpression (ExMeta "e")] subst
      result `shouldBe` numToBts 3.14

    Test.Hspec.it "parses negative number from string" $ do
      let expr = ("e", MvExpression (DataString (strToBts "-42")) defaultScope)
          subst = Subst (Map.fromList [expr])
      TeExpression (DataNumber result) <- buildTerm "number" [ArgExpression (ExMeta "e")] subst
      result `shouldBe` numToBts (-42)

    Test.Hspec.it "fails with non-string expression" $ do
      let expr = ("e", MvExpression (DataNumber (numToBts 42)) defaultScope)
          subst = Subst (Map.fromList [expr])
      buildTerm "number" [ArgExpression (ExMeta "e")] subst `shouldThrow` anyException

    Test.Hspec.it "fails with no arguments" $
      buildTerm "number" [] substEmpty `shouldThrow` anyException

  describe "sum function" $ do
    Test.Hspec.it "sums two numbers" $ do
      let e1 = ("e1", MvExpression (DataNumber (numToBts 10)) defaultScope)
          e2 = ("e2", MvExpression (DataNumber (numToBts 32)) defaultScope)
          subst = Subst (Map.fromList [e1, e2])
      TeExpression (DataNumber result) <- buildTerm "sum" [ArgExpression (ExMeta "e1"), ArgExpression (ExMeta "e2")] subst
      result `shouldBe` numToBts 42

    Test.Hspec.it "returns zero for empty arguments" $ do
      TeExpression (DataNumber result) <- buildTerm "sum" [] substEmpty
      result `shouldBe` numToBts 0

    Test.Hspec.it "sums multiple numbers" $ do
      let e1 = ("e1", MvExpression (DataNumber (numToBts 1)) defaultScope)
          e2 = ("e2", MvExpression (DataNumber (numToBts 2)) defaultScope)
          e3 = ("e3", MvExpression (DataNumber (numToBts 3)) defaultScope)
          subst = Subst (Map.fromList [e1, e2, e3])
      TeExpression (DataNumber result) <- buildTerm "sum" [ArgExpression (ExMeta "e1"), ArgExpression (ExMeta "e2"), ArgExpression (ExMeta "e3")] subst
      result `shouldBe` numToBts 6

    Test.Hspec.it "sums negative numbers" $ do
      let e1 = ("e1", MvExpression (DataNumber (numToBts 10)) defaultScope)
          e2 = ("e2", MvExpression (DataNumber (numToBts (-3))) defaultScope)
          subst = Subst (Map.fromList [e1, e2])
      TeExpression (DataNumber result) <- buildTerm "sum" [ArgExpression (ExMeta "e1"), ArgExpression (ExMeta "e2")] subst
      result `shouldBe` numToBts 7

  describe "random-tau function" $ do
    Test.Hspec.it "generates unique attribute excluding given attributes" $ do
      TeAttribute result <- buildTerm "random-tau" [ArgAttribute (AtLabel "x"), ArgAttribute (AtLabel "y")] substEmpty
      (result /= AtLabel "x" && result /= AtLabel "y") `shouldBe` True

    Test.Hspec.it "generates unique attribute excluding bindings" $ do
      let bds = ("B", MvBindings [BiVoid (AtLabel "a"), BiVoid (AtLabel "b")])
          subst = Subst (Map.fromList [bds])
      TeAttribute result <- buildTerm "random-tau" [ArgBinding (BiMeta "B")] subst
      (result /= AtLabel "a" && result /= AtLabel "b") `shouldBe` True

    Test.Hspec.it "ignores expression arguments" $ do
      TeAttribute result <- buildTerm "random-tau" [ArgExpression ExGlobal] substEmpty
      result `shouldSatisfy` \case
        AtLabel _ -> True
        _ -> False

    Test.Hspec.it "fails with bytes argument" $
      buildTerm "random-tau" [ArgBytes BtEmpty] substEmpty `shouldThrow` anyException

  describe "unsupported function" $ do
    Test.Hspec.it "fails with unknown function name" $
      buildTerm "unknown-function" [] substEmpty `shouldThrow` anyException

    Test.Hspec.it "fails with unicode function name" $
      buildTerm "функция" [] substEmpty `shouldThrow` anyException
