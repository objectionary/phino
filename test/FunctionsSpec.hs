{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module FunctionsSpec where

import AST
import Bytes (numToBts, strToBts)
import Control.Exception (SomeException)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Deps (Term (TeAttribute, TeBindings, TeBytes, TeExpression))
import Functions (buildTerm)
import Logger (logDebug)
import Matcher (MetaValue (MvBindings), Subst (Subst), substEmpty)
import Misc (uniqueBindings')
import Printer (printExpression)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldThrow)
import Text.Printf (printf)
import Yaml (ExtraArgument (ArgAttribute, ArgBinding, ArgBytes, ArgExpression))

throwsWith :: IO a -> String -> IO ()
throwsWith action needle =
  action `shouldThrow` (\exc -> needle `isInfixOf` show (exc :: SomeException))

-- 'Term' carries no 'Show'/'Eq' instance, so a term coming back from
-- 'buildTerm' is checked by pattern-matching out the constructor expected and
-- comparing the payload, which does have both.
expectExpression :: Term -> Expression -> Expectation
expectExpression (TeExpression got) want = got `shouldBe` want
expectExpression _ _ = fail "expected a TeExpression term"

expectBytes :: Term -> Bytes -> Expectation
expectBytes (TeBytes got) want = got `shouldBe` want
expectBytes _ _ = fail "expected a TeBytes term"

expectAttribute :: Term -> Attribute -> Expectation
expectAttribute (TeAttribute got) want = got `shouldBe` want
expectAttribute _ _ = fail "expected a TeAttribute term"

spec :: Spec
spec = describe "Functions" $ do
  it "contains only unique bindings after 'join'" $ do
    let first = ("B1", MvBindings [BiVoid AtRho, BiDelta BtEmpty, BiTau (AtLabel "x") ExRoot, BiVoid (AtLabel "a0")])
        second = ("B2", MvBindings [BiTau AtRho ExXi, BiLambda (Function "Func"), BiDelta (BtOne "00"), BiVoid (AtLabel "a1")])
        third = ("B3", MvBindings [BiLambda (Function "Some"), BiTau (AtLabel "y") ExXi, BiTau (AtLabel "x") ExXi, BiVoid (AtLabel "a0")])
        subst = Subst (Map.fromList [first, second, third])
    TeBindings bds <- buildTerm "join" [ArgBinding (BiMeta "B1"), ArgBinding (BiMeta "B2"), ArgBinding (BiMeta "B3")] subst
    bds' <- uniqueBindings' bds
    logDebug (printf "Joined bindings:\n%s" (printExpression (ExFormation bds')))
    length bds' `shouldBe` 9

  it "renames a duplicate tau binding (not rho/delta/lambda) instead of dropping it" $ do
    let first = ("B1", MvBindings [BiTau (AtLabel "x") ExRoot])
        second = ("B2", MvBindings [BiTau (AtLabel "x") ExXi])
        subst = Subst (Map.fromList [first, second])
    TeBindings bds <- buildTerm "join" [ArgBinding (BiMeta "B1"), ArgBinding (BiMeta "B2")] subst
    length bds `shouldBe` 2

  it "joins no bindings when given no arguments" $ do
    TeBindings bds <- buildTerm "join" [] substEmpty
    bds `shouldBe` []

  describe "contextualize" $ do
    it "replaces xi with the given context" $ do
      term <- buildTerm "contextualize" [ArgExpression ExXi, ArgExpression (ExFormation [BiVoid AtRho])] substEmpty
      expectExpression term (ExFormation [BiVoid AtRho])
    it "fails on the wrong number of arguments" $
      buildTerm "contextualize" [ArgExpression ExXi] substEmpty
        `throwsWith` "contextualize() requires exactly 2 arguments"

  describe "random-tau" $ do
    it "returns a fresh label attribute" $ do
      term <- buildTerm "random-tau" [] substEmpty
      case term of
        TeAttribute (AtLabel _) -> pure ()
        _ -> fail "expected a label attribute"
    it "fails when given arguments" $
      buildTerm "random-tau" [ArgExpression ExRoot] substEmpty
        `throwsWith` "random-tau() requires exactly 0 arguments"

  describe "dataize" $ do
    it "extracts bytes from a bytes argument" $ do
      term <- buildTerm "dataize" [ArgBytes (BtOne "00")] substEmpty
      expectBytes term (BtOne "00")
    it "extracts bytes from a data-object expression" $ do
      term <- buildTerm "dataize" [ArgExpression (DataNumber (numToBts 5))] substEmpty
      expectBytes term (numToBts 5)
    it "fails on a non-data-object expression" $
      buildTerm "dataize" [ArgExpression ExRoot] substEmpty
        `throwsWith` "Only data objects and bytes are supported"
    it "fails on the wrong number of arguments" $
      buildTerm "dataize" [ArgExpression ExRoot, ArgExpression ExXi] substEmpty
        `throwsWith` "dataize() requires exactly 1 argument"

  describe "concat" $
    it "concatenates several string arguments" $ do
      term <- buildTerm "concat" [ArgExpression (DataString (strToBts "foo")), ArgExpression (DataString (strToBts "bar"))] substEmpty
      expectExpression term (DataString (strToBts "foobar"))

  describe "sed" $ do
    it "replaces every occurrence with the 'g' flag" $ do
      term <- buildTerm "sed" [ArgExpression (DataString (strToBts "hello")), ArgExpression (DataString (strToBts "s/l/L/g"))] substEmpty
      expectExpression term (DataString (strToBts "heLLo"))
    it "replaces only the first occurrence without the 'g' flag" $ do
      term <- buildTerm "sed" [ArgExpression (DataString (strToBts "hello")), ArgExpression (DataString (strToBts "s/l/L/"))] substEmpty
      expectExpression term (DataString (strToBts "heLlo"))
    it "applies several patterns in sequence" $ do
      term <-
        buildTerm
          "sed"
          [ ArgExpression (DataString (strToBts "hello"))
          , ArgExpression (DataString (strToBts "s/h/H/"))
          , ArgExpression (DataString (strToBts "s/o/O/"))
          ]
          substEmpty
      expectExpression term (DataString (strToBts "HellO"))
    it "fails on fewer than two arguments" $
      buildTerm "sed" [ArgExpression (DataString (strToBts "hello"))] substEmpty
        `throwsWith` "sed() requires at least two arguments"
    it "fails when the pattern does not start with 's/'" $
      buildTerm "sed" [ArgExpression (DataString (strToBts "hello")), ArgExpression (DataString (strToBts "l/L/"))] substEmpty
        `throwsWith` "sed pattern must start with s/"
    it "fails when the pattern has an unknown trailing flag" $
      buildTerm "sed" [ArgExpression (DataString (strToBts "hello")), ArgExpression (DataString (strToBts "s/l/L/x"))] substEmpty
        `throwsWith` "sed pattern must be in format s/pat/rep/[g]"

  describe "random-string" $ do
    it "returns a literal pattern with no specials unchanged" $ do
      term <- buildTerm "random-string" [ArgExpression (DataString (strToBts "static-name"))] substEmpty
      expectExpression term (DataString (strToBts "static-name"))
    it "fails on the wrong number of arguments" $
      buildTerm "random-string" [] substEmpty
        `throwsWith` "random-string() requires exactly 1"

  describe "size" $ do
    it "counts the bindings bound to a meta" $ do
      let subst = Subst (Map.singleton "B" (MvBindings [BiVoid AtRho, BiVoid (AtLabel "x")]))
      term <- buildTerm "size" [ArgBinding (BiMeta "B")] subst
      expectExpression term (DataNumber (numToBts 2))
    it "fails on a non-meta binding argument" $
      buildTerm "size" [ArgBinding (BiVoid AtRho)] substEmpty
        `throwsWith` "size() requires exactly 1 meta binding"

  describe "tau" $ do
    it "parses an attribute out of a string expression" $ do
      term <- buildTerm "tau" [ArgExpression (DataString (strToBts "x"))] substEmpty
      expectAttribute term (AtLabel "x")
    it "fails on the wrong number of arguments" $
      buildTerm "tau" [] substEmpty
        `throwsWith` "tau() requires exactly 1 argument"

  describe "string" $ do
    it "converts a number expression to a string" $ do
      term <- buildTerm "string" [ArgExpression (DataNumber (numToBts 5))] substEmpty
      expectExpression term (DataString (strToBts "5"))
    it "keeps a string expression as is" $ do
      term <- buildTerm "string" [ArgExpression (DataString (strToBts "already"))] substEmpty
      expectExpression term (DataString (strToBts "already"))
    it "renders an attribute" $ do
      term <- buildTerm "string" [ArgAttribute AtRho] substEmpty
      expectExpression term (DataString (strToBts "ρ"))
    it "fails on an expression that is neither a number nor a string" $
      buildTerm "string" [ArgExpression ExRoot] substEmpty
        `throwsWith` "only 'Φ.number' or 'Φ.string' are allowed"
    it "fails on the wrong number of arguments" $
      buildTerm "string" [] substEmpty
        `throwsWith` "string() requires exactly 1 argument"

  describe "number" $ do
    it "parses a number out of a string expression" $ do
      term <- buildTerm "number" [ArgExpression (DataString (strToBts "42"))] substEmpty
      expectExpression term (DataNumber (numToBts 42))
    it "fails on an expression that is not a string" $
      buildTerm "number" [ArgExpression (DataNumber (numToBts 1))] substEmpty
        `throwsWith` "expects expression to be 'Φ.string'"
    it "fails on the wrong number of arguments" $
      buildTerm "number" [] substEmpty
        `throwsWith` "number() requires exactly 1 argument"

  describe "sum" $
    it "adds several numeric arguments" $ do
      term <- buildTerm "sum" [ArgExpression (DataNumber (numToBts 2)), ArgExpression (DataNumber (numToBts 3))] substEmpty
      expectExpression term (DataNumber (numToBts 5))

  describe "an unsupported function name" $
    it "fails with a descriptive message" $
      buildTerm "no-such-function" [] substEmpty
        `throwsWith` "Function no-such-function() is not supported or does not exist"
