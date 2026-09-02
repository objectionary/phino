{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module FunctionsSpec where

import AST
import Bytes (numToBts, strToBts)
import Control.Exception (SomeException)
import Control.Monad (forM_)
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
  describe "join" $ do
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

  describe "contextualize" $
    it "replaces xi with the given context" $ do
      term <- buildTerm "contextualize" [ArgExpression ExXi, ArgExpression (ExFormation [BiVoid AtRho])] substEmpty
      expectExpression term (ExFormation [BiVoid AtRho])

  describe "random-tau" $
    it "returns a fresh label attribute" $ do
      term <- buildTerm "random-tau" [] substEmpty
      case term of
        TeAttribute (AtLabel _) -> pure ()
        _ -> fail "expected a label attribute"

  describe "dataize" $ do
    it "extracts bytes from a bytes argument" $ do
      term <- buildTerm "dataize" [ArgBytes (BtOne "00")] substEmpty
      expectBytes term (BtOne "00")
    it "extracts bytes from a data-object expression" $ do
      term <- buildTerm "dataize" [ArgExpression (DataNumber (numToBts 5))] substEmpty
      expectBytes term (numToBts 5)

  describe "size" $
    it "counts the bindings bound to a meta" $ do
      let subst = Subst (Map.singleton "B" (MvBindings [BiVoid AtRho, BiVoid (AtLabel "x")]))
      term <- buildTerm "size" [ArgBinding (BiMeta "B")] subst
      expectExpression term (DataNumber (numToBts 2))

  describe "successful calls" $
    forM_
      successCases
      (\(desc, name, args, check) -> it desc (buildTerm name args substEmpty >>= check))

  describe "calls that fail with a descriptive error" $
    forM_
      failureCases
      (\(desc, name, args, message) -> it desc (buildTerm name args substEmpty `throwsWith` message))
  where
    successCases :: [(String, String, [ExtraArgument], Term -> Expectation)]
    successCases =
      [
        ( "concat concatenates several string arguments"
        , "concat"
        , [ArgExpression (DataString (strToBts "foo")), ArgExpression (DataString (strToBts "bar"))]
        , \term -> expectExpression term (DataString (strToBts "foobar"))
        )
      ,
        ( "sed replaces every occurrence with the 'g' flag"
        , "sed"
        , [ArgExpression (DataString (strToBts "hello")), ArgExpression (DataString (strToBts "s/l/L/g"))]
        , \term -> expectExpression term (DataString (strToBts "heLLo"))
        )
      ,
        ( "sed replaces only the first occurrence without the 'g' flag"
        , "sed"
        , [ArgExpression (DataString (strToBts "hello")), ArgExpression (DataString (strToBts "s/l/L/"))]
        , \term -> expectExpression term (DataString (strToBts "heLlo"))
        )
      ,
        ( "sed applies several patterns in sequence"
        , "sed"
        ,
          [ ArgExpression (DataString (strToBts "hello"))
          , ArgExpression (DataString (strToBts "s/h/H/"))
          , ArgExpression (DataString (strToBts "s/o/O/"))
          ]
        , \term -> expectExpression term (DataString (strToBts "HellO"))
        )
      ,
        ( "random-string returns a literal pattern with no specials unchanged"
        , "random-string"
        , [ArgExpression (DataString (strToBts "static-name"))]
        , \term -> expectExpression term (DataString (strToBts "static-name"))
        )
      ,
        ( "tau parses an attribute out of a string expression"
        , "tau"
        , [ArgExpression (DataString (strToBts "x"))]
        , \term -> expectAttribute term (AtLabel "x")
        )
      ,
        ( "string converts a number expression to a string"
        , "string"
        , [ArgExpression (DataNumber (numToBts 5))]
        , \term -> expectExpression term (DataString (strToBts "5"))
        )
      ,
        ( "string keeps a string expression as is"
        , "string"
        , [ArgExpression (DataString (strToBts "already"))]
        , \term -> expectExpression term (DataString (strToBts "already"))
        )
      ,
        ( "string renders an attribute"
        , "string"
        , [ArgAttribute AtRho]
        , \term -> expectExpression term (DataString (strToBts "ρ"))
        )
      ,
        ( "number parses a number out of a string expression"
        , "number"
        , [ArgExpression (DataString (strToBts "42"))]
        , \term -> expectExpression term (DataNumber (numToBts 42))
        )
      ,
        ( "sum adds several numeric arguments"
        , "sum"
        , [ArgExpression (DataNumber (numToBts 2)), ArgExpression (DataNumber (numToBts 3))]
        , \term -> expectExpression term (DataNumber (numToBts 5))
        )
      ]

    failureCases :: [(String, String, [ExtraArgument], String)]
    failureCases =
      [ ("contextualize fails on the wrong number of arguments", "contextualize", [ArgExpression ExXi], "contextualize() requires exactly 2 arguments")
      , ("random-tau fails when given arguments", "random-tau", [ArgExpression ExRoot], "random-tau() requires exactly 0 arguments")
      , ("dataize fails on a non-data-object expression", "dataize", [ArgExpression ExRoot], "Only data objects and bytes are supported")
      , ("dataize fails on the wrong number of arguments", "dataize", [ArgExpression ExRoot, ArgExpression ExXi], "dataize() requires exactly 1 argument")
      , ("sed fails on fewer than two arguments", "sed", [ArgExpression (DataString (strToBts "hello"))], "sed() requires at least two arguments")
      ,
        ( "sed fails when the pattern does not start with 's/'"
        , "sed"
        , [ArgExpression (DataString (strToBts "hello")), ArgExpression (DataString (strToBts "l/L/"))]
        , "sed pattern must start with s/"
        )
      ,
        ( "sed fails when the pattern has an unknown trailing flag"
        , "sed"
        , [ArgExpression (DataString (strToBts "hello")), ArgExpression (DataString (strToBts "s/l/L/x"))]
        , "sed pattern must be in format s/pat/rep/[g]"
        )
      , ("random-string fails on the wrong number of arguments", "random-string", [], "random-string() requires exactly 1")
      , ("size fails on a non-meta binding argument", "size", [ArgBinding (BiVoid AtRho)], "size() requires exactly 1 meta binding")
      , ("tau fails on the wrong number of arguments", "tau", [], "tau() requires exactly 1 argument")
      ,
        ( "string fails on an expression that is neither a number nor a string"
        , "string"
        , [ArgExpression ExRoot]
        , "only 'Φ.number' or 'Φ.string' are allowed"
        )
      , ("string fails on the wrong number of arguments", "string", [], "string() requires exactly 1 argument")
      , ("number fails on an expression that is not a string", "number", [ArgExpression (DataNumber (numToBts 1))], "expects expression to be 'Φ.string'")
      , ("number fails on the wrong number of arguments", "number", [], "number() requires exactly 1 argument")
      ,
        ( "an unsupported function name fails with a descriptive message"
        , "no-such-function"
        , []
        , "Function no-such-function() is not supported or does not exist"
        )
      ]
