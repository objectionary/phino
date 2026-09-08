{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module YamlSpec where

import AST (Alpha, Attribute, Binding, Bytes, Expression (ExRoot))
import Control.Exception (Exception (displayException), SomeException)
import Control.Monad
import Data.Either (isLeft)
import Data.List (isInfixOf, nub, (\\))
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml qualified as Yaml
import Files (allPathsIn)
import System.FilePath
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe, shouldSatisfy, shouldThrow)
import Yaml (Condition (..), ContextualizeRule (..), DataizeRule (..), MorphRule (..), Number, Operation (..), Premise (..), Rule, contextualizationRules, dataizationRules, morphingRules, yamlRule)

decodeYaml' :: (Yaml.FromJSON a) => String -> Either Yaml.ParseException a
decodeYaml' = Yaml.decodeEither' . encodeUtf8 . T.pack

failsWith :: String -> Either Yaml.ParseException a -> Bool
failsWith fragment decoded = case decoded of
  Left err -> fragment `isInfixOf` Yaml.prettyPrintParseException err
  Right _ -> False

failsAsRedundant :: Either Yaml.ParseException a -> Bool
failsAsRedundant = failsWith "redundant"

spec :: Spec
spec = do
  describe "parses yaml rule" $ do
    let resources = "test-resources/yaml-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      (\pth -> it (makeRelative resources pth) (void (yamlRule pth)))

  describe "fails on yaml typos" $ do
    let resources = "test-resources/yaml-typos"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth ->
          it (makeRelative resources pth) $
            shouldThrow
              (yamlRule pth)
              ( \e ->
                  let msg = displayException (e :: SomeException)
                   in "Unknown" `isInfixOf` msg || "Exactly one" `isInfixOf` msg
              )
      )

  describe "rejects malformed rule content" $ do
    let primYaml = "name: prim\nlabel: prim\nmatch: ⟦𝐵⟧\ne-match: 𝑒\nn-result: ⟦𝐵⟧"
        endYaml = "name: end\nlabel: end\nmatch: ⊥\ne-match: 𝑒\nd-result: '--'"
        cxiYaml = "name: cxi\nlabel: cxi\nmatch: ξ\nc-match: 𝑘\nc-result: 𝑘"
    forM_
      [ ("a label that equals the name in a morphing rule", primYaml, failsAsRedundant (decodeYaml' primYaml :: Either Yaml.ParseException MorphRule))
      , ("a label that equals the name in a dataization rule", endYaml, failsAsRedundant (decodeYaml' endYaml :: Either Yaml.ParseException DataizeRule))
      , ("a label that equals the name in a contextualization rule", cxiYaml, failsAsRedundant (decodeYaml' cxiYaml :: Either Yaml.ParseException ContextualizeRule))
      , ("a malformed embedded 𝜑-syntax: an index meta that does not parse", "'bogus'", isLeft (decodeYaml' "'bogus'" :: Either Yaml.ParseException Number))
      , ("a malformed embedded 𝜑-syntax: an attribute that does not parse", "'123'", isLeft (decodeYaml' "'123'" :: Either Yaml.ParseException Attribute))
      , ("a malformed embedded 𝜑-syntax: an alpha that does not parse", "'bogus'", isLeft (decodeYaml' "'bogus'" :: Either Yaml.ParseException Alpha))
      , ("a malformed embedded 𝜑-syntax: bytes that do not parse", "'0a-'", isLeft (decodeYaml' "'0a-'" :: Either Yaml.ParseException Bytes))
      , ("a malformed embedded 𝜑-syntax: an expression that does not parse", "'L>'", isLeft (decodeYaml' "'L>'" :: Either Yaml.ParseException Expression))
      , ("a malformed embedded 𝜑-syntax: a binding that does not parse", "'L>'", isLeft (decodeYaml' "'L>'" :: Either Yaml.ParseException Binding))
      ]
      ( \(desc, yaml, valid) ->
          it ("rejects " ++ desc) (unless valid (expectationFailure ("expected rejection for: " ++ yaml)))
      )

  describe "rejects an anonymous meta outside a pattern" $ do
    -- An anonymous meta is bound by the pattern it stands in and forgotten as
    -- soon as that pattern matches, so no other part of a rule has a name to
    -- read it back by. Writing one there is a mistake in the rule, caught as
    -- the rule loads rather than left to surface as a silent non-match.
    let rewriting :: String -> String
        rewriting field = "name: foo\npattern: '⟦ 𝜏1 ↦ 𝑒1 ⟧'\n" ++ field
        inferring :: String -> String
        inferring field = "name: foo\nmatch: '⟦ 𝜏1 ↦ 𝑒1 ⟧'\n" ++ field
    forM_
      [
        ( "in 'result' of a rewriting rule"
        , failsWith
            "anonymous meta '!e' cannot be referenced in 'result' of rule 'foo'"
            (decodeYaml' (rewriting "result: '𝑒'") :: Either Yaml.ParseException Rule)
        )
      ,
        ( "in 'when' of a rewriting rule"
        , failsWith
            "anonymous meta '!t' cannot be referenced in 'when' of rule 'foo'"
            (decodeYaml' (rewriting "result: '⟦ ⟧'\nwhen:\n  in: ['𝜏', '!B1']") :: Either Yaml.ParseException Rule)
        )
      ,
        ( "in 'where' of a rewriting rule"
        , failsWith
            "anonymous meta '!e' cannot be referenced in 'where' of rule 'foo'"
            (decodeYaml' (rewriting "result: '⟦ ⟧'\nwhere:\n  - meta: '!t1'\n    function: concat\n    args: ['𝑒']") :: Either Yaml.ParseException Rule)
        )
      ,
        ( "in 'having' of a rewriting rule"
        , failsWith
            "anonymous meta '!e' cannot be referenced in 'having' of rule 'foo'"
            (decodeYaml' (rewriting "result: '⟦ ⟧'\nhaving:\n  formation: '𝑒'") :: Either Yaml.ParseException Rule)
        )
      ,
        ( "as a bare index meta in 'when' of a rewriting rule"
        , failsWith
            "anonymous meta '!i' cannot be referenced in 'when' of rule 'foo'"
            (decodeYaml' (rewriting "result: '⟦ ⟧'\nwhen:\n  eq: ['𝑖', 1]") :: Either Yaml.ParseException Rule)
        )
      ,
        ( "inside an 'nf' condition of a rewriting rule"
        , failsWith
            "anonymous meta '!e' cannot be referenced in 'when' of rule 'foo'"
            (decodeYaml' (rewriting "result: '⟦ ⟧'\nwhen:\n  nf: '𝑒'") :: Either Yaml.ParseException Rule)
        )
      ,
        ( "in 'n-result' of a morphing rule"
        , failsWith
            "anonymous meta '!n' cannot be referenced in 'n-result' of rule 'foo'"
            (decodeYaml' (inferring "e-match: 𝑒0\nn-result: '𝑛'") :: Either Yaml.ParseException MorphRule)
        )
      ,
        ( "in a premise of a morphing rule"
        , failsWith
            "anonymous meta '!e' cannot be referenced in 'premises' of rule 'foo'"
            (decodeYaml' (inferring "e-match: 𝑒0\nn-result: 𝑛1\npremises:\n  - n-result: 𝑛1\n    normalize: '𝑒'") :: Either Yaml.ParseException MorphRule)
        )
      ,
        ( "in 'when' of a morphing rule"
        , failsWith
            "anonymous meta '!e' cannot be referenced in 'when' of rule 'foo'"
            (decodeYaml' (inferring "e-match: 𝑒0\nn-result: 𝑛1\nwhen:\n  formation: '𝑒'") :: Either Yaml.ParseException MorphRule)
        )
      ,
        ( "in 'd-result' of a dataization rule"
        , failsWith
            "anonymous meta '!d' cannot be referenced in 'd-result' of rule 'foo'"
            (decodeYaml' (inferring "e-match: 𝑒0\nd-result: 'δ'") :: Either Yaml.ParseException DataizeRule)
        )
      ,
        ( "in 'when' of a dataization rule"
        , failsWith
            "anonymous meta '!e' cannot be referenced in 'when' of rule 'foo'"
            (decodeYaml' (inferring "e-match: 𝑒0\nd-result: δ0\nwhen:\n  formation: '𝑒'") :: Either Yaml.ParseException DataizeRule)
        )
      ,
        ( "in a premise of a dataization rule"
        , failsWith
            "anonymous meta '!e' cannot be referenced in 'premises' of rule 'foo'"
            (decodeYaml' (inferring "e-match: 𝑒0\nd-result: δ0\npremises:\n  - d-result: δ0\n    dataize: '𝑒'") :: Either Yaml.ParseException DataizeRule)
        )
      ,
        ( "in a premise of a contextualization rule"
        , failsWith
            "anonymous meta '!e' cannot be referenced in 'premises' of rule 'foo'"
            (decodeYaml' (inferring "c-match: 𝑘0\nc-result: 𝑛1\npremises:\n  - n-result: 𝑛1\n    normalize: '𝑒'") :: Either Yaml.ParseException ContextualizeRule)
        )
      ,
        ( "in 'c-result' of a contextualization rule"
        , failsWith
            "anonymous meta '!k' cannot be referenced in 'c-result' of rule 'foo'"
            (decodeYaml' (inferring "c-match: 𝑘0\nc-result: '𝑘'") :: Either Yaml.ParseException ContextualizeRule)
        )
      ]
      (\(desc, rejected) -> it ("rejects an anonymous meta " ++ desc) (rejected `shouldBe` True))

  describe "keeps effective labels unique across rule sets" $
    -- The effective label of a rule is its 'label' when present, else its
    -- 'name'. 'explain' typesets that label as the rule's token, so two rules
    -- sharing an effective label become indistinguishable. Collect every
    -- effective label from the three embedded rule sets and assert no repeats.
    it "across morphing, dataization and contextualization rules" $ do
      let labels :: [String]
          labels =
            map (\MorphRule{name, label} -> fromMaybe name label) morphingRules
              ++ map (\DataizeRule{name, label} -> fromMaybe name label) dataizationRules
              ++ map (\ContextualizeRule{name, label} -> fromMaybe name label) contextualizationRules
      (labels \\ nub labels) `shouldBe` []

  describe "reserves 𝑛-family metas for normal forms" $
    -- 𝒞 ('contextualize') returns an expression that is not necessarily a normal
    -- form — that is why a 'normalize' premise follows it — so binding its result
    -- to an 𝑛-reserved meta (internal prefix "n") in a morphing or dataization
    -- rule conflates the calculus's 'e' (expression) with 'n' (normal form). Such
    -- a slip is notational, not functional (the meta name is only a
    -- substitution-map key), so it is easy to miss by eye; flag it automatically
    -- instead. 𝔼 ('evaluate') is excluded on purpose (partially reverting #971):
    -- it normalizes its atom's result internally, so its codomain is 𝓝 and an
    -- 𝑛-family result is exactly right (see #990). Contextualization keeps being
    -- flagged: its 𝒞-valued results are non-normal (see #971).
    it "no contextualize premise in a morphing or dataization rule binds an 𝑛-reserved meta" $ do
      let expressionValued :: Operation -> Bool
          expressionValued OpContextualize{} = True
          expressionValued _ = False
          verbOf :: Operation -> String
          verbOf OpContextualize{} = "contextualize"
          verbOf _ = "?"
          premisesOf :: [(String, [Premise])]
          premisesOf =
            map (\MorphRule{name, premises} -> (name, premises)) morphingRules
              ++ map (\DataizeRule{name, premises} -> (name, premises)) dataizationRules
          offenders :: [String]
          offenders =
            [ ruleName ++ ": " ++ verbOf operation ++ " result '" ++ T.unpack result ++ "'"
            | (ruleName, premises) <- premisesOf
            , Premise{result, operation} <- premises
            , expressionValued operation
            , T.isPrefixOf (T.pack "n") result
            ]
      offenders `shouldBe` []

  describe "parses a 'formation' condition" $
    it "decodes 'formation: <expr>' into IsFormation" $
      case (decodeYaml' "formation: 'Q'" :: Either Yaml.ParseException Condition) of
        Right cond -> cond `shouldBe` IsFormation ExRoot
        Left err -> expectationFailure (Yaml.prettyPrintParseException err)

  describe "rejects a condition object naming no known key" $
    it "fails with 'Unknown condition type'" $
      case (decodeYaml' "{}" :: Either Yaml.ParseException Condition) of
        Left err -> "Unknown condition type" `isInfixOf` Yaml.prettyPrintParseException err `shouldBe` True
        Right _ -> expectationFailure "expected decoding to fail"

  describe "rejects a condition whose arguments count is wrong" $
    -- 'asum' discards each branch's specific failure message once every
    -- branch has failed, so only the overall Left/Right outcome (not the
    -- message text) is observable from here; each case still exercises the
    -- condition's own "expects exactly two arguments" guard internally.
    forM_
      [ ("'eq' with a single argument", "eq: [1]")
      , ("'gt' with a single argument", "gt: [1]")
      , ("'in' with a single argument", "in: ['!t']")
      , ("'matches' with a single argument", "matches: ['hi']")
      , ("'part-of' with a single argument", "part-of: ['!e']")
      , ("'disjoint' with a single argument", "disjoint: [[]]")
      ]
      (\(desc, yaml) -> it desc ((decodeYaml' yaml :: Either Yaml.ParseException Condition) `shouldSatisfy` isLeft))

  describe "rejects a malformed premise" $
    forM_
      [ ("fails when neither 'n-result' nor 'd-result' is present", "morph: 𝑛")
      , ("fails when 'n-result' is not an expression meta", "n-result: Q\nmorph: 𝑛")
      , ("fails when 'd-result' is not a bytes meta", "d-result: '--'\ndataize: 𝑛")
      , ("fails when 'evaluate' does not take exactly two arguments", "n-result: 𝑛\nevaluate: [𝑛]")
      , ("fails when 'contextualize' does not take exactly two arguments", "n-result: 𝑛\ncontextualize: [𝑛]")
      ]
      (\(desc, yaml) -> it desc ((decodeYaml' yaml :: Either Yaml.ParseException Premise) `shouldSatisfy` isLeft))

  describe "rejects a numerable expression that is neither an object, a number nor an index meta" $
    it "fails on a bare boolean" $
      (decodeYaml' "true" :: Either Yaml.ParseException Number) `shouldSatisfy` isLeft

  describe "parses a literal number" $
    it "accepts a whole number" $
      (decodeYaml' "5" :: Either Yaml.ParseException Number) `shouldSatisfy` (not . isLeft)

  describe "rejects a fractional literal number" $
    it "fails on a fraction instead of silently rounding it" $
      (decodeYaml' "2.5" :: Either Yaml.ParseException Number) `shouldSatisfy` isLeft

  describe "rejects an empty 'and' condition" $
    it "fails on 'and: []'" $
      (decodeYaml' "and: []" :: Either Yaml.ParseException Condition) `shouldSatisfy` isLeft

  describe "rejects an empty 'or' condition" $
    it "fails on 'or: []'" $
      (decodeYaml' "or: []" :: Either Yaml.ParseException Condition) `shouldSatisfy` isLeft
