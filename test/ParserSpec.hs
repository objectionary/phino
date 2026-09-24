{-# LANGUAGE OverloadedStrings #-}
-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module ParserSpec where

import AST
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_)
import Data.Either (isLeft, isRight)
import Data.List (isInfixOf)
import Files (allPathsIn)
import Parser
import System.FilePath (takeBaseName)
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, anyException, describe, it, runIO, shouldBe, shouldReturn, shouldSatisfy, shouldStartWith, shouldThrow)
import Text.Megaparsec (parseMaybe)

test ::
  (Eq a, Show a) =>
  (String -> Either String a) ->
  [(String, Maybe a)] ->
  SpecWith (Arg Expectation)
test function useCases =
  forM_ useCases $ \(ipt, res) ->
    it ipt $ case res of
      Just right -> function ipt `shouldBe` Right right
      _ -> function ipt `shouldSatisfy` isLeft

fails ::
  (Show a) =>
  (String -> Either String a) ->
  [(String, String)] ->
  SpecWith (Arg Expectation)
fails function useCases =
  forM_ useCases $ \(ipt, fragment) ->
    it ipt (function ipt `shouldSatisfy` either (isInfixOf fragment) (const False))

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

renderFailure :: IO a -> IO String
renderFailure action = do
  result <- tryAny action
  case result of
    Left exc -> pure (displayException exc)
    Right _ -> fail "expected the parser action to fail"

spec :: Spec
spec = do
  describe "parse expression" $
    test
      parseExpression
      [ ("[[]]", Just (ExFormation []))
      , ("T(x -> Q)", Just (ExApplication ExTermination (ArTau (AtLabel "x") ExRoot)))
      , ("Q.org.eolang", Just (ExDispatch (ExDispatch ExRoot (AtLabel "org")) (AtLabel "eolang")))
      , ("[[x -> $, y -> ?]]", Just (ExFormation [BiTau (AtLabel "x") ExXi, BiVoid (AtLabel "y")]))
      , ("Q.!t1", Just (ExDispatch ExRoot (AtMeta "t1")))
      , ("[[]](!t1 -> $)", Just (ExApplication (ExFormation []) (ArTau (AtMeta "t1") ExXi)))
      ,
        ( "[[]](~0 -> $)(~11 -> Q)"
        , Just
            ( ExApplication
                ( ExApplication
                    (ExFormation [])
                    (ArAlpha (Alpha 0) ExXi)
                )
                (ArAlpha (Alpha 11) ExRoot)
            )
        )
      , ("[[]](x -> $, y -> Q)", Just (ExApplication (ExApplication (ExFormation []) (ArTau (AtLabel "x") ExXi)) (ArTau (AtLabel "y") ExRoot)))
      , ("[[!B1, !B2]]", Just (ExFormation [BiMeta "B1", BiMeta "B2"]))
      , ("[[!B2, !t2 -> $]]", Just (ExFormation [BiMeta "B2", BiTau (AtMeta "t2") ExXi]))
      , ("!e1", Just (ExMeta "e1"))
      , ("!k1", Just (ExMeta "k1"))
      , ("[[x -> !k1]]", Just (ExFormation [BiTau (AtLabel "x") (ExMeta "k1")]))
      , ("[[x -> !e1]]", Just (ExFormation [BiTau (AtLabel "x") (ExMeta "e1")]))
      , ("[[!t1 -> !e1]]", Just (ExFormation [BiTau (AtMeta "t1") (ExMeta "e1")]))
      , ("[[D> --]]", Just (ExFormation [BiDelta BtEmpty]))
      , ("[[D> 1F-]]", Just (ExFormation [BiDelta (BtOne "1F")]))
      , ("[[\n  L> Func,\n  D> 00-\n]]", Just (ExFormation [BiLambda (Function "Func"), BiDelta (BtOne "00")]))
      , ("[[D> 1F-2A-00]]", Just (ExFormation [BiDelta (BtMany ["1F", "2A", "00"])]))
      , ("[[D> !d1]]", Just (ExFormation [BiDelta (BtMeta "d1")]))
      , ("[[L> Function]]", Just (ExFormation [BiLambda (Function "Function")]))
      , ("[[L> !F3]]", Just (ExFormation [BiLambda (FnMeta "F3")]))
      , ("[[x() -> [[]] ]]", Just (ExFormation [BiTau (AtLabel "x") (ExFormation [])]))
      ,
        ( "[[y(^,@,z) -> [[q -> Q.a]] ]]"
        , Just
            ( ExFormation
                [ BiTau
                    (AtLabel "y")
                    ( ExFormation
                        [ BiVoid AtRho
                        , BiVoid AtPhi
                        , BiVoid (AtLabel "z")
                        , BiTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "a"))
                        ]
                    )
                ]
            )
        )
      ,
        ( "!e1(x(^,@) -> [[w -> !e2]])"
        , Just
            ( ExApplication
                (ExMeta "e1")
                ( ArTau
                    (AtLabel "x")
                    ( ExFormation
                        [ BiVoid AtRho
                        , BiVoid AtPhi
                        , BiTau (AtLabel "w") (ExMeta "e2")
                        ]
                    )
                )
            )
        )
      ,
        ( "[[x -> y.z, w -> ^, u -> @, p -> !t1, q -> !e1]]"
        , Just
            ( ExFormation
                [ BiTau
                    (AtLabel "x")
                    (ExDispatch (ExDispatch ExXi (AtLabel "y")) (AtLabel "z"))
                , BiTau
                    (AtLabel "w")
                    (ExDispatch ExXi AtRho)
                , BiTau
                    (AtLabel "u")
                    (ExDispatch ExXi AtPhi)
                , BiTau
                    (AtLabel "p")
                    (ExDispatch ExXi (AtMeta "t1"))
                , BiTau
                    (AtLabel "q")
                    (ExMeta "e1")
                ]
            )
        )
      ,
        ( "Q.x(y, [[]].z, Q.y(^,@))"
        , Just
            ( ExApplication
                ( ExApplication
                    ( ExApplication
                        (ExDispatch ExRoot (AtLabel "x"))
                        (ArAlpha (Alpha 0) (ExDispatch ExXi (AtLabel "y")))
                    )
                    (ArAlpha (Alpha 1) (ExDispatch (ExFormation []) (AtLabel "z")))
                )
                ( ArAlpha
                    (Alpha 2)
                    ( ExApplication
                        ( ExApplication
                            (ExDispatch ExRoot (AtLabel "y"))
                            (ArAlpha (Alpha 0) (ExDispatch ExXi AtRho))
                        )
                        (ArAlpha (Alpha 1) (ExDispatch ExXi AtPhi))
                    )
                )
            )
        )
      ,
        ( "5.plus(5.q(\"hello\".length))"
        , Just
            ( ExApplication
                ( ExDispatch
                    (DataNumber (BtMany ["40", "14", "00", "00", "00", "00", "00", "00"]))
                    (AtLabel "plus")
                )
                ( ArAlpha
                    (Alpha 0)
                    ( ExApplication
                        ( ExDispatch
                            (DataNumber (BtMany ["40", "14", "00", "00", "00", "00", "00", "00"]))
                            (AtLabel "q")
                        )
                        ( ArAlpha
                            (Alpha 0)
                            ( ExDispatch
                                (DataString (BtMany ["68", "65", "6C", "6C", "6F"]))
                                (AtLabel "length")
                            )
                        )
                    )
                )
            )
        )
      ,
        ( "[[𝐵1, 𝜏1 -> $, x -> 𝑒1]]"
        , Just
            ( ExFormation
                [ BiMeta "B1"
                , BiTau (AtMeta "t1") ExXi
                , BiTau (AtLabel "x") (ExMeta "e1")
                ]
            )
        )
      ]

  describe "just parses" $
    forM_
      [ "[[x -> $, y -> ?]]"
      , "[[x() -> [[]] ]]"
      , "Q.x(y() -> [[]])"
      , "Q.x(y(q) -> [[w -> !e]])"
      , "Q.x(~1(^,@) -> [[]])"
      , "Q.x.^.@.!t1"
      , "[[x -> y.z]]"
      , "[[x -> ^, y -> @, z -> !t]]"
      , "Q.x(a.b.c, Q.a(b), [[]])"
      , "Q.x(y, [[]].z, Q.y(^,@))"
      , "[[x -> 5.plus(5), y -> \"hello\", z -> 42.5]]"
      , "[[\n  x -> \"Hi\",\n  y -> 42\n]]"
      , "[[x -> -42, y -> +34]]"
      , "⟦x ↦ Φ.org.eolang(z ↦ ξ.f, φ ↦ ρ, t ↦ φ, first ↦ ⟦ λ ⤍ Function_name, Δ ⤍ 42- ⟧)⟧"
      , "[[x -> 1.00e+3, y -> 2.32e-4]]"
      , "[[ x -> \"\\u0001\\u0001\"]]"
      , "[[ x -> \"\\uD835\\uDF11\"]]"
      , "[[ x ↦ \"This plugin has \\x01\\x01\" ]]"
      , "[[ !tfoo -> !e1Some, !t-BAR -> !e_123someW, !Bhi123 ]]"
      , "[[ !B ]](α𝑖 -> !e)"
      , "[[ 𝜏 -> !e, 𝐵 ]]"
      ]
      (\expr -> it expr (parseExpression expr `shouldSatisfy` isRight))

  describe "prohibits" $
    test
      parseExpression
      ( map
          (\input -> (input, Nothing))
          [ "Q.x()"
          , "[[x(^, @, y) -> [[q -> QQ]] ]]"
          , "Q * !t1 * !t2"
          , "Q(x -> [[]])"
          , "$(x -> [[]])"
          , "Q.x(x -> ?)"
          , "Q.x(L> Func)"
          , "Q.x(D> --)"
          , "Q.x(~1 -> ?)"
          , "Q.x(L> !F)"
          , "Q.x(D> !b)"
          , "[[α0 -> Q.x]]"
          , "[[x(α1) -> [[]] ]]"
          , "[[y(!e) -> [[]] ]]"
          , "[[z(w) -> Q.x]]"
          , "Q.x(y(α1) -> [[]])"
          , "Q.x(1, 2, !B)"
          , "Q.x.α0"
          , "Q.x(~1 -> Q.y, x -> 5, !B1)"
          , "Q.x(𝐵1, 𝜏1 -> $, x -> 𝑒)"
          , "[[ x -> \"\\uD800\"]]"
          , "[[ x -> \"\\uDFFF\"]]"
          , "[[ x -> \"\\uD835\\u0041\"]]"
          , "[[ x -> 1, x -> 2 ]]"
          , "⟦ k ↦ ⟦ λ ⤍ Foo, λ ⤍ Bar ⟧ ⟧"
          , "⟦ k ↦ ⟦ Δ ⤍ 42-, Δ ⤍ 55- ⟧ ⟧"
          ]
      )

  describe "points at the typo instead of the beginning of the binding" $
    fails
      parseExpression
      [ ("[[ D> x ]]", "expression:1:7:")
      , ("[[ L> 42 ]]", "expression:1:7:")
      , ("⟦ a ↦ Φ.foo() ⟧", "expression:1:13:")
      , ("[[ x -> ]]", "expression:1:9:")
      , ("[[ x -> Q.y(] ]]", "expression:1:13:")
      , ("[[ y -> 5, x -> Q.z(} ]]", "expression:1:21:")
      ]

  describe "tells what a binding prefix may be followed by" $
    fails
      parseExpression
      [ ("[[ D> x ]]", "expecting bytes")
      , ("[[ L> 42 ]]", "function name")
      , ("[[ x -> ]]", "expecting '?', '∅', or expression head")
      ]

  describe "rejects a meta variable indexed with zero" $
    fails
      parseExpression
      [ ("!e0", "indexed with zero")
      , ("𝑛0", "indexed with zero")
      , ("[[ !t0 -> Q ]]", "indexed with zero")
      , ("[[ D> 𝛿0 ]]", "indexed with zero")
      , ("[[ λ ⤍ 𝜎0 ]]", "indexed with zero")
      , ("[[ !B ]](α𝑖0 -> !e)", "indexed with zero")
      ]

  describe "parse packs" $ do
    packs <- runIO (allPathsIn "test-resources/parser-packs")
    forM_
      packs
      ( \pack -> do
          content <- runIO (readFile pack)
          it (takeBaseName pack) (parseExpression content `shouldSatisfy` isRight)
      )

  describe "process typo packs" $ do
    packs <- runIO (allPathsIn "test-resources/phi-typos-packs")
    forM_
      packs
      ( \pack -> do
          content <- runIO (readFile pack)
          it (takeBaseName pack) (parseExpression content `shouldSatisfy` isLeft)
      )

  describe "parse bytes" $
    test
      parseBytes
      [ ("--", Just BtEmpty)
      , ("00-", Just (BtOne "00"))
      , ("FF-", Just (BtOne "FF"))
      , ("AB-", Just (BtOne "AB"))
      , ("1F-2A-00", Just (BtMany ["1F", "2A", "00"]))
      , ("01-02-03-04-05", Just (BtMany ["01", "02", "03", "04", "05"]))
      , ("!d1", Just (BtMeta "d1"))
      , ("!d2", Just (BtMeta "d2"))
      , ("!d_test", Just (BtMeta "d_test"))
      , ("𝛿1", Just (BtMeta "d1"))
      , ("𝛿2", Just (BtMeta "d2"))
      , ("δ0", Nothing)
      , ("GG-", Nothing)
      , ("0-", Nothing)
      , ("000-", Nothing)
      , ("zz-", Nothing)
      ]

  describe "parse binding" $
    test
      parseBinding
      [ ("x -> $", Just (BiTau (AtLabel "x") ExXi))
      , ("y -> Q", Just (BiTau (AtLabel "y") ExRoot))
      , ("z -> ?", Just (BiVoid (AtLabel "z")))
      , ("w -> ∅", Just (BiVoid (AtLabel "w")))
      , ("^ -> T", Just (BiTau AtRho ExTermination))
      , ("@ -> $", Just (BiTau AtPhi ExXi))
      , ("ρ -> Q", Just (BiTau AtRho ExRoot))
      , ("φ -> T", Just (BiTau AtPhi ExTermination))
      , ("!t1 -> $", Just (BiTau (AtMeta "t1") ExXi))
      , ("!t2 -> Q", Just (BiTau (AtMeta "t2") ExRoot))
      , ("D> --", Just (BiDelta BtEmpty))
      , ("D> 42-", Just (BiDelta (BtOne "42")))
      , ("D> 01-02-03", Just (BiDelta (BtMany ["01", "02", "03"])))
      , ("D> !d1", Just (BiDelta (BtMeta "d1")))
      , ("Δ ⤍ FF-", Just (BiDelta (BtOne "FF")))
      , ("Δ ⤍ --", Just (BiDelta BtEmpty))
      , ("L> Func", Just (BiLambda (Function "Func")))
      , ("L> Function_name", Just (BiLambda (Function "Function_name")))
      , ("L> Aφ", Just (BiLambda (Function "Aφ")))
      , ("λ ⤍ Test", Just (BiLambda (Function "Test")))
      , ("L> !F1", Just (BiLambda (FnMeta "F1")))
      , ("L> !F2", Just (BiLambda (FnMeta "F2")))
      , ("λ ⤍ 𝑓1", Just (BiLambda (FnMeta "F1")))
      , ("L> 𝑓2", Just (BiLambda (FnMeta "F2")))
      , ("L> 𝜎1", Just (BiLambda (FnSymbol 1)))
      , ("λ ⤍ 𝜎2", Just (BiLambda (FnSymbol 2)))
      , ("L> !S1", Just (BiLambda (FnSymbol 1)))
      , ("λ ⤍ !S2", Just (BiLambda (FnSymbol 2)))
      , ("L> 𝜎", Just (BiLambda (FnFresh (Slot "S" 3))))
      , ("λ ⤍ !S", Just (BiLambda (FnFresh (Slot "S" 4))))
      , ("!B1", Just (BiMeta "B1"))
      , ("!B2", Just (BiMeta "B2"))
      , ("!B_test", Just (BiMeta "B_test"))
      , ("𝐵1", Just (BiMeta "B1"))
      , ("𝐵1", Just (BiMeta "B1"))
      , ("x() -> [[]]", Just (BiTau (AtLabel "x") (ExFormation [])))
      , ("y(^) -> [[]]", Just (BiTau (AtLabel "y") (ExFormation [BiVoid AtRho])))
      , ("z(^, @) -> [[]]", Just (BiTau (AtLabel "z") (ExFormation [BiVoid AtRho, BiVoid AtPhi])))
      , ("x -> [[y -> $]]", Just (BiTau (AtLabel "x") (ExFormation [BiTau (AtLabel "y") ExXi])))
      , ("x ↦ ξ", Just (BiTau (AtLabel "x") ExXi))
      , ("x -> ", Nothing)
      , ("-> Q", Nothing)
      , ("L>", Nothing)
      , ("D>", Nothing)
      ]

  describe "parse attribute" $
    test
      parseAttribute
      [ ("x", Just (AtLabel "x"))
      , ("foo", Just (AtLabel "foo"))
      , ("camelCase", Just (AtLabel "camelCase"))
      , ("with_underscore", Just (AtLabel "with_underscore"))
      , ("with-dash", Just (AtLabel "with-dash"))
      , ("^", Just AtRho)
      , ("ρ", Just AtRho)
      , ("@", Just AtPhi)
      , ("φ", Just AtPhi)
      , ("!t1", Just (AtMeta "t1"))
      , ("!t2", Just (AtMeta "t2"))
      , ("!t_test", Just (AtMeta "t_test"))
      , ("𝜏1", Just (AtMeta "t1"))
      , ("𝜏2", Just (AtMeta "t2"))
      , ("a0", Just (AtLabel "a0"))
      , ("a1", Just (AtLabel "a1"))
      , ("a123", Just (AtLabel "a123"))
      , ("α0", Nothing)
      , ("α42", Nothing)
      , ("~0", Nothing)
      , ("~1", Nothing)
      , ("~123", Nothing)
      , ("X", Nothing)
      , ("123", Nothing)
      , ("", Nothing)
      ]

  describe "parse number" $
    test
      parseNumber
      [ ("0", Just (DataNumber (BtMany ["00", "00", "00", "00", "00", "00", "00", "00"])))
      , ("-0", Just (DataNumber (BtMany ["80", "00", "00", "00", "00", "00", "00", "00"])))
      , ("-0.0", Just (DataNumber (BtMany ["80", "00", "00", "00", "00", "00", "00", "00"])))
      , ("+0", Just (DataNumber (BtMany ["00", "00", "00", "00", "00", "00", "00", "00"])))
      , ("1", Just (DataNumber (BtMany ["3F", "F0", "00", "00", "00", "00", "00", "00"])))
      , ("-1", Just (DataNumber (BtMany ["BF", "F0", "00", "00", "00", "00", "00", "00"])))
      , ("+1", Just (DataNumber (BtMany ["3F", "F0", "00", "00", "00", "00", "00", "00"])))
      , ("42", Just (DataNumber (BtMany ["40", "45", "00", "00", "00", "00", "00", "00"])))
      , ("-42", Just (DataNumber (BtMany ["C0", "45", "00", "00", "00", "00", "00", "00"])))
      , ("3.14", Just (DataNumber (BtMany ["40", "09", "1E", "B8", "51", "EB", "85", "1F"])))
      , ("1.5", Just (DataNumber (BtMany ["3F", "F8", "00", "00", "00", "00", "00", "00"])))
      , ("-0.5", Just (DataNumber (BtMany ["BF", "E0", "00", "00", "00", "00", "00", "00"])))
      , ("1e3", Just (DataNumber (BtMany ["40", "8F", "40", "00", "00", "00", "00", "00"])))
      , ("1E3", Just (DataNumber (BtMany ["40", "8F", "40", "00", "00", "00", "00", "00"])))
      , ("1.5e2", Just (DataNumber (BtMany ["40", "62", "C0", "00", "00", "00", "00", "00"])))
      , ("2e-3", Just (DataNumber (BtMany ["3F", "60", "62", "4D", "D2", "F1", "A9", "FC"])))
      , ("-1e10", Just (DataNumber (BtMany ["C2", "02", "A0", "5F", "20", "00", "00", "00"])))
      , ("abc", Nothing)
      , ("", Nothing)
      ]

  describe "parse the non-finite doubles named off the root" $
    test
      parseExpression
      [ ("Q.nan", Just (DataNumber (BtMany ["7F", "F8", "00", "00", "00", "00", "00", "00"])))
      , ("Φ.nan", Just (DataNumber (BtMany ["7F", "F8", "00", "00", "00", "00", "00", "00"])))
      , ("Q.pinf", Just (DataNumber (BtMany ["7F", "F0", "00", "00", "00", "00", "00", "00"])))
      , ("Φ.pinf", Just (DataNumber (BtMany ["7F", "F0", "00", "00", "00", "00", "00", "00"])))
      , ("Q.ninf", Just (DataNumber (BtMany ["FF", "F0", "00", "00", "00", "00", "00", "00"])))
      , ("Φ.ninf", Just (DataNumber (BtMany ["FF", "F0", "00", "00", "00", "00", "00", "00"])))
      , -- only the exact names are special, everything else stays an ordinary dispatch
        ("Q.number", Just (ExDispatch ExRoot (AtLabel "number")))
      , ("Q.nanny", Just (ExDispatch ExRoot (AtLabel "nanny")))
      , ("Q.x.nan", Just (ExDispatch (ExDispatch ExRoot (AtLabel "x")) (AtLabel "nan")))
      , -- a bare name is still a ξ dispatch, as it always was
        ("nan", Just (ExDispatch ExXi (AtLabel "nan")))
      , ("$.nan", Just (ExDispatch ExXi (AtLabel "nan")))
      ]

  describe "parseExpressionThrows" $
    forM_
      [ ("returns expression on valid input 'T'", "T", Just ExTermination)
      , ("throws on invalid input 'invalid expression ]][['", "invalid expression ]][[", Nothing)
      , ("returns expression on valid input 'Q.x'", "Q.x", Just (ExDispatch ExRoot (AtLabel "x")))
      , ("throws on invalid input '[[invalid'", "[[invalid", Nothing)
      ]
      ( \(desc, input, expected) -> it desc $ case expected of
          Just result -> parseExpressionThrows input `shouldReturn` result
          Nothing -> parseExpressionThrows input `shouldThrow` anyException
      )

  describe "parseAttributeThrows" $
    forM_
      [ ("returns attribute on valid input", "foo", Just (AtLabel "foo"))
      , ("throws on invalid input", "123invalid", Nothing)
      ]
      ( \(desc, input, expected) -> it desc $ case expected of
          Just result -> parseAttributeThrows input `shouldReturn` result
          Nothing -> parseAttributeThrows input `shouldThrow` anyException
      )

  describe "parseNumberThrows" $
    forM_
      [ ("returns number on valid input", "42", Just (DataNumber (BtMany ["40", "45", "00", "00", "00", "00", "00", "00"])))
      , ("throws on invalid input", "notanumber", Nothing)
      ]
      ( \(desc, input, expected) -> it desc $ case expected of
          Just result -> parseNumberThrows input `shouldReturn` result
          Nothing -> parseNumberThrows input `shouldThrow` anyException
      )

  describe "parse string escapes" $
    test
      parseExpression
      [ ("\"hello\"", Just (DataString (BtMany ["68", "65", "6C", "6C", "6F"])))
      , ("\"\"", Just (DataString BtEmpty))
      , ("\"a\"", Just (DataString (BtOne "61")))
      , ("\"\\n\"", Just (DataString (BtOne "0A")))
      , ("\"\\r\"", Just (DataString (BtOne "0D")))
      , ("\"\\t\"", Just (DataString (BtOne "09")))
      , ("\"\\\\\"", Just (DataString (BtOne "5C")))
      , ("\"\\\"\"", Just (DataString (BtOne "22")))
      , ("\"\\b\"", Just (DataString (BtOne "08")))
      , ("\"\\f\"", Just (DataString (BtOne "0C")))
      , ("\"\\x41\"", Just (DataString (BtOne "41")))
      , ("\"\\x00\"", Just (DataString (BtOne "00")))
      , ("\"\\u0041\"", Just (DataString (BtOne "41")))
      , ("\"\\u0000\"", Just (DataString (BtOne "00")))
      , ("\"line1\\nline2\"", Just (DataString (BtMany ["6C", "69", "6E", "65", "31", "0A", "6C", "69", "6E", "65", "32"])))
      ]

  describe "parse unicode syntax" $
    test
      parseExpression
      [ ("ξ", Just ExXi)
      , ("Φ", Just ExRoot)
      , ("⊥", Just ExTermination)
      , ("⟦⟧", Just (ExFormation []))
      , ("⟦ x ↦ ξ ⟧", Just (ExFormation [BiTau (AtLabel "x") ExXi]))
      , ("ξ.ρ", Just (ExDispatch ExXi AtRho))
      , ("ξ.φ", Just (ExDispatch ExXi AtPhi))
      ]

  describe "parse labels with special characters" $
    test
      parseExpression
      [ ("foo123", Just (ExDispatch ExXi (AtLabel "foo123")))
      , ("with-dash", Just (ExDispatch ExXi (AtLabel "with-dash")))
      , ("with_underscore", Just (ExDispatch ExXi (AtLabel "with_underscore")))
      , ("aкирилиця", Just (ExDispatch ExXi (AtLabel "aкирилиця")))
      , ("a日本語", Just (ExDispatch ExXi (AtLabel "a日本語")))
      , ("name123_test", Just (ExDispatch ExXi (AtLabel "name123_test")))
      ]

  describe "parse complex formations" $
    test
      parseExpression
      [ ("[[^ -> ?]]", Just (ExFormation [BiVoid AtRho]))
      , ("[[@ -> ?]]", Just (ExFormation [BiVoid AtPhi]))
      , ("[[^ -> ?, @ -> ?]]", Just (ExFormation [BiVoid AtRho, BiVoid AtPhi]))
      , ("[[^ -> Q, @ -> $]]", Just (ExFormation [BiTau AtRho ExRoot, BiTau AtPhi ExXi]))
      ]

  describe "parse applications with mixed bindings" $
    test
      parseExpression
      [ ("[[]](Q)", Just (ExApplication (ExFormation []) (ArAlpha (Alpha 0) ExRoot)))
      , ("[[]](Q, T)", Just (ExApplication (ExApplication (ExFormation []) (ArAlpha (Alpha 0) ExRoot)) (ArAlpha (Alpha 1) ExTermination)))
      , ("Q.x(y -> $)", Just (ExApplication (ExDispatch ExRoot (AtLabel "x")) (ArTau (AtLabel "y") ExXi)))
      , ("[[x -> ?]].x(Q)", Just (ExApplication (ExDispatch (ExFormation [BiVoid (AtLabel "x")]) (AtLabel "x")) (ArAlpha (Alpha 0) ExRoot)))
      , ("[[]](~!i1 -> $)", Just (ExApplication (ExFormation []) (ArAlpha (AlMeta "i1") ExXi)))
      , ("[[]](α𝑖1 -> Q)", Just (ExApplication (ExFormation []) (ArAlpha (AlMeta "i1") ExRoot)))
      , ("Q.foo(a1 -> Q.y)", Just (ExApplication (ExDispatch ExRoot (AtLabel "foo")) (ArTau (AtLabel "a1") (ExDispatch ExRoot (AtLabel "y"))))) -- #875: "a"-prefixed label in argument position is a named binding, not a positional alpha
      ]

  describe "parse meta expressions" $
    test
      parseExpression
      [ ("!e1", Just (ExMeta "e1"))
      , ("!e2", Just (ExMeta "e2"))
      , ("!e_test", Just (ExMeta "e_test"))
      , ("𝑒1", Just (ExMeta "e1"))
      , ("𝑒2", Just (ExMeta "e2"))
      , ("!e1.x", Just (ExDispatch (ExMeta "e1") (AtLabel "x")))
      , ("!e1(Q)", Just (ExApplication (ExMeta "e1") (ArAlpha (Alpha 0) ExRoot)))
      , ("!n1", Just (ExMeta "n1"))
      , ("!n1", Just (ExMeta "n1"))
      , ("𝑛1", Just (ExMeta "n1"))
      , ("𝑛1", Just (ExMeta "n1"))
      , ("𝑛1.x", Just (ExDispatch (ExMeta "n1") (AtLabel "x")))
      ]

  describe "parse anonymous meta-variables" $
    -- A meta written without an index is anonymous: it stands for whatever term
    -- fills its place and no rule may name it afterwards. It is told apart from
    -- every other anonymous meta of the same term by the offset it starts at,
    -- which is why one formation may carry two of the same kind (#218).
    test
      parseExpression
      [ ("!e", Just (ExAny (Slot "e" 0)))
      , ("𝑒", Just (ExAny (Slot "e" 0)))
      , ("!n", Just (ExAny (Slot "n" 0)))
      , ("𝑘", Just (ExAny (Slot "k" 0)))
      , ("!e.x", Just (ExDispatch (ExAny (Slot "e" 0)) (AtLabel "x")))
      ,
        ( "⟦ 𝜏 ↦ 𝑒, 𝜏 ↦ 𝑒 ⟧"
        , Just
            ( ExFormation
                [ BiTau (AtAny (Slot "t" 2)) (ExAny (Slot "e" 6))
                , BiTau (AtAny (Slot "t" 9)) (ExAny (Slot "e" 13))
                ]
            )
        )
      ]

  describe "parse whitespace handling" $
    forM_
      [ "[[  x   ->   Q  ]]"
      , "[[\n\tx\n\t->\n\tQ\n\t]]"
      , "  Q  .  x  "
      , "Q.x(  y  ->  $  )"
      , "  [[  x  ->  Q  ]]  "
      ]
      (\expr -> it expr (parseExpression expr `shouldSatisfy` isRight))

  describe "parse unicode meta-k expression" $
    test
      parseExpression
      [ ("𝑘1", Just (ExMeta "k1"))
      , ("𝑘1", Just (ExMeta "k1"))
      , ("𝑘1.x", Just (ExDispatch (ExMeta "k1") (AtLabel "x")))
      ]

  describe "ParserException Show instance" $
    forM_
      [
        ( "renders CouldNotParseExpression via parseExpressionThrows, embedding the megaparsec cause"
        , renderFailure (parseExpressionThrows "invalid expression ]][[")
        , "Couldn't parse given phi expression, cause:"
        , "expression:1:"
        )
      ,
        ( "renders CouldNotParseAttribute via parseAttributeThrows, embedding the megaparsec cause"
        , renderFailure (parseAttributeThrows "123invalid")
        , "Couldn't parse given attribute, cause:"
        , "attribute:1:"
        )
      ,
        ( "renders CouldNotParseNumber via parseNumberThrows, embedding the megaparsec cause"
        , renderFailure (parseNumberThrows "notanumber")
        , "Couldn't parse given number to 'Φ.number', cause:"
        , "number:1:"
        )
      ]
      ( \(desc, action, prefix, fragment) -> it desc $ do
          rendered <- action
          rendered `shouldStartWith` prefix
          rendered `shouldSatisfy` isInfixOf fragment
      )

  describe "phiParser record" $
    forM_
      [ ("exposes an _alpha field parsing an alpha directly", parseMaybe (_alpha phiParser) "~3" `shouldBe` Just (Alpha 3))
      , ("exposes an _attribute field parsing an attribute directly", parseMaybe (_attribute phiParser) "foo" `shouldBe` Just (AtLabel "foo"))
      , ("exposes an _index field parsing an index meta directly", parseMaybe (_index phiParser) "!i1" `shouldBe` Just (Right "i1"))
      , ("exposes a _binding field parsing a binding directly", parseMaybe (_binding phiParser) "x -> $" `shouldBe` Just (BiTau (AtLabel "x") ExXi))
      , ("exposes an _expression field parsing an expression directly", parseMaybe (_expression phiParser) "Q.x" `shouldBe` Just (ExDispatch ExRoot (AtLabel "x")))
      , ("exposes a _string field parsing a quoted string directly", parseMaybe (_string phiParser) "\"hi\"" `shouldBe` Just "hi")
      ]
      (uncurry it)

  describe "parse bytes rejects a lowercase hex digit" $
    fails
      parseBytes
      [ ("0a-", "expected 0-9 or A-F")
      , ("a0-", "expected 0-9 or A-F")
      ]

  describe "parser errors are tagged with their entry point name" $
    forM_
      [ ("parseBinding error is tagged with its entry point name", parseBinding "L>" `shouldSatisfy` either (isInfixOf "binding:1:") (const False))
      , ("parseNumber error is tagged with its entry point name", parseNumber "abc" `shouldSatisfy` either (isInfixOf "number:1:") (const False))
      , ("parseAttribute error is tagged with its entry point name", parseAttribute "123" `shouldSatisfy` either (isInfixOf "attribute:1:") (const False))
      , ("parseAlpha error is tagged with its entry point name", parseAlpha "bogus" `shouldSatisfy` either (isInfixOf "alpha:1:") (const False))
      , ("parseIndex error is tagged with its entry point name", parseIndex "bogus" `shouldSatisfy` either (isInfixOf "index meta:1:") (const False))
      ]
      (uncurry it)

  describe "surrogate escape failures embed their specific cause" $
    fails
      parseExpression
      [ ("[[ x -> \"\\uD835\\u0041\"]]", "Invalid low surrogate:")
      , ("[[ x -> \"\\uDFFF\"]]", "Unexpected low surrogate:")
      ]

  describe "parse the one-binding formation sugar" $
    forM_
      [ ("FF-AA:Δ", "⟦ Δ ⤍ FF-AA ⟧")
      , ("FF-AA:D", "⟦ Δ ⤍ FF-AA ⟧")
      , ("--:Δ", "⟦ Δ ⤍ -- ⟧")
      , ("1F-:Δ", "⟦ Δ ⤍ 1F- ⟧")
      , ("𝛿1:Δ", "⟦ Δ ⤍ 𝛿1 ⟧")
      , ("𝜎1:λ", "⟦ λ ⤍ 𝜎1 ⟧")
      , ("!S1:L", "⟦ λ ⤍ 𝜎1 ⟧")
      , ("Plus:λ", "⟦ λ ⤍ Plus ⟧")
      , ("Q:λ", "⟦ λ ⤍ Q ⟧")
      , ("T:L", "⟦ λ ⤍ T ⟧")
      , ("Qx:λ", "⟦ λ ⤍ Qx ⟧")
      , ("T_1 : λ", "⟦ λ ⤍ T_1 ⟧")
      , ("T:x", "⟦ x ↦ ⊥ ⟧")
      , ("Q.x:y", "⟦ y ↦ Φ.x ⟧")
      , ("!F1:L", "⟦ λ ⤍ 𝑓1 ⟧")
      , ("∅:a", "⟦ a ↦ ∅ ⟧")
      , ("?:a", "⟦ a ↦ ∅ ⟧")
      , ("?:!t1", "⟦ 𝜏1 ↦ ∅ ⟧")
      , ("ξ.a:φ", "⟦ φ ↦ ξ.a ⟧")
      , ("$.a:@", "⟦ φ ↦ ξ.a ⟧")
      , ("$.a : @", "⟦ φ ↦ ξ.a ⟧")
      , ("ξ:ρ", "⟦ ρ ↦ ξ ⟧")
      , ("Q:x", "⟦ x ↦ Φ ⟧")
      , ("Q.x(y):z", "⟦ z ↦ Φ.x(y) ⟧")
      , ("42:φ", "⟦ φ ↦ 42 ⟧")
      , ("1E-05:φ", "⟦ φ ↦ 1E-05 ⟧")
      , ("\"hi\":φ", "⟦ φ ↦ \"hi\" ⟧")
      , ("𝑒1:𝜏1", "⟦ 𝜏1 ↦ 𝑒1 ⟧")
      , ("ξ.a:φ.b", "⟦ φ ↦ ξ.a ⟧.b")
      , ("ξ.a:φ:b", "⟦ b ↦ ⟦ φ ↦ ξ.a ⟧ ⟧")
      , ("FF-:Δ.x", "⟦ Δ ⤍ FF- ⟧.x")
      , ("Q.x(ξ.a:φ)", "Q.x(⟦ φ ↦ ξ.a ⟧)")
      , ("⟦ x ↦ y:φ, z ↦ FF-:Δ ⟧", "⟦ x ↦ ⟦ φ ↦ ξ.y ⟧, z ↦ ⟦ Δ ⤍ FF- ⟧ ⟧")
      , ("⟦ x ↦ ∅:a, y ↦ ∅ ⟧", "⟦ x ↦ ⟦ a ↦ ∅ ⟧, y ↦ ∅ ⟧")
      , ("[[ x -> ?:a ]]", "⟦ x ↦ ⟦ a ↦ ∅ ⟧ ⟧")
      , ("Q.x(y -> ?:a)", "Q.x(y ↦ ⟦ a ↦ ∅ ⟧)")
      , ("Q.x(α0 ↦ Plus:λ)", "Q.x(α0 ↦ ⟦ λ ⤍ Plus ⟧)")
      ]
      ( \(sweet, plain) ->
          it sweet $ do
            parseExpression plain `shouldSatisfy` isRight
            parseExpression sweet `shouldBe` parseExpression plain
      )

  describe "rejects a broken one-binding formation sugar" $
    test
      parseExpression
      ( map
          (\ipt -> (ipt, Nothing :: Maybe Expression))
          ["FF-AA:φ", "Plus:x", "∅:Δ", "ξ.a:", ":φ", "ξ.a:Δ", "ξ.a:λ"]
      )
