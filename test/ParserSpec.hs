{-# LANGUAGE OverloadedStrings #-}
-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module ParserSpec where

import AST
import Control.Monad (forM_)
import Data.Either (isLeft, isRight)
import Misc
import Parser
import System.FilePath (takeBaseName)
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, anyException, describe, it, runIO, shouldBe, shouldReturn, shouldSatisfy, shouldThrow)

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

spec :: Spec
spec = do
  describe "parse program" $
    test
      parseProgram
      [ ("Q -> [[]]", Just (Program (ExFormation [BiVoid AtRho])))
      , ("Q -> T(x -> Q)", Just (Program (ExApplication ExTermination (ArTau (AtLabel "x") ExRoot))))
      , ("Q -> Q.org.eolang", Just (Program (ExDispatch (ExDispatch ExRoot (AtLabel "org")) (AtLabel "eolang"))))
      , ("Q -> [[x -> $, y -> ?]]", Just (Program (ExFormation [BiTau (AtLabel "x") ExXi, BiVoid (AtLabel "y"), BiVoid AtRho])))
      ]

  describe "parse expression" $
    test
      parseExpression
      [ ("Q.!t", Just (ExDispatch ExRoot (AtMeta "t")))
      , ("[[]](!t1 -> $)", Just (ExApplication (ExFormation [BiVoid AtRho]) (ArTau (AtMeta "t1") ExXi)))
      ,
        ( "[[]](~0 -> $)(~11 -> Q)"
        , Just
            ( ExApplication
                ( ExApplication
                    (ExFormation [BiVoid AtRho])
                    (ArAlpha (Alpha 0) ExXi)
                )
                (ArAlpha (Alpha 11) ExRoot)
            )
        )
      , ("[[]](x -> $, y -> Q)", Just (ExApplication (ExApplication (ExFormation [BiVoid AtRho]) (ArTau (AtLabel "x") ExXi)) (ArTau (AtLabel "y") ExRoot)))
      , ("[[!B, !B1]]", Just (ExFormation [BiMeta "B", BiMeta "B1"]))
      , ("[[!B2, !t2 -> $]]", Just (ExFormation [BiMeta "B2", BiTau (AtMeta "t2") ExXi]))
      , ("!e0", Just (ExMeta "e0"))
      , ("!k", Just (ExMeta "k"))
      , ("[[x -> !k1]]", Just (ExFormation [BiTau (AtLabel "x") (ExMeta "k1"), BiVoid AtRho]))
      , ("[[x -> !e]]", Just (ExFormation [BiTau (AtLabel "x") (ExMeta "e"), BiVoid AtRho]))
      , ("[[!t -> !e1]]", Just (ExFormation [BiTau (AtMeta "t") (ExMeta "e1")]))
      , ("[[D> --]]", Just (ExFormation [BiDelta BtEmpty, BiVoid AtRho]))
      , ("[[D> 1F-]]", Just (ExFormation [BiDelta (BtOne "1F"), BiVoid AtRho]))
      , ("[[\n  L> Func,\n  D> 00-\n]]", Just (ExFormation [BiLambda (Function "Func"), BiDelta (BtOne "00"), BiVoid AtRho]))
      , ("[[D> 1F-2A-00]]", Just (ExFormation [BiDelta (BtMany ["1F", "2A", "00"]), BiVoid AtRho]))
      , ("[[D> !d0]]", Just (ExFormation [BiDelta (BtMeta "d0"), BiVoid AtRho]))
      , ("[[L> Function]]", Just (ExFormation [BiLambda (Function "Function"), BiVoid AtRho]))
      , ("[[L> !F3]]", Just (ExFormation [BiLambda (FnMeta "F3"), BiVoid AtRho]))
      , ("[[x() -> [[]] ]]", Just (ExFormation [BiTau (AtLabel "x") (ExFormation [BiVoid AtRho]), BiVoid AtRho]))
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
                , BiVoid AtRho
                ]
            )
        )
      ,
        ( "!e(x(^,@) -> [[w -> !e1]])"
        , Just
            ( ExApplication
                (ExMeta "e")
                ( ArTau
                    (AtLabel "x")
                    ( ExFormation
                        [ BiVoid AtRho
                        , BiVoid AtPhi
                        , BiTau (AtLabel "w") (ExMeta "e1")
                        ]
                    )
                )
            )
        )
      ,
        ( "[[x -> y.z, w -> ^, u -> @, p -> !t, q -> !e]]"
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
                    (ExDispatch ExXi (AtMeta "t"))
                , BiTau
                    (AtLabel "q")
                    (ExMeta "e")
                , BiVoid AtRho
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
                    (ArAlpha (Alpha 1) (ExDispatch (ExFormation [BiVoid AtRho]) (AtLabel "z")))
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
        ( "[[𝐵1, 𝜏0 -> $, x -> 𝑒]]"
        , Just
            ( ExFormation
                [ BiMeta "B1"
                , BiTau (AtMeta "t0") ExXi
                , BiTau (AtLabel "x") (ExMeta "e")
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
      , "Q.x.^.@.!t0"
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
          , "Q.x(𝐵1, 𝜏0 -> $, x -> 𝑒)"
          , "[[ x -> \"\\uD800\"]]"
          , "[[ x -> \"\\uDFFF\"]]"
          , "[[ x -> \"\\uD835\\u0041\"]]"
          , "[[ x -> 1, x -> 2 ]]"
          , "⟦ k ↦ ⟦ λ ⤍ Foo, λ ⤍ Bar ⟧ ⟧"
          , "⟦ k ↦ ⟦ Δ ⤍ 42-, Δ ⤍ 55- ⟧ ⟧"
          ]
      )

  describe "parse packs" $ do
    packs <- runIO (allPathsIn "test-resources/parser-packs")
    forM_
      packs
      ( \pack -> do
          content <- runIO (readFile pack)
          it (takeBaseName pack) (parseProgram content `shouldSatisfy` isRight)
      )

  describe "process typo packs" $ do
    packs <- runIO (allPathsIn "test-resources/phi-typos-packs")
    forM_
      packs
      ( \pack -> do
          content <- runIO (readFile pack)
          it (takeBaseName pack) (parseProgram content `shouldSatisfy` isLeft)
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
      , ("!d", Just (BtMeta "d"))
      , ("!d0", Just (BtMeta "d0"))
      , ("!d_test", Just (BtMeta "d_test"))
      , ("δ", Just (BtMeta "d"))
      , ("δ0", Just (BtMeta "d0"))
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
      , ("!t -> $", Just (BiTau (AtMeta "t") ExXi))
      , ("!t0 -> Q", Just (BiTau (AtMeta "t0") ExRoot))
      , ("D> --", Just (BiDelta BtEmpty))
      , ("D> 42-", Just (BiDelta (BtOne "42")))
      , ("D> 01-02-03", Just (BiDelta (BtMany ["01", "02", "03"])))
      , ("D> !d", Just (BiDelta (BtMeta "d")))
      , ("Δ ⤍ FF-", Just (BiDelta (BtOne "FF")))
      , ("Δ ⤍ --", Just (BiDelta BtEmpty))
      , ("L> Func", Just (BiLambda (Function "Func")))
      , ("L> Function_name", Just (BiLambda (Function "Function_name")))
      , ("L> Aφ", Just (BiLambda (Function "Aφ")))
      , ("λ ⤍ Test", Just (BiLambda (Function "Test")))
      , ("L> !F", Just (BiLambda (FnMeta "F")))
      , ("L> !F0", Just (BiLambda (FnMeta "F0")))
      , ("λ ⤍ 𝐹", Just (BiLambda (FnMeta "F")))
      , ("L> 𝐹2", Just (BiLambda (FnMeta "F2")))
      , ("!B", Just (BiMeta "B"))
      , ("!B0", Just (BiMeta "B0"))
      , ("!B_test", Just (BiMeta "B_test"))
      , ("𝐵", Just (BiMeta "B"))
      , ("𝐵1", Just (BiMeta "B1"))
      , ("x() -> [[]]", Just (BiTau (AtLabel "x") (ExFormation [BiVoid AtRho])))
      , ("y(^) -> [[]]", Just (BiTau (AtLabel "y") (ExFormation [BiVoid AtRho])))
      , ("z(^, @) -> [[]]", Just (BiTau (AtLabel "z") (ExFormation [BiVoid AtRho, BiVoid AtPhi])))
      , ("x -> [[y -> $]]", Just (BiTau (AtLabel "x") (ExFormation [BiTau (AtLabel "y") ExXi, BiVoid AtRho])))
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
      , ("!t", Just (AtMeta "t"))
      , ("!t0", Just (AtMeta "t0"))
      , ("!t_test", Just (AtMeta "t_test"))
      , ("𝜏", Just (AtMeta "t"))
      , ("𝜏0", Just (AtMeta "t0"))
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

  describe "parseProgramThrows" $ do
    it "returns program on valid input" $
      parseProgramThrows "Q -> T" `shouldReturn` Program ExTermination
    it "throws on invalid input" $
      parseProgramThrows "invalid program ]][[" `shouldThrow` anyException

  describe "parseExpressionThrows" $ do
    it "returns expression on valid input" $
      parseExpressionThrows "Q.x" `shouldReturn` ExDispatch ExRoot (AtLabel "x")
    it "throws on invalid input" $
      parseExpressionThrows "[[invalid" `shouldThrow` anyException

  describe "parseAttributeThrows" $ do
    it "returns attribute on valid input" $
      parseAttributeThrows "foo" `shouldReturn` AtLabel "foo"
    it "throws on invalid input" $
      parseAttributeThrows "123invalid" `shouldThrow` anyException

  describe "parseNumberThrows" $ do
    it "returns number on valid input" $ do
      result <- parseNumberThrows "42"
      case result of
        DataNumber _ -> return ()
        _ -> fail "expected DataNumber"
    it "throws on invalid input" $
      parseNumberThrows "notanumber" `shouldThrow` anyException

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
      , ("⟦⟧", Just (ExFormation [BiVoid AtRho]))
      , ("⟦ x ↦ ξ ⟧", Just (ExFormation [BiTau (AtLabel "x") ExXi, BiVoid AtRho]))
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
      , ("[[@ -> ?]]", Just (ExFormation [BiVoid AtPhi, BiVoid AtRho]))
      , ("[[^ -> ?, @ -> ?]]", Just (ExFormation [BiVoid AtRho, BiVoid AtPhi]))
      , ("[[^ -> Q, @ -> $]]", Just (ExFormation [BiTau AtRho ExRoot, BiTau AtPhi ExXi]))
      ]

  describe "parse applications with mixed bindings" $
    test
      parseExpression
      [ ("[[]](Q)", Just (ExApplication (ExFormation [BiVoid AtRho]) (ArAlpha (Alpha 0) ExRoot)))
      , ("[[]](Q, T)", Just (ExApplication (ExApplication (ExFormation [BiVoid AtRho]) (ArAlpha (Alpha 0) ExRoot)) (ArAlpha (Alpha 1) ExTermination)))
      , ("Q.x(y -> $)", Just (ExApplication (ExDispatch ExRoot (AtLabel "x")) (ArTau (AtLabel "y") ExXi)))
      , ("[[x -> ?]].x(Q)", Just (ExApplication (ExDispatch (ExFormation [BiVoid (AtLabel "x"), BiVoid AtRho]) (AtLabel "x")) (ArAlpha (Alpha 0) ExRoot)))
      , ("[[]](~!i -> $)", Just (ExApplication (ExFormation [BiVoid AtRho]) (ArAlpha (AlMeta "i") ExXi)))
      , ("[[]](α𝑖 -> Q)", Just (ExApplication (ExFormation [BiVoid AtRho]) (ArAlpha (AlMeta "i") ExRoot)))
      , ("Q.foo(a1 -> Q.y)", Just (ExApplication (ExDispatch ExRoot (AtLabel "foo")) (ArTau (AtLabel "a1") (ExDispatch ExRoot (AtLabel "y"))))) -- #875: "a"-prefixed label in argument position is a named binding, not a positional alpha
      ]

  describe "parse meta expressions" $
    test
      parseExpression
      [ ("!e", Just (ExMeta "e"))
      , ("!e0", Just (ExMeta "e0"))
      , ("!e_test", Just (ExMeta "e_test"))
      , ("𝑒", Just (ExMeta "e"))
      , ("𝑒0", Just (ExMeta "e0"))
      , ("!e.x", Just (ExDispatch (ExMeta "e") (AtLabel "x")))
      , ("!e(Q)", Just (ExApplication (ExMeta "e") (ArAlpha (Alpha 0) ExRoot)))
      , ("!n", Just (ExMeta "n"))
      , ("!n1", Just (ExMeta "n1"))
      , ("𝑛", Just (ExMeta "n"))
      , ("𝑛1", Just (ExMeta "n1"))
      , ("𝑛.x", Just (ExDispatch (ExMeta "n") (AtLabel "x")))
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
