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
      , ("Q -> T(x -> Q)", Just (Program (ExApplication ExTermination (BiTau (AtLabel "x") ExGlobal))))
      , ("Q -> Q.org.eolang", Just (Program (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang"))))
      , ("Q -> [[x -> $, y -> ?]]", Just (Program (ExFormation [BiTau (AtLabel "x") ExThis, BiVoid (AtLabel "y"), BiVoid AtRho])))
      ]

  describe "parse expression" $
    test
      parseExpression
      [ ("Q.!a1", Just (ExDispatch ExGlobal (AtMeta (Just "a1"))))
      , ("[[]](!a1 -> $)", Just (ExApplication (ExFormation [BiVoid AtRho]) (BiTau (AtMeta (Just "a1")) ExThis)))
      ,
        ( "[[]](~0 -> $)(~11 -> Q)"
        , Just
            ( ExApplication
                ( ExApplication
                    (ExFormation [BiVoid AtRho])
                    (BiTau (AtAlpha 0) ExThis)
                )
                (BiTau (AtAlpha 11) ExGlobal)
            )
        )
      , ("[[]](x -> $, y -> Q)", Just (ExApplication (ExApplication (ExFormation [BiVoid AtRho]) (BiTau (AtLabel "x") ExThis)) (BiTau (AtLabel "y") ExGlobal)))
      , ("[[!B0, !B1]]", Just (ExFormation [BiMeta (Just "B0"), BiMeta (Just "B1")]))
      , ("[[!B2, !a2 -> $]]", Just (ExFormation [BiMeta (Just "B2"), BiTau (AtMeta (Just "a2")) ExThis]))
      , ("!e0", Just (ExMeta (Just "e0")))
      , ("[[x -> !e1]]", Just (ExFormation [BiTau (AtLabel "x") (ExMeta (Just "e1")), BiVoid AtRho]))
      , ("[[!a1 -> !e1]]", Just (ExFormation [BiTau (AtMeta (Just "a1")) (ExMeta (Just "e1"))]))
      , ("Q * !t1", Just (ExMetaTail ExGlobal (Just "t1")))
      , ("[[]](x -> $) * !t1", Just (ExMetaTail (ExApplication (ExFormation [BiVoid AtRho]) (BiTau (AtLabel "x") ExThis)) (Just "t1")))
      , ("[[D> --]]", Just (ExFormation [BiDelta BtEmpty, BiVoid AtRho]))
      , ("[[D> 1F-]]", Just (ExFormation [BiDelta (BtOne "1F"), BiVoid AtRho]))
      , ("[[\n  L> Func,\n  D> 00-\n]]", Just (ExFormation [BiLambda "Func", BiDelta (BtOne "00"), BiVoid AtRho]))
      , ("[[D> 1F-2A-00]]", Just (ExFormation [BiDelta (BtMany ["1F", "2A", "00"]), BiVoid AtRho]))
      , ("[[D> !d0]]", Just (ExFormation [BiDelta (BtMeta (Just "d0")), BiVoid AtRho]))
      , ("[[L> Function]]", Just (ExFormation [BiLambda "Function", BiVoid AtRho]))
      , ("[[L> !F3]]", Just (ExFormation [BiMetaLambda (Just "F3"), BiVoid AtRho]))
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
                        , BiTau (AtLabel "q") (ExDispatch ExGlobal (AtLabel "a"))
                        ]
                    )
                , BiVoid AtRho
                ]
            )
        )
      ,
        ( "!e0(x(^,@) -> [[w -> !e1]])"
        , Just
            ( ExApplication
                (ExMeta (Just "e0"))
                ( BiTau
                    (AtLabel "x")
                    ( ExFormation
                        [ BiVoid AtRho
                        , BiVoid AtPhi
                        , BiTau (AtLabel "w") (ExMeta (Just "e1"))
                        ]
                    )
                )
            )
        )
      ,
        ( "[[x -> y.z, w -> ^, u -> @, p -> !a1, q -> !e1]]"
        , Just
            ( ExFormation
                [ BiTau
                    (AtLabel "x")
                    (ExDispatch (ExDispatch ExThis (AtLabel "y")) (AtLabel "z"))
                , BiTau
                    (AtLabel "w")
                    (ExDispatch ExThis AtRho)
                , BiTau
                    (AtLabel "u")
                    (ExDispatch ExThis AtPhi)
                , BiTau
                    (AtLabel "p")
                    (ExDispatch ExThis (AtMeta (Just "a1")))
                , BiTau
                    (AtLabel "q")
                    (ExMeta (Just "e1"))
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
                        (ExDispatch ExGlobal (AtLabel "x"))
                        (BiTau (AtAlpha 0) (ExDispatch ExThis (AtLabel "y")))
                    )
                    (BiTau (AtAlpha 1) (ExDispatch (ExFormation [BiVoid AtRho]) (AtLabel "z")))
                )
                ( BiTau
                    (AtAlpha 2)
                    ( ExApplication
                        ( ExApplication
                            (ExDispatch ExGlobal (AtLabel "y"))
                            (BiTau (AtAlpha 0) (ExDispatch ExThis AtRho))
                        )
                        (BiTau (AtAlpha 1) (ExDispatch ExThis AtPhi))
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
                ( BiTau
                    (AtAlpha 0)
                    ( ExApplication
                        ( ExDispatch
                            (DataNumber (BtMany ["40", "14", "00", "00", "00", "00", "00", "00"]))
                            (AtLabel "q")
                        )
                        ( BiTau
                            (AtAlpha 0)
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
        ( "[[𝐵1, 𝜏0 -> $, x -> 𝑒1]]"
        , Just
            ( ExFormation
                [ BiMeta (Just "B1")
                , BiTau (AtMeta (Just "a0")) ExThis
                , BiTau (AtLabel "x") (ExMeta (Just "e1"))
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
      , "Q.x.^.@.!a0"
      , "[[x -> y.z]]"
      , "[[x -> ^, y -> @, z -> !a]]"
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
      , "[[ !afoo -> !e1Some, !a-BAR -> !e_123someW, !Bhi123 ]]"
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
          , "[[~0 -> Q.x]]"
          , "[[x(~1) -> [[]] ]]"
          , "[[y(!e) -> [[]] ]]"
          , "[[z(w) -> Q.x]]"
          , "Q.x(y(~1) -> [[]])"
          , "Q.x(1, 2, !B)"
          , "Q.x.~0"
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
      , ("!d0", Just (BtMeta (Just "d0")))
      , ("!d_test", Just (BtMeta (Just "d_test")))
      , ("δ0", Just (BtMeta (Just "d0")))
      , ("GG-", Nothing)
      , ("0-", Nothing)
      , ("000-", Nothing)
      , ("zz-", Nothing)
      ]

  describe "parse binding" $
    test
      parseBinding
      [ ("x -> $", Just (BiTau (AtLabel "x") ExThis))
      , ("y -> Q", Just (BiTau (AtLabel "y") ExGlobal))
      , ("z -> ?", Just (BiVoid (AtLabel "z")))
      , ("w -> ∅", Just (BiVoid (AtLabel "w")))
      , ("^ -> T", Just (BiTau AtRho ExTermination))
      , ("@ -> $", Just (BiTau AtPhi ExThis))
      , ("ρ -> Q", Just (BiTau AtRho ExGlobal))
      , ("φ -> T", Just (BiTau AtPhi ExTermination))
      , ("!a0 -> Q", Just (BiTau (AtMeta (Just "a0")) ExGlobal))
      , ("D> --", Just (BiDelta BtEmpty))
      , ("D> 42-", Just (BiDelta (BtOne "42")))
      , ("D> 01-02-03", Just (BiDelta (BtMany ["01", "02", "03"])))
      , ("D> !d0", Just (BiDelta (BtMeta (Just "d0"))))
      , ("Δ ⤍ FF-", Just (BiDelta (BtOne "FF")))
      , ("Δ ⤍ --", Just (BiDelta BtEmpty))
      , ("L> Func", Just (BiLambda "Func"))
      , ("L> Function_name", Just (BiLambda "Function_name"))
      , ("L> Aφ", Just (BiLambda "Aφ"))
      , ("λ ⤍ Test", Just (BiLambda "Test"))
      , ("L> !F0", Just (BiMetaLambda (Just "F0")))
      , ("!B0", Just (BiMeta (Just "B0")))
      , ("!B_test", Just (BiMeta (Just "B_test")))
      , ("𝐵1", Just (BiMeta (Just "B1")))
      , ("x() -> [[]]", Just (BiTau (AtLabel "x") (ExFormation [BiVoid AtRho])))
      , ("y(^) -> [[]]", Just (BiTau (AtLabel "y") (ExFormation [BiVoid AtRho])))
      , ("z(^, @) -> [[]]", Just (BiTau (AtLabel "z") (ExFormation [BiVoid AtRho, BiVoid AtPhi])))
      , ("x -> [[y -> $]]", Just (BiTau (AtLabel "x") (ExFormation [BiTau (AtLabel "y") ExThis, BiVoid AtRho])))
      , ("x ↦ ξ", Just (BiTau (AtLabel "x") ExThis))
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
      , ("!a0", Just (AtMeta (Just "a0")))
      , ("!a_test", Just (AtMeta (Just "a_test")))
      , ("𝜏0", Just (AtMeta (Just "a0")))
      , ("~0", Just (AtAlpha 0))
      , ("~1", Just (AtAlpha 1))
      , ("~123", Just (AtAlpha 123))
      , ("α0", Just (AtAlpha 0))
      , ("α42", Just (AtAlpha 42))
      , ("X", Nothing)
      , ("123", Nothing)
      , ("", Nothing)
      ]

  describe "parse number" $
    test
      parseNumber
      [ ("0", Just (DataNumber (BtMany ["00", "00", "00", "00", "00", "00", "00", "00"])))
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
      parseExpressionThrows "Q.x" `shouldReturn` ExDispatch ExGlobal (AtLabel "x")
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
      [ ("ξ", Just ExThis)
      , ("Φ", Just ExGlobal)
      , ("⊥", Just ExTermination)
      , ("⟦⟧", Just (ExFormation [BiVoid AtRho]))
      , ("⟦ x ↦ ξ ⟧", Just (ExFormation [BiTau (AtLabel "x") ExThis, BiVoid AtRho]))
      , ("ξ.ρ", Just (ExDispatch ExThis AtRho))
      , ("ξ.φ", Just (ExDispatch ExThis AtPhi))
      ]

  describe "parse labels with special characters" $
    test
      parseExpression
      [ ("foo123", Just (ExDispatch ExThis (AtLabel "foo123")))
      , ("with-dash", Just (ExDispatch ExThis (AtLabel "with-dash")))
      , ("with_underscore", Just (ExDispatch ExThis (AtLabel "with_underscore")))
      , ("aкирилиця", Just (ExDispatch ExThis (AtLabel "aкирилиця")))
      , ("a日本語", Just (ExDispatch ExThis (AtLabel "a日本語")))
      , ("name123_test", Just (ExDispatch ExThis (AtLabel "name123_test")))
      ]

  describe "parse complex formations" $
    test
      parseExpression
      [ ("[[^ -> ?]]", Just (ExFormation [BiVoid AtRho]))
      , ("[[@ -> ?]]", Just (ExFormation [BiVoid AtPhi, BiVoid AtRho]))
      , ("[[^ -> ?, @ -> ?]]", Just (ExFormation [BiVoid AtRho, BiVoid AtPhi]))
      , ("[[^ -> Q, @ -> $]]", Just (ExFormation [BiTau AtRho ExGlobal, BiTau AtPhi ExThis]))
      ]

  describe "parse applications with mixed bindings" $
    test
      parseExpression
      [ ("[[]](Q)", Just (ExApplication (ExFormation [BiVoid AtRho]) (BiTau (AtAlpha 0) ExGlobal)))
      , ("[[]](Q, T)", Just (ExApplication (ExApplication (ExFormation [BiVoid AtRho]) (BiTau (AtAlpha 0) ExGlobal)) (BiTau (AtAlpha 1) ExTermination)))
      , ("Q.x(y -> $)", Just (ExApplication (ExDispatch ExGlobal (AtLabel "x")) (BiTau (AtLabel "y") ExThis)))
      , ("[[x -> ?]].x(Q)", Just (ExApplication (ExDispatch (ExFormation [BiVoid (AtLabel "x"), BiVoid AtRho]) (AtLabel "x")) (BiTau (AtAlpha 0) ExGlobal)))
      ]

  describe "parse meta expressions" $
    test
      parseExpression
      [ ("!e0", Just (ExMeta (Just "e0")))
      , ("!e_test", Just (ExMeta (Just "e_test")))
      , ("𝑒0", Just (ExMeta (Just "e0")))
      , ("!e1.x", Just (ExDispatch (ExMeta (Just "e1")) (AtLabel "x")))
      , ("!e1(Q)", Just (ExApplication (ExMeta (Just "e1")) (BiTau (AtAlpha 0) ExGlobal)))
      ]

  describe "parse meta tails" $
    test
      parseExpression
      [ ("Q * !t1", Just (ExMetaTail ExGlobal (Just "t1")))
      , ("Q.x * !t1", Just (ExMetaTail (ExDispatch ExGlobal (AtLabel "x")) (Just "t1")))
      , ("[[]].y * !t0", Just (ExMetaTail (ExDispatch (ExFormation [BiVoid AtRho]) (AtLabel "y")) (Just "t0")))
      , ("Q * !t1 * !t2", Nothing)
      ]

  describe "parse anonymous metas (no index)" $ do
    let isAnonAttr (Right (ExDispatch _ (AtMeta Nothing))) = True
        isAnonAttr _ = False
        isAnonExpr (Right (ExMeta Nothing)) = True
        isAnonExpr _ = False
    it "Q.!a parses as anonymous attribute meta" $
      parseExpression "Q.!a" `shouldSatisfy` isAnonAttr
    it "𝜏 parses as anonymous attribute meta" $
      parseExpression "Q.𝜏" `shouldSatisfy` isAnonAttr
    it "!e parses as anonymous expression meta" $
      parseExpression "!e" `shouldSatisfy` isAnonExpr
    it "𝑒 parses as anonymous expression meta" $
      parseExpression "𝑒" `shouldSatisfy` isAnonExpr

  describe "parse whitespace handling" $
    forM_
      [ "[[  x   ->   Q  ]]"
      , "[[\n\tx\n\t->\n\tQ\n\t]]"
      , "  Q  .  x  "
      , "Q.x(  y  ->  $  )"
      , "  [[  x  ->  Q  ]]  "
      ]
      (\expr -> it expr (parseExpression expr `shouldSatisfy` isRight))
