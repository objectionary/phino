-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module ParserSpec where

import AST
import Control.Monad (forM_)
import Data.Either (isLeft, isRight)
import Misc (allPathsIn)
import Parser
import System.FilePath (takeBaseName)
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, describe, it, runIO, shouldBe, shouldSatisfy)

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
      [ ("Q -> [[]]", Just (Program (ExFormation [BiVoid AtRho]))),
        ("Q -> T(x -> Q)", Just (Program (ExApplication ExTermination (BiTau (AtLabel "x") ExGlobal)))),
        ("Q -> Q.org.eolang", Just (Program (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")))),
        ("Q -> [[x -> $, y -> ?]]", Just (Program (ExFormation [BiTau (AtLabel "x") ExThis, BiVoid (AtLabel "y"), BiVoid AtRho]))),
        ("{[[foo ↦ QQ]]}", Just (Program (ExFormation [BiTau (AtLabel "foo") (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")), BiVoid AtRho])))
      ]

  describe "parse expression" $
    test
      parseExpression
      [ ("Q.!a", Just (ExDispatch ExGlobal (AtMeta "a"))),
        ("[[]](!a1 -> $)", Just (ExApplication (ExFormation [BiVoid AtRho]) (BiTau (AtMeta "a1") ExThis))),
        ( "[[]](~0 -> $)(~11 -> Q)",
          Just
            ( ExApplication
                ( ExApplication
                    (ExFormation [BiVoid AtRho])
                    (BiTau (AtAlpha 0) ExThis)
                )
                (BiTau (AtAlpha 11) ExGlobal)
            )
        ),
        ("[[]](x -> $, y -> Q)", Just (ExApplication (ExApplication (ExFormation [BiVoid AtRho]) (BiTau (AtLabel "x") ExThis)) (BiTau (AtLabel "y") ExGlobal))),
        ("[[!B, !B1]]", Just (ExFormation [BiMeta "B", BiMeta "B1"])),
        ("[[!B2, !a2 -> $]]", Just (ExFormation [BiMeta "B2", BiTau (AtMeta "a2") ExThis])),
        ("!e0", Just (ExMeta "e0")),
        ("[[x -> !e]]", Just (ExFormation [BiTau (AtLabel "x") (ExMeta "e"), BiVoid AtRho])),
        ("[[!a -> !e1]]", Just (ExFormation [BiTau (AtMeta "a") (ExMeta "e1")])),
        ("Q * !t", Just (ExMetaTail ExGlobal "t")),
        ("[[]](x -> $) * !t1", Just (ExMetaTail (ExApplication (ExFormation [BiVoid AtRho]) (BiTau (AtLabel "x") ExThis)) "t1")),
        ("[[D> --]]", Just (ExFormation [BiDelta BtEmpty, BiVoid AtRho])),
        ("[[D> 1F-]]", Just (ExFormation [BiDelta (BtOne "1F"), BiVoid AtRho])),
        ("[[\n  L> Func,\n  D> 00-\n]]", Just (ExFormation [BiLambda "Func", BiDelta (BtOne "00"), BiVoid AtRho])),
        ("[[D> 1F-2A-00]]", Just (ExFormation [BiDelta (BtMany ["1F", "2A", "00"]), BiVoid AtRho])),
        ("[[D> !d0]]", Just (ExFormation [BiDelta (BtMeta "d0"), BiVoid AtRho])),
        ("[[L> Function]]", Just (ExFormation [BiLambda "Function", BiVoid AtRho])),
        ("[[L> !F3]]", Just (ExFormation [BiMetaLambda "F3", BiVoid AtRho])),
        ("[[x() -> [[]] ]]", Just (ExFormation [BiTau (AtLabel "x") (ExFormation [BiVoid AtRho]), BiVoid AtRho])),
        ( "[[y(^,@,z) -> [[q -> Q.a]] ]]",
          Just
            ( ExFormation
                [ BiTau
                    (AtLabel "y")
                    ( ExFormation
                        [ BiVoid AtRho,
                          BiVoid AtPhi,
                          BiVoid (AtLabel "z"),
                          BiTau (AtLabel "q") (ExDispatch ExGlobal (AtLabel "a"))
                        ]
                    ),
                  BiVoid AtRho
                ]
            )
        ),
        ( "!e(x(^,@) -> [[w -> !e1]])",
          Just
            ( ExApplication
                (ExMeta "e")
                ( BiTau
                    (AtLabel "x")
                    ( ExFormation
                        [ BiVoid AtRho,
                          BiVoid AtPhi,
                          BiTau (AtLabel "w") (ExMeta "e1")
                        ]
                    )
                )
            )
        ),
        ( "[[x -> y.z, w -> ^, u -> @, p -> !a, q -> !e]]",
          Just
            ( ExFormation
                [ BiTau
                    (AtLabel "x")
                    (ExDispatch (ExDispatch ExThis (AtLabel "y")) (AtLabel "z")),
                  BiTau
                    (AtLabel "w")
                    (ExDispatch ExThis AtRho),
                  BiTau
                    (AtLabel "u")
                    (ExDispatch ExThis AtPhi),
                  BiTau
                    (AtLabel "p")
                    (ExDispatch ExThis (AtMeta "a")),
                  BiTau
                    (AtLabel "q")
                    (ExMeta "e"),
                  BiVoid AtRho
                ]
            )
        ),
        ( "Q.x(y, [[]].z, Q.y(^,@))",
          Just
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
        ),
        ( "5.plus(5.q(\"hello\".length))",
          Just
            ( ExApplication
                ( ExDispatch
                    ( ExApplication
                        (ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel "number"))
                        ( BiTau
                            (AtAlpha 0)
                            ( ExApplication
                                (ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel "bytes"))
                                ( BiTau
                                    (AtAlpha 0)
                                    (ExFormation [BiDelta (BtMany ["40", "14", "00", "00", "00", "00", "00", "00"]), BiVoid AtRho])
                                )
                            )
                        )
                    )
                    (AtLabel "plus")
                )
                ( BiTau
                    (AtAlpha 0)
                    ( ExApplication
                        ( ExDispatch
                            ( ExApplication
                                (ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel "number"))
                                ( BiTau
                                    (AtAlpha 0)
                                    ( ExApplication
                                        (ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel "bytes"))
                                        ( BiTau
                                            (AtAlpha 0)
                                            (ExFormation [BiDelta (BtMany ["40", "14", "00", "00", "00", "00", "00", "00"]), BiVoid AtRho])
                                        )
                                    )
                                )
                            )
                            (AtLabel "q")
                        )
                        ( BiTau
                            (AtAlpha 0)
                            ( ExDispatch
                                ( ExApplication
                                    (ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel "string"))
                                    ( BiTau
                                        (AtAlpha 0)
                                        ( ExApplication
                                            (ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel "bytes"))
                                            ( BiTau
                                                (AtAlpha 0)
                                                (ExFormation [BiDelta (BtMany ["68", "65", "6C", "6C", "6F"]), BiVoid AtRho])
                                            )
                                        )
                                    )
                                )
                                (AtLabel "length")
                            )
                        )
                    )
                )
            )
        ),
        ( "[[𝐵1, 𝜏0 -> $, x -> 𝑒]]",
          Just
            ( ExFormation
                [ BiMeta "B1",
                  BiTau (AtMeta "a0") ExThis,
                  BiTau (AtLabel "x") (ExMeta "e")
                ]
            )
        )
      ]

  describe "just parses" $
    forM_
      [ "[[x -> $, y -> ?]]",
        "[[x() -> [[]] ]]",
        "[[x(^, @, y) -> [[q -> QQ]] ]]",
        "Q.x(y() -> [[]])",
        "Q.x(y(q) -> [[w -> !e]])",
        "Q.x(~1(^,@) -> [[]])",
        "Q.x.^.@.!a0",
        "[[x -> y.z]]",
        "[[x -> ^, y -> @, z -> !a]]",
        "Q.x(a.b.c, Q.a(b), [[]])",
        "Q.x(y, [[]].z, Q.y(^,@))",
        "[[x -> 5.plus(5), y -> \"hello\", z -> 42.5]]",
        "[[\n  x -> \"Hi\",\n  y -> 42\n]]",
        "[[x -> -42, y -> +34]]",
        "⟦x ↦ Φ.org.eolang(z ↦ ξ.f, φ ↦ ρ, t ↦ φ, first ↦ ⟦ λ ⤍ Function_name, Δ ⤍ 42- ⟧)⟧",
        "[[x -> 1.00e+3, y -> 2.32e-4]]",
        "[[ x -> \"\\u0001\\u0001\"]]",
        "[[ x -> \"\\uD835\\uDF11\"]]",
        "[[ x ↦ \"This plugin has \\x01\\x01\" ]]",
        "[[ !afoo -> !e1Some, !a-BAR -> !e_123someW, !Bhi123 ]]"
      ]
      (\expr -> it expr (parseExpression expr `shouldSatisfy` isRight))

  describe "prohibits" $
    test
      parseExpression
      ( map
          (\input -> (input, Nothing))
          [ "Q.x()",
            "Q * !t1 * !t2",
            "Q(x -> [[]])",
            "$(x -> [[]])",
            "Q.x(x -> ?)",
            "Q.x(L> Func)",
            "Q.x(D> --)",
            "Q.x(~1 -> ?)",
            "Q.x(L> !F)",
            "Q.x(D> !b)",
            "[[~0 -> Q.x]]",
            "[[x(~1) -> [[]] ]]",
            "[[y(!e) -> [[]] ]]",
            "[[z(w) -> Q.x]]",
            "Q.x(y(~1) -> [[]])",
            "Q.x(1, 2, !B)",
            "Q.x.~0",
            "Q.x(~1 -> Q.y, x -> 5, !B1)",
            "Q.x(𝐵1, 𝜏0 -> $, x -> 𝑒)",
            "[[ x -> \"\\uD800\"]]",
            "[[ x -> \"\\uDFFF\"]]",
            "[[ x -> \"\\uD835\\u0041\"]]",
            "[[ x -> 1, x -> 2 ]]",
            "⟦ k ↦ ⟦ λ ⤍ Foo, λ ⤍ Bar ⟧ ⟧",
            "⟦ k ↦ ⟦ Δ ⤍ 42-, Δ ⤍ 55- ⟧ ⟧"
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
