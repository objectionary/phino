-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module ParserSpec where

import Ast
import Control.Monad (forM_)
import Data.Either (isLeft, isRight)
import Misc (allPathsIn)
import Parser
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, describe, it, shouldBe, shouldSatisfy, runIO)
import System.FilePath (takeBaseName)

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
      [ ("Q -> [[]]", Just (Program (ExFormation []))),
        ("Q -> T(x -> Q)", Just (Program (ExApplication ExTermination [BiTau (AtLabel "x") ExGlobal]))),
        ("Q -> Q.org.eolang", Just (Program (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")))),
        ("Q -> [[x -> $, ~1 -> ?]]", Just (Program (ExFormation [BiTau (AtLabel "x") ExThis, BiVoid (AtAlpha 1)])))
      ]

  describe "parse expression" $
    test
      parseExpression
      [ ("Q.!a", Just (ExDispatch ExGlobal (AtMeta "a"))),
        ("[[]](!a1 -> $)", Just (ExApplication (ExFormation []) [BiTau (AtMeta "a1") ExThis])),
        ( "[[]](~0 -> $)(~1 -> Q)",
          Just
            ( ExApplication
                ( ExApplication
                    (ExFormation [])
                    [BiTau (AtAlpha 0) ExThis]
                )
                [BiTau (AtAlpha 1) ExGlobal]
            )
        ),
        ("[[]](x -> $, y -> Q)", Just (ExApplication (ExFormation []) [BiTau (AtLabel "x") ExThis, BiTau (AtLabel "y") ExGlobal])),
        ("[[!B, !B1]]", Just (ExFormation [BiMeta "B", BiMeta "B1"])),
        ("[[!B2, !a2 -> $]]", Just (ExFormation [BiMeta "B2", BiTau (AtMeta "a2") ExThis])),
        ("!e0", Just (ExMeta "e0")),
        ("[[x -> !e]]", Just (ExFormation [BiTau (AtLabel "x") (ExMeta "e")])),
        ("[[!a -> !e1]]", Just (ExFormation [BiTau (AtMeta "a") (ExMeta "e1")])),
        ("Q * !t", Just (ExMetaTail ExGlobal "t")),
        ("[[]](x -> $) * !t1", Just (ExMetaTail (ExApplication (ExFormation []) [BiTau (AtLabel "x") ExThis]) "t1")),
        ("[[D> --]]", Just (ExFormation [BiDelta "--"])),
        ("[[D> 1F-]]", Just (ExFormation [BiDelta "1F-"])),
        ("[[\n  L> Func,\n  D> 00-\n]]", Just (ExFormation [BiLambda "Func", BiDelta "00-"])),
        ("[[D> 1F-2A-00]]", Just (ExFormation [BiDelta "1F-2A-00"])),
        ("[[D> !b0]]", Just (ExFormation [BiMetaDelta "b0"])),
        ("[[L> Function]]", Just (ExFormation [BiLambda "Function"])),
        ("[[L> !F3]]", Just (ExFormation [BiMetaLambda "F3"]))
      ]

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
            "Q.x(!B)",
            "Q.x(~1 -> ?)",
            "Q.x(L> !F)",
            "Q.x(D> !b)"
          ]
      )

  describe "parse packs" $ do
    packs <- runIO (allPathsIn "test/resources/parser-packs")
    forM_
      packs
      ( \pack -> do
          content <- runIO (readFile pack)
          it (takeBaseName pack) (parseProgram content `shouldSatisfy` isRight)
      )
