{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module ReplacerSpec where

import AST
import Control.Monad (forM_)
import Replacer
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, describe, it, shouldBe)

test :: ReplaceProgramFunc -> [(String, Expression, [Expression], [Expression], Expression)] -> SpecWith (Arg Expectation)
test function useCases =
  forM_ useCases $ \(desc, prog, ptns, repls, res) ->
    it desc $ function (prog, ptns, map const repls) `shouldBe` res

spec :: Spec
spec = do
  describe "replace program: Program => ([Expression], [Expression]) => Program" $
    test
      replaceProgram
      [
        ( "Q -> Q.y.x => ([Q.y], [$]) => Q -> $.x"
        , ExDispatch (ExDispatch ExRoot (AtLabel "y")) (AtLabel "x")
        , [ExDispatch ExRoot (AtLabel "y")]
        , [ExXi]
        , ExDispatch ExXi (AtLabel "x")
        )
      ,
        ( "Q -> [[x -> [[y -> $]], z -> [[w -> $]] ]] => ([[y -> $], [w -> $]], [Q.y, Q.w]) => Q -> [[x -> Q.y, z -> Q.w]]"
        , ExFormation
            [ BiTau (AtLabel "x") (ExFormation [BiTau (AtLabel "y") ExXi])
            , BiTau (AtLabel "z") (ExFormation [BiTau (AtLabel "w") ExXi])
            ]
        , [ExFormation [BiTau (AtLabel "y") ExXi], ExFormation [BiTau (AtLabel "w") ExXi]]
        , [ExDispatch ExRoot (AtLabel "y"), ExDispatch ExRoot (AtLabel "w")]
        , ExFormation
            [ BiTau (AtLabel "x") (ExDispatch ExRoot (AtLabel "y"))
            , BiTau (AtLabel "z") (ExDispatch ExRoot (AtLabel "w"))
            ]
        )
      , ("Q -> [[]] => ([], [$]) => X", ExFormation [], [], [ExXi], ExFormation [])
      ,
        ( "Q -> [[L> Func, D> 00-]] => ([ [[L> Func, D> 00-]] ], [Q]) => Q -> Q"
        , ExFormation [BiLambda (Function "Func"), BiDelta (BtOne "00")]
        , [ExFormation [BiLambda (Function "Func"), BiDelta (BtOne "00")]]
        , [ExRoot]
        , ExRoot
        )
      ,
        ( "Q -> Q.org.eolang => ([Q.org.eolang, Q.org], [$, $]) => $"
        , ExDispatch (ExDispatch ExRoot (AtLabel "org")) (AtLabel "eolang")
        , [ExDispatch (ExDispatch ExRoot (AtLabel "org")) (AtLabel "eolang"), ExDispatch ExRoot (AtLabel "org")]
        , [ExXi, ExXi]
        , ExXi
        )
      ,
        ( "Q -> [[ x -> $.t, t -> ? ]].t(^ -> [[ x -> $.t, t -> ? ]]) => ([ [[ x -> $.t, t -> ? ]].t ], [T]) => T(^ -> [[ x -> $.t, t -> ? ]])"
        , ExApplication
            ( ExDispatch
                ( ExFormation
                    [ BiTau (AtLabel "x") (ExDispatch ExXi (AtLabel "t"))
                    , BiVoid (AtLabel "t")
                    ]
                )
                (AtLabel "t")
            )
            ( ArTau
                AtRho
                ( ExFormation
                    [ BiTau (AtLabel "x") (ExDispatch ExXi (AtLabel "t"))
                    , BiVoid (AtLabel "t")
                    ]
                )
            )
        ,
          [ ExDispatch
              ( ExFormation
                  [ BiTau (AtLabel "x") (ExDispatch ExXi (AtLabel "t"))
                  , BiVoid (AtLabel "t")
                  ]
              )
              (AtLabel "t")
          ]
        , [ExTermination]
        , ExApplication
            ExTermination
            ( ArTau
                AtRho
                ( ExFormation
                    [ BiTau (AtLabel "x") (ExDispatch ExXi (AtLabel "t"))
                    , BiVoid (AtLabel "t")
                    ]
                )
            )
        )
      ,
        ( "Q -> T => ([], []) => Q -> T"
        , ExTermination
        , []
        , []
        , ExTermination
        )
      ,
        ( "Q -> $ => ([$], [Q]) => Q -> Q"
        , ExXi
        , [ExXi]
        , [ExRoot]
        , ExRoot
        )
      ,
        ( "Q -> [[D> --]] => ([[D> --]], [[[L> Функція]]]) => Q -> [[L> Функція]]"
        , ExFormation [BiDelta BtEmpty]
        , [ExFormation [BiDelta BtEmpty]]
        , [ExFormation [BiLambda (Function "Функція")]]
        , ExFormation [BiLambda (Function "Функція")]
        )
      ,
        ( "Q -> Q.プログラム => ([Q.プログラム], [$.コード]) => Q -> $.コード"
        , ExDispatch ExRoot (AtLabel "プログラム")
        , [ExDispatch ExRoot (AtLabel "プログラム")]
        , [ExDispatch ExXi (AtLabel "コード")]
        , ExDispatch ExXi (AtLabel "コード")
        )
      ,
        ( "Q -> [[^ -> T, @ -> T]] => ([T, T], [Q, $]) => Q -> [[^ -> Q, @ -> $]]"
        , ExFormation [BiTau AtRho ExTermination, BiTau AtPhi ExTermination]
        , [ExTermination, ExTermination]
        , [ExRoot, ExXi]
        , ExFormation [BiTau AtRho ExRoot, BiTau AtPhi ExXi]
        )
      ,
        ( "Q -> [[x -> [[y -> Q]]]].x => ([[y -> Q]], [[[z -> $]]]) => Q -> [[x -> [[z -> $]]]].x"
        , ExDispatch (ExFormation [BiTau (AtLabel "x") (ExFormation [BiTau (AtLabel "y") ExRoot])]) (AtLabel "x")
        , [ExFormation [BiTau (AtLabel "y") ExRoot]]
        , [ExFormation [BiTau (AtLabel "z") ExXi]]
        , ExDispatch (ExFormation [BiTau (AtLabel "x") (ExFormation [BiTau (AtLabel "z") ExXi])]) (AtLabel "x")
        )
      ,
        ( "Q -> Q.a(b -> Q.c) => ([Q.a, Q.c], [$, T]) => Q -> $(b -> T)"
        , ExApplication (ExDispatch ExRoot (AtLabel "a")) (ArTau (AtLabel "b") (ExDispatch ExRoot (AtLabel "c")))
        , [ExDispatch ExRoot (AtLabel "a"), ExDispatch ExRoot (AtLabel "c")]
        , [ExXi, ExTermination]
        , ExApplication ExXi (ArTau (AtLabel "b") ExTermination)
        )
      ,
        ( "Q -> [[D> 00-01-02-]] => ([[D> 00-01-02-]], [[[D> FF-]]]) => Q -> [[D> FF-]]"
        , ExFormation [BiDelta (BtMany ["00", "01", "02"])]
        , [ExFormation [BiDelta (BtMany ["00", "01", "02"])]]
        , [ExFormation [BiDelta (BtOne "FF")]]
        , ExFormation [BiDelta (BtOne "FF")]
        )
      ,
        ( "Q -> [[@ -> $.x, ^ -> $.y]] => ([$.x, $.y], [T, Q]) => Q -> [[@ -> T, ^ -> Q]]"
        , ExFormation [BiTau AtPhi (ExDispatch ExXi (AtLabel "x")), BiTau AtRho (ExDispatch ExXi (AtLabel "y"))]
        , [ExDispatch ExXi (AtLabel "x"), ExDispatch ExXi (AtLabel "y")]
        , [ExTermination, ExRoot]
        , ExFormation [BiTau AtPhi ExTermination, BiTau AtRho ExRoot]
        )
      ,
        ( "Q -> Q.a.b.c => ([Q.a.b.c, Q.a.b, Q.a], [$, T, Q]) => Q -> $"
        , ExDispatch (ExDispatch (ExDispatch ExRoot (AtLabel "a")) (AtLabel "b")) (AtLabel "c")
        ,
          [ ExDispatch (ExDispatch (ExDispatch ExRoot (AtLabel "a")) (AtLabel "b")) (AtLabel "c")
          , ExDispatch (ExDispatch ExRoot (AtLabel "a")) (AtLabel "b")
          , ExDispatch ExRoot (AtLabel "a")
          ]
        , [ExXi, ExTermination, ExRoot]
        , ExXi
        )
      ]

  describe "replace program fast: Program => ([Expression], [Expression]) => Program" $
    test
      (replaceProgramFast (ReplaceCtx 3))
      [
        ( "Q -> [[^ -> ?, @ -> ?, D> -> ?]] => [[ !B1, !t -> ?, !B2 ]] => [[ !B1, !t -> $, !B2 ]] => Q -> [[ ^ -> $, @ -> $, D> -> $ ]]"
        , ExFormation [BiVoid AtRho, BiVoid AtPhi, BiVoid AtDelta]
        , [ExFormation [BiVoid AtRho], ExFormation [BiVoid AtPhi], ExFormation [BiVoid AtDelta]]
        , [ExFormation [BiTau AtRho ExXi], ExFormation [BiTau AtPhi ExXi], ExFormation [BiTau AtDelta ExXi]]
        , ExFormation [BiTau AtRho ExXi, BiTau AtPhi ExXi, BiTau AtDelta ExXi]
        )
      ,
        ( "Q -> [[ ^ -> ? ]] => [[ !B1, !t -> ?, !B2 ]] => [[ !B1, !t -> [[ !t -> ? ]], !B2 ]] => Q -> [[ ^ -> [[ ^ -> [[ ^ -> [[ ^ -> ? ]] ]] ]] ]]"
        , ExFormation [BiVoid AtRho]
        , [ExFormation [BiVoid AtRho]]
        , [ExFormation [BiTau AtRho (ExFormation [BiVoid AtRho])]]
        , ExFormation [BiTau AtRho (ExFormation [BiTau AtRho (ExFormation [BiTau AtRho (ExFormation [BiVoid AtRho])])])]
        )
      ,
        ( "Q -> [[ ^ -> T ]](^ -> [[ ^ -> $]]).@ => [[ !B1, !t -> ?, !B2 ]] => [[ !B1, !t -> $, !B2 ]] => Q -> [[ ^ -> $ ]].@"
        , ExDispatch (ExApplication (ExFormation [BiTau AtRho ExTermination]) (ArTau AtRho (ExFormation [BiTau AtRho ExXi]))) AtPhi
        , [ExFormation [BiTau AtRho ExTermination], ExFormation [BiTau AtRho ExXi]]
        , [ExFormation [BiTau AtRho ExRoot], ExFormation [BiVoid AtPhi]]
        , ExDispatch (ExApplication (ExFormation [BiTau AtRho ExRoot]) (ArTau AtRho (ExFormation [BiVoid AtPhi]))) AtPhi
        )
      ,
        ( "Q -> [[ ]] => ([], []) => Q -> [[ ]]"
        , ExFormation []
        , []
        , []
        , ExFormation []
        )
      ,
        ( "Q -> $ => ([$], [T]) => Q -> $"
        , ExXi
        , [ExXi]
        , [ExTermination]
        , ExXi
        )
      ,
        ( "Q -> [[ a -> ?, b -> ?, c -> ? ]] => ([[a -> ?]], [[a -> Q]]) => Q -> [[a -> Q, b -> ?, c -> ?]]"
        , ExFormation [BiVoid (AtLabel "a"), BiVoid (AtLabel "b"), BiVoid (AtLabel "c")]
        , [ExFormation [BiVoid (AtLabel "a")]]
        , [ExFormation [BiTau (AtLabel "a") ExRoot]]
        , ExFormation [BiTau (AtLabel "a") ExRoot, BiVoid (AtLabel "b"), BiVoid (AtLabel "c")]
        )
      ,
        ( "Q -> [[ λ -> ?, D> 00- ]] => ([[λ -> ?]], [[λ -> $]]) => Q -> [[λ -> $, D> 00-]]"
        , ExFormation [BiVoid AtLambda, BiDelta (BtOne "00")]
        , [ExFormation [BiVoid AtLambda]]
        , [ExFormation [BiTau AtLambda ExXi]]
        , ExFormation [BiTau AtLambda ExXi, BiDelta (BtOne "00")]
        )
      ,
        ( "Q -> [[x -> [[y -> ?]]]].x => ([[y -> ?]], [[y -> Q]]) => Q -> [[x -> [[y -> Q]]]].x"
        , ExDispatch (ExFormation [BiTau (AtLabel "x") (ExFormation [BiVoid (AtLabel "y")])]) (AtLabel "x")
        , [ExFormation [BiVoid (AtLabel "y")]]
        , [ExFormation [BiTau (AtLabel "y") ExRoot]]
        , ExDispatch (ExFormation [BiTau (AtLabel "x") (ExFormation [BiTau (AtLabel "y") ExRoot])]) (AtLabel "x")
        )
      ,
        ( "Q -> [[アイテム -> ?]] => ([[アイテム -> ?]], [[アイテム -> $]]) => Q -> [[アイテム -> $]]"
        , ExFormation [BiVoid (AtLabel "アイテム")]
        , [ExFormation [BiVoid (AtLabel "アイテム")]]
        , [ExFormation [BiTau (AtLabel "アイテム") ExXi]]
        , ExFormation [BiTau (AtLabel "アイテム") ExXi]
        )
      ,
        ( "Q -> [[a -> ?, a -> ?]] => ([[a -> ?]], [[a -> Q]]) => Q -> [[a -> Q, a -> Q]]"
        , ExFormation [BiVoid (AtLabel "a"), BiVoid (AtLabel "a")]
        , [ExFormation [BiVoid (AtLabel "a")]]
        , [ExFormation [BiTau (AtLabel "a") ExRoot]]
        , ExFormation [BiTau (AtLabel "a") ExRoot, BiTau (AtLabel "a") ExRoot]
        )
      ,
        ( "Q -> Q.a(b -> [[c -> ?]]) => ([[c -> ?]], [[c -> T]]) => Q -> Q.a(b -> [[c -> T]])"
        , ExApplication (ExDispatch ExRoot (AtLabel "a")) (ArTau (AtLabel "b") (ExFormation [BiVoid (AtLabel "c")]))
        , [ExFormation [BiVoid (AtLabel "c")]]
        , [ExFormation [BiTau (AtLabel "c") ExTermination]]
        , ExApplication (ExDispatch ExRoot (AtLabel "a")) (ArTau (AtLabel "b") (ExFormation [BiTau (AtLabel "c") ExTermination]))
        )
      ,
        ( "Q -> [[ L> Функція ]] => ([[ L> Функція ]], [[ L> Код ]]) => Q -> [[ L> Код ]]"
        , ExFormation [BiLambda (Function "Функція")]
        , [ExFormation [BiLambda (Function "Функція")]]
        , [ExFormation [BiLambda (Function "Код")]]
        , ExFormation [BiLambda (Function "Код")]
        )
      ]

  describe "replace program fast with depth 0" $
    test
      (replaceProgramFast (ReplaceCtx 0))
      [
        ( "Q -> [[a -> ?]] => ([[a -> ?]], [[a -> $]]) => Q -> [[a -> ?]]"
        , ExFormation [BiVoid (AtLabel "a")]
        , [ExFormation [BiVoid (AtLabel "a")]]
        , [ExFormation [BiTau (AtLabel "a") ExXi]]
        , ExFormation [BiVoid (AtLabel "a")]
        )
      ]

  describe "replace program fast with depth 1" $
    test
      (replaceProgramFast (ReplaceCtx 1))
      [
        ( "Q -> [[ ^ -> ? ]] => [[ ^ -> ? ]] => [[ ^ -> [[ ^ -> ? ]] ]] => Q -> [[ ^ -> [[ ^ -> ? ]] ]]"
        , ExFormation [BiVoid AtRho]
        , [ExFormation [BiVoid AtRho]]
        , [ExFormation [BiTau AtRho (ExFormation [BiVoid AtRho])]]
        , ExFormation [BiTau AtRho (ExFormation [BiVoid AtRho])]
        )
      ]
