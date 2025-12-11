-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module ReplacerSpec where

import AST
import Control.Monad (forM_)
import Replacer
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, describe, it, shouldBe)

test :: ReplaceProgramFunc -> [(String, Program, [Expression], [Expression], Program)] -> SpecWith (Arg Expectation)
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
        , Program (ExDispatch (ExDispatch ExGlobal (AtLabel "y")) (AtLabel "x"))
        , [ExDispatch ExGlobal (AtLabel "y")]
        , [ExThis]
        , Program (ExDispatch ExThis (AtLabel "x"))
        )
      ,
        ( "Q -> [[x -> [[y -> $]], z -> [[w -> $]] ]] => ([[y -> $], [w -> $]], [Q.y, Q.w]) => Q -> [[x -> Q.y, z -> Q.w]]"
        , Program
            ( ExFormation
                [ BiTau (AtLabel "x") (ExFormation [BiTau (AtLabel "y") ExThis])
                , BiTau (AtLabel "z") (ExFormation [BiTau (AtLabel "w") ExThis])
                ]
            )
        , [ExFormation [BiTau (AtLabel "y") ExThis], ExFormation [BiTau (AtLabel "w") ExThis]]
        , [ExDispatch ExGlobal (AtLabel "y"), ExDispatch ExGlobal (AtLabel "w")]
        , Program
            ( ExFormation
                [ BiTau (AtLabel "x") (ExDispatch ExGlobal (AtLabel "y"))
                , BiTau (AtLabel "z") (ExDispatch ExGlobal (AtLabel "w"))
                ]
            )
        )
      , ("Q -> [[]] => ([], [$]) => X", Program (ExFormation []), [], [ExThis], Program (ExFormation []))
      ,
        ( "Q -> [[L> Func, D> 00-]] => ([ [[L> Func, D> 00-]] ], [Q]) => Q -> Q"
        , Program (ExFormation [BiLambda "Func", BiDelta (BtOne "00")])
        , [ExFormation [BiLambda "Func", BiDelta (BtOne "00")]]
        , [ExGlobal]
        , Program ExGlobal
        )
      ,
        ( "Q -> Q.org.eolang => ([Q.org.eolang, Q.org], [$, $]) => $"
        , Program (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang"))
        , [ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang"), ExDispatch ExGlobal (AtLabel "org")]
        , [ExThis, ExThis]
        , Program ExThis
        )
      ,
        ( "Q -> [[ x -> $.t, t -> ? ]].t(^ -> [[ x -> $.t, t -> ? ]]) => ([ [[ x -> $.t, t -> ? ]].t ], [T]) => T(^ -> [[ x -> $.t, t -> ? ]])"
        , Program
            ( ExApplication
                ( ExDispatch
                    ( ExFormation
                        [ BiTau (AtLabel "x") (ExDispatch ExThis (AtLabel "t"))
                        , BiVoid (AtLabel "t")
                        ]
                    )
                    (AtLabel "t")
                )
                ( BiTau
                    AtRho
                    ( ExFormation
                        [ BiTau (AtLabel "x") (ExDispatch ExThis (AtLabel "t"))
                        , BiVoid (AtLabel "t")
                        ]
                    )
                )
            )
        ,
          [ ExDispatch
              ( ExFormation
                  [ BiTau (AtLabel "x") (ExDispatch ExThis (AtLabel "t"))
                  , BiVoid (AtLabel "t")
                  ]
              )
              (AtLabel "t")
          ]
        , [ExTermination]
        , Program
            ( ExApplication
                ExTermination
                ( BiTau
                    AtRho
                    ( ExFormation
                        [ BiTau (AtLabel "x") (ExDispatch ExThis (AtLabel "t"))
                        , BiVoid (AtLabel "t")
                        ]
                    )
                )
            )
        )
      ,
        ( "Q -> T => ([], []) => Q -> T"
        , Program ExTermination
        , []
        , []
        , Program ExTermination
        )
      ,
        ( "Q -> $ => ([$], [Q]) => Q -> Q"
        , Program ExThis
        , [ExThis]
        , [ExGlobal]
        , Program ExGlobal
        )
      ,
        ( "Q -> Q.α0 => ([Q.α0], [$.α1]) => Q -> $.α1"
        , Program (ExDispatch ExGlobal (AtAlpha 0))
        , [ExDispatch ExGlobal (AtAlpha 0)]
        , [ExDispatch ExThis (AtAlpha 1)]
        , Program (ExDispatch ExThis (AtAlpha 1))
        )
      ,
        ( "Q -> [[D> --]] => ([[D> --]], [[[L> Функція]]]) => Q -> [[L> Функція]]"
        , Program (ExFormation [BiDelta BtEmpty])
        , [ExFormation [BiDelta BtEmpty]]
        , [ExFormation [BiLambda "Функція"]]
        , Program (ExFormation [BiLambda "Функція"])
        )
      ,
        ( "Q -> Q.プログラム => ([Q.プログラム], [$.コード]) => Q -> $.コード"
        , Program (ExDispatch ExGlobal (AtLabel "プログラム"))
        , [ExDispatch ExGlobal (AtLabel "プログラム")]
        , [ExDispatch ExThis (AtLabel "コード")]
        , Program (ExDispatch ExThis (AtLabel "コード"))
        )
      ,
        ( "Q -> [[^ -> T, @ -> T]] => ([T, T], [Q, $]) => Q -> [[^ -> Q, @ -> $]]"
        , Program (ExFormation [BiTau AtRho ExTermination, BiTau AtPhi ExTermination])
        , [ExTermination, ExTermination]
        , [ExGlobal, ExThis]
        , Program (ExFormation [BiTau AtRho ExGlobal, BiTau AtPhi ExThis])
        )
      ,
        ( "Q -> [[x -> [[y -> Q]]]].x => ([[y -> Q]], [[[z -> $]]]) => Q -> [[x -> [[z -> $]]]].x"
        , Program (ExDispatch (ExFormation [BiTau (AtLabel "x") (ExFormation [BiTau (AtLabel "y") ExGlobal])]) (AtLabel "x"))
        , [ExFormation [BiTau (AtLabel "y") ExGlobal]]
        , [ExFormation [BiTau (AtLabel "z") ExThis]]
        , Program (ExDispatch (ExFormation [BiTau (AtLabel "x") (ExFormation [BiTau (AtLabel "z") ExThis])]) (AtLabel "x"))
        )
      ,
        ( "Q -> Q.a(b -> Q.c) => ([Q.a, Q.c], [$, T]) => Q -> $(b -> T)"
        , Program (ExApplication (ExDispatch ExGlobal (AtLabel "a")) (BiTau (AtLabel "b") (ExDispatch ExGlobal (AtLabel "c"))))
        , [ExDispatch ExGlobal (AtLabel "a"), ExDispatch ExGlobal (AtLabel "c")]
        , [ExThis, ExTermination]
        , Program (ExApplication ExThis (BiTau (AtLabel "b") ExTermination))
        )
      ,
        ( "Q -> [[D> 00-01-02-]] => ([[D> 00-01-02-]], [[[D> FF-]]]) => Q -> [[D> FF-]]"
        , Program (ExFormation [BiDelta (BtMany ["00", "01", "02"])])
        , [ExFormation [BiDelta (BtMany ["00", "01", "02"])]]
        , [ExFormation [BiDelta (BtOne "FF")]]
        , Program (ExFormation [BiDelta (BtOne "FF")])
        )
      ,
        ( "Q -> [[@ -> $.x, ^ -> $.y]] => ([$.x, $.y], [T, Q]) => Q -> [[@ -> T, ^ -> Q]]"
        , Program (ExFormation [BiTau AtPhi (ExDispatch ExThis (AtLabel "x")), BiTau AtRho (ExDispatch ExThis (AtLabel "y"))])
        , [ExDispatch ExThis (AtLabel "x"), ExDispatch ExThis (AtLabel "y")]
        , [ExTermination, ExGlobal]
        , Program (ExFormation [BiTau AtPhi ExTermination, BiTau AtRho ExGlobal])
        )
      ,
        ( "Q -> Q.a.b.c => ([Q.a.b.c, Q.a.b, Q.a], [$, T, Q]) => Q -> $"
        , Program (ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "a")) (AtLabel "b")) (AtLabel "c"))
        ,
          [ ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "a")) (AtLabel "b")) (AtLabel "c")
          , ExDispatch (ExDispatch ExGlobal (AtLabel "a")) (AtLabel "b")
          , ExDispatch ExGlobal (AtLabel "a")
          ]
        , [ExThis, ExTermination, ExGlobal]
        , Program ExThis
        )
      ]

  describe "replace program fast: Program => ([Expression], [Expression]) => Program" $
    test
      (replaceProgramFast (ReplaceCtx 3))
      [
        ( "Q -> [[^ -> ?, @ -> ?, D> -> ?]] => [[ !B1, !a -> ?, !B2 ]] => [[ !B1, !a -> $, !B2 ]] => Q -> [[ ^ -> $, @ -> $, D> -> $ ]]"
        , Program (ExFormation [BiVoid AtRho, BiVoid AtPhi, BiVoid AtDelta])
        , [ExFormation [BiVoid AtRho], ExFormation [BiVoid AtPhi], ExFormation [BiVoid AtDelta]]
        , [ExFormation [BiTau AtRho ExThis], ExFormation [BiTau AtPhi ExThis], ExFormation [BiTau AtDelta ExThis]]
        , Program (ExFormation [BiTau AtRho ExThis, BiTau AtPhi ExThis, BiTau AtDelta ExThis])
        )
      ,
        ( "Q -> [[ ^ -> ? ]] => [[ !B1, !a -> ?, !B2 ]] => [[ !B1, !a -> [[ !a -> ? ]], !B2 ]] => Q -> [[ ^ -> [[ ^ -> [[ ^ -> [[ ^ -> ? ]] ]] ]] ]]"
        , Program (ExFormation [BiVoid AtRho])
        , [ExFormation [BiVoid AtRho]]
        , [ExFormation [BiTau AtRho (ExFormation [BiVoid AtRho])]]
        , Program (ExFormation [BiTau AtRho (ExFormation [BiTau AtRho (ExFormation [BiTau AtRho (ExFormation [BiVoid AtRho])])])])
        )
      ,
        ( "Q -> [[ ^ -> T ]](^ -> [[ ^ -> $]]).@ => [[ !B1, !a -> ?, !B2 ]] => [[ !B1, !a -> $, !B2 ]] => Q -> [[ ^ -> $ ]].@"
        , Program (ExDispatch (ExApplication (ExFormation [BiTau AtRho ExTermination]) (BiTau AtRho (ExFormation [BiTau AtRho ExThis]))) AtPhi)
        , [ExFormation [BiTau AtRho ExTermination], ExFormation [BiTau AtRho ExThis]]
        , [ExFormation [BiTau AtRho ExGlobal], ExFormation [BiVoid AtPhi]]
        , Program (ExDispatch (ExApplication (ExFormation [BiTau AtRho ExGlobal]) (BiTau AtRho (ExFormation [BiVoid AtPhi]))) AtPhi)
        )
      ,
        ( "Q -> [[ ]] => ([], []) => Q -> [[ ]]"
        , Program (ExFormation [])
        , []
        , []
        , Program (ExFormation [])
        )
      ,
        ( "Q -> $ => ([$], [T]) => Q -> $"
        , Program ExThis
        , [ExThis]
        , [ExTermination]
        , Program ExThis
        )
      ,
        ( "Q -> [[ a -> ?, b -> ?, c -> ? ]] => ([[a -> ?]], [[a -> Q]]) => Q -> [[a -> Q, b -> ?, c -> ?]]"
        , Program (ExFormation [BiVoid (AtLabel "a"), BiVoid (AtLabel "b"), BiVoid (AtLabel "c")])
        , [ExFormation [BiVoid (AtLabel "a")]]
        , [ExFormation [BiTau (AtLabel "a") ExGlobal]]
        , Program (ExFormation [BiTau (AtLabel "a") ExGlobal, BiVoid (AtLabel "b"), BiVoid (AtLabel "c")])
        )
      ,
        ( "Q -> [[ λ -> ?, D> 00- ]] => ([[λ -> ?]], [[λ -> $]]) => Q -> [[λ -> $, D> 00-]]"
        , Program (ExFormation [BiVoid AtLambda, BiDelta (BtOne "00")])
        , [ExFormation [BiVoid AtLambda]]
        , [ExFormation [BiTau AtLambda ExThis]]
        , Program (ExFormation [BiTau AtLambda ExThis, BiDelta (BtOne "00")])
        )
      ,
        ( "Q -> [[x -> [[y -> ?]]]].x => ([[y -> ?]], [[y -> Q]]) => Q -> [[x -> [[y -> Q]]]].x"
        , Program (ExDispatch (ExFormation [BiTau (AtLabel "x") (ExFormation [BiVoid (AtLabel "y")])]) (AtLabel "x"))
        , [ExFormation [BiVoid (AtLabel "y")]]
        , [ExFormation [BiTau (AtLabel "y") ExGlobal]]
        , Program (ExDispatch (ExFormation [BiTau (AtLabel "x") (ExFormation [BiTau (AtLabel "y") ExGlobal])]) (AtLabel "x"))
        )
      ,
        ( "Q -> [[アイテム -> ?]] => ([[アイテム -> ?]], [[アイテム -> $]]) => Q -> [[アイテム -> $]]"
        , Program (ExFormation [BiVoid (AtLabel "アイテム")])
        , [ExFormation [BiVoid (AtLabel "アイテム")]]
        , [ExFormation [BiTau (AtLabel "アイテム") ExThis]]
        , Program (ExFormation [BiTau (AtLabel "アイテム") ExThis])
        )
      ,
        ( "Q -> [[a -> ?, a -> ?]] => ([[a -> ?]], [[a -> Q]]) => Q -> [[a -> Q, a -> Q]]"
        , Program (ExFormation [BiVoid (AtLabel "a"), BiVoid (AtLabel "a")])
        , [ExFormation [BiVoid (AtLabel "a")]]
        , [ExFormation [BiTau (AtLabel "a") ExGlobal]]
        , Program (ExFormation [BiTau (AtLabel "a") ExGlobal, BiTau (AtLabel "a") ExGlobal])
        )
      ,
        ( "Q -> Q.a(b -> [[c -> ?]]) => ([[c -> ?]], [[c -> T]]) => Q -> Q.a(b -> [[c -> T]])"
        , Program (ExApplication (ExDispatch ExGlobal (AtLabel "a")) (BiTau (AtLabel "b") (ExFormation [BiVoid (AtLabel "c")])))
        , [ExFormation [BiVoid (AtLabel "c")]]
        , [ExFormation [BiTau (AtLabel "c") ExTermination]]
        , Program (ExApplication (ExDispatch ExGlobal (AtLabel "a")) (BiTau (AtLabel "b") (ExFormation [BiTau (AtLabel "c") ExTermination])))
        )
      ,
        ( "Q -> [[ L> Функція ]] => ([[ L> Функція ]], [[ L> Код ]]) => Q -> [[ L> Код ]]"
        , Program (ExFormation [BiLambda "Функція"])
        , [ExFormation [BiLambda "Функція"]]
        , [ExFormation [BiLambda "Код"]]
        , Program (ExFormation [BiLambda "Код"])
        )
      ]

  describe "replace program fast with depth 0" $
    test
      (replaceProgramFast (ReplaceCtx 0))
      [
        ( "Q -> [[a -> ?]] => ([[a -> ?]], [[a -> $]]) => Q -> [[a -> ?]]"
        , Program (ExFormation [BiVoid (AtLabel "a")])
        , [ExFormation [BiVoid (AtLabel "a")]]
        , [ExFormation [BiTau (AtLabel "a") ExThis]]
        , Program (ExFormation [BiVoid (AtLabel "a")])
        )
      ]

  describe "replace program fast with depth 1" $
    test
      (replaceProgramFast (ReplaceCtx 1))
      [
        ( "Q -> [[ ^ -> ? ]] => [[ ^ -> ? ]] => [[ ^ -> [[ ^ -> ? ]] ]] => Q -> [[ ^ -> [[ ^ -> ? ]] ]]"
        , Program (ExFormation [BiVoid AtRho])
        , [ExFormation [BiVoid AtRho]]
        , [ExFormation [BiTau AtRho (ExFormation [BiVoid AtRho])]]
        , Program (ExFormation [BiTau AtRho (ExFormation [BiVoid AtRho])])
        )
      ]
