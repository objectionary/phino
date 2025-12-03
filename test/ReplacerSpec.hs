-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module ReplacerSpec where

import AST
import Control.Monad (forM_)
import Replacer
import Test.Hspec (Example (Arg), Expectation, Spec, SpecWith, describe, it, shouldBe)

test :: ReplaceProgramFunc -> [(String, Program, [Expression], [Expression], Maybe Program)] -> SpecWith (Arg Expectation)
test function useCases =
  forM_ useCases $ \(desc, prog, ptns, repls, res) ->
    it desc $ function ptns repls (ReplaceProgramContext prog 3) `shouldBe` res

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
        , Just (Program (ExDispatch ExThis (AtLabel "x")))
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
        , Just
            ( Program
                ( ExFormation
                    [ BiTau (AtLabel "x") (ExDispatch ExGlobal (AtLabel "y"))
                    , BiTau (AtLabel "z") (ExDispatch ExGlobal (AtLabel "w"))
                    ]
                )
            )
        )
      , ("Q -> [[]] => ([], [$]) => X", Program (ExFormation []), [], [ExThis], Nothing)
      ,
        ( "Q -> [[L> Func, D> 00-]] => ([ [[L> Func, D> 00-]] ], [Q]) => Q -> Q"
        , Program (ExFormation [BiLambda "Func", BiDelta (BtOne "00")])
        , [ExFormation [BiLambda "Func", BiDelta (BtOne "00")]]
        , [ExGlobal]
        , Just (Program ExGlobal)
        )
      ,
        ( "Q -> Q.org.eolang => ([Q.org.eolang, Q.org], [$, $]) => $"
        , Program (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang"))
        , [ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang"), ExDispatch ExGlobal (AtLabel "org")]
        , [ExThis, ExThis]
        , Just (Program ExThis)
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
        , Just
            ( Program
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
        )
      ]

  describe "replace program fast: Program => ([Expression], [Expression]) => Program" $
    test
      replaceProgramFast
      [
        ( "Q -> [[^ -> ?, @ -> ?, D> -> ?]] => [[ !B1, !a -> ?, !B2 ]] => [[ !B1, !a -> $, !B2 ]] => Q -> [[ ^ -> $, @ -> $, D> -> $ ]]"
        , Program (ExFormation [BiVoid AtRho, BiVoid AtPhi, BiVoid AtDelta])
        , [ExFormation [BiVoid AtRho], ExFormation [BiVoid AtPhi], ExFormation [BiVoid AtDelta]]
        , [ExFormation [BiTau AtRho ExThis], ExFormation [BiTau AtPhi ExThis], ExFormation [BiTau AtDelta ExThis]]
        , Just (Program (ExFormation [BiTau AtRho ExThis, BiTau AtPhi ExThis, BiTau AtDelta ExThis]))
        )
      ,
        ( "Q -> [[ ^ -> ? ]] => [[ !B1, !a -> ?, !B2 ]] => [[ !B1, !a -> [[ !a -> ? ]], !B2 ]] => Q -> [[ ^ -> [[ ^ -> [[ ^ -> [[ ^ -> ? ]] ]] ]] ]]"
        , Program (ExFormation [BiVoid AtRho])
        , [ExFormation [BiVoid AtRho]]
        , [ExFormation [BiTau AtRho (ExFormation [BiVoid AtRho])]]
        , Just (Program (ExFormation [BiTau AtRho (ExFormation [BiTau AtRho (ExFormation [BiTau AtRho (ExFormation [BiVoid AtRho])])])]))
        )
      ,
        ( "Q -> [[ ^ -> T ]](^ -> [[ ^ -> $]]).@ => [[ !B1, !a -> ?, !B2 ]] => [[ !B1, !a -> $, !B2 ]] => Q -> [[ ^ -> $ ]].@"
        , Program (ExDispatch (ExApplication (ExFormation [BiTau AtRho ExTermination]) (BiTau AtRho (ExFormation [BiTau AtRho ExThis]))) AtPhi)
        , [ExFormation [BiTau AtRho ExTermination], ExFormation [BiTau AtRho ExThis]]
        , [ExFormation [BiTau AtRho ExGlobal], ExFormation [BiVoid AtPhi]]
        , Just (Program (ExDispatch (ExApplication (ExFormation [BiTau AtRho ExGlobal]) (BiTau AtRho (ExFormation [BiVoid AtPhi]))) AtPhi))
        )
      ]
