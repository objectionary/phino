-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module DataizeSpec (spec) where

import Ast (Attribute (AtLabel, AtPhi, AtRho), Binding (BiDelta, BiTau, BiVoid), Expression (ExApplication, ExDispatch, ExFormation, ExGlobal, ExTermination, ExThis), Program (Program))
import Control.Monad
import Dataize (dataize, dataize', morph)
import Parser (parseProgramThrows)
import Test.Hspec

test :: (Eq a, Show a) => (Expression -> Program -> IO (Maybe a)) -> [(String, Expression, Expression, Maybe a)] -> Spec
test func useCases =
  forM_ useCases $ \(desc, input, prog, output) ->
    it desc $ do
      res <- func input (Program prog)
      res `shouldBe` output

spec :: Spec
spec = do
  describe "morph" $
    test
      morph
      [ ("[[ D> 00- ]] => [[ D> 00- ]]", ExFormation [BiDelta "00-"], ExGlobal, Just (ExFormation [BiDelta "00-"])),
        ("T => T", ExTermination, ExGlobal, Just ExTermination),
        ("$ => X", ExThis, ExGlobal, Nothing),
        ("Q => X", ExGlobal, ExGlobal, Nothing),
        ( "Q.x (Q -> [[ x -> [[]] ]]) => [[]]",
          ExDispatch ExGlobal (AtLabel "x"),
          ExFormation [BiTau (AtLabel "x") (ExFormation [])],
          Just (ExFormation [])
        )
      ]

  describe "dataize" $
    test
      dataize'
      [ ("[[ D> 00- ]] => 00-", ExFormation [BiDelta "00-"], ExGlobal, Just "00-"),
        ("T => X", ExTermination, ExGlobal, Nothing),
        ( "[[ @ -> [[ D> 00-]] ]] => 00-",
          ExFormation [BiTau AtPhi (ExFormation [BiDelta "00-", BiVoid AtRho]), BiVoid AtRho],
          ExGlobal,
          Just "00-"
        ),
        ( "[[ x -> [[ D> 01- ]] ]].x => 01-",
          ExDispatch (ExFormation [BiTau (AtLabel "x") (ExFormation [BiDelta "01-", BiVoid AtRho]), BiVoid AtRho]) (AtLabel "x"),
          ExGlobal,
          Just "01-"
        ),
        ( "[[ @ -> [[ x -> [[ D> 01-, y -> ? ]](y -> [[ ]]) ]].x ]] => 01-",
          ExFormation
            [ BiTau
                AtPhi
                ( ExDispatch
                    ( ExFormation
                        [ BiTau
                            (AtLabel "x")
                            ( ExApplication
                                ( ExFormation
                                    [ BiDelta "01-",
                                      BiVoid (AtLabel "y"),
                                      BiVoid AtRho
                                    ]
                                )
                                (BiTau (AtLabel "y") (ExFormation []))
                            )
                        ]
                    )
                    (AtLabel "x")
                )
            ],
          ExGlobal,
          Just "01-"
        )
      ]

  it "dataizes 5.plus(5)" $ do
    prog <-
      parseProgramThrows
        ( unlines
            [ "Q -> [[",
              "  org -> [[",
              "    eolang -> [[",
              "      bytes -> [[",
              "        data -> ?,",
              "        @ -> $.data",
              "      ]],",
              "      number -> [[",
              "        as-bytes -> ?,",
              "        @ -> $.as-bytes,",
              "        plus -> [[ x -> ?, L> L_org_eolang_number_plus ]]",
              "      ]]",
              "    ]]",
              "  ]],",
              "  @ -> 5.plus(5)",
              "]]"
            ]
        )
    value <- dataize prog
    value `shouldBe` Just "40-24-00-00-00-00-00-00"
  
  it "dataizes Fahrenheit test" $ do
    prog <-
      parseProgramThrows
        ( unlines 
            [ "Q -> [[",
              "  org -> [[",
              "    eolang -> [[",
              "      bytes -> [[",
              "        data -> ?,",
              "        @ -> $.data",
              "      ]],",
              "      number -> [[",
              "        as-bytes -> ?,",
              "        @ -> $.as-bytes,",
              "        plus -> [[ x -> ?, L> L_org_eolang_number_plus ]],",
              "        times -> [[ x -> ?, L> L_org_eolang_number_times ]]",
              "      ]]",
              "    ]]",
              "  ]],",
              "  @ -> $.c.times(1.8).plus(32),",
              "  c -> 25",
              "]]"
            ]
        )
    value <- dataize prog
    value `shouldBe` Just "40-53-40-00-00-00-00-00"
