-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module DataizeSpec (spec) where

import Ast
import Control.Monad
import Dataize (dataize, dataize', morph, DataizeContext (DataizeContext))
import Parser (parseProgramThrows)
import Test.Hspec
import Functions (buildTerm)

defaultDataizeContext :: Program -> DataizeContext
defaultDataizeContext prog = DataizeContext prog 25 1 False buildTerm

test :: (Eq a, Show a) => (Expression -> DataizeContext -> IO (Maybe a)) -> [(String, Expression, Expression, Maybe a)] -> Spec
test func useCases =
  forM_ useCases $ \(desc, input, prog, output) ->
    it desc $ do
      res <- func input (defaultDataizeContext (Program prog))
      res `shouldBe` output

testDataize :: [(String, String, Bytes)] -> Spec
testDataize useCases =
  forM_ useCases $ \(name, prog, res) ->
    it name $ do
      prog' <- parseProgramThrows prog
      value <- dataize prog' (defaultDataizeContext prog')
      value `shouldBe` Just res

spec :: Spec
spec = do
  describe "morph" $
    test
      morph
      [ ("[[ D> 00- ]] => [[ D> 00- ]]", ExFormation [BiDelta (BtOne "00")], ExGlobal, Just (ExFormation [BiDelta (BtOne "00")])),
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
      [ ("[[ D> 00- ]] => 00-", ExFormation [BiDelta (BtOne "00")], ExGlobal, Just (BtOne "00")),
        ("T => X", ExTermination, ExGlobal, Nothing),
        ( "[[ @ -> [[ D> 00-]] ]] => 00-",
          ExFormation [BiTau AtPhi (ExFormation [BiDelta (BtOne "00"), BiVoid AtRho]), BiVoid AtRho],
          ExGlobal,
          Just (BtOne "00")
        ),
        ( "[[ x -> [[ D> 01- ]] ]].x => 01-",
          ExDispatch (ExFormation [BiTau (AtLabel "x") (ExFormation [BiDelta (BtOne "01"), BiVoid AtRho]), BiVoid AtRho]) (AtLabel "x"),
          ExGlobal,
          Just (BtOne "01")
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
                                    [ BiDelta (BtOne "01"),
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
          Just (BtOne "01")
        )
      ]

  testDataize
    [ ( "5.plus(5)",
        unlines
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
          ],
        BtMany ["40", "24", "00", "00", "00", "00", "00", "00"]
      ),
      ( "Fahrenheit",
        unlines
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
          ],
        BtMany ["40", "53", "40", "00", "00", "00", "00", "00"]
      ),
      ( "Factorial",
        unlines
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
            "        times -> [[ x -> ?, L> L_org_eolang_number_times ]],",
            "        plus -> [[ x -> ?, L> L_org_eolang_number_plus ]],",
            "        eq -> [[ x -> ?, y -> ?, L> L_org_eolang_number_eq ]]",
            "      ]]",
            "    ]]",
            "  ]],",
            "  fac -> [[",
            "    x -> ?,",
            "    @ -> $.x.eq(",
            "      1,",
            "      $.x.times($.^.fac($.x.plus(-1)))",
            "    )",
            "  ]],",
            "  @ -> $.fac(3)",
            "]]"
          ],
        BtMany ["40", "18", "00", "00", "00", "00", "00", "00"]
      )
    ]
