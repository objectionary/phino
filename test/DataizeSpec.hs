-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module DataizeSpec (spec) where

import AST
import Control.Monad
import Dataize (DataizeContext (DataizeContext), dataize, dataize', morph)
import Deps (dontSaveStep)
import Functions (buildTerm)
import Parser (parseExpressionThrows, parseProgramThrows)
import Rewriter (Rewritten)
import Test.Hspec

defaultDataizeContext :: Expression -> Program -> DataizeContext
defaultDataizeContext loc prog = DataizeContext loc prog 25 25 False buildTerm dontSaveStep

test :: (Eq a, Show a) => ((Expression, [Rewritten]) -> DataizeContext -> IO (Maybe a, [Rewritten])) -> [(String, Expression, Expression, Maybe a)] -> Spec
test func useCases =
  forM_ useCases $ \(desc, input, expr, output) ->
    it desc $ do
      let prog = Program expr
      (res, _) <- func (input, [(prog, Nothing)]) (defaultDataizeContext ExGlobal prog)
      res `shouldBe` output

test' :: (Eq a, Show a) => ((Expression, [Rewritten]) -> DataizeContext -> IO (a, [Rewritten])) -> [(String, Expression, Expression, a)] -> Spec
test' func useCases =
  forM_ useCases $ \(desc, input, expr, output) ->
    it desc $ do
      let prog = Program expr
      (res, _) <- func (input, [(prog, Nothing)]) (defaultDataizeContext ExGlobal prog)
      res `shouldBe` output

testDataize :: [(String, String, String, Bytes)] -> Spec
testDataize useCases =
  forM_ useCases $ \(name, loc, prog, res) ->
    it name $ do
      prog' <- parseProgramThrows prog
      loc' <- parseExpressionThrows loc
      (value, _) <- dataize (defaultDataizeContext loc' prog')
      value `shouldBe` Just res

spec :: Spec
spec = do
  describe "morph" $
    test'
      morph
      [ ("[[ D> 00- ]] => [[ D> 00- ]]", ExFormation [BiDelta (BtOne "00")], ExGlobal, ExFormation [BiDelta (BtOne "00")])
      , ("T => T", ExTermination, ExGlobal, ExTermination)
      , ("$ => X", ExThis, ExGlobal, ExTermination)
      , ("Q => X", ExGlobal, ExGlobal, ExTermination)
      ,
        ( "Q.x (Q -> [[ x -> [[]] ]]) => [[]]"
        , ExDispatch ExGlobal (AtLabel "x")
        , ExFormation [BiTau (AtLabel "x") (ExFormation [])]
        , ExFormation []
        )
      ]

  describe "dataize" $
    test
      dataize'
      [ ("[[ D> 00- ]] => 00-", ExFormation [BiDelta (BtOne "00")], ExGlobal, Just (BtOne "00"))
      , ("T => X", ExTermination, ExGlobal, Nothing)
      ,
        ( "[[ @ -> [[ D> 00-]] ]] => 00-"
        , ExFormation [BiTau AtPhi (ExFormation [BiDelta (BtOne "00"), BiVoid AtRho]), BiVoid AtRho]
        , ExGlobal
        , Just (BtOne "00")
        )
      ,
        ( "[[ x -> [[ D> 01- ]] ]].x => 01-"
        , ExDispatch (ExFormation [BiTau (AtLabel "x") (ExFormation [BiDelta (BtOne "01"), BiVoid AtRho]), BiVoid AtRho]) (AtLabel "x")
        , ExGlobal
        , Just (BtOne "01")
        )
      ,
        ( "[[ @ -> [[ x -> [[ D> 01-, y -> ? ]](y -> [[ ]]) ]].x ]] => 01-"
        , ExFormation
            [ BiTau
                AtPhi
                ( ExDispatch
                    ( ExFormation
                        [ BiTau
                            (AtLabel "x")
                            ( ExApplication
                                ( ExFormation
                                    [ BiDelta (BtOne "01")
                                    , BiVoid (AtLabel "y")
                                    , BiVoid AtRho
                                    ]
                                )
                                (BiTau (AtLabel "y") (ExFormation []))
                            )
                        ]
                    )
                    (AtLabel "x")
                )
            ]
        , ExGlobal
        , Just (BtOne "01")
        )
      ]

  testDataize
    [
      ( "5.plus(6)"
      , "Q"
      , unlines
          [ "Q -> [["
          , "  bytes(data) -> [["
          , "    @ -> $.data"
          , "  ]],"
          , "  number(as-bytes) -> [["
          , "    @ -> $.as-bytes,"
          , "    plus(x) -> [[ L> L_number_plus ]]"
          , "  ]],"
          , "  @ -> 5.plus(6)"
          , "]]"
          ]
      , BtMany ["40", "26", "00", "00", "00", "00", "00", "00"]
      )
    ,
      ( "Fahrenheit"
      , "Q"
      , unlines
          [ "Q -> [["
          , "  bytes -> [["
          , "    data -> ?,"
          , "    @ -> $.data"
          , "  ]],"
          , "  number -> [["
          , "    as-bytes -> ?,"
          , "    @ -> $.as-bytes,"
          , "    plus -> [[ x -> ?, L> L_number_plus ]],"
          , "    times -> [[ x -> ?, L> L_number_times ]]"
          , "  ]],"
          , "  @ -> $.c.times(1.8).plus(32),"
          , "  c -> 25"
          , "]]"
          ]
      , BtMany ["40", "53", "40", "00", "00", "00", "00", "00"]
      )
    ,
      ( "Factorial"
      , "Q"
      , unlines
          [ "Q -> [["
          , "  bytes -> [["
          , "    data -> ?,"
          , "    @ -> $.data"
          , "  ]],"
          , "  number -> [["
          , "    as-bytes -> ?,"
          , "    @ -> $.as-bytes,"
          , "    times -> [[ x -> ?, L> L_number_times ]],"
          , "    plus -> [[ x -> ?, L> L_number_plus ]],"
          , "    eq -> [[ x -> ?, y -> ?, L> L_number_eq ]]"
          , "  ]],"
          , "  fac -> [["
          , "    x -> ?,"
          , "    @ -> $.x.eq("
          , "      1,"
          , "      $.x.times($.^.fac($.x.plus(-1)))"
          , "    )"
          , "  ]],"
          , "  @ -> $.fac(3)"
          , "]]"
          ]
      , BtMany ["40", "18", "00", "00", "00", "00", "00", "00"]
      )
    ,
      ( "Located"
      , "Q.foo.bar"
      , unlines
          [ "Q -> [["
          , "  foo -> [["
          , "    bar -> [["
          , "      @ -> Q.x"
          , "    ]]"
          , "  ]],"
          , "  x -> [[ D> 42- ]]"
          , "]]"
          ]
      , BtOne "42"
      )
    ,
      ( "Five"
      , "Q.x"
      , unlines
          [ "Q -> [["
          , "  number(as-bytes) -> [[ @ -> as-bytes ]],"
          , "  bytes(data) -> [[ @ -> data ]],"
          , "  x -> 5"
          , "]]"
          ]
      , BtMany ["40", "14", "00", "00", "00", "00", "00", "00"]
      )
    ]
