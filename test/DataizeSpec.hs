{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module DataizeSpec (spec) where

import AST
import Control.Monad
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Dataize (DataizeContext (DataizeContext), dataize, dataize', dataizeByRules, mdBuildTerm, morph, morphByRules)
import Deps (Term (TeExpression), dontSaveStep)
import Functions (buildTerm)
import Matcher (substEmpty)
import Parser (parseExpressionThrows, parseProgramThrows)
import Rewriter (Rewritten)
import Test.Hspec
import Yaml qualified

defaultDataizeContext :: Expression -> Program -> DataizeContext
defaultDataizeContext loc prog = DataizeContext loc prog 25 25 False buildTerm dontSaveStep

test :: (Eq a, Show a) => ((Expression, NonEmpty Rewritten) -> DataizeContext -> IO (Maybe a, [Rewritten])) -> [(String, Expression, Expression, Maybe a)] -> Spec
test func useCases =
  forM_ useCases $ \(desc, input, expr, output) ->
    it desc $ do
      let prog = Program expr
      (res, _) <- func (input, (prog, Nothing) :| []) (defaultDataizeContext ExGlobal prog)
      res `shouldBe` output

test' :: (Eq a, Show a) => ((Expression, NonEmpty Rewritten) -> DataizeContext -> IO (a, NonEmpty Rewritten)) -> [(String, Expression, Expression, a)] -> Spec
test' func useCases =
  forM_ useCases $ \(desc, input, expr, output) ->
    it desc $ do
      let prog = Program expr
      (res, _) <- func (input, (prog, Nothing) :| []) (defaultDataizeContext ExGlobal prog)
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

  describe "mdBuildTerm" $ do
    it "resolves global dispatch from the universe Q" $ do
      prog <- parseProgramThrows "Q -> [[ x -> [[ D> 42- ]] ]]"
      expected <- parseExpressionThrows "[[ D> 42- ]]"
      term <- mdBuildTerm (defaultDataizeContext ExGlobal prog) "global" [Yaml.ArgAttribute (AtLabel "x")] substEmpty
      case term of
        TeExpression actual -> actual `shouldBe` expected
        _ -> expectationFailure "global() did not return an expression"
    it "reduces an expression to its normal form" $ do
      reducible <- parseExpressionThrows "⊥.x"
      term <- mdBuildTerm (defaultDataizeContext ExGlobal (Program ExGlobal)) "normalize" [Yaml.ArgExpression reducible] substEmpty
      case term of
        TeExpression actual -> actual `shouldBe` ExTermination
        _ -> expectationFailure "normalize() did not return an expression"

  describe "morphByRules matches the executor" $ do
    let labels f input universe = do
          (e, sq) <- f (input, (Program universe, Nothing) :| []) (defaultDataizeContext ExGlobal (Program universe))
          pure (e, [label | (_, Just label) <- NE.toList sq])
    forM_
      [ ("a delta formation is primitive", ExFormation [BiDelta (BtOne "00")], ExGlobal)
      , ("termination is primitive", ExTermination, ExGlobal)
      ,
        ( "global dispatch resolves and reduces"
        , ExDispatch ExGlobal (AtLabel "x")
        , ExFormation [BiTau (AtLabel "x") (ExFormation [])]
        )
      ,
        ( "a reducible dispatch normalizes"
        , ExDispatch (ExFormation [BiTau (AtLabel "x") (ExFormation [BiDelta (BtOne "00")])]) (AtLabel "x")
        , ExGlobal
        )
      ]
      ( \(desc, input, universe) -> it desc $ do
          expected <- labels morph input universe
          actual <- labels morphByRules input universe
          actual `shouldBe` expected
      )

  describe "dataizeByRules matches the executor" $ do
    let run f input universe = do
          (bytes, chain) <- f (input, (Program universe, Nothing) :| []) (defaultDataizeContext ExGlobal (Program universe))
          pure (bytes, [label | (_, Just label) <- chain])
    forM_
      [ ("a delta formation yields bytes", ExFormation [BiDelta (BtOne "00")], ExGlobal)
      , ("termination has no data", ExTermination, ExGlobal)
      ,
        ( "a box dataizes its body"
        , ExFormation [BiTau AtPhi (ExFormation [BiDelta (BtOne "00"), BiVoid AtRho]), BiVoid AtRho]
        , ExGlobal
        )
      ,
        ( "a dispatch dataizes through morphing"
        , ExDispatch (ExFormation [BiTau (AtLabel "x") (ExFormation [BiDelta (BtOne "01"), BiVoid AtRho]), BiVoid AtRho]) (AtLabel "x")
        , ExGlobal
        )
      ]
      ( \(desc, input, universe) -> it desc $ do
          expected <- run dataize' input universe
          actual <- run dataizeByRules input universe
          actual `shouldBe` expected
      )

  describe "labels every step with a defined rule or operation" $ do
    let funcs = maybe [] (map Yaml.function)
        allowed =
          map (.name) Yaml.morphingRules
            ++ map (.name) Yaml.dataizationRules
            ++ map (.name) Yaml.normalizationRules
            ++ concatMap (funcs . (.where_)) Yaml.morphingRules
            ++ concatMap (funcs . (.where_)) Yaml.dataizationRules
    it "uses no step label without a defining rule or operation" $ do
      prog <-
        parseProgramThrows
          ( unlines
              [ "Q -> [["
              , "  bytes(data) -> [[ @ -> $.data ]],"
              , "  number(as-bytes) -> [[ @ -> $.as-bytes, plus(x) -> [[ L> L_number_plus ]] ]],"
              , "  @ -> 5.plus(6)"
              , "]]"
              ]
          )
      loc <- parseExpressionThrows "Q"
      (_, chain) <- dataize (defaultDataizeContext loc prog)
      let orphans = nub [label | (_, Just label) <- chain, label `notElem` allowed]
      unless
        (null orphans)
        (expectationFailure ("Dataization emitted step labels with no defining rule or operation: " ++ show orphans))

  describe "preserves the reduction label sequence" $ do
    let labelsOf loc src = do
          prog <- parseProgramThrows src
          loc' <- parseExpressionThrows loc
          (_, chain) <- dataize (defaultDataizeContext loc' prog)
          pure [label | (_, Just label) <- chain]
    it "dataizes 5.plus(6) through the expected rules" $ do
      labels <-
        labelsOf
          "Q"
          "Q -> [[ bytes(data) -> [[ @ -> $.data ]], number(as-bytes) -> [[ @ -> $.as-bytes, plus(x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6) ]]"
      labels
        `shouldBe` [ "contextualize"
                   , "Mphi"
                   , "alpha"
                   , "copy"
                   , "dot"
                   , "copy"
                   , "alpha"
                   , "copy"
                   , "Mlambda"
                   , "Mphi"
                   , "alpha"
                   , "copy"
                   , "Mprim"
                   , "contextualize"
                   , "dot"
                   , "Mphi"
                   , "alpha"
                   , "copy"
                   , "copy"
                   , "Mprim"
                   , "contextualize"
                   , "dot"
                   , "copy"
                   , "Mprim"
                   ]
    it "dataizes a located reference through the expected rules" $ do
      labels <- labelsOf "Q.foo.bar" "Q -> [[ foo -> [[ bar -> [[ @ -> Q.x ]] ]], x -> [[ D> 42- ]] ]]"
      labels `shouldBe` ["contextualize", "Mphi", "Mprim"]

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
