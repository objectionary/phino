{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module DataizeSpec (spec) where

import AST
import Control.Exception (SomeException)
import Control.Monad
import Data.Aeson (FromJSON)
import Data.IORef (newIORef)
import Data.List (find, isInfixOf, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Yaml qualified as Decode
import Dataize (Outcome (..), dataize, dataize', reduction)
import Deps (Judgment (..), State, dontSaveEval, dontSaveStep)
import Engine (Engine (_normal), building)
import Evaluate (evaluation, fired)
import Files (allPathsIn)
import Fixtures (defaultReduceContext, fixtureLambdas, linked, loopingLambdas, overdue, primitives, recorded, withLambdas)
import GHC.Generics (Generic)
import Lambdas (Lambdas, emptyLambdas, readLambdas)
import Matcher (substEmpty)
import Morph (ReduceContext (..), Steps (..), emptyState, execBuildTerm)
import Parser (parseBytes, parseExpressionThrows)
import Rewriter (Rewritten)
import Rule (RuleContext (RuleContext), matchExpressionWithRule')
import System.FilePath (makeRelative)
import Test.Hspec
import Yaml qualified

test :: (Eq a, Show a) => ((Expression, NonEmpty Rewritten) -> Expression -> State -> ReduceContext -> IO ((a, [Rewritten]), State)) -> [(String, Expression, Expression, a)] -> Spec
test func useCases =
  forM_ useCases $ \(desc, input, expr, output) ->
    it desc $ do
      ((res, _), _) <- func (input, (expr, Nothing) :| []) expr emptyState =<< defaultReduceContext ExRoot
      res `shouldBe` output

data DataizePack = DataizePack
  { location :: Maybe String
  , input :: String
  , model :: Maybe Bool
  , symbolic :: Maybe Bool
  , result :: Maybe String
  , fails :: Maybe String
  }
  deriving (Generic, Show, FromJSON)

testDataize :: Lambdas -> FilePath -> Expectation
testDataize known pth = do
  DataizePack{..} <- Decode.decodeFileThrow pth
  expr <- parseExpressionThrows (if model == Just True then primitives input else input)
  loc <- parseExpressionThrows (fromMaybe "Q" location)
  base <- defaultReduceContext loc
  let ctx = base{_symbolic = if symbolic == Just True then known else emptyLambdas}
  case (result, fails) of
    (Just res, Nothing) -> do
      bts <- either (fail . ("cannot read the expected bytes: " ++)) pure (parseBytes res)
      (value, _, _) <- dataize expr emptyState ctx
      value `shouldBe` Dataized bts
    (Nothing, Just message) ->
      dataize expr emptyState ctx `shouldThrow` (\err -> message `isInfixOf` show (err :: SomeException))
    _ -> expectationFailure "The pack holds neither a single 'result' nor a single 'fails'"

partially :: Lambdas -> String -> IO ((Outcome, [Rewritten]), String)
partially known src = do
  expr <- parseExpressionThrows (primitives src)
  recorded $ \record -> do
    base <- defaultReduceContext ExRoot
    let ctx = (withLambdas known base){_partial = True, _saveEval = record}
    (outcome, chain, _) <- dataize expr emptyState ctx
    pure (outcome, chain)

looping :: (Lambdas -> IO a) -> IO a
looping action = loopingLambdas (readLambdas >=> action)

spec :: Spec
spec = do
  known <- runIO fixtureLambdas

  describe "dataize' fails when no dataization rule matches the term" $
    it "throws instead of treating the unmatched meta as ⊥" $
      (dataize' (ExMeta "unbound", (ExRoot, Nothing) :| []) ExRoot emptyState =<< defaultReduceContext ExRoot)
        `shouldThrow` (\e -> "no dataization rule matched" `isInfixOf` show (e :: SomeException))

  describe "dataization 'norm' is disjoint from the specific clauses" $ do
    ctx <- runIO (defaultReduceContext ExRoot)
    let rctx = RuleContext (execBuildTerm ExRoot ctx) Nothing (_normal linked)
        dataizeRule :: String -> Yaml.DataizeRule
        dataizeRule nm = fromMaybe (error ("no dataization rule named " ++ nm)) (find (\r -> r.name == nm) Yaml.dataizationRules)
        asRule :: Yaml.DataizeRule -> Yaml.Rule
        asRule r = Yaml.Rule r.name Nothing Nothing r.match ExRoot r.when Nothing Nothing
    it "does not fire on a formation" $ do
      substs <- matchExpressionWithRule' [substEmpty] (ExFormation [BiDelta (BtOne "00")]) (asRule (dataizeRule "norm")) rctx
      substs `shouldBe` []
    it "does not fire on the termination ⊥" $ do
      substs <- matchExpressionWithRule' [substEmpty] ExTermination (asRule (dataizeRule "norm")) rctx
      substs `shouldBe` []
    it "still fires on a non-formation, non-termination normal form" $ do
      substs <- matchExpressionWithRule' [substEmpty] (ExDispatch ExXi (AtLabel "x")) (asRule (dataizeRule "norm")) rctx
      null substs `shouldBe` False
    forM_
      ["delta", "fire"]
      ( \name ->
          it ("leaves '" ++ name ++ "' off a formation holding both Δ and λ") $ do
            substs <- matchExpressionWithRule' [substEmpty] (ExFormation [BiDelta (BtOne "01"), BiLambda (Function "Foo")]) (asRule (dataizeRule name)) rctx
            substs `shouldBe` []
      )

  describe "dataize" $ do
    let resources = "test-resources/dataization-packs"
    packs <- runIO (allPathsIn resources)
    forM_ packs (\pth -> it (makeRelative resources pth) (testDataize known pth))

  describe "dataize'" $
    test
      dataize'
      [ ("[[ D> 00- ]] => 00-", ExFormation [BiDelta (BtOne "00")], ExRoot, BtOne "00")
      ,
        ( "[[ @ -> [[ D> 00-]] ]] => 00-"
        , ExFormation [BiTau AtPhi (ExFormation [BiDelta (BtOne "00"), BiVoid AtRho]), BiVoid AtRho]
        , ExRoot
        , BtOne "00"
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
                                (ArTau (AtLabel "y") (ExFormation []))
                            )
                        ]
                    )
                    (AtLabel "x")
                )
            ]
        , ExRoot
        , BtOne "01"
        )
      ]

  describe "fails to dataize the terminator" $ do
    let failsOn desc input =
          it desc $
            (dataize' (input, (ExRoot, Nothing) :| []) ExRoot emptyState =<< defaultReduceContext ExRoot)
              `shouldThrow` (\e -> "terminator" `isInfixOf` show (e :: SomeException))
    failsOn "throws on ⊥ instead of mapping it to empty bytes" ExTermination
    failsOn "throws on a data-less formation, which dataizes ⊥" (ExFormation [])
    failsOn
      "throws on a void slot fed a non-absolute argument instead of looping forever"
      (ExApplication (ExFormation [BiVoid (AtLabel "x")]) (ArTau (AtLabel "x") (ExDispatch ExXi (AtLabel "foo"))))

  describe "stops a dataization that never reaches bytes" $ do
    it "fails on the step limit instead of morphing forever" $
      looping $ \endless -> do
        expr <- parseExpressionThrows "⟦ @ ↦ ⟦ λ ⤍ L_loop ⟧ ⟧"
        minted <- newIORef 0
        dataize expr emptyState (ReduceContext ExRoot ExRoot Nothing 25 25 (Steps 40 0) Nothing minted Nothing Nothing 1 False True False False 1 Nothing Dataization [] Map.empty endless (building linked) reduction evaluation fired dontSaveStep dontSaveEval linked)
          `shouldThrow` (\e -> "--max-steps=40" `isInfixOf` show (e :: SomeException))

    it "parks the step limit as a residual with --partial" $
      looping $ \endless -> do
        expr <- parseExpressionThrows "⟦ @ ↦ ⟦ λ ⤍ L_loop ⟧ ⟧"
        minted <- newIORef 0
        (outcome, _, _) <- dataize expr emptyState (ReduceContext ExRoot ExRoot Nothing 25 25 (Steps 40 0) Nothing minted Nothing Nothing 1 False True True False 1 Nothing Dataization [] Map.empty endless (building linked) reduction evaluation fired dontSaveStep dontSaveEval linked)
        case outcome of
          Residual _ -> pure ()
          Dataized bts -> expectationFailure ("expected a residual, dataized to " ++ show bts)

  describe "stops a dataization by the clock of --max-seconds" $
    it "fails a partial dataization once the deadline has passed" $ do
      expr <- parseExpressionThrows "[[ @ -> [[ D> 7E- ]] ]]"
      deadline <- overdue 29
      ctx <- defaultReduceContext ExRoot
      dataize expr emptyState ctx{_deadline = Just deadline, _partial = True}
        `shouldThrow` (\e -> "--max-seconds=29" `isInfixOf` show (e :: SomeException))

  describe "partially evaluates around a λ function that cannot fire (--partial)" $ do
    let placeholder = ExFormation [BiLambda (Function "Sym_arg_0")]
    it "fails on it without the flag, naming the λ function" $ do
      expr <- parseExpressionThrows (primitives "2.times(3).nope")
      (dataize expr emptyState . withLambdas known =<< defaultReduceContext ExRoot)
        `shouldThrow` (\e -> "No entry of --symbolic answers the λ function 'L_number_nope'" `isInfixOf` show (e :: SomeException))
    it "leaves the application of the unanswered λ function in place" $ do
      ((outcome, _), _) <- partially known "2.times(3).nope"
      case outcome of
        Residual (ExFormation bds) -> bds `shouldContain` [BiLambda (Function "L_number_nope")]
        other -> expectationFailure ("expected a residual formation, got " ++ show other)
    it "keeps what was evaluated before the stuck site in the residue" $ do
      ((outcome, _), _) <- partially known "2.times(3).nope"
      case outcome of
        Residual (ExFormation bds) -> do
          let rho = [value | BiTau AtRho value <- bds]
          length rho `shouldBe` 1
          [() | ExApplication (ExDispatch ExRoot (AtLabel "number")) (ArTau AtPhi _) <- rho] `shouldBe` [()]
        other -> expectationFailure ("expected a residual formation, got " ++ show other)
    it "writes the firing that answered into the protocol and stops at the stuck one" $ do
      (_, protocol) <- partially known "2.times(3).nope"
      protocol
        `shouldBe` unlines
          [ "  formation(⟦ bytes(φ) ↦ ⟦ not(ρ) ↦ L_bytes_not:λ, eq(ρ, b) ↦ L_bytes_eq:λ ⟧, bool(φ) ↦ ⟦ if(ρ, then, else) ↦ L_fork:λ ⟧, number(φ) ↦ ⟦ as-bytes ↦ φ, plus(ρ, x) ↦ L_number_plus:λ, times(ρ, x) ↦ L_number_times:λ, div(ρ, x) ↦ L_number_div:λ, gt(ρ, x) ↦ L_number_gt:λ, eq(ρ, x) ↦ ρ.as-bytes.eq( x.as-bytes ):φ, nope(ρ) ↦ L_number_nope:λ ⟧, φ ↦ 2.times( 3 ).nope ⟧)  # 𝔻(Φ)"
          , "    applied(𝑛.0.1) := 2  # 𝕄(Φ)"
          , "    applied(𝑛.0.2) := 𝑛.0.1.times( x ↦ 3 )  # 𝕄(Φ)"
          , "    𝔼(L_number_times)  # 𝕄(Φ)"
          , "      applied(𝑛.1.1) := 2  # 𝕄(Φ.a🌵17)"
          , "      formation(𝑛.1.1)  # 𝔻(Φ.a🌵17)"
          , "        applied(𝑛.1.2) := Φ.bytes( φ ↦ 40-00-00-00-00-00-00-00:Δ )  # 𝕄(Φ.a🌵17)"
          , "        formation(𝑛.1.2)  # 𝔻(Φ.a🌵17)"
          , "      𝛿1.1 := 40-00-00-00-00-00-00-00  # 𝔻(ξ.ρ)"
          , "      applied(𝑛.1.3) := 3  # 𝕄(Φ.a🌵18)"
          , "      formation(𝑛.1.3)  # 𝔻(Φ.a🌵18)"
          , "        applied(𝑛.1.4) := Φ.bytes( φ ↦ 40-08-00-00-00-00-00-00:Δ )  # 𝕄(Φ.a🌵18)"
          , "        formation(𝑛.1.4)  # 𝔻(Φ.a🌵18)"
          , "      𝛿2.1 := 40-08-00-00-00-00-00-00  # 𝔻(ξ.x)"
          , "      𝑛.1.5 := Φ.number( φ ↦ 𝜎1:λ )  # 𝑛"
          , "      applied(𝑛.1.6) := Φ.number( φ ↦ 𝜎1:λ )  # 𝕄(Φ)"
          , "      𝑛.1.7 := 𝑛.1.6  # 𝕄(𝑛.1.5)"
          , "    unanswered(L_number_nope)  # 𝔻(⟦ ρ ↦ 𝑛.1.6, λ ⤍ L_number_nope ⟧)"
          ]
    it "leaves an unanswered λ function dataized directly as the whole residue" $ do
      ((outcome, chain), protocol) <- partially known "[[ L> Sym_arg_0 ]]"
      outcome `shouldBe` Residual placeholder
      protocol `shouldBe` "  formation(⟦ bytes(φ) ↦ ⟦ not(ρ) ↦ L_bytes_not:λ, eq(ρ, b) ↦ L_bytes_eq:λ ⟧, bool(φ) ↦ ⟦ if(ρ, then, else) ↦ L_fork:λ ⟧, number(φ) ↦ ⟦ as-bytes ↦ φ, plus(ρ, x) ↦ L_number_plus:λ, times(ρ, x) ↦ L_number_times:λ, div(ρ, x) ↦ L_number_div:λ, gt(ρ, x) ↦ L_number_gt:λ, eq(ρ, x) ↦ ρ.as-bytes.eq( x.as-bytes ):φ, nope(ρ) ↦ L_number_nope:λ ⟧, φ ↦ Sym_arg_0:λ ⟧)  # 𝔻(Φ)\n    unanswered(Sym_arg_0)  # 𝔻(Sym_arg_0:λ)\n"
      map fst chain `shouldEndWith` [placeholder]
    it "still reaches the manufactured datum when nothing is stuck" $ do
      ((outcome, _), _) <- partially known "2.times(3)"
      outcome `shouldBe` Dataized (BtMany ["40", "45", "00", "00", "00", "00", "00", "00"])
    it "parks a firing whose operand dataizes the terminator ⊥" $ do
      ((outcome, _), protocol) <- partially known "5.plus( ⟦ ⟧ )"
      case outcome of
        Residual (ExFormation bds) -> bds `shouldContain` [BiLambda (Function "L_number_plus")]
        other -> expectationFailure ("expected a residual formation, got " ++ show other)
      protocol `shouldSatisfy` isInfixOf "unanswered(⊥)  # 𝔻(⊥)"

  describe "ReduceContext's --max-depth/--max-cycles reach into the normalization it splices in" $ do
    let boxed = "[[ @ -> [[ D> 00- ]] ]]"
    forM_
      [
        ( "--max-cycles"
        , \minted -> ReduceContext ExRoot ExRoot Nothing 25 0 (Steps 250 0) Nothing minted Nothing Nothing 1 True True False False 1 Nothing Dataization [] Map.empty emptyLambdas (building linked) reduction evaluation fired dontSaveStep dontSaveEval linked
        , "--max-cycles=0"
        )
      ,
        ( "--max-depth"
        , \minted -> ReduceContext ExRoot ExRoot Nothing 0 25 (Steps 250 0) Nothing minted Nothing Nothing 1 True True False False 1 Nothing Dataization [] Map.empty emptyLambdas (building linked) reduction evaluation fired dontSaveStep dontSaveEval linked
        , "--max-depth=0"
        )
      ]
      ( \(flag, reducing, message) ->
          it ("throws once " ++ flag ++ " is exhausted with --depth-sensitive") $ do
            expr <- parseExpressionThrows "[[ @ -> [[ x -> [[ D> 00- ]] ]].x ]]"
            ctx <- reducing <$> newIORef 0
            dataize expr emptyState ctx `shouldThrow` (\e -> message `isInfixOf` show (e :: SomeException))
      )
    it "does not throw without --depth-sensitive even once --max-depth is exhausted" $ do
      expr <- parseExpressionThrows boxed
      minted <- newIORef 0
      (value, _, _) <- dataize expr emptyState (ReduceContext ExRoot ExRoot Nothing 0 25 (Steps 250 0) Nothing minted Nothing Nothing 1 False True False False 1 Nothing Dataization [] Map.empty emptyLambdas (building linked) reduction evaluation fired dontSaveStep dontSaveEval linked)
      value `shouldBe` Dataized (BtOne "00")
    it "throws once --max-cycles is exhausted even without --depth-sensitive" $ do
      expr <- parseExpressionThrows boxed
      minted <- newIORef 0
      dataize expr emptyState (ReduceContext ExRoot ExRoot Nothing 25 0 (Steps 250 0) Nothing minted Nothing Nothing 1 False True False False 1 Nothing Dataization [] Map.empty emptyLambdas (building linked) reduction evaluation fired dontSaveStep dontSaveEval linked)
        `shouldThrow` (\e -> "--max-cycles=0" `isInfixOf` show (e :: SomeException))

  describe "labels every step with a defined rule or operation" $ do
    let verb op = case op of
          Yaml.OpMorph _ _ -> "morph"
          Yaml.OpNormalize _ -> "normalize"
          Yaml.OpEvaluate _ _ -> "evaluate"
          Yaml.OpContextualize _ _ -> "contextualize"
          Yaml.OpDataize _ _ -> "dataize"
        allowed =
          map (.name) Yaml.morphingRules
            ++ map (.name) Yaml.dataizationRules
            ++ map (.name) Yaml.normalizationRules
            ++ concatMap (map (verb . (.operation)) . (.premises)) Yaml.morphingRules
            ++ concatMap (map (verb . (.operation)) . (.premises)) Yaml.dataizationRules
    it "uses no step label without a defining rule or operation" $ do
      expr <- parseExpressionThrows (primitives "5.plus(6)")
      loc <- parseExpressionThrows "Q"
      (_, chain, _) <- dataize expr emptyState . withLambdas known =<< defaultReduceContext loc
      let orphans = nub [label | (_, Just (_, label)) <- chain, label `notElem` allowed, label /= "symbol"]
      unless
        (null orphans)
        (expectationFailure ("Dataization emitted step labels with no defining rule or operation: " ++ show orphans))
    it "takes the step of a firing by evaluation" $ do
      expr <- parseExpressionThrows (primitives "5.plus(6)")
      loc <- parseExpressionThrows "Q"
      (_, chain, _) <- dataize expr emptyState . withLambdas known =<< defaultReduceContext loc
      map snd chain `shouldContain` [Just (Evaluation, "evaluate")]
    it "takes the step of a box by contextualization" $ do
      expr <- parseExpressionThrows "[[ @ -> [[ D> 0A- ]] ]]"
      (_, chain, _) <- dataize expr emptyState =<< defaultReduceContext ExRoot
      map snd chain `shouldContain` [Just (Contextualization, "contextualize")]
    it "takes the step of a delta by dataization" $ do
      expr <- parseExpressionThrows "[[ D> 3C- ]]"
      (_, chain, _) <- dataize expr emptyState =<< defaultReduceContext ExRoot
      map snd chain `shouldBe` [Just (Dataization, "delta"), Nothing]

  describe "names every rule uniquely across rule sets" $
    it "shares no rule name between morphing, dataization, normalization and contextualization" $ do
      let names =
            map (.name) Yaml.morphingRules
              ++ map (.name) Yaml.dataizationRules
              ++ map (.name) Yaml.normalizationRules
              ++ map (.name) Yaml.contextualizationRules
          clashes = nub (filter (\n -> length (filter (== n) names) > 1) names)
      clashes `shouldBe` []

  describe "preserves the reduction label sequence" $ do
    let labelsOf loc src = do
          expr <- parseExpressionThrows src
          loc' <- parseExpressionThrows loc
          (_, chain, _) <- dataize expr emptyState . withLambdas known =<< defaultReduceContext loc'
          pure [label | (_, Just (_, label)) <- chain]
    it "dataizes 5.plus(6) through the expected rules" $ do
      labels <-
        labelsOf
          "Q"
          "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(^, x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6) ]]"
      labels
        `shouldBe` [ "contextualize"
                   , "maa"
                   , "alpha"
                   , "copy"
                   , "mf"
                   , "evaluate"
                   , "contextualize"
                   , "symbol"
                   ]
    it "dataizes a located reference through the expected rules" $ do
      labels <- labelsOf "Q.foo.bar" "[[ foo -> [[ bar -> [[ @ -> Q.x ]] ]], x -> [[ D> 42- ]] ]]"
      labels `shouldBe` ["contextualize", "md", "dot", "skip", "mf", "delta"]
    it "takes every step of 5.plus(6) by the judgment of its rule" $ do
      expr <- parseExpressionThrows "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(^, x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6) ]]"
      (_, chain, _) <- dataize expr emptyState . withLambdas known =<< defaultReduceContext ExRoot
      [judgment | (_, Just (judgment, _)) <- chain]
        `shouldBe` [ Contextualization
                   , Morphing
                   , Normalization
                   , Normalization
                   , Morphing
                   , Evaluation
                   , Contextualization
                   , Dataization
                   ]
