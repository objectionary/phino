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
import Data.List (find, isInfixOf, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Yaml qualified as Decode
import Dataize (Outcome (..), dataize, dataize', reduction)
import Deps (State, dontSaveEval, dontSaveStep)
import Evaluate (evaluation, fired)
import Files (allPathsIn)
import Fixtures (defaultReduceContext, fixtureLambdas, loopingLambdas, primitives, recorded, withLambdas)
import Functions (buildTerm)
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
      ((res, _), _) <- func (input, (expr, Nothing) :| []) expr emptyState (defaultReduceContext ExRoot)
      res `shouldBe` output

-- One case of 𝔻, as a pack of 'test-resources/dataization-packs' spells it: the
-- program under 'input', wrapped in the fixture object model where 'model' says
-- so and run against the fixture λ functions where 'symbolic' does, entered at
-- 'location' and answering either the bytes under 'result' or the failure under
-- 'fails'.
data DataizePack = DataizePack
  { location :: Maybe String
  , input :: String
  , model :: Maybe Bool
  , symbolic :: Maybe Bool
  , result :: Maybe String
  , fails :: Maybe String
  }
  deriving (Generic, Show, FromJSON)

-- Dataize one such pack and check what it answers
testDataize :: Lambdas -> FilePath -> Expectation
testDataize known pth = do
  DataizePack{..} <- Decode.decodeFileThrow pth
  expr <- parseExpressionThrows (if model == Just True then primitives input else input)
  loc <- parseExpressionThrows (fromMaybe "Q" location)
  let ctx = (defaultReduceContext loc){_symbolic = if symbolic == Just True then known else emptyLambdas}
  case (result, fails) of
    (Just res, Nothing) -> do
      bts <- either (fail . ("cannot read the expected bytes: " ++)) pure (parseBytes res)
      (value, _, _) <- dataize expr emptyState ctx
      value `shouldBe` Dataized bts
    (Nothing, Just message) ->
      dataize expr emptyState ctx `shouldThrow` (\err -> message `isInfixOf` show (err :: SomeException))
    _ -> expectationFailure "The pack holds neither a single 'result' nor a single 'fails'"

-- Dataize under '--partial', handing back the protocol of '--protocol'
-- alongside the answer, verbatim
partially :: Lambdas -> String -> IO ((Outcome, [Rewritten]), String)
partially known src = do
  expr <- parseExpressionThrows (primitives src)
  recorded $ \record -> do
    let ctx = (withLambdas known (defaultReduceContext ExRoot)){_partial = True, _saveEval = record}
    (outcome, chain, _) <- dataize expr emptyState ctx
    pure (outcome, chain)

-- The one λ function that answers with a firing of itself, read the way
-- '--symbolic' reads it, so that a run fires it until the step budget is gone
looping :: (Lambdas -> IO a) -> IO a
looping action = loopingLambdas (readLambdas >=> action)

spec :: Spec
spec = do
  -- Every λ function a case may fire comes from the fixture file, read once
  -- here: phino carries none of its own (see 'Fixtures').
  known <- runIO fixtureLambdas

  -- Symmetric to the morphing fallback above: every normal form 𝔻 actually
  -- receives is covered by 'delta'/'box'/'fire'/'none' (formations) or 'norm'
  -- (everything else, disjoint from ⊥ and formations), so this fallback is
  -- unreachable through the public 'dataize'/'dataize'' entry points on any
  -- term produced by normalization. A raw meta again reaches it directly,
  -- proving the fallback itself is live code, not dead weight.
  describe "dataize' fails when no dataization rule matches the term" $
    it "throws instead of treating the unmatched meta as ⊥" $
      dataize' (ExMeta "unbound", (ExRoot, Nothing) :| []) ExRoot emptyState (defaultReduceContext ExRoot)
        `shouldThrow` (\e -> "no dataization rule matched" `isInfixOf` show (e :: SomeException))

  -- 'norm' matches the bare meta 𝑛, which unifies with any expression, so it is
  -- guarded to fire only when 𝑛 is neither a formation ('not (formation 𝑛)',
  -- left to 'delta'/'box'/'fire'/'none') nor the termination ⊥ ('not (𝑛 = ⊥)').
  -- 𝔻 is partial: ⊥ matches no clause and lands on the unmatched-term error
  -- (#955). The dataization clauses are therefore disjoint and their order in
  -- 'resources/dataization' cannot change behavior.
  describe "dataization 'norm' is disjoint from the specific clauses" $ do
    let rctx = RuleContext (execBuildTerm ExRoot (defaultReduceContext ExRoot))
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

  -- Most cases of 𝔻 are four plain values — the program, where the run enters
  -- it, which λ functions answer it and what it must dataize to — so they are
  -- packs of 'test-resources/dataization-packs' rather than Haskell (#1201).
  -- Which λ functions exist is no longer phino's business: the YAML file given
  -- with '--symbolic' decides, and each entry of it answers the firing with a
  -- term of the calculus (see 'Lambdas'). What a pack with 'symbolic' on
  -- asserts is that such an answer lands in the derivation exactly where a
  -- built-in atom's answer used to: 𝔼 normalizes it and 𝔻 carries on. Nothing
  -- is computed on the way, so every one of them ends on the datum a symbol is
  -- manufactured for.
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

  -- 𝔻 is partial (#955): the terminator ⊥ signals an error and lies outside its
  -- domain, so it matches no dataization clause and 𝔻 stops there instead of
  -- yielding empty bytes. A data-less formation ⟦⟧ ('none') dataizes ⊥, so it
  -- fails through the very same path — it has nothing to dataize.
  describe "fails to dataize the terminator" $ do
    let failsOn desc input =
          it desc $
            dataize' (input, (ExRoot, Nothing) :| []) ExRoot emptyState (defaultReduceContext ExRoot)
              `shouldThrow` (\e -> "terminator" `isInfixOf` show (e :: SomeException))
    failsOn "throws on ⊥ instead of mapping it to empty bytes" ExTermination
    failsOn "throws on a data-less formation, which dataizes ⊥" (ExFormation [])
    -- A void slot fed a non-absolute argument morphs to ⊥ via 'mad' (#959) and
    -- then fails through the same terminator path. The regression is that this
    -- test terminates at all: before the fix 'ma' re-morphed the stuck term
    -- forever and dataization never returned.
    failsOn
      "throws on a void slot fed a non-absolute argument instead of looping forever"
      (ExApplication (ExFormation [BiVoid (AtLabel "x")]) (ArTau (AtLabel "x") (ExDispatch ExXi (AtLabel "foo"))))

  -- '--max-cycles' and '--max-depth' reach only the normalization run inside a
  -- single step, so the 𝕄/𝔻 recursion itself was unbounded: a λ function that
  -- answers with a firing of itself sent 'morph'' through md → ma → universe →
  -- mf → mphi → ml forever and no CLI option could stop it (#1052). Recursion
  -- is nothing phino prevents — whether a λ function ends is the object model's
  -- business — so '--max-steps' is what bounds that recursion and fails once
  -- the budget is gone.
  describe "stops a dataization that never reaches bytes" $ do
    it "fails on the step limit instead of morphing forever" $
      looping $ \endless -> do
        expr <- parseExpressionThrows "⟦ @ ↦ ⟦ λ ⤍ L_loop ⟧ ⟧"
        dataize expr emptyState (ReduceContext ExRoot 25 25 (Steps 40 0) 1 False True False False False [] Map.empty endless buildTerm reduction evaluation fired dontSaveStep dontSaveEval)
          `shouldThrow` (\e -> "--max-steps=40" `isInfixOf` show (e :: SomeException))

    -- A budget spent on a cycle is a stuck site just as a λ function that
    -- cannot fire is: under '_partial' the run ends on the residual the spine
    -- had reached instead of failing hard (#1078)
    it "parks the step limit as a residual with --partial" $
      looping $ \endless -> do
        expr <- parseExpressionThrows "⟦ @ ↦ ⟦ λ ⤍ L_loop ⟧ ⟧"
        (outcome, _, _) <- dataize expr emptyState (ReduceContext ExRoot 25 25 (Steps 40 0) 1 False True True False False [] Map.empty endless buildTerm reduction evaluation fired dontSaveStep dontSaveEval)
        case outcome of
          Residual _ -> pure ()
          Dataized bts -> expectationFailure ("expected a residual, dataized to " ++ show bts)

  -- A λ function no entry of the '--symbolic' file answers — a name the file
  -- does not carry, such as the placeholder ⟦ λ ⤍ Sym_arg_0 ⟧ standing in for a
  -- data input (#1060) — fails the run. Under '_partial' the run ends on the
  -- residue instead: the working expression the spine had reached, with the
  -- stuck application intact and everything the calculus demanded before it
  -- already evaluated, while the protocol of '--protocol' keeps the firings
  -- that did answer.
  describe "partially evaluates around a λ function that cannot fire (--partial)" $ do
    -- the parser gives every formation its void ρ
    let placeholder = ExFormation [BiLambda (Function "Sym_arg_0"), BiVoid AtRho]
    it "fails on it without the flag, naming the λ function" $ do
      expr <- parseExpressionThrows (primitives "2.times(3).nope")
      dataize expr emptyState (withLambdas known (defaultReduceContext ExRoot))
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
          -- the times application is gone: ρ is the number it answered, its 'as-bytes' bound
          [() | ExFormation inner <- rho, BiTau (AtLabel "as-bytes") _ <- inner] `shouldBe` [()]
        other -> expectationFailure ("expected a residual formation, got " ++ show other)
    it "writes the firing that answered into the protocol and stops at the stuck one" $ do
      (_, protocol) <- partially known "2.times(3).nope"
      protocol
        `shouldBe` unlines
          [ "  𝔼(L_number_times)"
          , "    𝛿1.1 := 40-00-00-00-00-00-00-00  # ξ.ρ"
          , "    𝛿2.1 := 40-08-00-00-00-00-00-00  # ξ.x"
          , "    𝑛.1.1 := Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )  # 𝑛"
          , "    𝑛.1.2 := ⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, as-bytes ↦ φ, plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧, times(x) ↦ ⟦ λ ⤍ L_number_times ⟧, div(x) ↦ ⟦ λ ⤍ L_number_div ⟧, gt(x) ↦ ⟦ λ ⤍ L_number_gt ⟧, eq(x) ↦ ⟦ φ ↦ ρ.as-bytes.eq( x.as-bytes ) ⟧, nope ↦ ⟦ λ ⤍ L_number_nope ⟧, ρ ↦ ⟦ bytes(φ) ↦ ⟦ not ↦ ⟦ λ ⤍ L_bytes_not ⟧, eq(b) ↦ ⟦ λ ⤍ L_bytes_eq ⟧ ⟧, bool(φ) ↦ ⟦ if(then, else) ↦ ⟦ λ ⤍ L_fork ⟧ ⟧, number(φ) ↦ ⟦ as-bytes ↦ φ, plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧, times(x) ↦ ⟦ λ ⤍ L_number_times ⟧, div(x) ↦ ⟦ λ ⤍ L_number_div ⟧, gt(x) ↦ ⟦ λ ⤍ L_number_gt ⟧, eq(x) ↦ ⟦ φ ↦ ρ.as-bytes.eq( x.as-bytes ) ⟧, nope ↦ ⟦ λ ⤍ L_number_nope ⟧ ⟧, φ ↦ 2.times( 3 ).nope ⟧ ⟧  # 𝕄(𝑛.1.1)"
          , "  ?(L_number_nope)  # ⟦ λ ⤍ L_number_nope, ρ ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, as-bytes ↦ φ, plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧, times(x) ↦ ⟦ λ ⤍ L_number_times ⟧, div(x) ↦ ⟦ λ ⤍ L_number_div ⟧, gt(x) ↦ ⟦ λ ⤍ L_number_gt ⟧, eq(x) ↦ ⟦ φ ↦ ρ.as-bytes.eq( x.as-bytes ) ⟧, nope ↦ ⟦ λ ⤍ L_number_nope ⟧, ρ ↦ ⟦ bytes(φ) ↦ ⟦ not ↦ ⟦ λ ⤍ L_bytes_not ⟧, eq(b) ↦ ⟦ λ ⤍ L_bytes_eq ⟧ ⟧, bool(φ) ↦ ⟦ if(then, else) ↦ ⟦ λ ⤍ L_fork ⟧ ⟧, number(φ) ↦ ⟦ as-bytes ↦ φ, plus(x) ↦ ⟦ λ ⤍ L_number_plus ⟧, times(x) ↦ ⟦ λ ⤍ L_number_times ⟧, div(x) ↦ ⟦ λ ⤍ L_number_div ⟧, gt(x) ↦ ⟦ λ ⤍ L_number_gt ⟧, eq(x) ↦ ⟦ φ ↦ ρ.as-bytes.eq( x.as-bytes ) ⟧, nope ↦ ⟦ λ ⤍ L_number_nope ⟧ ⟧, φ ↦ 2.times( 3 ).nope ⟧ ⟧ ⟧"
          ]
    it "leaves an unanswered λ function dataized directly as the whole residue" $ do
      ((outcome, chain), protocol) <- partially known "[[ L> Sym_arg_0 ]]"
      outcome `shouldBe` Residual placeholder
      protocol `shouldBe` "  ?(Sym_arg_0)  # ⟦ λ ⤍ Sym_arg_0 ⟧\n"
      map fst chain `shouldEndWith` [placeholder]
    it "still reaches the manufactured datum when nothing is stuck" $ do
      ((outcome, _), _) <- partially known "2.times(3)"
      outcome `shouldBe` Dataized (BtMany ["40", "45", "00", "00", "00", "00", "00", "00"])
    it "stops on the terminator ⊥ as before, since a data-less formation is not a stuck λ function" $ do
      expr <- parseExpressionThrows (primitives "5.plus( ⟦ ⟧ )")
      dataize expr emptyState ((withLambdas known (defaultReduceContext ExRoot)){_partial = True})
        `shouldThrow` (\e -> "terminator" `isInfixOf` show (e :: SomeException))

  describe "ReduceContext's --max-depth/--max-cycles reach into the normalization it splices in" $ do
    let boxed = "[[ @ -> [[ D> 00- ]] ]]"
    forM_
      [
        ( "--max-cycles"
        , ReduceContext ExRoot 25 0 (Steps 250 0) 1 True True False False False [] Map.empty emptyLambdas buildTerm reduction evaluation fired dontSaveStep dontSaveEval
        , "--max-cycles=0"
        )
      ,
        ( "--max-depth"
        , ReduceContext ExRoot 0 25 (Steps 250 0) 1 True True False False False [] Map.empty emptyLambdas buildTerm reduction evaluation fired dontSaveStep dontSaveEval
        , "--max-depth=0"
        )
      ]
      ( \(flag, ctx, message) ->
          it ("throws once " ++ flag ++ " is exhausted with --depth-sensitive") $ do
            expr <- parseExpressionThrows boxed
            dataize expr emptyState ctx `shouldThrow` (\e -> message `isInfixOf` show (e :: SomeException))
      )
    forM_
      [ ("--max-cycles", ReduceContext ExRoot 25 0 (Steps 250 0) 1 False True False False False [] Map.empty emptyLambdas buildTerm reduction evaluation fired dontSaveStep dontSaveEval)
      , ("--max-depth", ReduceContext ExRoot 0 25 (Steps 250 0) 1 False True False False False [] Map.empty emptyLambdas buildTerm reduction evaluation fired dontSaveStep dontSaveEval)
      ]
      ( \(flag, ctx) ->
          it ("does not throw without --depth-sensitive even once " ++ flag ++ " is exhausted") $ do
            expr <- parseExpressionThrows boxed
            (value, _, _) <- dataize expr emptyState ctx
            value `shouldBe` Dataized (BtOne "00")
      )

  describe "labels every step with a defined rule or operation" $ do
    let verb op = case op of
          Yaml.OpMorph _ -> "morph"
          Yaml.OpNormalize _ -> "normalize"
          Yaml.OpEvaluate _ _ -> "evaluate"
          Yaml.OpContextualize _ _ -> "contextualize"
          Yaml.OpDataize _ -> "dataize"
        allowed =
          map (.name) Yaml.morphingRules
            ++ map (.name) Yaml.dataizationRules
            ++ map (.name) Yaml.normalizationRules
            ++ concatMap (map (verb . (.operation)) . (.premises)) Yaml.morphingRules
            ++ concatMap (map (verb . (.operation)) . (.premises)) Yaml.dataizationRules
    it "uses no step label without a defining rule or operation" $ do
      expr <- parseExpressionThrows (primitives "5.plus(6)")
      loc <- parseExpressionThrows "Q"
      (_, chain, _) <- dataize expr emptyState (withLambdas known (defaultReduceContext loc))
      let orphans = nub [label | (_, Just label) <- chain, label `notElem` allowed, label /= "symbol"]
      unless
        (null orphans)
        (expectationFailure ("Dataization emitted step labels with no defining rule or operation: " ++ show orphans))

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
          (_, chain, _) <- dataize expr emptyState (withLambdas known (defaultReduceContext loc'))
          pure [label | (_, Just label) <- chain]
    -- 'evaluate' is followed straight by the 'contextualize' of the answer's
    -- own 𝔻 and not by the 'ma'/'copy'/'mf' that used to reduce it on the
    -- spine: 𝔼 morphs what it answers before it hands it over, so the spine is
    -- given a formation and has nothing left to peel (#1268)
    it "dataizes 5.plus(6) through the expected rules" $ do
      labels <-
        labelsOf
          "Q"
          "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6) ]]"
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
      labels `shouldBe` ["contextualize", "md", "dot", "copy", "mf", "delta"]
