{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module DataizeSpec (spec) where

import AST
import Atoms (Registry, emptyRegistry)
import Control.Exception (SomeException)
import Control.Monad
import Data.Aeson (FromJSON)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (find, isInfixOf, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isJust)
import Data.Yaml qualified as Decode
import Dataize (Outcome (..), dataize, dataize', reduction)
import Deps (Evaluation (..), dontSaveEval, dontSaveStep)
import Files (allPathsIn)
import Fixtures (defaultReduceContext, fixtureRegistry, primitives, withAtoms, withNode)
import Functions (buildTerm)
import GHC.Generics (Generic)
import Matcher (substEmpty)
import Morph (ReduceContext (..), Steps (..), emptyState, execBuildTerm)
import Parser (parseBytes, parseExpressionThrows)
import Rewriter (Rewritten)
import Rule (RuleContext (RuleContext), matchExpressionWithRule')
import System.FilePath (makeRelative)
import Test.Hspec
import Yaml qualified

test :: (Eq a, Show a) => ((Expression, NonEmpty Rewritten) -> Expression -> String -> ReduceContext -> IO ((a, [Rewritten]), String)) -> [(String, Expression, Expression, a)] -> Spec
test func useCases =
  forM_ useCases $ \(desc, input, expr, output) ->
    it desc $ do
      ((res, _), _) <- func (input, (expr, Nothing) :| []) expr emptyState (defaultReduceContext ExRoot)
      res `shouldBe` output

-- One case of 𝔻, as a pack of 'test-resources/dataization-packs' spells it: the
-- program under 'input', wrapped in the fixture object model where 'model' says
-- so and run against the fixture λ functions where 'atoms' does, entered at
-- 'location' and answering either the bytes under 'result' or the failure under
-- 'fails'.
data DataizePack = DataizePack
  { location :: Maybe String
  , input :: String
  , model :: Maybe Bool
  , atoms :: Maybe Bool
  , result :: Maybe String
  , fails :: Maybe String
  }
  deriving (Generic, Show, FromJSON)

-- Dataize one such pack and check what it answers. A pack that registers the
-- fixture λ functions runs an external script, so it is pending where 'node' is
-- not installed.
testDataize :: Registry -> FilePath -> Expectation
testDataize registry pth = do
  DataizePack{..} <- Decode.decodeFileThrow pth
  expr <- parseExpressionThrows (if model == Just True then primitives input else input)
  loc <- parseExpressionThrows (fromMaybe "Q" location)
  let ctx = (defaultReduceContext loc){_atoms = if atoms == Just True then registry else emptyRegistry}
      checked :: Expectation
      checked = case (result, fails) of
        (Just res, Nothing) -> do
          bts <- either (fail . ("cannot read the expected bytes: " ++)) pure (parseBytes res)
          (value, _) <- dataize expr ctx
          value `shouldBe` Dataized bts
        (Nothing, Just message) ->
          dataize expr ctx `shouldThrow` (\err -> message `isInfixOf` show (err :: SomeException))
        _ -> expectationFailure "The pack holds neither a single 'result' nor a single 'fails'"
  if atoms == Just True then withNode checked else checked

-- Dataize under '--partial', collecting every report 𝔼 makes on the way, in
-- the order it makes them
partially :: Registry -> String -> IO ((Outcome, [Rewritten]), [Evaluation])
partially registry src = do
  expr <- parseExpressionThrows (primitives src)
  reports <- newIORef []
  let ctx =
        (withAtoms registry (defaultReduceContext ExRoot))
          { _partial = True
          , _saveEval = \report -> modifyIORef' reports (report :)
          }
  result <- dataize expr ctx
  collected <- readIORef reports
  pure (result, reverse collected)

spec :: Spec
spec = do
  -- Every λ function a case may fire comes from the fixture registry, read
  -- once here: phino carries none of its own (see 'Fixtures').
  registry <- runIO fixtureRegistry

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
  -- Which λ functions exist is no longer phino's business: the registry given
  -- with '--atoms' decides, and each one runs as an external script (see
  -- 'Atoms'). What a pack with 'atoms' on asserts is that the answer of such a
  -- script lands in the derivation exactly where a built-in atom's answer used
  -- to: 𝔼 normalizes it and 𝔻 carries on. The λ functions themselves are the
  -- fixture ones (see 'Fixtures'), and 'number.eq' is composed out of
  -- 'L_bytes_eq' the way 'eq.eo' composes it, so the EO-level composition is
  -- exercised too.
  describe "dataize" $ do
    let resources = "test-resources/dataization-packs"
    packs <- runIO (allPathsIn resources)
    forM_ packs (\pth -> it (makeRelative resources pth) (testDataize registry pth))

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
  -- single step, so the 𝕄/𝔻 recursion itself was unbounded: this division, whose
  -- λ-atom keeps re-firing on a term that never reduces to bytes, sent 'morph''
  -- through md → ma → universe → mf → mphi → ml forever and no CLI option could
  -- stop it (#1052). '--max-steps' bounds that recursion and fails once the
  -- budget is gone.
  describe "stops a dataization that never reaches bytes" $ do
    it "fails on the step limit instead of morphing forever" $
      withNode $ do
        expr <- parseExpressionThrows "⟦ @ ↦ ⟦ λ ⤍ L_number_div, ρ ↦ ⟦ Δ ⤍ 40-45-00-00-00-00-00-00 ⟧, x ↦ ⟦ Δ ⤍ 40-00-00-00-00-00-00-00 ⟧ ⟧ ⟧"
        dataize expr (ReduceContext ExRoot 25 25 (Steps 40 0) False True False False registry buildTerm reduction dontSaveStep dontSaveEval)
          `shouldThrow` (\e -> "--max-steps=40" `isInfixOf` show (e :: SomeException))

    -- A budget spent on a cycle is a stuck site just as an atom that cannot
    -- fire is: under '_partial' the run ends on the residual the spine had
    -- reached instead of failing hard (#1078)
    it "parks the step limit as a residual with --partial" $
      withNode $ do
        expr <- parseExpressionThrows "⟦ @ ↦ ⟦ λ ⤍ L_number_div, ρ ↦ ⟦ Δ ⤍ 40-45-00-00-00-00-00-00 ⟧, x ↦ ⟦ Δ ⤍ 40-00-00-00-00-00-00-00 ⟧ ⟧ ⟧"
        (outcome, _) <- dataize expr (ReduceContext ExRoot 25 25 (Steps 40 0) False True True False registry buildTerm reduction dontSaveStep dontSaveEval)
        case outcome of
          Residual _ -> pure ()
          Dataized bts -> expectationFailure ("expected a residual, dataized to " ++ show bts)

  -- An atom phino does not know — a name the '--atoms' registry does not carry,
  -- such as the placeholder ⟦ λ ⤍ Sym_arg_0 ⟧ standing in for a data input
  -- (#1060) — fails the run. Under '_partial' the run ends on the residue
  -- instead: the working expression the spine had reached, with the stuck
  -- application intact and everything the calculus demanded before it already
  -- evaluated, while 𝔼 reports each parked site with no result. Since the atoms
  -- moved out of the binary, an operand that cannot be reduced is the script's
  -- own business, so what parks here is the unregistered λ function alone.
  describe "partially evaluates around an atom that cannot fire (--partial)" $ do
    -- the parser gives every formation its void ρ
    let placeholder = ExFormation [BiLambda (Function "Sym_arg_0"), BiVoid AtRho]
    it "fails on it without the flag, naming the unknown atom" $
      withNode $ do
        expr <- parseExpressionThrows (primitives "2.times(3).nope")
        dataize expr (withAtoms registry (defaultReduceContext ExRoot))
          `shouldThrow` (\e -> "Atom 'L_number_nope' does not exist" `isInfixOf` show (e :: SomeException))
    it "leaves the application of the unregistered atom in place" $
      withNode $ do
        ((outcome, _), _) <- partially registry "2.times(3).nope"
        case outcome of
          Residual (ExFormation bds) -> bds `shouldContain` [BiLambda (Function "L_number_nope")]
          other -> expectationFailure ("expected a residual formation, got " ++ show other)
    it "keeps what was evaluated before the stuck site in the residue" $
      withNode $ do
        ((outcome, _), _) <- partially registry "2.times(3).nope"
        case outcome of
          Residual (ExFormation bds) -> do
            let rho = [value | BiTau AtRho value <- bds]
            length rho `shouldBe` 1
            -- 2 × 3 = 6.0, whose IEEE 754 bytes are 40-18-00-00-00-00-00-00
            show rho `shouldContain` show (BtMany ["40", "18", "00", "00", "00", "00", "00", "00"])
            -- the times application is gone: ρ is the number it produced, its 'as-bytes' bound
            [() | ExFormation inner <- rho, BiTau (AtLabel "as-bytes") _ <- inner] `shouldBe` [()]
          other -> expectationFailure ("expected a residual formation, got " ++ show other)
    it "reports the firing that succeeded with its result and the stuck site without one" $
      withNode $ do
        (_, reports) <- partially registry "2.times(3).nope"
        map (._function) reports `shouldBe` ["L_number_times", "L_number_nope"]
        map (isJust . (._result)) reports `shouldBe` [True, False]
    it "leaves an unknown atom dataized directly as the whole residue" $
      withNode $ do
        ((outcome, chain), reports) <- partially registry "[[ L> Sym_arg_0 ]]"
        outcome `shouldBe` Residual placeholder
        map (._function) reports `shouldBe` ["Sym_arg_0"]
        map fst chain `shouldEndWith` [placeholder]
    it "still reaches bytes when nothing is stuck" $
      withNode $ do
        ((outcome, _), reports) <- partially registry "2.times(3)"
        outcome `shouldBe` Dataized (BtMany ["40", "18", "00", "00", "00", "00", "00", "00"])
        map (._function) reports `shouldBe` ["L_number_times"]
    it "stops on the terminator ⊥ as before, since a wrong operand is not a stuck atom" $
      withNode $ do
        expr <- parseExpressionThrows (primitives "5.plus( Φ.bytes( φ ↦ ⟦ Δ ⤍ -- ⟧ ) )")
        dataize expr ((withAtoms registry (defaultReduceContext ExRoot)){_partial = True})
          `shouldThrow` (\e -> "terminator" `isInfixOf` show (e :: SomeException))

  describe "ReduceContext's --max-depth/--max-cycles reach into the normalization it splices in" $ do
    let boxed = "[[ @ -> [[ D> 00- ]] ]]"
    forM_
      [
        ( "--max-cycles"
        , ReduceContext ExRoot 25 0 (Steps 250 0) True True False False emptyRegistry buildTerm reduction dontSaveStep dontSaveEval
        , "--max-cycles=0"
        )
      ,
        ( "--max-depth"
        , ReduceContext ExRoot 0 25 (Steps 250 0) True True False False emptyRegistry buildTerm reduction dontSaveStep dontSaveEval
        , "--max-depth=0"
        )
      ]
      ( \(flag, ctx, message) ->
          it ("throws once " ++ flag ++ " is exhausted with --depth-sensitive") $ do
            expr <- parseExpressionThrows boxed
            dataize expr ctx `shouldThrow` (\e -> message `isInfixOf` show (e :: SomeException))
      )
    forM_
      [ ("--max-cycles", ReduceContext ExRoot 25 0 (Steps 250 0) False True False False emptyRegistry buildTerm reduction dontSaveStep dontSaveEval)
      , ("--max-depth", ReduceContext ExRoot 0 25 (Steps 250 0) False True False False emptyRegistry buildTerm reduction dontSaveStep dontSaveEval)
      ]
      ( \(flag, ctx) ->
          it ("does not throw without --depth-sensitive even once " ++ flag ++ " is exhausted") $ do
            expr <- parseExpressionThrows boxed
            (value, _) <- dataize expr ctx
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
    it "uses no step label without a defining rule or operation" $
      withNode $ do
        expr <- parseExpressionThrows (primitives "5.plus(6)")
        loc <- parseExpressionThrows "Q"
        (_, chain) <- dataize expr (withAtoms registry (defaultReduceContext loc))
        let orphans = nub [label | (_, Just label) <- chain, label `notElem` allowed]
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
          (_, chain) <- dataize expr (withAtoms registry (defaultReduceContext loc'))
          pure [label | (_, Just label) <- chain]
    it "dataizes 5.plus(6) through the expected rules" $
      withNode $ do
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
                     , "ma"
                     , "copy"
                     , "mf"
                     , "contextualize"
                     , "ma"
                     , "copy"
                     , "mf"
                     , "contextualize"
                     , "delta"
                     ]
    it "dataizes a located reference through the expected rules" $ do
      labels <- labelsOf "Q.foo.bar" "[[ foo -> [[ bar -> [[ @ -> Q.x ]] ]], x -> [[ D> 42- ]] ]]"
      labels `shouldBe` ["contextualize", "md", "dot", "copy", "mf", "delta"]
