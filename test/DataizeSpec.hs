{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module DataizeSpec (spec) where

import AST
import Atoms (Registry, emptyRegistry)
import Control.Exception (SomeException)
import Control.Monad
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (find, isInfixOf, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isJust)
import Dataize (Outcome (..), dataize, dataize', reduction)
import Deps (Evaluation (..), dontSaveEval, dontSaveStep)
import Fixtures (defaultReduceContext, fixtureRegistry, primitives, withAtoms, withNode)
import Functions (buildTerm)
import Matcher (substEmpty)
import Morph (ReduceContext (..), Steps (..), emptyState, execBuildTerm)
import Parser (parseExpressionThrows)
import Rewriter (Rewritten)
import Rule (RuleContext (RuleContext), matchExpressionWithRule')
import Test.Hspec
import Yaml qualified

test :: (Eq a, Show a) => ((Expression, NonEmpty Rewritten) -> Expression -> String -> ReduceContext -> IO ((a, [Rewritten]), String)) -> [(String, Expression, Expression, a)] -> Spec
test func useCases =
  forM_ useCases $ \(desc, input, expr, output) ->
    it desc $ do
      ((res, _), _) <- func (input, (expr, Nothing) :| []) expr emptyState (defaultReduceContext ExRoot)
      res `shouldBe` output

testDataize :: [(String, String, String, Bytes)] -> Spec
testDataize useCases =
  forM_ useCases $ \(name, loc, src, res) ->
    it name $ do
      expr <- parseExpressionThrows src
      loc' <- parseExpressionThrows loc
      (value, _) <- dataize expr (defaultReduceContext loc')
      value `shouldBe` Dataized res

-- Wrap a hex literal into the bytes object that EO source spells as a bare '20-1F'
raw :: String -> String
raw bts = "Φ.bytes( φ ↦ ⟦ Δ ⤍ " ++ bts ++ " ⟧ )"

-- Dataize an expression against the fixture universe, with the fixture λ
-- functions registered. Every such case runs an external script, so it is
-- pending where 'node' is not installed.
testAtom :: Registry -> [(String, String, Bytes)] -> Spec
testAtom registry useCases =
  forM_ useCases $ \(name, src, res) ->
    it name $
      withNode $ do
        expr <- parseExpressionThrows (primitives src)
        loc <- parseExpressionThrows "Q"
        (value, _) <- dataize expr (withAtoms registry (defaultReduceContext loc))
        value `shouldBe` Dataized res

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

-- An atom with no answer yields ⊥, which stops the whole dataization
testStuckAtom :: Registry -> [(String, String)] -> Spec
testStuckAtom registry useCases =
  forM_ useCases $ \(name, src) ->
    it name $
      withNode $ do
        expr <- parseExpressionThrows (primitives src)
        loc <- parseExpressionThrows "Q"
        dataize expr (withAtoms registry (defaultReduceContext loc))
          `shouldThrow` (\e -> "terminator" `isInfixOf` show (e :: SomeException))

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

  describe "dataize" $
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
        expr <- parseExpressionThrows (primitives ("5.plus( " ++ raw "--" ++ " )"))
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
    -- The 'none' rule dataizes ⊥ (𝔻(⟦⟧) → 𝔻(⊥)), which matches no clause now
    -- that there is no 'end' rule, so an empty formation reduces through one
    -- labelled 'dataize' step and then fails: it has nothing to dataize (#955).
    it "fails to dataize an empty formation, which dataizes ⊥" $ do
      expr <- parseExpressionThrows "[[ ]]"
      loc <- parseExpressionThrows "Q"
      dataize expr (defaultReduceContext loc)
        `shouldThrow` (\e -> "terminator" `isInfixOf` show (e :: SomeException))

  -- Every case below reaches its bytes without firing an atom, so none of them
  -- needs the registry: what they exercise is the calculus itself.
  testDataize
    [
      ( "Located"
      , "Q.foo.bar"
      , unlines
          [ "[["
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
          [ "[["
          , "  number ↦ ⟦ φ ↦ ∅ ⟧,"
          , "  bytes ↦ ⟦ φ ↦ ∅ ⟧,"
          , "  x -> 5"
          , "]]"
          ]
      , BtMany ["40", "14", "00", "00", "00", "00", "00", "00"]
      )
    , -- Dispatching an absent attribute on a φ-decorated formation now resolves
      -- the inherited attribute through morphing 'mphi' (#973): PHI used to be a
      -- normalization rule, but following the decoration is a semantic 𝕄 step,
      -- so it moved into 'resources/morphing'. Here '.t' is missing from the outer
      -- formation, so 𝕄 walks the '@' decoration to the parent that defines 't'
      -- and dataizes its datum.

      ( "InheritedThroughPhi"
      , "Q"
      , "[[ @ -> [[ t -> [[ D> 2A- ]] ]] ]].t"
      , BtOne "2A"
      )
    ]

  -- Which λ functions exist is no longer phino's business: the registry given
  -- with '--atoms' decides, and each one runs as an external script (see
  -- 'Atoms'). What the cases below assert is that the answer of such a script
  -- lands in the derivation exactly where a built-in atom's answer used to: 𝔼
  -- normalizes it and 𝔻 carries on. The λ functions themselves are the fixture
  -- ones (see 'Fixtures'), and 'number.eq' is composed out of 'L_bytes_eq' the
  -- way 'eq.eo' composes it, so the EO-level composition is exercised too.
  describe "atoms come from the registry" $ do
    testAtom
      registry
      [ ("adds two numbers", "5.plus( 6 )", BtMany ["40", "26", "00", "00", "00", "00", "00", "00"])
      , ("multiplies two numbers", "5.times( 6 )", BtMany ["40", "3E", "00", "00", "00", "00", "00", "00"])
      , -- Two firings in a row: 'ml' reduces the head of the second dispatch,
        -- which fires the first atom, before the second one is handed its own
        -- formation to fire against
        ("fires twice down a chain of dispatches", "5.plus( 6 ).plus( 7 )", BtMany ["40", "32", "00", "00", "00", "00", "00", "00"])
      , ("divides a positive dividend", "256.div( 16 )", BtMany ["40", "30", "00", "00", "00", "00", "00", "00"])
      , ("divides by zero into infinity", "2.div( 0 )", BtMany ["7F", "F0", "00", "00", "00", "00", "00", "00"])
      , ("tells 1000 is greater than 200", "1000.gt( 200 )", BtOne "FF")
      , ("tells 42 is not greater than 42.5", "42.gt( 42.5 )", BtOne "00")
      , ("tells zero is greater than a negative", "0.gt( -5 )", BtOne "FF")
      , ("tells 5 equals 5", "5.eq( 5 )", BtOne "FF")
      , ("tells 5 is not equal to 6", "5.eq( 6 )", BtOne "00")
      , ("inverts bytes", raw "CA-FE-BE-BE" ++ ".not", BtMany ["35", "01", "41", "41"])
      , ("tells equal bytes are equal", raw "CA-FE" ++ ".eq( " ++ raw "CA-FE" ++ " )", BtOne "FF")
      , ("tells different bytes are not equal", raw "CA-FE" ++ ".eq( " ++ raw "CA-FF" ++ " )", BtOne "00")
      ]

    -- A whole program, not a single operation: every atom on the way is an
    -- external script and the run still lands on the bytes EO's own
    -- 'Fahrenheit' example lands on
    it "dataizes a program whose every operation is an external atom" $
      withNode $ do
        expr <-
          parseExpressionThrows
            ( unlines
                [ "[["
                , "  bytes -> [["
                , "    φ -> ?"
                , "  ]],"
                , "  number -> [["
                , "    φ -> ?,"
                , "    as-bytes -> $.φ,"
                , "    plus -> [[ x -> ?, L> L_number_plus ]],"
                , "    times -> [[ x -> ?, L> L_number_times ]]"
                , "  ]],"
                , "  @ -> $.c.times(1.8).plus(32),"
                , "  c -> 25"
                , "]]"
                ]
            )
        loc <- parseExpressionThrows "Q"
        (value, _) <- dataize expr (withAtoms registry (defaultReduceContext loc))
        value `shouldBe` Dataized (BtMany ["40", "53", "40", "00", "00", "00", "00", "00"])

    -- A name the registry does not carry has no λ function at all: 𝔼 gets
    -- stuck on it, which is the only behaviour phino itself is left with
    it "gets stuck on a λ function the registry does not carry" $ do
      expr <- parseExpressionThrows (primitives "5.nope")
      loc <- parseExpressionThrows "Q"
      dataize expr (withAtoms registry (defaultReduceContext loc))
        `shouldThrow` (\e -> "Atom 'L_number_nope' does not exist" `isInfixOf` show (e :: SomeException))

    -- An operand carrying no number is what an EO number atom answers ⊥ to, and
    -- dataizing ⊥ fails through the terminator path. The judgment is the
    -- script's now, so what these cases prove is that a ⊥ coming back from a
    -- script stops 𝔻 exactly as a built-in ⊥ used to.
    testStuckAtom
      registry
      [ ("cannot add a non-numeric operand", "5.plus( " ++ raw "--" ++ " )")
      , ("cannot multiply by a non-numeric operand", "5.times( " ++ raw "--" ++ " )")
      , ("cannot divide by a non-numeric divisor", "5.div( " ++ raw "--" ++ " )")
      , ("cannot compare against a non-numeric threshold", "5.gt( " ++ raw "--" ++ " )")
      , -- A byte array whose length is not 8 carries no number either (#1072)
        ("cannot add a 5-byte operand", "5.plus( " ++ raw "68-65-6C-6C-6F" ++ " )")
      , ("cannot multiply by a 2-byte operand", "5.times( " ++ raw "20-1F" ++ " )")
      ]
