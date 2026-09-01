{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module DataizeSpec (spec) where

import AST
import Control.Exception (SomeException)
import Control.Monad
import Data.List (find, isInfixOf, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Dataize (DataizeContext (..), Steps (..), dataize, dataize', emptyState, execBuildTerm, morph)
import Deps (dontSaveStep)
import Functions (buildTerm)
import Matcher (substEmpty)
import Parser (parseExpressionThrows)
import Rewriter (Rewritten)
import Rule (RuleContext (RuleContext), matchExpressionWithRule')
import Test.Hspec
import Yaml (ExtraArgument (..))
import Yaml qualified

-- Shuffle is enabled so the suite exercises the order-independence of the
-- dataization rules (#909): a hidden overlap surfaces as a nondeterministic
-- failure instead of staying silently green.
defaultDataizeContext :: Expression -> DataizeContext
defaultDataizeContext loc = DataizeContext loc 25 25 (Steps 250 0) False True buildTerm dontSaveStep

test :: (Eq a, Show a) => ((Expression, NonEmpty Rewritten) -> Expression -> String -> DataizeContext -> IO ((a, [Rewritten]), String)) -> [(String, Expression, Expression, a)] -> Spec
test func useCases =
  forM_ useCases $ \(desc, input, expr, output) ->
    it desc $ do
      ((res, _), _) <- func (input, (expr, Nothing) :| []) expr emptyState (defaultDataizeContext ExRoot)
      res `shouldBe` output

test' :: (Eq a, Show a) => ((Expression, NonEmpty Rewritten) -> Expression -> String -> DataizeContext -> IO ((a, NonEmpty Rewritten), String)) -> [(String, Expression, Expression, a)] -> Spec
test' func useCases =
  forM_ useCases $ \(desc, input, expr, output) ->
    it desc $ do
      ((res, _), _) <- func (input, (expr, Nothing) :| []) expr emptyState (defaultDataizeContext ExRoot)
      res `shouldBe` output

testDataize :: [(String, String, String, Bytes)] -> Spec
testDataize useCases =
  forM_ useCases $ \(name, loc, src, res) ->
    it name $ do
      expr <- parseExpressionThrows src
      loc' <- parseExpressionThrows loc
      (value, _) <- dataize expr (defaultDataizeContext loc')
      value `shouldBe` res

-- The 12 primitive λ-atoms every EO data operation reduces to, declared the way
-- 'number.eo' and 'bytes.eo' declare them, so a case below only has to spell the
-- expression under φ. Alongside them stand the objects the atoms hand results
-- to: 'string' carries the 'cant-slice' complaint, while 'true' and 'false' fill
-- in for the real bool objects, since the single byte an EO bool dataizes to is
-- all these cases assert.
primitives :: String -> String
primitives src =
  unlines
    [ "[["
    , "  bytes -> [["
    , "    data -> ?,"
    , "    @ -> $.data,"
    , "    and -> [[ b -> ?, L> L_bytes_and ]],"
    , "    or -> [[ b -> ?, L> L_bytes_or ]],"
    , "    not -> [[ L> L_bytes_not ]],"
    , "    concat -> [[ b -> ?, L> L_bytes_concat ]],"
    , "    eq -> [[ b -> ?, L> L_bytes_eq ]],"
    , "    size -> [[ L> L_bytes_size ]],"
    , "    right -> [[ x -> ?, L> L_bytes_right ]],"
    , "    slice -> [[ start -> ?, len -> ?, cant-slice -> ?, L> L_bytes_slice ]]"
    , "  ]],"
    , "  number -> [["
    , "    as-bytes -> ?,"
    , "    @ -> $.as-bytes,"
    , "    plus -> [[ x -> ?, L> L_number_plus ]],"
    , "    times -> [[ x -> ?, L> L_number_times ]],"
    , "    div -> [[ x -> ?, L> L_number_div ]],"
    , "    gt -> [[ x -> ?, L> L_number_gt ]],"
    , "    eq -> [[ x -> ?, y -> ?, L> L_number_eq ]]"
    , "  ]],"
    , "  string -> [[ as-bytes -> ?, @ -> $.as-bytes ]],"
    , "  true -> [[ @ -> [[ D> 01- ]] ]],"
    , "  false -> [[ @ -> [[ D> 00- ]] ]],"
    , "  @ -> " ++ src
    , "]]"
    ]

-- Wrap a hex literal into the bytes object that EO source spells as a bare '20-1F'
raw :: String -> String
raw bts = "Q.bytes( data -> [[ D> " ++ bts ++ " ]] )"

testAtom :: [(String, String, Bytes)] -> Spec
testAtom useCases =
  forM_ useCases $ \(name, src, res) ->
    it name $ do
      expr <- parseExpressionThrows (primitives src)
      loc <- parseExpressionThrows "Q"
      (value, _) <- dataize expr (defaultDataizeContext loc)
      value `shouldBe` res

-- An atom with no answer yields ⊥, which stops the whole dataization
testStuckAtom :: [(String, String)] -> Spec
testStuckAtom useCases =
  forM_ useCases $ \(name, src) ->
    it name $ do
      expr <- parseExpressionThrows (primitives src)
      loc <- parseExpressionThrows "Q"
      dataize expr (defaultDataizeContext loc)
        `shouldThrow` (\e -> "terminator" `isInfixOf` show (e :: SomeException))

spec :: Spec
spec = do
  describe "morph" $
    test'
      morph
      [ ("[[ D> 00- ]] => [[ D> 00- ]]", ExFormation [BiDelta (BtOne "00")], ExRoot, ExFormation [BiDelta (BtOne "00")])
      , ("T => T", ExTermination, ExRoot, ExTermination)
      , ("$ => X", ExXi, ExRoot, ExTermination)
      , ("Q => X", ExRoot, ExRoot, ExTermination)
      ,
        ( "Q.x (Q -> [[ x -> [[]] ]]) => [[ ρ -> Q ]]"
        , ExDispatch ExRoot (AtLabel "x")
        , ExFormation [BiTau (AtLabel "x") (ExFormation [])]
        , ExFormation [BiTau AtRho (ExFormation [BiTau (AtLabel "x") (ExFormation [BiVoid AtRho]), BiVoid AtRho])]
        )
      , -- A void slot fed a non-absolute argument can never be filled, so 'copy'
        -- cannot fire and the application is a stuck normal form. Before #959,
        -- 'ma' re-morphed this identical term forever; now the 'mad' axiom
        -- morphs it straight to ⊥, keeping 𝕄 total.

        ( "[[ x -> ? ]](x -> $.foo) => T"
        , ExApplication (ExFormation [BiVoid (AtLabel "x")]) (ArTau (AtLabel "x") (ExDispatch ExXi (AtLabel "foo")))
        , ExRoot
        , ExTermination
        )
      , -- Same as above but through the alpha-argument sibling 'maad' instead of
        -- 'mad': a void slot fed a non-absolute alpha-indexed argument also
        -- morphs straight to ⊥.

        ( "[[ ^ -> ? ]](α0 -> $.foo) => T"
        , ExApplication (ExFormation [BiVoid AtRho]) (ArAlpha (Alpha 0) (ExDispatch ExXi (AtLabel "foo")))
        , ExRoot
        , ExTermination
        )
      , -- 'universe' fires only when the universe 'e' differs from Φ itself
        -- ('not (eq(e, Φ))'); it then normalizes and re-morphs that universe.
        -- Here the universe is a plain formation, already a normal form, so
        -- re-morphing it lands straight on 'mf' and returns it unchanged.

        ( "Q => [[]] (a universe distinct from Φ) => [[]]"
        , ExRoot
        , ExFormation []
        , ExFormation []
        )
      ]

  -- 𝕄's first argument is always a normal form reachable through normalization,
  -- and every such normal form is covered by some morphing clause (an axiom
  -- like 'mf'/'dead'/'xi'/'universe'/'mg' or a recursive rule), so the "no rule
  -- matched" fallback never fires along any real derivation. It is still total
  -- code, reachable by calling 'morph' directly (bypassing normalization) on a
  -- raw meta 𝑛, an AST node the matcher never binds to any concrete pattern.
  describe "morph fails when no morphing rule matches the term" $
    it "throws instead of looping when handed a bare, unmatched meta" $
      morph (ExMeta "unbound", (ExRoot, Nothing) :| []) ExRoot emptyState (defaultDataizeContext ExRoot)
        `shouldThrow` (\e -> "no morphing rule matched" `isInfixOf` show (e :: SomeException))

  -- Symmetric to the morphing fallback above: every normal form 𝔻 actually
  -- receives is covered by 'delta'/'box'/'fire'/'none' (formations) or 'norm'
  -- (everything else, disjoint from ⊥ and formations), so this fallback is
  -- unreachable through the public 'dataize'/'dataize'' entry points on any
  -- term produced by normalization. A raw meta again reaches it directly,
  -- proving the fallback itself is live code, not dead weight.
  describe "dataize' fails when no dataization rule matches the term" $
    it "throws instead of treating the unmatched meta as ⊥" $
      dataize' (ExMeta "unbound", (ExRoot, Nothing) :| []) ExRoot emptyState (defaultDataizeContext ExRoot)
        `shouldThrow` (\e -> "no dataization rule matched" `isInfixOf` show (e :: SomeException))

  -- 'execBuildTerm's "evaluate" and "morph" cases expose 𝔼 and 𝕄 to the
  -- matcher's condition path (guards in 'when'/'having'). No built-in rule's
  -- guard actually calls either function, so these error paths — reachable only
  -- by malformed arguments — are exercised here directly through the exported
  -- 'execBuildTerm', the same way the matcher would call it.
  describe "execBuildTerm 'evaluate'" $ do
    let univ = ExFormation []
        ctx = defaultDataizeContext ExRoot
        runEvaluate args = execBuildTerm univ ctx "evaluate" args substEmpty
    it "throws when the first argument is not a formation" $
      runEvaluate [ArgExpression ExRoot, ArgExpression univ]
        `shouldThrow` (\e -> "Function evaluate() expects a formation" `isInfixOf` show (e :: SomeException))
    it "throws when the formation has no λ binding at all" $
      runEvaluate [ArgExpression (ExFormation []), ArgExpression univ]
        `shouldThrow` (\e -> "expects a formation with a" `isInfixOf` show (e :: SomeException))
    it "throws when a non-λ formation still has other bindings" $
      runEvaluate [ArgExpression (ExFormation [BiVoid AtRho]), ArgExpression univ]
        `shouldThrow` (\e -> "expects a formation with a" `isInfixOf` show (e :: SomeException))
    it "throws when not given exactly two expression arguments" $
      runEvaluate [ArgExpression univ]
        `shouldThrow` (\e -> "requires exactly 2 expression arguments" `isInfixOf` show (e :: SomeException))
    it "evaluates a λ-bearing formation given well-formed arguments" $ do
      let form = ExFormation [BiLambda (Function "L_bytes_not"), BiTau AtRho (ExFormation [BiDelta (BtOne "00")])]
      _ <- runEvaluate [ArgExpression form, ArgExpression univ]
      pure ()

  describe "execBuildTerm 'morph'" $ do
    let univ = ExFormation []
        ctx = defaultDataizeContext ExRoot
    it "throws when not given exactly one expression argument" $
      execBuildTerm univ ctx "morph" [] substEmpty
        `shouldThrow` (\e -> "requires exactly 1 expression argument" `isInfixOf` show (e :: SomeException))
    it "morphs a single expression argument given well-formed arguments" $ do
      _ <- execBuildTerm univ ctx "morph" [ArgExpression (ExFormation [BiDelta (BtOne "00")])] substEmpty
      pure ()

  -- Every atom's operand is fetched through the synthetic '_dataize', which
  -- rebuilds the universe as a formation to bind the operand into before
  -- reducing it. A universe that is not itself a formation can never arise
  -- from the public 'dataize' entry point (its own universe argument doubles
  -- as the located root of a real program, always a formation), but 'dataize''
  -- lets a test drive an atom-bearing term against one directly, proving the
  -- guard fires instead of the atom looping or crashing some other way.
  describe "atoms refuse to run under a non-formation universe" $
    it "fails fast instead of dispatching against a non-formation universe" $ do
      let form = ExFormation [BiLambda (Function "L_bytes_not"), BiVoid AtRho]
      dataize' (form, (ExRoot, Nothing) :| []) ExRoot emptyState (defaultDataizeContext ExRoot)
        `shouldThrow` (\e -> "non-formation universe" `isInfixOf` show (e :: SomeException))

  -- 'DataizeContext' and 'Steps' thread their fields through 'RecordWildCards'
  -- everywhere else in the module, so their derived accessor functions are
  -- otherwise never called by name. A direct call here is the cheapest way to
  -- exercise those declarations without contorting the real control flow.
  describe "DataizeContext and Steps expose their fields" $
    it "reads back every field through its derived accessor" $ do
      let steps = Steps 250 3
          ctx = DataizeContext ExRoot 25 25 steps False True buildTerm dontSaveStep
      (_limit steps, _spent steps) `shouldBe` (250, 3)
      (_limit (_steps ctx), _spent (_steps ctx)) `shouldBe` (250, 3)
      ( _locator ctx
        , _maxDepth ctx
        , _maxCycles ctx
        , _depthSensitive ctx
        , _shuffle ctx
        )
        `shouldBe` (ExRoot, 25, 25, False, True)
      _saveStep ctx ExRoot
      _ <- _buildTerm ctx "random-tau" [] substEmpty
      pure ()

  -- 'defaultDataizeContext' runs with '_shuffle' on, so 'morph' walks the
  -- morphing rules in a random order on every step. Every clause is
  -- order-independent (the known overlaps were removed in #856 and #860), so the
  -- outcome must never depend on that order: morphing each input many times under
  -- a shuffling context yields exactly the formation the fixed declaration order
  -- does, proving the rules may be applied in any order with the same result.
  -- Were a hidden overlap re-introduced, some of these random orders would
  -- disagree and 'nub' would collect more than the single expected form.
  describe "morphing is order-independent under --shuffle" $ do
    let cases =
          [ ("a byte formation", ExFormation [BiDelta (BtOne "00")], ExRoot, ExFormation [BiDelta (BtOne "00")])
          , ("termination", ExTermination, ExRoot, ExTermination)
          , ("xi", ExXi, ExRoot, ExTermination)
          , ("the global object", ExRoot, ExRoot, ExTermination)
          ,
            ( "a dispatch over a formation"
            , ExDispatch ExRoot (AtLabel "x")
            , ExFormation [BiTau (AtLabel "x") (ExFormation [])]
            , ExFormation [BiTau AtRho (ExFormation [BiTau (AtLabel "x") (ExFormation [BiVoid AtRho]), BiVoid AtRho])]
            )
          ]
    forM_ cases $ \(desc, input, univ, expected) ->
      it ("morphs " ++ desc ++ " to the same form across 100 random rule orders") $ do
        results <- replicateM 100 (fst . fst <$> morph (input, (univ, Nothing) :| []) univ emptyState (defaultDataizeContext ExRoot))
        nub results `shouldBe` [expected]

  -- 'md' fires only when its head is not a formation ('not (formation 𝑛)'),
  -- so a formation head — λ-bearing or not — is left to 'ml'/'mf'. The
  -- two clauses are mutually exclusive and their order in 'morphing.yaml'
  -- cannot change behavior.
  describe "morphing 'md' is disjoint from 'ml'" $ do
    let rctx = RuleContext (execBuildTerm ExRoot (defaultDataizeContext ExRoot))
        morphRule :: String -> Yaml.MorphRule
        morphRule nm = fromMaybe (error ("no morphing rule named " ++ nm)) (find (\r -> r.name == nm) Yaml.morphingRules)
        asRule :: Yaml.MorphRule -> Yaml.Rule
        asRule r = Yaml.Rule r.name Nothing Nothing r.match ExRoot r.when Nothing Nothing
        lambdaFormation = ExFormation [BiLambda (Function "L_dummy"), BiVoid AtRho]
    it "does not fire on a λ-bearing formation dispatch" $ do
      substs <- matchExpressionWithRule' [substEmpty] (ExDispatch lambdaFormation (AtLabel "x")) (asRule (morphRule "md")) rctx
      substs `shouldBe` []
    it "still fires on a non-λ-formation dispatch" $ do
      substs <- matchExpressionWithRule' [substEmpty] (ExDispatch ExXi (AtLabel "x")) (asRule (morphRule "md")) rctx
      null substs `shouldBe` False
    -- ⟦λ ⤍ F⟧.a.b.c : 'md' peels .c then .b (their heads are dispatches,
    -- not λ-formations, so 'λ ∉ 𝐵' holds), then 'ml' handles the base
    -- ⟦λ ⤍ F⟧.a and fires the atom. The chain therefore routes
    -- md → md → ml; firing the undefined atom 'F' is what
    -- raises the error, proving the base λ-formation reached 'ml'.
    it "drills a chained λ-formation dispatch down to the base 'ml'" $ do
      let base = ExFormation [BiLambda (Function "F")]
          chain = ExDispatch (ExDispatch (ExDispatch base (AtLabel "a")) (AtLabel "b")) (AtLabel "c")
      morph (chain, (ExRoot, Nothing) :| []) ExRoot emptyState (defaultDataizeContext ExRoot)
        `shouldThrow` (\e -> "Atom 'F' does not exist" `isInfixOf` show (e :: SomeException))

  -- 'norm' matches the bare meta 𝑛, which unifies with any expression, so it is
  -- guarded to fire only when 𝑛 is neither a formation ('not (formation 𝑛)',
  -- left to 'delta'/'box'/'fire'/'none') nor the termination ⊥ ('not (𝑛 = ⊥)').
  -- 𝔻 is partial: ⊥ matches no clause and lands on the unmatched-term error
  -- (#955). The dataization clauses are therefore disjoint and their order in
  -- 'dataization.yaml' cannot change behavior.
  describe "dataization 'norm' is disjoint from the specific clauses" $ do
    let rctx = RuleContext (execBuildTerm ExRoot (defaultDataizeContext ExRoot))
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
            dataize' (input, (ExRoot, Nothing) :| []) ExRoot emptyState (defaultDataizeContext ExRoot)
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
  -- λ-atom keeps re-firing on a term that never reduces to bytes, sent 'morph'
  -- through md → ma → universe → mf → mphi → ml forever and no CLI option could
  -- stop it (#1052). '--max-steps' bounds that recursion and fails once the
  -- budget is gone.
  describe "stops a dataization that never reaches bytes" $
    it "fails on the step limit instead of morphing forever" $ do
      expr <- parseExpressionThrows "⟦ @ ↦ ⟦ λ ⤍ L_number_div, ρ ↦ ⟦ Δ ⤍ 40-45-00-00-00-00-00-00 ⟧, x ↦ ⟦ Δ ⤍ 40-00-00-00-00-00-00-00 ⟧ ⟧ ⟧"
      dataize expr (DataizeContext ExRoot 25 25 (Steps 40 0) False True buildTerm dontSaveStep)
        `shouldThrow` (\e -> "--max-steps=40" `isInfixOf` show (e :: SomeException))

  -- '_maxDepth'/'_maxCycles' bound the normalization rewriter that a 'box' or
  -- 'norm' dataization step splices in (see 'normalized'); with
  -- '_depthSensitive' on, exhausting either one propagates the very same
  -- exception the rewriter itself throws, and with it off the limit is
  -- absorbed silently, so dataization still reaches an answer.
  describe "DataizeContext's --max-depth/--max-cycles reach into the normalization it splices in" $ do
    let boxed = "[[ @ -> [[ D> 00- ]] ]]"
    it "throws once --max-cycles is exhausted with --depth-sensitive" $ do
      expr <- parseExpressionThrows boxed
      dataize expr (DataizeContext ExRoot 25 0 (Steps 250 0) True True buildTerm dontSaveStep)
        `shouldThrow` (\e -> "--max-cycles=0" `isInfixOf` show (e :: SomeException))
    it "throws once --max-depth is exhausted with --depth-sensitive" $ do
      expr <- parseExpressionThrows boxed
      dataize expr (DataizeContext ExRoot 0 25 (Steps 250 0) True True buildTerm dontSaveStep)
        `shouldThrow` (\e -> "--max-depth=0" `isInfixOf` show (e :: SomeException))
    it "does not throw without --depth-sensitive even once --max-cycles is exhausted" $ do
      expr <- parseExpressionThrows boxed
      (value, _) <- dataize expr (DataizeContext ExRoot 25 0 (Steps 250 0) False True buildTerm dontSaveStep)
      value `shouldBe` BtOne "00"
    it "does not throw without --depth-sensitive even once --max-depth is exhausted" $ do
      expr <- parseExpressionThrows boxed
      (value, _) <- dataize expr (DataizeContext ExRoot 0 25 (Steps 250 0) False True buildTerm dontSaveStep)
      value `shouldBe` BtOne "00"

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
      expr <-
        parseExpressionThrows
          ( unlines
              [ "[["
              , "  bytes(data) -> [[ @ -> $.data ]],"
              , "  number(as-bytes) -> [[ @ -> $.as-bytes, plus(x) -> [[ L> L_number_plus ]] ]],"
              , "  @ -> 5.plus(6)"
              , "]]"
              ]
          )
      loc <- parseExpressionThrows "Q"
      (_, chain) <- dataize expr (defaultDataizeContext loc)
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
          (_, chain) <- dataize expr (defaultDataizeContext loc')
          pure [label | (_, Just label) <- chain]
    it "dataizes 5.plus(6) through the expected rules" $ do
      labels <-
        labelsOf
          "Q"
          "[[ bytes(data) -> [[ @ -> $.data ]], number(as-bytes) -> [[ @ -> $.as-bytes, plus(x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6) ]]"
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
                   , "dot"
                   , "ma"
                   , "stay"
                   , "mf"
                   , "contextualize"
                   , "dot"
                   , "copy"
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
      dataize expr (defaultDataizeContext loc)
        `shouldThrow` (\e -> "terminator" `isInfixOf` show (e :: SomeException))

  testDataize
    [
      ( "5.plus(6)"
      , "Q"
      , unlines
          [ "[["
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
          [ "[["
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
          [ "[["
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
          , "  number(as-bytes) -> [[ @ -> as-bytes ]],"
          , "  bytes(data) -> [[ @ -> data ]],"
          , "  x -> 5"
          , "]]"
          ]
      , BtMany ["40", "14", "00", "00", "00", "00", "00", "00"]
      )
    , -- Dispatching an absent attribute on a φ-decorated formation now resolves
      -- the inherited attribute through morphing 'mphi' (#973): PHI used to be a
      -- normalization rule, but following the decoration is a semantic 𝕄 step,
      -- so it moved into 'morphing.yaml'. Here '.t' is missing from the outer
      -- formation, so 𝕄 walks the '@' decoration to the parent that defines 't'
      -- and dataizes its datum.

      ( "InheritedThroughPhi"
      , "Q"
      , "[[ @ -> [[ t -> [[ D> 2A- ]] ]] ]].t"
      , BtOne "2A"
      )
    ]

  describe "atoms" $ do
    testAtom
      [ ("divides a positive dividend", "256.div( 16 )", BtMany ["40", "30", "00", "00", "00", "00", "00", "00"])
      , ("divides by zero into infinity", "2.div( 0 )", BtMany ["7F", "F0", "00", "00", "00", "00", "00", "00"])
      , ("tells 1000 is greater than 200", "1000.gt( 200 )", BtOne "01")
      , ("tells 42 is not greater than 42.5", "42.gt( 42.5 )", BtOne "00")
      , ("tells zero is greater than a negative", "0.gt( -5 )", BtOne "01")
      ,
        ( "conjoins two long bytes"
        , raw "02-EF-D4-05-5E-78-3A" ++ ".and( " ++ raw "12-33-C1-B5-5E-71-55" ++ " )"
        , BtMany ["02", "23", "C0", "05", "5E", "70", "10"]
        )
      ,
        ( "disjoins negative bytes with one"
        , raw "FF-FF-FF-FF-00-00-00-00" ++ ".or( " ++ raw "00-00-00-00-00-00-00-01" ++ " )"
        , BtMany ["FF", "FF", "FF", "FF", "00", "00", "00", "01"]
        )
      , ("inverts bytes", raw "CA-FE-BE-BE" ++ ".not", BtMany ["35", "01", "41", "41"])
      ,
        ( "concats two long bytes"
        , raw "02-EF-D4-05-5E-78-3A" ++ ".concat( " ++ raw "12-33-C1-B5-5E-71-55" ++ " )"
        , BtMany ["02", "EF", "D4", "05", "5E", "78", "3A", "12", "33", "C1", "B5", "5E", "71", "55"]
        )
      ,
        ( "concats bytes with empty ones"
        , raw "05-5E-78" ++ ".concat( " ++ raw "--" ++ " )"
        , BtMany ["05", "5E", "78"]
        )
      , ("counts the size of bytes", raw "F1-20-5F-EC-B5-90-32" ++ ".size", BtMany ["40", "1C", "00", "00", "00", "00", "00", "00"])
      , ("tells equal bytes are equal", raw "CA-FE" ++ ".eq( " ++ raw "CA-FE" ++ " )", BtOne "01")
      , ("tells different bytes are not equal", raw "CA-FE" ++ ".eq( " ++ raw "CA-FF" ++ " )", BtOne "00")
      , ("takes a part of bytes", raw "20-1F-EE-B5-90" ++ ".slice( 1, 3 )", BtMany ["1F", "EE", "B5"])
      ,
        ( "shifts right an even negative"
        , raw "C0-43-00-00-00-00-00-00" ++ ".right( 1 )"
        , BtMany ["60", "21", "80", "00", "00", "00", "00", "00"]
        )
      ,
        ( "shifts right minus one"
        , raw "BF-F0-00-00-00-00-00-00" ++ ".right( 4 )"
        , BtMany ["0B", "FF", "00", "00", "00", "00", "00", "00"]
        )
      ,
        ( "shifts right by the integer minimum"
        , raw "BF-F0-00-00-00-00-00-00" ++ ".right( -2147483648 )"
        , BtMany ["00", "00", "00", "00", "00", "00", "00", "00"]
        )
      ,
        ( "recovers from an out-of-bounds slice"
        , raw "20-1F-EE-B5-90" ++ ".slice( 3, 10, [[ message -> ?, @ -> \"recovered\" ]] )"
        , BtMany ["72", "65", "63", "6F", "76", "65", "72", "65", "64"]
        )
      ,
        ( "recovers from a slice whose start plus length overflows"
        , raw "20-1F-EE-B5-90" ++ ".slice( 2000000000, 2000000000, [[ message -> ?, @ -> \"recovered\" ]] )"
        , BtMany ["72", "65", "63", "6F", "76", "65", "72", "65", "64"]
        )
      ]
    testStuckAtom
      [ ("cannot conjoin bytes of different lengths", raw "20-1F" ++ ".and( " ++ raw "CA-FE-BE" ++ " )")
      , ("cannot disjoin bytes of different lengths", raw "20-1F" ++ ".or( " ++ raw "CA-FE-BE" ++ " )")
      , ("cannot slice from an offset beyond the int range", raw "20-1F-EE-B5-90" ++ ".slice( 3000000000, 1 )")
      , ("cannot slice a negative length", raw "20-1F-EE-B5-90" ++ ".slice( 1, -1 )")
      , -- A number atom rejects an operand that carries no number (empty bytes),
        -- yielding ⊥ rather than a result; dataizing ⊥ then fails through the
        -- terminator path, exactly like the bytes-atom cases above.
        ("cannot add a non-numeric operand", "5.plus( " ++ raw "--" ++ " )")
      , ("cannot multiply by a non-numeric operand", "5.times( " ++ raw "--" ++ " )")
      , ("cannot divide by a non-numeric divisor", "5.div( " ++ raw "--" ++ " )")
      , ("cannot compare against a non-numeric threshold", "5.gt( " ++ raw "--" ++ " )")
      , ("cannot test equality against a non-numeric operand", "5.eq( " ++ raw "--" ++ ", 6 )")
      , -- 'right' rejects a shift distance that is not a plain 8-byte integer;
        -- empty bytes carry no such integer, so the shift atom is stuck too.
        ("cannot shift right by a non-integer distance", raw "C0-43-00-00-00-00-00-00" ++ ".right( " ++ raw "--" ++ " )")
      ]
