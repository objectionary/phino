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
import Dataize (DataizeContext (..), Outcome (..), Steps (..), dataize, dataize', emptyState, execBuildTerm, insideUniverse, morph, morph')
import Deps (Evaluation (..), Term (TeExpression), dontSaveEval, dontSaveStep)
import Fixtures (fixtureRegistry, withNode)
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
-- failure instead of staying silently green. The registry of λ functions is
-- empty, since phino implements none of them: a case that needs an atom to
-- answer brings the fixture registry in through 'withAtoms'.
defaultDataizeContext :: Expression -> DataizeContext
defaultDataizeContext loc = DataizeContext loc 25 25 (Steps 250 0) False True False False emptyRegistry buildTerm dontSaveStep dontSaveEval

-- The same context with the fixture λ functions registered (see 'Fixtures').
withAtoms :: Registry -> DataizeContext -> DataizeContext
withAtoms registry ctx = ctx{_atoms = registry}

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
      value `shouldBe` Dataized res

testMorph :: [(String, String, String, String)] -> Spec
testMorph useCases =
  forM_ useCases $ \(name, loc, src, res) ->
    it name $ do
      expr <- parseExpressionThrows src
      loc' <- parseExpressionThrows loc
      expected <- parseExpressionThrows res
      (morphed, _) <- morph expr (defaultDataizeContext loc')
      morphed `shouldBe` expected

-- The same as 'testMorph', with the deep walk on ('_deep') and the fixture λ
-- functions registered, since a case that reduces anything has to fire one: it
-- is pending where 'node' is not installed.
testDeep :: Registry -> [(String, String, String, String)] -> Spec
testDeep registry useCases =
  forM_ useCases $ \(name, loc, src, res) ->
    it name $
      withNode $ do
        expr <- parseExpressionThrows src
        loc' <- parseExpressionThrows loc
        expected <- parseExpressionThrows res
        (morphed, _) <- morph expr (withAtoms registry (defaultDataizeContext loc')){_deep = True}
        morphed `shouldBe` expected

-- The EO objects the fixture λ functions answer for, declared the way
-- 'number.eo' and 'bytes.eo' declare them, so a case below only has to spell
-- the expression under φ. 'number.eq' is the one operation with no atom of its
-- own: EO spells it out of 'L_bytes_eq' (eq.eo), so the fixture composes it the
-- same way. Alongside them stand the objects the atoms hand results to: 'string'
-- carries what a byte-array complaint would say, while 'true' and 'false' fill
-- in for the real bool objects, since the single byte an EO bool dataizes to is
-- all these cases assert. Those bytes are EO's own: 'true.eo' asserts
-- 'true.as-bytes.eq FF-' and 'bool.eo' branches 'if' over 'FF-' and '00-', so a
-- universe copied from here starts with a bool an EO program recognizes.
-- 'number.nope' is declared and left out of the registry on purpose: it is the
-- λ function that cannot fire, the one '--partial' parks on.
primitives :: String -> String
primitives src =
  unlines
    [ "[["
    , "  bytes -> [["
    , "    φ -> ?,"
    , "    not -> [[ L> L_bytes_not ]],"
    , "    eq -> [[ b -> ?, L> L_bytes_eq ]]"
    , "  ]],"
    , "  number -> [["
    , "    as-bytes -> ?,"
    , "    @ -> $.as-bytes,"
    , "    plus -> [[ x -> ?, L> L_number_plus ]],"
    , "    times -> [[ x -> ?, L> L_number_times ]],"
    , "    div -> [[ x -> ?, L> L_number_div ]],"
    , "    gt -> [[ x -> ?, L> L_number_gt ]],"
    , "    eq -> [[ x -> ?, @ -> $.^.as-bytes.eq( x.as-bytes ) ]],"
    , "    nope -> [[ L> L_number_nope ]]"
    , "  ]],"
    , "  string -> [[ as-bytes -> ?, @ -> $.as-bytes ]],"
    , "  true -> [[ @ -> [[ D> FF- ]] ]],"
    , "  false -> [[ @ -> [[ D> 00- ]] ]],"
    , "  @ -> " ++ src
    , "]]"
    ]

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
        (value, _) <- dataize expr (withAtoms registry (defaultDataizeContext loc))
        value `shouldBe` Dataized res

-- Dataize under '--partial', collecting every report 𝔼 makes on the way, in
-- the order it makes them
partially :: Registry -> String -> IO ((Outcome, [Rewritten]), [Evaluation])
partially registry src = do
  expr <- parseExpressionThrows (primitives src)
  reports <- newIORef []
  let ctx =
        (withAtoms registry (defaultDataizeContext ExRoot))
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
        dataize expr (withAtoms registry (defaultDataizeContext loc))
          `shouldThrow` (\e -> "terminator" `isInfixOf` show (e :: SomeException))

spec :: Spec
spec = do
  -- Every λ function a case may fire comes from the fixture registry, read
  -- once here: phino carries none of its own (see 'Fixtures').
  registry <- runIO fixtureRegistry

  -- The top-level 𝕄 entry point, the one the 'morph' command runs: it locates
  -- the subterm, threads the whole input expression as the universe and hands
  -- back the morphed expression together with the chain that led to it (#1114).
  describe "morph" $ do
    testMorph
      [ ("hands the top formation back untouched under the Q locator", "Q", "[[ D> 00- ]]", "[[ D> 00- ]]")
      , -- 𝕄 is total where 𝔻 is not: the 'xi' axiom morphs ξ to ⊥, so the run
        -- ends with an answer rather than with a failure
        ("answers ⊥ where no formation is reachable", "Q.x", "[[ x -> $ ]]", "T")
      ]

    -- The chain runs oldest step first and carries the rule that produced the
    -- step after it, exactly as 'dataize' reports its own, so '--sequence'
    -- prints both the same way
    it "reports the chain of steps oldest first" $ do
      expr <- parseExpressionThrows "[[ D> 00- ]]"
      (morphed, chain) <- morph expr (defaultDataizeContext ExRoot)
      morphed `shouldBe` expr
      map snd chain `shouldBe` [Just "mf", Nothing]
      map fst chain `shouldBe` [expr, expr]

    -- 𝕄 never fires a bare λ-formation, so only an atom sitting under a
    -- dispatch (the 'ml' rule) can get stuck
    describe "a stuck atom under 'ml'" $ do
      let stuck :: IO (Expression, Expression)
          stuck = (,) <$> parseExpressionThrows "[[ x -> [[ L> Sym_arg_0 ]].foo ]]" <*> parseExpressionThrows "Q.x"
      it "fails the run without '_partial'" $ do
        (expr, loc) <- stuck
        morph expr (defaultDataizeContext loc)
          `shouldThrow` (\e -> "Atom 'Sym_arg_0' does not exist" `isInfixOf` show (e :: SomeException))

      it "is parked in the residue under '_partial'" $ do
        (expr, loc) <- stuck
        expected <- parseExpressionThrows "[[ L> Sym_arg_0 ]].foo"
        (residue, _) <- morph expr (defaultDataizeContext loc){_partial = True}
        residue `shouldBe` expected

  -- 𝕄 stops at the first formation 'mf' hands back and leaves its bindings as
  -- they were written, since firing a bare λ is 𝔻's business, so a program
  -- whose parts nothing demands is never reduced (#1124). The deep walk
  -- ('_deep') enters every binding and finishes what 'mf' left, while what no
  -- atom touched keeps the shape it was written in and the answer stays a
  -- program.
  describe "morph with '_deep'" $ do
    testDeep
      registry
      [
        ( "stands the answer of the λ that 'mf' left bare in its place"
        , "Q.@"
        , primitives "[[ x -> 5.plus( 6 ) ]]"
        , "[[ x -> Q.number( as-bytes -> Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-26-00-00-00-00-00-00 ⟧ ) ) ]]"
        )
      ,
        ( "keeps the answer of the last atom fired along one chain of them"
        , "Q.@"
        , primitives "[[ x -> 5.plus( 6 ).plus( 7 ) ]]"
        , "[[ x -> Q.number( as-bytes -> Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-32-00-00-00-00-00-00 ⟧ ) ) ]]"
        )
      ,
        ( "resolves the ξ of a binding against the formation that holds it"
        , "Q.@"
        , primitives "[[ n -> 5, x -> $.n.plus( 6 ) ]]"
        , "[[ n -> 5, x -> Q.number( as-bytes -> Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-26-00-00-00-00-00-00 ⟧ ) ) ]]"
        )
      , -- The registry carries no 'L_number_nope', so there is nothing to fire
        -- and the binding keeps the name it was written under

        ( "leaves the λ the registry does not serve as it was written"
        , "Q.@"
        , primitives "[[ x -> 5.nope ]]"
        , "[[ x -> 5.nope ]]"
        )
      , -- A λ-formation whose bindings are still void is a method waiting to be
        -- applied, not an application waiting to be computed: nothing demands
        -- one, so 𝔻 never meets one, while the walk meets every one the object
        -- model declares. Both the void ρ of 'not' and the void 'b' of 'eq'
        -- keep their atoms unfired here.

        ( "leaves a λ-formation still waiting for its arguments alone"
        , "Q.bytes"
        , primitives "[[ ]]"
        , "[[ φ -> ?, not -> [[ L> L_bytes_not ]], eq -> [[ b -> ?, L> L_bytes_eq ]] ]]"
        )
      , -- Nothing demands the argument of an atom that cannot fire, so 𝔻 never
        -- reaches it; the walk does, and the atom around it stays in place

        ( "walks into the argument of an atom it cannot fire"
        , "Q.@"
        , primitives "[[ x -> [[ y -> ?, L> L_bar ]]( y -> 6.plus( 7 ) ) ]]"
        , "[[ x -> [[ y -> ?, L> L_bar ]]( y -> Q.number( as-bytes -> Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-2A-00-00-00-00-00-00 ⟧ ) ) ) ]]"
        )
      ]

    -- An atom deeper on a binding's spine gets stuck exactly as it does under
    -- 𝕄 alone: the run fails, unless '_partial' parks it, and then the binding
    -- stays as it was written and the walk goes on
    describe "a stuck atom on the spine of a binding" $ do
      let stuck :: IO Expression
          stuck = parseExpressionThrows "[[ x -> [[ L> Sym_arg_0 ]].foo ]]"
      it "fails the run without '_partial'" $ do
        expr <- stuck
        morph expr (defaultDataizeContext ExRoot){_deep = True}
          `shouldThrow` (\e -> "Atom 'Sym_arg_0' does not exist" `isInfixOf` show (e :: SomeException))

      it "leaves the binding as it was written under '_partial'" $ do
        expr <- stuck
        (morphed, _) <- morph expr (defaultDataizeContext ExRoot){_deep = True, _partial = True}
        morphed `shouldBe` expr

  describe "morph'" $
    test'
      morph'
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
  -- code, reachable by calling 'morph'' directly (bypassing normalization) on a
  -- raw meta 𝑛, an AST node the matcher never binds to any concrete pattern.
  describe "morph' fails when no morphing rule matches the term" $
    it "throws instead of looping when handed a bare, unmatched meta" $
      morph' (ExMeta "unbound", (ExRoot, Nothing) :| []) ExRoot emptyState (defaultDataizeContext ExRoot)
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
        ctx = withAtoms registry (defaultDataizeContext ExRoot)
        runEvaluate args = execBuildTerm univ ctx "evaluate" args substEmpty
    forM_
      [
        ( "the first argument is not a formation"
        , [ArgExpression ExRoot, ArgExpression univ]
        , "Function evaluate() expects a formation"
        )
      ,
        ( "the formation has no λ binding at all"
        , [ArgExpression (ExFormation []), ArgExpression univ]
        , "expects a formation with a"
        )
      ,
        ( "a non-λ formation still has other bindings"
        , [ArgExpression (ExFormation [BiVoid AtRho]), ArgExpression univ]
        , "expects a formation with a"
        )
      ,
        ( "not given exactly two expression arguments"
        , [ArgExpression univ]
        , "requires exactly 2 expression arguments"
        )
      ]
      ( \(desc, args, message) ->
          it ("throws when " ++ desc) $
            runEvaluate args `shouldThrow` (\e -> message `isInfixOf` show (e :: SomeException))
      )
    it "evaluates a λ-bearing formation to the atom's normalized result" $
      withNode $ do
        let form = ExFormation [BiLambda (Function "L_bytes_not"), BiTau AtRho (ExFormation [BiDelta (BtOne "00")])]
        result <- runEvaluate [ArgExpression form, ArgExpression univ]
        case result of
          TeExpression expr -> expr `shouldBe` dataBytes (BtOne "FF")
          _ -> expectationFailure "expected TeExpression"

  describe "execBuildTerm 'morph'" $ do
    let univ = ExFormation []
        ctx = defaultDataizeContext ExRoot
    it "throws when not given exactly one expression argument" $
      execBuildTerm univ ctx "morph" [] substEmpty
        `shouldThrow` (\e -> "requires exactly 1 expression argument" `isInfixOf` show (e :: SomeException))
    it "morphs a single expression argument to its already-normal form" $ do
      result <- execBuildTerm univ ctx "morph" [ArgExpression (ExFormation [BiDelta (BtOne "00")])] substEmpty
      case result of
        TeExpression expr -> expr `shouldBe` ExFormation [BiDelta (BtOne "00")]
        _ -> expectationFailure "expected TeExpression"

  -- An expression that is not part of the program — the operand an atom script
  -- asks phino to reduce — is bound to a synthetic attribute of the universe and
  -- that attribute is what 𝔻 is aimed at. This is what the '--inside' option
  -- runs, and what phino did internally while the atoms still lived in the
  -- binary.
  describe "insideUniverse" $ do
    let universe = "[[ y -> [[ D> 02- ]] ]]"
        reduced src = do
          univ <- parseExpressionThrows universe
          target <- parseExpressionThrows src
          (extended, ctx) <- insideUniverse target univ (defaultDataizeContext ExRoot)
          fst <$> dataize extended ctx
    it "reduces an expression the program does not contain" $ do
      value <- reduced "Q.y"
      value `shouldBe` Dataized (BtOne "02")
    -- 𝔻 accepts normal forms only, and a dispatch off a formation is not one:
    -- 'dot' still applies to it. So the expression is normalized first, which
    -- is the whole reason an atom script cannot simply splice it into the
    -- universe itself.
    it "normalizes what it is handed before 𝔻 sees it" $ do
      value <- reduced "[[ x -> [[ D> 01- ]] ]].x"
      value `shouldBe` Dataized (BtOne "01")
    it "refuses a universe which is not a formation" $ do
      target <- parseExpressionThrows "Q.y"
      insideUniverse target ExRoot (defaultDataizeContext ExRoot)
        `shouldThrow` (\e -> "not a formation" `isInfixOf` show (e :: SomeException))

  -- 'defaultDataizeContext' runs with '_shuffle' on, so 'morph'' walks the
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
        results <- replicateM 100 (fst . fst <$> morph' (input, (univ, Nothing) :| []) univ emptyState (defaultDataizeContext ExRoot))
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
      morph' (chain, (ExRoot, Nothing) :| []) ExRoot emptyState (defaultDataizeContext ExRoot)
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
  -- λ-atom keeps re-firing on a term that never reduces to bytes, sent 'morph''
  -- through md → ma → universe → mf → mphi → ml forever and no CLI option could
  -- stop it (#1052). '--max-steps' bounds that recursion and fails once the
  -- budget is gone.
  describe "stops a dataization that never reaches bytes" $
    it "fails on the step limit instead of morphing forever" $
      withNode $ do
        expr <- parseExpressionThrows "⟦ @ ↦ ⟦ λ ⤍ L_number_div, ρ ↦ ⟦ Δ ⤍ 40-45-00-00-00-00-00-00 ⟧, x ↦ ⟦ Δ ⤍ 40-00-00-00-00-00-00-00 ⟧ ⟧ ⟧"
        dataize expr (DataizeContext ExRoot 25 25 (Steps 40 0) False True False False registry buildTerm dontSaveStep dontSaveEval)
          `shouldThrow` (\e -> "--max-steps=40" `isInfixOf` show (e :: SomeException))

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
        dataize expr (withAtoms registry (defaultDataizeContext ExRoot))
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
        dataize expr ((withAtoms registry (defaultDataizeContext ExRoot)){_partial = True})
          `shouldThrow` (\e -> "terminator" `isInfixOf` show (e :: SomeException))

  describe "DataizeContext's --max-depth/--max-cycles reach into the normalization it splices in" $ do
    let boxed = "[[ @ -> [[ D> 00- ]] ]]"
    forM_
      [
        ( "--max-cycles"
        , DataizeContext ExRoot 25 0 (Steps 250 0) True True False False emptyRegistry buildTerm dontSaveStep dontSaveEval
        , "--max-cycles=0"
        )
      ,
        ( "--max-depth"
        , DataizeContext ExRoot 0 25 (Steps 250 0) True True False False emptyRegistry buildTerm dontSaveStep dontSaveEval
        , "--max-depth=0"
        )
      ]
      ( \(flag, ctx, message) ->
          it ("throws once " ++ flag ++ " is exhausted with --depth-sensitive") $ do
            expr <- parseExpressionThrows boxed
            dataize expr ctx `shouldThrow` (\e -> message `isInfixOf` show (e :: SomeException))
      )
    forM_
      [ ("--max-cycles", DataizeContext ExRoot 25 0 (Steps 250 0) False True False False emptyRegistry buildTerm dontSaveStep dontSaveEval)
      , ("--max-depth", DataizeContext ExRoot 0 25 (Steps 250 0) False True False False emptyRegistry buildTerm dontSaveStep dontSaveEval)
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
        (_, chain) <- dataize expr (withAtoms registry (defaultDataizeContext loc))
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
          (_, chain) <- dataize expr (withAtoms registry (defaultDataizeContext loc'))
          pure [label | (_, Just label) <- chain]
    it "dataizes 5.plus(6) through the expected rules" $
      withNode $ do
        labels <-
          labelsOf
            "Q"
            "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(as-bytes) -> [[ @ -> $.as-bytes, plus(x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6) ]]"
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
          , "  number(as-bytes) -> [[ @ -> as-bytes ]],"
          , "  bytes ↦ ⟦ φ ↦ ∅ ⟧,"
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
                , "    as-bytes -> ?,"
                , "    @ -> $.as-bytes,"
                , "    plus -> [[ x -> ?, L> L_number_plus ]],"
                , "    times -> [[ x -> ?, L> L_number_times ]]"
                , "  ]],"
                , "  @ -> $.c.times(1.8).plus(32),"
                , "  c -> 25"
                , "]]"
                ]
            )
        loc <- parseExpressionThrows "Q"
        (value, _) <- dataize expr (withAtoms registry (defaultDataizeContext loc))
        value `shouldBe` Dataized (BtMany ["40", "53", "40", "00", "00", "00", "00", "00"])

    -- A name the registry does not carry has no λ function at all: 𝔼 gets
    -- stuck on it, which is the only behaviour phino itself is left with
    it "gets stuck on a λ function the registry does not carry" $ do
      expr <- parseExpressionThrows (primitives "5.nope")
      loc <- parseExpressionThrows "Q"
      dataize expr (withAtoms registry (defaultDataizeContext loc))
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
