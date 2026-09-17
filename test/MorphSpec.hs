{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module MorphSpec (spec) where

import AST
import Control.Exception (SomeException)
import Control.Monad
import Data.Aeson (FromJSON)
import Data.List (find, isInfixOf, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Yaml qualified as Decode
import Dataize (Outcome (..), dataize)
import Deps (State, Term (TeExpression))
import Files (allPathsIn)
import Fixtures (defaultReduceContext, fixtureLambdas, primitives, withLambdas, withLambdasOf)
import GHC.Generics (Generic)
import Lambdas (Lambdas, emptyLambdas, readLambdas)
import Matcher (substEmpty)
import Morph (ReduceContext (..), emptyState, execBuildTerm, insideUniverse, morph, morph')
import Parser (parseExpressionThrows)
import Rewriter (Rewritten)
import Rule (RuleContext (RuleContext), matchExpressionWithRule')
import System.FilePath (makeRelative)
import Tau (seedTaus)
import Test.Hspec
import Yaml (ExtraArgument (..))
import Yaml qualified

test' :: (Eq a, Show a) => ((Expression, NonEmpty Rewritten) -> Expression -> State -> ReduceContext -> IO ((a, NonEmpty Rewritten), State)) -> [(String, Expression, Expression, a)] -> Spec
test' func useCases =
  forM_ useCases $ \(desc, input, expr, output) ->
    it desc $ do
      ((res, _), _) <- func (input, (expr, Nothing) :| []) expr emptyState (defaultReduceContext ExRoot)
      res `shouldBe` output

-- One case of 𝕄, as a pack of 'test-resources/morph-packs' — or, for the deep
-- walk, of 'test-resources/morph-deep-packs' — spells it: the program under
-- 'input', wrapped in the fixture object model where 'model' says so and run
-- against the fixture λ functions where 'symbolic' does, entered at 'location'
-- and answering either the program under 'result' or the failure under 'fails'.
data MorphPack = MorphPack
  { location :: Maybe String
  , input :: String
  , model :: Maybe Bool
  , symbolic :: Maybe Bool
  , partial :: Maybe Bool
  , result :: Maybe String
  , fails :: Maybe String
  }
  deriving (Generic, Show, FromJSON)

-- Morph one such pack and check what it answers, walking every binding where
-- 'deep' says so, since that is what tells the two pack directories apart.
testMorph :: Lambdas -> Bool -> FilePath -> Expectation
testMorph known deep pth = do
  MorphPack{..} <- Decode.decodeFileThrow pth
  expr <- parseExpressionThrows (if model == Just True then primitives input else input)
  loc <- parseExpressionThrows (fromMaybe "Q" location)
  -- Reseeded from the program before the run, the way the command does it (see
  -- 'testSymbols' in 'EvaluateSpec').
  seedTaus expr
  let ctx =
        (defaultReduceContext loc)
          { _deep = deep
          , _partial = partial == Just True
          , _symbolic = if symbolic == Just True then known else emptyLambdas
          }
  case (result, fails) of
    (Just res, Nothing) -> do
      expected <- parseExpressionThrows res
      (morphed, _, _) <- morph expr emptyState ctx
      morphed `shouldBe` expected
    (Nothing, Just message) ->
      morph expr emptyState ctx `shouldThrow` (\err -> message `isInfixOf` show (err :: SomeException))
    _ -> expectationFailure "The pack holds neither a single 'result' nor a single 'fails'"

spec :: Spec
spec = do
  -- Every λ function a case may fire comes from the fixture file, read once
  -- here: phino carries none of its own (see 'Fixtures').
  known <- runIO fixtureLambdas

  -- The top-level 𝕄 entry point, the one the 'morph' command runs: it locates
  -- the subterm, threads the whole input expression as the universe and hands
  -- back the morphed expression together with the chain that led to it (#1114).
  describe "morph" $ do
    let resources = "test-resources/morph-packs"
    packs <- runIO (allPathsIn resources)
    forM_ packs (\pth -> it (makeRelative resources pth) (testMorph known False pth))

    -- The chain runs oldest step first and carries the rule that produced the
    -- step after it, exactly as 'dataize' reports its own, so '--sequence'
    -- prints both the same way
    it "reports the chain of steps oldest first" $ do
      expr <- parseExpressionThrows "[[ D> 00- ]]"
      (morphed, chain, _) <- morph expr emptyState (defaultReduceContext ExRoot)
      morphed `shouldBe` expr
      map snd chain `shouldBe` [Just "mf", Nothing]
      map fst chain `shouldBe` [expr, expr]

  -- 𝕄 stops at the first formation 'mf' hands back and leaves its bindings as
  -- they were written, since firing a bare λ is 𝔻's business, so a program
  -- whose parts nothing demands is never reduced (#1124). The deep walk
  -- ('_deep') enters every binding and finishes what 'mf' left, while what no
  -- atom touched keeps the shape it was written in and the answer stays a
  -- program.
  describe "morph with '_deep'" $ do
    let resources = "test-resources/morph-deep-packs"
    packs <- runIO (allPathsIn resources)
    forM_ packs (\pth -> it (makeRelative resources pth) (testMorph known True pth))

    -- The walk enters a dispatch through its target and fires the box it finds
    -- there before 𝕄 is ever asked about the dispatch, while 'ml' demands that
    -- λ only where the dispatched attribute is none of the box's own (#1187)
    describe "a dispatch naming an attribute of the formation it stands on" $
      it "cannot fire the λ the dispatch does not demand" $
        withLambdasOf "- λ: L_answer\n  𝑛: ⟦ Δ ⤍ FF- ⟧\n" $ \file -> do
          box <- readLambdas file
          world <- parseExpressionThrows "[[ foo -> [[ f -> [[ a -> ?, @ -> $.a, L> L_answer ]] ]], x -> Q.foo.f( a -> [[ D> 01- ]] ).@ ]]"
          (morphed, _, _) <- morph world emptyState (withLambdas box (defaultReduceContext ExRoot)){_deep = True}
          morphed `shouldBe` world

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
      morph' (ExMeta "unbound", (ExRoot, Nothing) :| []) ExRoot emptyState (defaultReduceContext ExRoot)
        `shouldThrow` (\e -> "no morphing rule matched" `isInfixOf` show (e :: SomeException))

  -- 'execBuildTerm's "morph" case exposes 𝕄 to the matcher's condition path
  -- (guards in 'when'/'having'), the way its "evaluate" case exposes 𝔼 (see
  -- 'EvaluateSpec'). No built-in rule's guard actually calls the function, so
  -- these error paths — reachable only by malformed arguments — are exercised
  -- here directly through the exported 'execBuildTerm', the same way the
  -- matcher would call it.
  describe "execBuildTerm 'morph'" $ do
    let univ = ExFormation []
        ctx = defaultReduceContext ExRoot
    it "throws when not given exactly one expression argument" $
      execBuildTerm univ ctx "morph" [] substEmpty
        `shouldThrow` (\e -> "requires exactly 1 expression argument" `isInfixOf` show (e :: SomeException))
    it "morphs a single expression argument to its already-normal form" $ do
      result <- execBuildTerm univ ctx "morph" [ArgExpression (ExFormation [BiDelta (BtOne "00")])] substEmpty
      case result of
        TeExpression expr -> expr `shouldBe` ExFormation [BiDelta (BtOne "00")]
        _ -> expectationFailure "expected TeExpression"

  -- An expression that is not part of the program is bound to a synthetic
  -- attribute of the universe and that attribute is what 𝔻 is aimed at. This is
  -- what the '--inside' option runs, and what the 'dataize' block of a λ
  -- function runs for every operand it names.
  describe "insideUniverse" $ do
    let universe = "[[ y -> [[ D> 02- ]] ]]"
        reduced src = do
          univ <- parseExpressionThrows universe
          target <- parseExpressionThrows src
          (extended, ctx) <- insideUniverse target univ (defaultReduceContext ExRoot)
          (outcome, _, _) <- dataize extended emptyState ctx
          pure outcome
    it "reduces an expression the program does not contain" $ do
      value <- reduced "Q.y"
      value `shouldBe` Dataized (BtOne "02")
    -- 𝔻 accepts normal forms only, and a dispatch off a formation is not one:
    -- 'dot' still applies to it. So the expression is normalized first, which
    -- is the whole reason an operand cannot simply be spliced into the universe
    -- as it was written.
    it "normalizes what it is handed before 𝔻 sees it" $ do
      value <- reduced "[[ x -> [[ D> 01- ]] ]].x"
      value `shouldBe` Dataized (BtOne "01")
    it "refuses a universe which is not a formation" $ do
      target <- parseExpressionThrows "Q.y"
      insideUniverse target ExRoot (defaultReduceContext ExRoot)
        `shouldThrow` (\e -> "not a formation" `isInfixOf` show (e :: SomeException))

  -- 'defaultReduceContext' runs with '_shuffle' on, so 'morph'' walks the
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
        results <- replicateM 100 (fst . fst <$> morph' (input, (univ, Nothing) :| []) univ emptyState (defaultReduceContext ExRoot))
        nub results `shouldBe` [expected]

  -- 'md' fires only when its head is not a formation ('not (formation 𝑛)'),
  -- so a formation head — λ-bearing or not — is left to 'ml'/'mf'. The
  -- two clauses are mutually exclusive and their order in 'resources/morphing'
  -- cannot change behavior.
  describe "morphing 'md' is disjoint from 'ml'" $ do
    let rctx = RuleContext (execBuildTerm ExRoot (defaultReduceContext ExRoot))
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
      morph' (chain, (ExRoot, Nothing) :| []) ExRoot emptyState (defaultReduceContext ExRoot)
        `shouldThrow` (\e -> "No entry of --symbolic answers the λ function 'F'" `isInfixOf` show (e :: SomeException))
