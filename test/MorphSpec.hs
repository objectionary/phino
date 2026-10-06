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
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (find, isInfixOf, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Yaml qualified as Decode
import Dataize (Outcome (..), dataize)
import Deps (Acyclic (..), Judgment (..), State, Term (TeExpression))
import Engine (Engine (_normal))
import Files (allPathsIn)
import Fixtures (defaultReduceContext, fixtureLambdas, linked, overdue, primitives, withLambdas, withLambdasOf)
import GHC.Clock (getMonotonicTime)
import GHC.Generics (Generic)
import Inference (Conclusion (Answered), Premises (Concludes, Morphs), direct)
import Lambdas (Lambdas, emptyLambdas, readLambdas)
import Matcher (substEmpty)
import Morph (Deadline (..), ReduceContext (..), emptyState, enter, execBuildTerm, inferred, insideUniverse, morph, morph')
import Parser (parseExpressionThrows)
import Rewriter (Rewritten)
import Rule (RuleContext (RuleContext), matchExpressionWithRule')
import System.FilePath (makeRelative)
import System.Timeout (timeout)
import Tau (seedTaus)
import Test.Hspec
import Yaml (ExtraArgument (..))
import Yaml qualified

test' :: (Eq a, Show a) => ((Expression, NonEmpty Rewritten) -> Expression -> State -> ReduceContext -> IO ((a, NonEmpty Rewritten), State)) -> [(String, Expression, Expression, a)] -> Spec
test' func useCases =
  forM_ useCases $ \(desc, input, expr, output) ->
    it desc $ do
      ((res, _), _) <- func (input, (expr, Nothing) :| []) expr emptyState =<< defaultReduceContext ExRoot
      res `shouldBe` output

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

testMorph :: Lambdas -> Bool -> FilePath -> Expectation
testMorph known deep pth = do
  MorphPack{..} <- Decode.decodeFileThrow pth
  expr <- parseExpressionThrows (if model == Just True then primitives input else input)
  seedTaus expr
  loc <- parseExpressionThrows (fromMaybe "Q" location)
  base <- defaultReduceContext loc
  let ctx =
        base
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
  known <- runIO fixtureLambdas

  describe "morph" $ do
    let resources = "test-resources/morph-packs"
    packs <- runIO (allPathsIn resources)
    forM_ packs (\pth -> it (makeRelative resources pth) (testMorph known False pth))

    it "reports the chain of steps oldest first" $ do
      expr <- parseExpressionThrows "[[ D> 00- ]]"
      (morphed, chain, _) <- morph expr emptyState =<< defaultReduceContext ExRoot
      morphed `shouldBe` expr
      map snd chain `shouldBe` [Just (Morphing, "mf"), Nothing]
      map fst chain `shouldBe` [expr, expr]

    it "resolves Φ to the world it has already normalized" $ do
      expr <- parseExpressionThrows "[[ w -> [[ k -> [[ ]] ]].k, y -> Q.w ]]"
      loc <- parseExpressionThrows "Q.y"
      saved <- newIORef (0 :: Int)
      ctx <- defaultReduceContext loc
      _ <- morph expr emptyState ctx{_saveStep = const (modifyIORef' saved (+ 1))}
      readIORef saved `shouldReturn` 2

  describe "morph with '_deep'" $ do
    let resources = "test-resources/morph-deep-packs"
    packs <- runIO (allPathsIn resources)
    forM_ packs (\pth -> it (makeRelative resources pth) (testMorph known True pth))

    describe "a dispatch naming an attribute of the formation it stands on" $
      it "cannot fire the λ the dispatch does not demand" $
        withLambdasOf "- λ: L_answer\n  𝑛: ⟦ Δ ⤍ FF- ⟧\n" $ \file -> do
          box <- readLambdas file
          world <- parseExpressionThrows "[[ foo -> [[ f -> [[ a -> ?, @ -> $.a, L> L_answer ]] ]], x -> Q.foo.f( a -> [[ D> 01- ]] ).@ ]]"
          ctx <- withLambdas box <$> defaultReduceContext ExRoot
          (morphed, _, _) <- morph world emptyState ctx{_deep = True}
          morphed `shouldBe` world

  describe "morph'" $
    test'
      morph'
      [ ("[[ D> 00- ]] => [[ D> 00- ]]", ExFormation [BiDelta (BtOne "00")], ExRoot, ExFormation [BiDelta (BtOne "00")])
      , ("T => T", ExTermination, ExRoot, ExTermination)
      , ("$ => X", ExXi, ExRoot, ExTermination)
      , ("Q => X", ExRoot, ExRoot, ExTermination)
      ,
        ( "Q.x (Q -> [[ x -> [[]] ]]) => [[]]"
        , ExDispatch ExRoot (AtLabel "x")
        , ExFormation [BiTau (AtLabel "x") (ExFormation [])]
        , ExFormation []
        )
      ,
        ( "Q.x (Q -> [[ x -> [[ ^ -> ? ]] ]]) => [[ ρ -> Q ]]"
        , ExDispatch ExRoot (AtLabel "x")
        , ExFormation [BiTau (AtLabel "x") (ExFormation [BiVoid AtRho])]
        , ExFormation [BiTau AtRho ExRoot]
        )
      ,
        ( "[[ x -> ? ]](x -> $.foo) => T"
        , ExApplication (ExFormation [BiVoid (AtLabel "x")]) (ArTau (AtLabel "x") (ExDispatch ExXi (AtLabel "foo")))
        , ExRoot
        , ExTermination
        )
      ,
        ( "[[ ^ -> ? ]](α0 -> $.foo) => T"
        , ExApplication (ExFormation [BiVoid AtRho]) (ArAlpha (Alpha 0) (ExDispatch ExXi (AtLabel "foo")))
        , ExRoot
        , ExTermination
        )
      ,
        ( "Q => [[]] (a universe distinct from Φ) => [[]]"
        , ExRoot
        , ExFormation []
        , ExFormation []
        )
      ]

  describe "inferred" $
    it "morphs a premise in the universe it names, not in the one the frame is in" $ do
      world <- parseExpressionThrows "[[ x -> [[ ]] ]]"
      ctx <- defaultReduceContext ExRoot
      Just (Answered _ answer, _) <-
        inferred
          ExRoot
          ExRoot
          emptyState
          ctx
          [direct (\_ _ -> [Morphs ExRoot world (pure . Concludes . Answered (Morphing, "premise"))])]
      answer `shouldBe` world

  describe "morph' fails when no morphing rule matches the term" $
    it "throws instead of looping when handed a bare, unmatched meta" $
      (morph' (ExMeta "unbound", (ExRoot, Nothing) :| []) ExRoot emptyState =<< defaultReduceContext ExRoot)
        `shouldThrow` (\e -> "Morphing expects a normal form" `isInfixOf` show (e :: SomeException))

  describe "execBuildTerm 'morph'" $ do
    let univ = ExFormation []
    it "throws when not given exactly one expression argument" $ do
      ctx <- defaultReduceContext ExRoot
      execBuildTerm univ ctx "morph" [] substEmpty
        `shouldThrow` (\e -> "requires exactly 1 expression argument" `isInfixOf` show (e :: SomeException))
    it "morphs a single expression argument to its already-normal form" $ do
      ctx <- defaultReduceContext ExRoot
      result <- execBuildTerm univ ctx "morph" [ArgExpression (ExFormation [BiDelta (BtOne "00")])] substEmpty
      case result of
        TeExpression expr -> expr `shouldBe` ExFormation [BiDelta (BtOne "00")]
        _ -> expectationFailure "expected TeExpression"

  describe "insideUniverse" $ do
    let universe = "[[ y -> [[ D> 02- ]] ]]"
        reduced src = do
          univ <- parseExpressionThrows universe
          target <- parseExpressionThrows src
          (extended, ctx) <- insideUniverse target univ =<< defaultReduceContext ExRoot
          (outcome, _, _) <- dataize extended emptyState ctx
          pure outcome
    it "reduces an expression the program does not contain" $ do
      value <- reduced "Q.y"
      value `shouldBe` Dataized (BtOne "02")
    it "normalizes what it is handed before 𝔻 sees it" $ do
      value <- reduced "[[ x -> [[ D> 01- ]] ]].x"
      value `shouldBe` Dataized (BtOne "01")
    it "refuses a universe which is not a formation" $ do
      target <- parseExpressionThrows "Q.y"
      (insideUniverse target ExRoot =<< defaultReduceContext ExRoot)
        `shouldThrow` (\e -> "not a formation" `isInfixOf` show (e :: SomeException))

  describe "morphing is order-independent under --shuffle" $ do
    let cases =
          [ ("a byte formation", ExFormation [BiDelta (BtOne "00")], ExRoot, ExFormation [BiDelta (BtOne "00")])
          , ("termination", ExTermination, ExRoot, ExTermination)
          , ("xi", ExXi, ExRoot, ExTermination)
          , ("the global object", ExRoot, ExRoot, ExTermination)
          ,
            ( "a dispatch over a formation"
            , ExDispatch ExRoot (AtLabel "x")
            , ExFormation [BiTau (AtLabel "x") (ExFormation [BiVoid AtRho])]
            , ExFormation [BiTau AtRho ExRoot]
            )
          ]
    forM_ cases $ \(desc, input, univ, expected) ->
      it ("morphs " ++ desc ++ " to the same form across 100 random rule orders") $ do
        results <- replicateM 100 (fst . fst <$> (morph' (input, (univ, Nothing) :| []) univ emptyState =<< defaultReduceContext ExRoot))
        nub results `shouldBe` [expected]

  describe "morphing 'md' is disjoint from 'ml'" $ do
    ctx <- runIO (defaultReduceContext ExRoot)
    let rctx = RuleContext (execBuildTerm ExRoot ctx) Nothing (_normal linked)
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
    it "drills a chained λ-formation dispatch down to the base 'ml'" $ do
      let base = ExFormation [BiLambda (Function "F")]
          chain = ExDispatch (ExDispatch (ExDispatch base (AtLabel "a")) (AtLabel "b")) (AtLabel "c")
      (morph' (chain, (ExRoot, Nothing) :| []) ExRoot emptyState =<< defaultReduceContext ExRoot)
        `shouldThrow` (\e -> "No entry of --symbolic answers the λ function 'F'" `isInfixOf` show (e :: SomeException))

  describe "stops by the clock of --max-seconds" $ do
    it "fails a morphing that fires nothing once the deadline has passed" $ do
      expr <- parseExpressionThrows "[[ k -> [[ D> 3F- ]] ]]"
      deadline <- overdue 17
      ctx <- defaultReduceContext ExRoot
      morph expr emptyState ctx{_deadline = Just deadline}
        `shouldThrow` (\e -> "--max-seconds=17" `isInfixOf` show (e :: SomeException))
    it "fails a partial morphing once the deadline has passed" $ do
      expr <- parseExpressionThrows "[[ q -> [[ D> 5A- ]] ]]"
      deadline <- overdue 23
      ctx <- defaultReduceContext ExRoot
      morph expr emptyState ctx{_deadline = Just deadline, _partial = True}
        `shouldThrow` (\e -> "--max-seconds=23" `isInfixOf` show (e :: SomeException))

  describe "stops an entrance by the clock of --max-seconds" $
    it "fails a comparison that outlasts the deadline" $ do
      due <- (+ 0.2) <$> getMonotonicTime
      let formation :: Expression -> Int -> Expression
          formation base depth = ExFormation [BiTau (AtLabel "x") (iterate (`ExDispatch` AtLabel "w") base !! depth), BiLambda (Function "L_q")]
      base <- defaultReduceContext ExRoot
      ctx <- enter (formation ExRoot 1300) base{_acyclic = Just Plausible, _deadline = Just (Deadline 31 due)}
      timeout 10000000 (enter (formation ExXi 1700) ctx)
        `shouldThrow` (\e -> "--max-seconds=31" `isInfixOf` show (e :: SomeException))
