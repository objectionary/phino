{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CompiledSpec where

import AST
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (filterM, (>=>))
import Data.Aeson (FromJSON)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import Dataize (dataize')
import Deps (dontSaveStep)
import Engine (Engine (..), building, fresh, stepOf, yaml)
import Files (allPathsIn)
import Fixtures (defaultReduceContext, linked, withLambdasOf)
import GHC.Generics (Generic)
import Lambdas (Lambdas, readLambdas)
import Morph (ReduceContext (..), Steps (..), emptyState, morph')
import Must (Must (MtDisabled))
import Parser (parseExpressionThrows)
import Rewriter (RewriteContext (RewriteContext), rewrite)
import System.Random (StdGen, mkStdGen, randomR)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Yaml qualified as Y

newtype Pack = Pack {input :: String}
  deriving (Generic, FromJSON)

spec :: Spec
spec =
  describe "compiled" $ do
    it "runs the rules phino carries" $
      fresh linked `shouldBe` True
    it "normalizes the term of every rewriter pack into the chain the rules of YAML make" $ do
      terms <- allPathsIn "test-resources/rewriter-packs" >>= mapM (Yaml.decodeFileThrow >=> parseExpressionThrows . input)
      filterM (\expr -> (/=) <$> chain linked Nothing expr <*> chain yaml Nothing expr) terms
        `shouldReturn` []
    it "normalizes random terms into the chains the rules of YAML make" $
      filterM (\seed -> (/=) <$> chain linked Nothing (term False seed) <*> chain yaml Nothing (term False seed)) [1 .. 400]
        `shouldReturn` []
    it "normalizes random terms standing in a world into the chains the rules of YAML make" $
      filterM (\seed -> (/=) <$> chain linked (Just (term False seed)) (term False seed) <*> chain yaml (Just (term False seed)) (term False seed)) [401 .. 800]
        `shouldReturn` []
    it "normalizes random terms holding metas into the chains the rules of YAML make" $
      filterM (\seed -> (/=) <$> chain linked Nothing (term True seed) <*> chain yaml Nothing (term True seed)) [801 .. 1200]
        `shouldReturn` []
    it "tells a normal form the way the rules of YAML do" $
      filter (\seed -> _normal linked (term False seed) /= _normal yaml (term False seed)) [1 .. 3000]
        `shouldBe` []
    it "tells a normal form of a term holding metas the way the rules of YAML do" $
      filter (\seed -> _normal linked (term True seed) /= _normal yaml (term True seed)) [3001 .. 6000]
        `shouldBe` []
    it "contextualizes random terms the way the rules of YAML do, failures included" $
      filterM (\seed -> (/=) <$> contextualized linked (term True seed) (term False (seed + 1)) <*> contextualized yaml (term True seed) (term False (seed + 1))) [1 .. 3000]
        `shouldReturn` []
    it "morphs an object of random programs into the chains the rules of YAML make, failures included" $
      withLambdasOf "- λ: L_q\n  𝑛: ⟦ φ ↦ ⟦ λ ⤍ 𝜎 ⟧ ⟧\n" $ \path -> do
        lambdas <- readLambdas path
        filterM (\seed -> (/=) <$> morphed lambdas linked (program seed) <*> morphed lambdas yaml (program seed)) [1 .. 400]
          `shouldReturn` []
    it "dataizes an object of random programs into the chains the rules of YAML make, failures included" $
      withLambdasOf "- λ: L_q\n  𝑛: ⟦ φ ↦ ⟦ λ ⤍ 𝜎 ⟧ ⟧\n" $ \path -> do
        lambdas <- readLambdas path
        filterM (\seed -> (/=) <$> dataized lambdas linked (program seed) <*> dataized lambdas yaml (program seed)) [1 .. 400]
          `shouldReturn` []
  where
    chain :: Engine -> Maybe Expression -> Expression -> IO (Either String String)
    chain engine universe expr =
      settled
        ( show . fst
            <$> rewrite
              expr
              (map (stepOf engine) Y.normalizationRules)
              (RewriteContext ExRoot 25 25 False universe (building engine) (_normal engine) MtDisabled Nothing dontSaveStep)
        )
    morphed :: Lambdas -> Engine -> Expression -> IO (Either String String)
    morphed lambdas engine world = settled (show . fst <$> morph' (ExDispatch ExRoot (AtLabel "x"), (world, Nothing) :| []) world emptyState (reducing lambdas engine))
    dataized :: Lambdas -> Engine -> Expression -> IO (Either String String)
    dataized lambdas engine world = settled (show . fst <$> dataize' (ExDispatch ExRoot (AtLabel "x"), (world, Nothing) :| []) world emptyState (reducing lambdas engine))
    reducing :: Lambdas -> Engine -> ReduceContext
    reducing lambdas engine = (defaultReduceContext ExRoot){_engine = engine, _buildTerm = building engine, _shuffle = False, _steps = Steps 40 0, _symbolic = lambdas}
    program :: Int -> Expression
    program seed = fst (formation False 4 (mkStdGen seed))
    contextualized :: Engine -> Expression -> Expression -> IO (Either String String)
    contextualized engine expr context = settled (show <$> _contextualize engine expr context)
    settled :: IO String -> IO (Either String String)
    settled action = either (Left . show) Right <$> (try (action >>= \text -> evaluate (length text) >> pure text) :: IO (Either SomeException String))
    term :: Bool -> Int -> Expression
    term metas seed = fst (grown metas (4 :: Int) (mkStdGen seed))
    grown :: Bool -> Int -> StdGen -> (Expression, StdGen)
    grown metas depth gen =
      let (pick, gen') = randomR (0, if depth == 0 then 3 else 9 :: Int) gen
       in case pick of
            0 -> (ExXi, gen')
            1 -> (ExRoot, gen')
            2 -> (if metas then ExMeta "e9" else ExTermination, gen')
            3 -> (ExTermination, gen')
            4 -> formation metas depth gen'
            5 -> formation metas depth gen'
            6 ->
              let (expr, gen'') = grown metas (depth - 1) gen'
                  (attr, gen''') = attribute gen''
               in (ExDispatch expr attr, gen''')
            7 ->
              let (expr, gen'') = grown metas (depth - 1) gen'
                  (attr, gen''') = attribute gen''
                  (arg, gen'''') = grown metas (depth - 1) gen'''
               in (ExApplication expr (ArTau attr arg), gen'''')
            8 ->
              let (expr, gen'') = grown metas (depth - 1) gen'
                  (idx, gen''') = randomR (0, 2) gen''
                  (arg, gen'''') = grown metas (depth - 1) gen'''
               in (ExApplication expr (ArAlpha (Alpha idx) arg), gen'''')
            _ -> formation metas depth gen'
    formation :: Bool -> Int -> StdGen -> (Expression, StdGen)
    formation metas depth gen =
      let (bds, gen') = foldr binding ([], gen) [AtLabel "x", AtLabel "y", AtRho, AtPhi, AtDelta, AtLambda]
       in (ExFormation bds, gen')
      where
        binding :: Attribute -> ([Binding], StdGen) -> ([Binding], StdGen)
        binding attr (bds, source) =
          let (pick, source') = randomR (0, 3 :: Int) source
           in case (attr, pick) of
                (_, 0) -> (bds, source')
                (AtDelta, 1) -> (BiDelta (BtOne "0A") : bds, source')
                (AtDelta, _) -> (bds, source')
                (AtLambda, 1) -> (BiLambda (Function (T.pack "L_q")) : bds, source')
                (AtLambda, _) -> (bds, source')
                (_, 1) -> (BiVoid attr : bds, source')
                _ ->
                  let (expr, source'') = grown metas (depth - 1) source'
                   in (BiTau attr expr : bds, source'')
    attribute :: StdGen -> (Attribute, StdGen)
    attribute gen =
      let (pick, gen') = randomR (0, 3 :: Int) gen
       in ([AtLabel "x", AtLabel "y", AtRho, AtPhi] !! pick, gen')
