-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Main where

import AST (Attribute (AtLabel), Binding (BiTau), Expression (ExFormation, ExRoot), hashExpression)
import CLI.Helpers (started)
import Compiled (compiled)
import Control.Exception (evaluate)
import Control.Monad (replicateM, replicateM_)
import Data.IORef (IORef, newIORef)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Time.Clock
import Dataize (reduction)
import Deps (Acyclic (Plausible, Proven), Judgment (Morphing), dontSaveEval, dontSaveStep)
import Encoding (Encoding (UNICODE))
import Engine (Engine (_matching, _normal), building, stepOf, yaml)
import Evaluate (evaluation, fired)
import Lambdas (Lambdas, readLambdas)
import Lining (LineFormat (MULTILINE, SINGLELINE))
import Margin (defaultMargin)
import Merge (merge)
import Morph (Memo, ReduceContext (ReduceContext), Steps (Steps), emptyState, memoized, morph)
import Must (Must (MtDisabled))
import Parser (parseExpressionThrows)
import Printer (printExpression')
import Rewriter (RewriteContext (RewriteContext), rewrite)
import Sugar (SugarType (SALTY, SWEET))
import Tau (seedTaus)
import Text.Printf (printf)
import XMIR (parseXMIRThrows, xmirToPhi)
import Yaml (normalizationRules)

warmups :: Int
warmups = 3

iterations :: Int
iterations = 10

targetBatchMs :: Double
targetBatchMs = 20.0

budget :: Double
budget = 30.0 * 1e6

rewriteCtx :: RewriteContext
rewriteCtx =
  RewriteContext
    ExRoot
    100
    100
    False
    Nothing
    (building linked)
    (_normal linked)
    (_matching linked)
    MtDisabled
    Nothing
    dontSaveStep

symbolicCtx :: Acyclic -> Maybe Memo -> IORef Int -> Lambdas -> Expression -> ReduceContext
symbolicCtx acyclic memo minted lambdas locator =
  ReduceContext
    locator
    locator
    Nothing
    25
    25
    (Steps 1000 0)
    Nothing
    minted
    Nothing
    memo
    1
    False
    False
    True
    True
    1
    (Just acyclic)
    Morphing
    []
    Map.empty
    lambdas
    (building linked)
    reduction
    evaluation
    fired
    dontSaveStep
    dontSaveEval
    linked

linked :: Engine
linked = fromMaybe yaml compiled

timeAction :: IO a -> IO Double
timeAction action = do
  start <- getCurrentTime
  _ <- evaluate =<< action
  end <- getCurrentTime
  pure (realToFrac (diffUTCTime end start) * 1e6)

timeBatch :: Int -> IO a -> IO Double
timeBatch batch action = do
  start <- getCurrentTime
  replicateM_ batch (evaluate =<< action)
  end <- getCurrentTime
  pure (realToFrac (diffUTCTime end start) * 1e6 / fromIntegral batch)

stdDev :: [Double] -> Double -> Double
stdDev xs avg = sqrt (sum (map (\x -> (x - avg) ^ (2 :: Int)) xs) / fromIntegral (length xs))

runBench :: String -> IO a -> IO ()
runBench name action = do
  single <- timeAction action
  let batch = max 1 (round (targetBatchMs * 1000.0 / single))
      afford = max 1 (floor (budget / (single * fromIntegral batch)) :: Int)
      warms = max 0 (min warmups (afford `div` 3))
      iters = max 1 (min iterations (afford - warms))
  replicateM_ warms action
  times <- replicateM iters (timeBatch batch action)
  let total = sum times * fromIntegral batch
      avg = sum times / fromIntegral iters
      mn = minimum times
      mx = maximum times
      sd = stdDev times avg
  putStrLn $ "=== " ++ name ++ " ==="
  putStrLn $ printf "  warmup:     %d iterations" warms
  putStrLn $ printf "  batches:    %d x %d" iters batch
  putStrLn $ printf "  total:      %.3f μs" total
  putStrLn $ printf "  avg:        %.3f μs" avg
  putStrLn $ printf "  min:        %.3f μs" mn
  putStrLn $ printf "  max:        %.3f μs" mx
  putStrLn $ printf "  std dev:    %.3f μs" sd

main :: IO ()
main = do
  src <- readFile "benchmark/tmp/native.phi"
  xsrc <- readFile "benchmark/tmp/Native.xmir"
  dsrc <- readFile "benchmark/demo.phi"
  expr <- parseExpressionThrows src
  demo <- parseExpressionThrows dsrc
  merged <- merge [demo, expr]
  lambdas <- readLambdas "benchmark/atoms.yaml"
  asrc <- readFile "benchmark/accum.phi"
  accum <- parseExpressionThrows asrc
  method <- parseExpressionThrows "⟦ b ↦ ∅, φ ↦ ξ.ρ.plus( b ↦ ξ.b ), ρ ↦ ∅ ⟧"
  counters <- readLambdas "benchmark/accum.yaml"
  runBench "parse/phi" (parseExpressionThrows src)
  runBench "parse/xmir" (parseXMIRThrows xsrc >>= xmirToPhi)
  runBench "rewrite/normalize" (hashExpression . fst . NE.last . fst <$> rewrite expr (map (stepOf linked) normalizationRules) rewriteCtx)
  runBench
    "print/sweet/multiline"
    (evaluate (length (printExpression' expr (SWEET, UNICODE, MULTILINE, defaultMargin))))
  runBench
    "print/sweet/flat"
    (evaluate (length (printExpression' expr (SWEET, UNICODE, SINGLELINE, defaultMargin))))
  runBench
    "print/salty/multiline"
    (evaluate (length (printExpression' expr (SALTY, UNICODE, MULTILINE, defaultMargin))))
  mapM_ (aimed "demo" demo lambdas) entries
  aimed "native" merged lambdas probe
  mapM_ (\count -> looped count (padded method count accum) counters) paddings
  where
    entries :: [String]
    entries = ["e1", "e2", "e3", "e4", "e5"]
    probe :: String
    probe = "e5"
    aimed :: String -> Expression -> Lambdas -> String -> IO ()
    aimed label universe lambdas name = do
      locator <- parseExpressionThrows ("Φ.l🌵." ++ name)
      runBench (printf "morph/symbolic/%s/%s" label name) (symbolic Proven universe lambdas locator)
    paddings :: [Int]
    paddings = [0, 400]
    looped :: Int -> Expression -> Lambdas -> IO ()
    looped count universe counters = do
      locator <- parseExpressionThrows "Φ.l🌵"
      runBench (printf "morph/symbolic/accum/%d" count) (symbolic Plausible universe counters locator)
    padded :: Expression -> Int -> Expression -> Expression
    padded method count (ExFormation bds) = ExFormation (map grown bds)
      where
        grown :: Binding -> Binding
        grown (BiTau attr (ExFormation inner))
          | attr == AtLabel (fromString "number") =
              BiTau attr (ExFormation (inner ++ map copy [1 .. count]))
        grown binding = binding
        copy :: Int -> Binding
        copy index = BiTau (AtLabel (fromString (printf "m%d" index))) method
    padded _ _ universe = universe
    symbolic :: Acyclic -> Expression -> Lambdas -> Expression -> IO Int
    symbolic acyclic universe lambdas locator = do
      seedTaus universe
      memo <- memoized (Just acyclic)
      minted <- newIORef 0
      let ctx = symbolicCtx acyclic memo minted lambdas locator
      started universe ctx
      (answer, _, _) <- morph universe emptyState ctx
      pure (hashExpression answer)
