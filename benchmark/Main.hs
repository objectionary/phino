-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Main where

import AST (Attribute (AtLabel), Binding (BiTau), Expression (ExFormation, ExRoot), hashExpression)
import CLI.Helpers (started)
import Control.Exception (evaluate)
import Control.Monad (replicateM, replicateM_)
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import Data.Time.Clock
import Dataize (reduction)
import Deps (Acyclic (Plausible, Proven), Judgment (Morphing), dontSaveEval, dontSaveStep)
import Encoding (Encoding (UNICODE))
import Evaluate (evaluation, fired)
import Functions (buildTerm)
import Lambdas (Lambdas, readLambdas)
import Lining (LineFormat (MULTILINE, SINGLELINE))
import Margin (defaultMargin)
import Merge (merge)
import Morph (ReduceContext (ReduceContext), Steps (Steps), morph)
import Must (Must (MtDisabled))
import Normals (noNormals)
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

-- The wall-clock, in microseconds, one case may spend on its warmups and its
-- measured batches together. Every case that only parses, prints or rewrites
-- runs in microseconds and batches up to the window above, so ten batches of
-- it cost a fraction of a second and the budget never binds. A symbolic
-- morphing takes whole seconds per run, and inside a world the size of
-- 'native.phi' tens of them, so three warmups plus ten batches of one would
-- outlast the jobs the workflows run the suite in — 'regression-check' runs
-- the whole binary ten times over, once per round per side. The warmups and
-- the iterations are therefore cut to what the budget affords, never below one
-- measured batch, so an expensive case still reports the same lines as every
-- other one.
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
    buildTerm
    MtDisabled
    Nothing
    dontSaveStep

-- The step budget, the rewriting bounds and the flags the symbolic cases morph
-- with, which are the defaults of the 'morph' command plus the three switches
-- the regression was seen under: '--deep', so a λ function standing anywhere
-- inside the term is fired and not only the one on the spine; '--acyclic', so
-- a term coming back to itself parks instead of spending the whole step
-- budget, under whichever mode the case asks for; and '--partial', so a λ function no entry answers parks too and the
-- run still reaches an answer to measure. Nothing is written anywhere: the
-- protocol of '--protocol' and the steps of '--steps-dir' are files, and a
-- benchmark measuring the calculus has no business measuring the disk.
symbolicCtx :: Acyclic -> Lambdas -> Expression -> ReduceContext
symbolicCtx acyclic lambdas locator =
  ReduceContext
    locator -- _locator
    locator -- _site
    Nothing -- _universe
    noNormals -- _normals
    25 -- _maxDepth
    25 -- _maxCycles
    (Steps 1000 0) -- _steps
    1 -- _nesting
    False -- _depthSensitive
    False -- _shuffle
    True -- _partial
    True -- _deep
    (Just acyclic) -- _acyclic
    Morphing -- _judgment
    [] -- _parked
    Map.empty -- _entered
    lambdas -- _symbolic
    buildTerm -- _buildTerm
    reduction -- _reduce
    evaluation -- _evaluate
    fired -- _fire
    dontSaveStep -- _saveStep
    dontSaveEval -- _saveEval

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
  runBench "rewrite/normalize" (rewrite expr normalizationRules rewriteCtx)
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
    -- The entries of the demo world, each one term the λ functions of
    -- 'benchmark/atoms.yaml' answer and each one case of the suite, so that a
    -- slowdown of one of them is a line of its own rather than a share of a
    -- single total.
    entries :: [String]
    entries = ["e1", "e2", "e3", "e4", "e5"]
    -- The one entry timed inside 'native.phi' as well, whose two numbers say
    -- between them what the world around an entry costs — the very comparison
    -- nothing in the suite used to make, and the one 'number.neg' was seen to
    -- lose two orders of magnitude on (#1291). It is the smallest entry of the
    -- demo world, a single λ function fired against one unknown, because the
    -- cost measured here is the world's and not the term's: the bigger entries
    -- pay the same price per firing and merely pay it more often, which inside
    -- a megabyte of 'native.phi' is more than a benchmark can wait for.
    probe :: String
    probe = "e5"
    -- One case of the symbolic suite: the entry of the demo world 𝕄 is aimed
    -- at, inside the world it is aimed in.
    aimed :: String -> Expression -> Lambdas -> String -> IO ()
    aimed label universe lambdas name = do
      locator <- parseExpressionThrows ("Φ.l🌵." ++ name)
      runBench (printf "morph/symbolic/%s/%s" label name) (symbolic Proven universe lambdas locator)
    -- How many methods 'number' of 'benchmark/accum.phi' gets that the loop
    -- never calls. A step of the loop should cost the redex it rewrites and
    -- not the objects standing around it, so the two numbers should be close,
    -- and they were sixteen-fold apart before #1453 made them so.
    paddings :: [Int]
    paddings = [0, 400]
    -- One case of the accumulator suite: the loop of #1453 over a 'number'
    -- carrying so many unused methods, cut by '--acyclic=plausible'.
    looped :: Int -> Expression -> Lambdas -> IO ()
    looped count universe counters = do
      locator <- parseExpressionThrows "Φ.l🌵"
      runBench (printf "morph/symbolic/accum/%d" count) (symbolic Plausible universe counters locator)
    -- The world with 'number' declaring the given number of copies of the
    -- method besides its own, each under a name of its own. They are made
    -- here rather than checked in, since four hundred of them are a file
    -- nobody would read.
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
    -- One symbolic morphing of one entry, the way the 'morph' command runs it:
    -- the 𝜏-labels of the universe are scanned once, the run starts from the
    -- state that world already carries and 𝕄 is aimed at the entry. The answer
    -- is hashed rather than merely forced to weak head normal form, since a
    -- term left as a thunk is work the benchmark asked for and did not wait
    -- for.
    symbolic :: Acyclic -> Expression -> Lambdas -> Expression -> IO Int
    symbolic acyclic universe lambdas locator = do
      seedTaus universe
      (answer, _, _) <- morph universe (started universe) (symbolicCtx acyclic lambdas locator)
      pure (hashExpression answer)
