-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Main where

import AST (Expression (ExGlobal))
import Control.Exception (evaluate)
import Control.Monad (replicateM, replicateM_)
import Data.Time.Clock
import Deps (dontSaveStep)
import Functions (buildTerm)
import Must (Must (MtDisabled))
import Parser (parseProgramThrows)
import Rewriter (RewriteContext (RewriteContext), rewrite)
import Text.Printf (printf)
import XMIR (parseXMIRThrows, xmirToPhi)
import Yaml (normalizationRules)

warmups :: Int
warmups = 3

iterations :: Int
iterations = 10

targetBatchMs :: Double
targetBatchMs = 20.0

rewriteCtx :: RewriteContext
rewriteCtx =
  RewriteContext
    ExGlobal
    100
    100
    False
    buildTerm
    MtDisabled
    Nothing
    dontSaveStep

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

calibrate :: IO a -> IO Int
calibrate action = do
  t <- timeAction action
  pure (max 1 (round (targetBatchMs * 1000.0 / t)))

stdDev :: [Double] -> Double -> Double
stdDev xs avg = sqrt (sum (map (\x -> (x - avg) ^ (2 :: Int)) xs) / fromIntegral (length xs))

runBench :: String -> IO a -> IO ()
runBench name action = do
  replicateM_ warmups action
  batch <- calibrate action
  times <- replicateM iterations (timeBatch batch action)
  let total = sum times * fromIntegral batch
      avg = sum times / fromIntegral iterations
      mn = minimum times
      mx = maximum times
      sd = stdDev times avg
  putStrLn $ "=== " ++ name ++ " ==="
  putStrLn $ printf "  warmup:     %d iterations" warmups
  putStrLn $ printf "  batches:    %d x %d" iterations batch
  putStrLn $ printf "  total:      %.3f μs" total
  putStrLn $ printf "  avg:        %.3f μs" avg
  putStrLn $ printf "  min:        %.3f μs" mn
  putStrLn $ printf "  max:        %.3f μs" mx
  putStrLn $ printf "  std dev:    %.3f μs" sd

main :: IO ()
main = do
  src <- readFile "benchmark/tmp/native.phi"
  xsrc <- readFile "benchmark/tmp/Native.xmir"
  prog <- parseProgramThrows src
  runBench "parse/phi" (parseProgramThrows src)
  runBench "parse/xmir" (parseXMIRThrows xsrc >>= xmirToPhi)
  runBench "rewrite/normalize" (rewrite prog normalizationRules rewriteCtx)
