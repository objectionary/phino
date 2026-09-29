{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module DepsSpec where

import AST (Binding (BiLambda), Bytes (BtOne), Expression (ExFormation, ExRoot, ExXi), Function (FnSymbol), symbols)
import Control.Exception (bracket)
import Control.Monad (replicateM_, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Deps (Evaluation (EvFiring, EvFormation, EvJoined, EvMinted, EvRun, EvTerm), Judgment (Morphing), dontSaveEval, dontSaveStep, emptyProgress, progressed, renumbered, saveStep)
import Logger (LogLevel (DEBUG, ERROR, INFO), setLogConfig)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , getTemporaryDirectory
  , removeDirectoryRecursive
  )
import System.FilePath ((</>))
import System.IO (stderr)
import System.IO.Silently (hCapture_, hSilence)
import Test.Hspec (Spec, after_, describe, expectationFailure, it, shouldBe, shouldSatisfy)

withScratchDir :: (FilePath -> IO a) -> IO a
withScratchDir =
  bracket
    ( do
        tmp <- getTemporaryDirectory
        stamp <- getPOSIXTime
        pure (tmp </> ("phino-deps-spec-" ++ show (floor (stamp * 1000000) :: Integer)))
    )
    ( \dir -> do
        exists <- doesDirectoryExist dir
        when exists (removeDirectoryRecursive dir)
    )

spec :: Spec
spec = do
  describe "dontSaveStep" $
    it "is a no-op that never touches the filesystem" $
      withScratchDir $ \dir -> do
        dontSaveStep ExRoot
        exists <- doesDirectoryExist dir
        exists `shouldBe` False

  describe "saveStep" $ do
    it "creates the directory if missing, writes the rendered step and logs it" $ withScratchDir $ \dir -> do
      setLogConfig DEBUG 25
      hSilence [stderr] (saveStep (Just dir) "phi" (pure . show) 3 ExRoot)
      setLogConfig ERROR 25
      let path = dir </> "00003.phi"
      exists <- doesFileExist path
      exists `shouldBe` True
      content <- readFile path
      content `shouldBe` show ExRoot

    it "numbers the file after the given step, zero padded to five digits" $ withScratchDir $ \dir -> do
      saveStep (Just dir) "txt" (pure . show) 42 ExRoot
      exists <- doesFileExist (dir </> "00042.txt")
      exists `shouldBe` True

  describe "progressed" $ after_ (setLogConfig ERROR 25) $ do
    it "passes every record on to the recording function it wraps" $ do
      cursor <- newIORef (emptyProgress 0)
      seen <- newIORef (0 :: Int)
      hSilence [stderr] (mapM_ (progressed cursor 3600 (const (pure "Φ.q")) (const (modifyIORef' seen (+ 1)))) [EvRun Morphing "Φ", EvFiring 1 "L_x" Morphing ExXi, EvFormation 2 ExRoot ExXi])
      count <- readIORef seen
      count `shouldBe` 3

    it "counts the firings when the interval has passed" $ do
      setLogConfig INFO 25
      cursor <- newIORef (emptyProgress 0)
      captured <- hCapture_ [stderr] (replicateM_ 7 (progressed cursor 0 (const (pure "Φ.q")) dontSaveEval (EvFiring 1 "L_y" Morphing ExXi)))
      last (lines captured) `shouldSatisfy` isInfixOf "fired 7 λ functions"

    it "counts the formations entered when the interval has passed" $ do
      setLogConfig INFO 25
      cursor <- newIORef (emptyProgress 0)
      captured <- hCapture_ [stderr] (replicateM_ 4 (progressed cursor 0 (const (pure "Φ.q")) dontSaveEval (EvFormation 3 ExRoot ExXi)))
      last (lines captured) `shouldSatisfy` isInfixOf "Entered 4 formations"

    it "names the site of the latest record" $ do
      setLogConfig INFO 25
      cursor <- newIORef (emptyProgress 0)
      captured <- hCapture_ [stderr] (progressed cursor 0 (const (pure "Φ.org.ёж")) dontSaveEval (EvFiring 2 "L_z" Morphing ExXi))
      captured `shouldSatisfy` isInfixOf "now at Φ.org.ёж"

    it "stays silent on a record that comes before the interval has passed" $ do
      setLogConfig INFO 25
      cursor <- newIORef (emptyProgress 0)
      captured <- hCapture_ [stderr] (replicateM_ 5 (progressed cursor 3600 (const (pure "Φ.q")) dontSaveEval (EvFiring 1 "L_w" Morphing ExXi)))
      length (lines captured) `shouldBe` 1

    it "says nothing about a record that carries no site" $ do
      setLogConfig INFO 25
      cursor <- newIORef (emptyProgress 0)
      captured <- hCapture_ [stderr] (progressed cursor 0 (const (pure "Φ.q")) dontSaveEval (EvRun Morphing "Φ.k"))
      captured `shouldBe` ""

  describe "renumbered" $ do
    it "raises the symbols a record names above the floor" $
      case renumbered 3 10 (EvMinted 2 5 [Left 4, Left 1, Right (BtOne "7C")]) of
        EvMinted _ minted operands -> (minted, operands) `shouldBe` (15, [Left 14, Left 1, Right (BtOne "7C")])
        _ -> expectationFailure "The record did not stay the record it was"
    it "raises the symbols the terms of a record carry above the floor" $
      case renumbered 1 6 (EvTerm 4 "𝑛1" (ExFormation [BiLambda (FnSymbol 1)]) (ExFormation [BiLambda (FnSymbol 2)])) of
        EvTerm _ _ operand term -> (symbols operand, symbols term) `shouldBe` ([1], [8])
        _ -> expectationFailure "The record did not stay the record it was"
    it "does not change the depth a record stands at" $
      case renumbered 0 9 (EvJoined 7 1 (2, 3)) of
        EvJoined depth fresh pair -> (depth, fresh, pair) `shouldBe` (7, 10, (11, 12))
        _ -> expectationFailure "The record did not stay the record it was"
