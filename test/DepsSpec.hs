{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module DepsSpec where

import AST (Argument (ArTau), Attribute (AtLabel), Binding (BiLambda, BiTau), Bytes (BtOne), Expression (ExApplication, ExDispatch, ExFormation, ExRoot, ExXi), Function (FnSymbol), symbols)
import Control.Exception (bracket)
import Control.Monad (replicateM_, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf, isPrefixOf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Deps (Evaluation (EvDeferred, EvFiring, EvFormation, EvJoined, EvMinted, EvRun, EvTerm), Judgment (Morphing), Nesting (..), Protocol (..), dontSaveEval, dontSaveStep, emptyNesting, emptyProgress, emptyProtocol, endEval, endEvalXml, perSecond, progressed, renumbered, saveStep)
import Fixtures (readUtf8)
import GHC.Clock (getMonotonicTime)
import Logger (LogLevel (DEBUG, ERROR, INFO), setLogConfig)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getTemporaryDirectory
  , removeDirectoryRecursive
  )
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), stderr, withFile)
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
    it "raises the symbol a deferred copy stands for and the symbols it carries above the floor" $
      case renumbered 2 5 (EvDeferred 3 4 Morphing (ExFormation [BiTau (AtLabel "x") (ExFormation [BiLambda (FnSymbol 1)]), BiTau (AtLabel "y") (ExFormation [BiLambda (FnSymbol 3)])]) Nothing ExXi) of
        EvDeferred _ fresh _ copy _ _ -> (fresh, symbols copy) `shouldBe` (9, [1, 8])
        _ -> expectationFailure "The record did not stay the record it was"
    it "raises the symbols the call a deferred copy stands for carries above the floor" $
      case renumbered 2 5 (EvDeferred 3 4 Morphing (ExFormation []) (Just (ExApplication (ExDispatch ExRoot (AtLabel "box")) (ArTau (AtLabel "x") (ExFormation [BiLambda (FnSymbol 7)])))) ExXi) of
        EvDeferred _ _ _ _ call _ -> fmap symbols call `shouldBe` Just [12]
        _ -> expectationFailure "The record did not stay the record it was"

  describe "perSecond" $ do
    it "divides the firings by the seconds the run took" $
      perSecond 20 2000 `shouldBe` 10
    it "floors the milliseconds at one so a run under one never divides by zero" $
      perSecond 5 0 `shouldBe` 5000

  describe "endEval" $ do
    it "writes nothing once a run that never opened the protocol closes" $ withScratchDir $ \dir -> do
      let path = dir </> "protocol.txt"
      createDirectoryIfMissing True dir
      cursor <- newIORef emptyProtocol
      began <- getMonotonicTime
      withFile path WriteMode (\handle -> endEval handle cursor began)
      content <- readUtf8 path
      content `shouldBe` ""

    it "closes a run that opened the protocol with its msec, firings and fps" $ withScratchDir $ \dir -> do
      let path = dir </> "protocol.txt"
      createDirectoryIfMissing True dir
      cursor <- newIORef emptyProtocol{_begun = True, _fired = 5}
      began <- getMonotonicTime
      withFile path WriteMode (\handle -> endEval handle cursor began)
      content <- readUtf8 path
      map (takeWhile (/= '(')) (lines content) `shouldBe` ["msec", "firings", "fps"]

    it "names the firings of the run it closes" $ withScratchDir $ \dir -> do
      let path = dir </> "protocol.txt"
      createDirectoryIfMissing True dir
      cursor <- newIORef emptyProtocol{_begun = True, _fired = 5}
      began <- getMonotonicTime
      withFile path WriteMode (\handle -> endEval handle cursor began)
      content <- readUtf8 path
      lines content `shouldSatisfy` elem "firings(5)"

  describe "endEvalXml" $ do
    it "writes nothing once a run that never opened the protocol closes" $ withScratchDir $ \dir -> do
      let path = dir </> "protocol.xml"
      createDirectoryIfMissing True dir
      cursor <- newIORef emptyNesting
      began <- getMonotonicTime
      withFile path WriteMode (\handle -> endEvalXml handle cursor began)
      content <- readUtf8 path
      content `shouldBe` ""

    it "closes every element still open before it writes any total" $ withScratchDir $ \dir -> do
      let path = dir </> "protocol.xml"
      createDirectoryIfMissing True dir
      cursor <- newIORef emptyNesting{_fires = 3, _closing = [(1, "evaluate"), (0, "morph"), (-1, "protocol")]}
      began <- getMonotonicTime
      withFile path WriteMode (\handle -> endEvalXml handle cursor began)
      content <- readUtf8 path
      take 2 (lines content) `shouldBe` ["    </evaluate>", "  </morph>"]

    it "names the firings of the run it closes" $ withScratchDir $ \dir -> do
      let path = dir </> "protocol.xml"
      createDirectoryIfMissing True dir
      cursor <- newIORef emptyNesting{_fires = 3, _closing = [(0, "morph"), (-1, "protocol")]}
      began <- getMonotonicTime
      withFile path WriteMode (\handle -> endEvalXml handle cursor began)
      content <- readUtf8 path
      lines content `shouldSatisfy` elem "  <firings>3</firings>"

    it "closes the document with '</protocol>' once every total is written" $ withScratchDir $ \dir -> do
      let path = dir </> "protocol.xml"
      createDirectoryIfMissing True dir
      cursor <- newIORef emptyNesting{_closing = [(-1, "protocol")]}
      began <- getMonotonicTime
      withFile path WriteMode (\handle -> endEvalXml handle cursor began)
      content <- readUtf8 path
      last (lines content) `shouldBe` "</protocol>"

    it "writes the msec before the firings it closes with" $ withScratchDir $ \dir -> do
      let path = dir </> "protocol.xml"
      createDirectoryIfMissing True dir
      cursor <- newIORef emptyNesting{_closing = [(-1, "protocol")]}
      began <- getMonotonicTime
      withFile path WriteMode (\handle -> endEvalXml handle cursor began)
      content <- readUtf8 path
      take 1 (lines content) `shouldSatisfy` any (isPrefixOf "  <msec>")

    it "writes the fps after the firings it closes with" $ withScratchDir $ \dir -> do
      let path = dir </> "protocol.xml"
      createDirectoryIfMissing True dir
      cursor <- newIORef emptyNesting{_closing = [(-1, "protocol")]}
      began <- getMonotonicTime
      withFile path WriteMode (\handle -> endEvalXml handle cursor began)
      content <- readUtf8 path
      (lines content !! 2) `shouldSatisfy` isPrefixOf "  <fps>"
