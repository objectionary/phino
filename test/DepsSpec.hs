{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module DepsSpec where

import AST (Argument (ArTau), Attribute (AtLabel, AtPhi), Binding (BiLambda, BiTau), Bytes (BtMany, BtOne), Expression (ExApplication, ExDispatch, ExFormation, ExRoot, ExXi), Function (FnSymbol), symbols)
import Control.Exception (bracket)
import Control.Monad (replicateM_, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf, isPrefixOf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Deps (Acyclic (Proven), Evaluation (EvAnswer, EvApplied, EvBuilt, EvComputed, EvData, EvDeferred, EvDelta, EvFiring, EvJoined, EvLooped, EvMinted, EvRun, EvStarted, EvTerm), Judgment (Dataization, Morphing), Nesting (..), Protocol (..), dontSaveEval, dontSaveStep, emptyNesting, emptyProgress, emptyProtocol, endEval, endEvalXml, perSecond, progressed, renumbered, resited, saveStep)
import Fixtures (readUtf8, recorded, recordedXml)
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
import Test.Hspec (Spec, after_, describe, expectationFailure, it, shouldBe, shouldContain, shouldSatisfy)

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
      hSilence [stderr] (mapM_ (progressed cursor 3600 (const (pure "Φ.q")) (const (modifyIORef' seen (+ 1)))) [EvRun Morphing "Φ", EvFiring 1 "L_x" Morphing ExXi, EvStarted 2 Dataization ExXi])
      count <- readIORef seen
      count `shouldBe` 3

    it "counts the firings when the interval has passed" $ do
      setLogConfig INFO 25
      cursor <- newIORef (emptyProgress 0)
      captured <- hCapture_ [stderr] (replicateM_ 7 (progressed cursor 0 (const (pure "Φ.q")) dontSaveEval (EvFiring 1 "L_y" Morphing ExXi)))
      last (lines captured) `shouldSatisfy` isInfixOf "fired 7 λ functions"

    it "counts the dataizations started when the interval has passed" $ do
      setLogConfig INFO 25
      cursor <- newIORef (emptyProgress 0)
      captured <- hCapture_ [stderr] (replicateM_ 4 (progressed cursor 0 (const (pure "Φ.q")) dontSaveEval (EvStarted 3 Dataization ExXi)))
      last (lines captured) `shouldSatisfy` isInfixOf "Started 4 dataizations"
    it "does not count a morphing as a dataization started" $ do
      setLogConfig INFO 25
      cursor <- newIORef (emptyProgress 0)
      captured <- hCapture_ [stderr] (mapM_ (progressed cursor 0 (const (pure "Φ.q")) dontSaveEval) [EvStarted 3 Morphing ExXi, EvStarted 2 Dataization ExXi])
      last (lines captured) `shouldSatisfy` isInfixOf "Started 1 dataizations"

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
    it "raises the symbol a cut answers a copy with and the symbols of its call above the floor" $
      case renumbered 2 5 (EvLooped 3 Morphing Proven (ExFormation []) ExXi (Just (4, Just (ExApplication (ExDispatch ExRoot (AtLabel "box")) (ArTau (AtLabel "n") (ExFormation [BiLambda (FnSymbol 7)])))))) of
        EvLooped _ _ _ _ _ answer -> fmap (fmap (fmap symbols)) answer `shouldBe` Just (9, Just [12])
        _ -> expectationFailure "The record did not stay the record it was"
    it "raises the symbols an application and the object it made carry above the floor" $
      case renumbered 2 5 (EvApplied 3 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "box")) (ArTau (AtLabel "x") (ExFormation [BiLambda (FnSymbol 4)]))) (ExFormation [BiTau (AtLabel "x") (ExFormation [BiLambda (FnSymbol 1)])]) ExXi) of
        EvApplied _ _ call object _ -> (symbols call, symbols object) `shouldBe` ([9], [1])
        _ -> expectationFailure "The record did not stay the record it was"
    it "raises the symbols an object carries before and after the walk computed inside it above the floor" $
      case renumbered 3 4 (EvComputed 2 (ExFormation [BiTau (AtLabel "ш") (ExFormation [BiLambda (FnSymbol 2)])]) (ExFormation [BiTau (AtLabel "ш") (ExFormation [BiLambda (FnSymbol 6)])])) of
        EvComputed _ before after -> (symbols before, symbols after) `shouldBe` ([2], [10])
        _ -> expectationFailure "The record did not stay the record it was"
    it "raises the symbols the site of a started judgment carries above the floor" $
      case renumbered 1 3 (EvStarted 2 Morphing (ExFormation [BiLambda (FnSymbol 5)])) of
        EvStarted _ _ site -> symbols site `shouldBe` [8]
        _ -> expectationFailure "The record did not stay the record it was"

  describe "resited" $ do
    it "moves an application made at one site to another" $
      case resited (ExDispatch ExRoot (AtLabel "ёж")) ExXi (EvApplied 2 Morphing (ExFormation []) (ExFormation []) (ExDispatch ExRoot (AtLabel "ёж"))) of
        EvApplied _ _ _ _ site -> site `shouldBe` ExXi
        _ -> expectationFailure "The record did not stay the record it was"
    it "moves a firing asked for at one site to another" $
      case resited ExRoot ExXi (EvFiring 2 "L_ц" Morphing ExRoot) of
        EvFiring _ _ _ site -> site `shouldBe` ExXi
        _ -> expectationFailure "The record did not stay the record it was"
    it "moves a judgment started at one site to another" $
      case resited ExRoot ExXi (EvStarted 2 Morphing ExRoot) of
        EvStarted _ _ site -> site `shouldBe` ExXi
        _ -> expectationFailure "The record did not stay the record it was"
    it "leaves a record made at another site where it was" $
      case resited ExRoot ExXi (EvApplied 2 Morphing (ExFormation []) (ExFormation []) (ExDispatch ExRoot (AtLabel "щ"))) of
        EvApplied _ _ _ _ site -> site `shouldBe` ExDispatch ExRoot (AtLabel "щ")
        _ -> expectationFailure "The record did not stay the record it was"

  describe "saveEval" $ do
    it "writes an application as a line binding what it made to a fresh 𝑛" $ do
      (_, written) <- recorded (\record -> record (EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w"))))
      written `shouldBe` "  𝑛·0·1 := Φ.ёж( q ↦ ⟦⟧ )  # 𝕄(Φ.w)\n"
    it "spells an object an application made by its name on a later line" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvFiring 1 "L_щ" Morphing ExRoot, EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) ExRoot, EvTerm 2 "𝑛1" (ExDispatch ExXi (AtLabel "z")) (ExFormation [BiTau (AtLabel "z") (ExFormation [BiTau (AtLabel "q") (ExFormation [])]), BiTau (AtLabel "у") (ExFormation [])])])
      last (lines written) `shouldBe` "    𝑛1·1 := ⟦ z ↦ 𝑛·1·1, у ↦ ⟦⟧ ⟧  # 𝕄(ξ.z)"
    it "numbers the answer of a firing past the objects applications made inside it" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvFiring 1 "L_ю" Morphing ExRoot, EvBuilt 2 (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) ExRoot, EvAnswer 2 (ExFormation [BiTau (AtLabel "q") (ExFormation [])])])
      last (lines written) `shouldBe` "    𝑛·1·3 := 𝑛·1·2  # 𝕄(𝑛·1·1)"
    it "spells an application an earlier line wrote by its name in the argument of a later one" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) ExRoot, EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "жук")) (ArTau (AtLabel "w") (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))))) (ExFormation [BiTau (AtLabel "w") (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation [])))]) ExRoot])
      last (lines written) `shouldBe` "  𝑛·0·2 := Φ.жук( w ↦ 𝑛·0·1 )  # 𝕄(Φ)"
    it "spells an application made again by its head and argument rather than by the name of the first one" $ do
      (_, written) <- recorded (\record -> replicateM_ 2 (record (EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) ExRoot)))
      last (lines written) `shouldBe` "  𝑛·0·2 := Φ.ёж( q ↦ ⟦⟧ )  # 𝕄(Φ)"
    it "writes a deferred copy as the call it was made of even when an earlier application spelled that call" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 3)]))) (ExFormation [BiTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 3)])]) ExRoot, EvDeferred 1 4 Morphing (ExFormation [BiTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 3)])]) (Just (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 3)])))) ExRoot])
      last (lines written) `shouldBe` "  deferred(𝜎4) := Φ.ёж( q ↦ 𝜎3:λ )  # 𝕄(Φ)"
    it "spells an object the walk computed inside by the name its application gave it" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvFiring 1 "L_ъ" Morphing ExRoot, EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ф")))) (ExFormation [BiTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ф"))]) ExRoot, EvComputed 2 (ExFormation [BiTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ф"))]) (ExFormation [BiTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 8)])]), EvTerm 2 "𝑛1" (ExDispatch ExXi (AtLabel "z")) (ExFormation [BiTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 8)])])])
      last (lines written) `shouldBe` "    𝑛1·1 := 𝑛·1·1  # 𝕄(ξ.z)"
    it "spells out an object the walk computed inside when no application named it" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvComputed 1 (ExFormation [BiTau (AtLabel "ю") ExRoot]) (ExFormation [BiTau (AtLabel "ю") (ExFormation [BiLambda (FnSymbol 3)])]), EvTerm 1 "𝑛1" (ExDispatch ExXi (AtLabel "z")) (ExFormation [BiTau (AtLabel "ю") (ExFormation [BiLambda (FnSymbol 3)])])])
      last (lines written) `shouldBe` "  𝑛1·0 := 𝜎3:λ:ю  # 𝕄(ξ.z)"
    it "writes the datum the delta rule found under a name of its own" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvFiring 1 "L_ж" Dataization ExRoot, EvDelta 2 (BtMany ["1F", "E0"]), EvDelta 2 (BtOne "33")])
      last (lines written) `shouldBe` "    𝛿·1·2 := 33-"
    it "comments a datum of eight bytes with the whole number it holds as a double" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvFiring 1 "L_ж" Dataization ExRoot, EvDelta 2 (BtMany ["40", "45", "00", "00", "00", "00", "00", "00"])])
      last (lines written) `shouldBe` "    𝛿·1·1 := 40-45-00-00-00-00-00-00  # 42.0"
    it "comments a datum of eight bytes with the fraction it holds" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvFiring 1 "L_ж" Dataization ExRoot, EvDelta 2 (BtMany ["C0", "09", "1E", "B8", "51", "EB", "85", "1F"])])
      last (lines written) `shouldBe` "    𝛿·1·1 := C0-09-1E-B8-51-EB-85-1F  # -3.14"
    it "spells the datum of an operand by the name the delta rule just gave it" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvFiring 1 "L_ж" Dataization ExRoot, EvDelta 3 (BtOne "7F"), EvData 2 "𝛿1" (ExDispatch ExXi (AtLabel "щ")) (Right (BtOne "7F"))])
      last (lines written) `shouldBe` "    𝛿1·1 := 𝛿·1·1  # 𝔻(ξ.щ)"
    it "spells the datum of an operand in full when the line before it found no datum" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvFiring 1 "L_ж" Dataization ExRoot, EvDelta 3 (BtOne "7F"), EvFiring 2 "L_з" Dataization ExRoot, EvData 2 "𝛿1" (ExDispatch ExXi (AtLabel "щ")) (Right (BtOne "7F"))])
      last (lines written) `shouldBe` "    𝛿1·1 := 7F-  # 𝔻(ξ.щ)"
    it "writes no heading for a judgment nothing stood under" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvDelta 1 (BtOne "0A")])
      lines written `shouldBe` ["𝔻(Φ):", "  𝛿·0·1 := 0A-"]
    it "writes the heading of a judgment above the first line under it and drops the comment naming it" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w"))])
      lines written `shouldBe` ["𝔻(Φ):", "  𝕄(Φ.w):", "    𝑛·0·1 := Φ.ёж( q ↦ ⟦⟧ )"]
    it "keeps the comment of a line made at a site other than the one of its block" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ.w", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "е"))])
      last (lines written) `shouldBe` "  𝑛·0·1 := Φ.ёж( q ↦ ⟦⟧ )  # 𝕄(Φ.е)"
    it "keeps the comment of a line made by a judgment other than the one of its block" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ.w", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w"))])
      last (lines written) `shouldBe` "  𝑛·0·1 := Φ.ёж( q ↦ ⟦⟧ )  # 𝕄(Φ.w)"
    it "drops the comment of a firing the judgment of its block asked for at its site" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvFiring 1 "L_ш" Dataization ExRoot])
      last (lines written) `shouldBe` "  𝔼(L_ш):"
    it "keeps the comment of a line standing in a firing" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ш" Morphing ExRoot, EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) ExRoot])
      last (lines written) `shouldBe` "    𝑛·1·1 := Φ.ёж( q ↦ ⟦⟧ )  # 𝕄(Φ)"
    it "writes one heading for two blocks of one judgment at one site in a row" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w")), EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w"))])
      length (filter (== "  𝕄(Φ.w):") (lines written)) `shouldBe` 1
    it "writes the heading again for a block that follows a line beside it" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w")), EvDelta 1 (BtOne "0B"), EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w"))])
      length (filter (== "  𝕄(Φ.w):") (lines written)) `shouldBe` 2
    it "keeps the mode of a cut made at the site of its block" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvLooped 1 Morphing Proven (ExFormation []) ExRoot Nothing])
      last (lines written) `shouldBe` "  looped(⟦⟧)  # proven"
    it "names the block over an answer by the line that built it" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvFiring 1 "L_ф" Dataization ExRoot, EvBuilt 2 (ExDispatch ExRoot (AtLabel "ю")), EvStarted 2 Morphing (ExDispatch ExRoot (AtLabel "ю")), EvApplied 3 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "ю"))])
      lines written `shouldContain` ["    𝕄(𝑛·1·1):"]
    it "writes no line for an object the walk computed inside" $ do
      (_, written) <- recorded (\record -> record (EvComputed 1 (ExFormation [BiTau (AtLabel "ю") ExRoot]) (ExFormation [BiTau (AtLabel "ю") (ExFormation [BiLambda (FnSymbol 3)])])))
      written `shouldBe` ""

  describe "saveEvalXml" $ do
    it "writes an application as an element naming its head and holding its argument" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ.w", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w"))])
      lines written `shouldContain` ["  <applied meta=\"𝑛·0·1\" by=\"morph\" at=\"Φ.w\" of=\"Φ.ёж\"><attr name=\"q\">⟦⟧</attr></applied>"]
    it "spells an argument an earlier application made by its name" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) ExRoot, EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "жук")) (ArTau (AtLabel "w") (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))))) (ExFormation [BiTau (AtLabel "w") (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation [])))]) ExRoot])
      lines written `shouldContain` ["  <applied meta=\"𝑛·0·2\" by=\"morph\" at=\"Φ\" of=\"Φ.жук\"><attr name=\"w\">𝑛·0·1</attr></applied>"]
    it "spells an argument that is a bare symbol as that symbol" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "цапля")) (ArTau AtPhi (ExFormation [BiLambda (FnSymbol 7)]))) (ExFormation [BiTau AtPhi (ExFormation [BiLambda (FnSymbol 7)])]) ExRoot])
      lines written `shouldContain` ["  <applied meta=\"𝑛·0·1\" by=\"morph\" at=\"Φ\" of=\"Φ.цапля\"><attr name=\"φ\">𝜎7</attr></applied>"]
    it "spells an object an application made by its name in a later element" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ы" Morphing ExRoot, EvBuilt 2 (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) ExRoot, EvAnswer 2 (ExFormation [BiTau (AtLabel "q") (ExFormation [])])])
      lines written `shouldContain` ["    <answer meta=\"𝑛·1·3\">𝑛·1·2</answer>"]
    it "writes the datum the delta rule found as an element with its name" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvDelta 1 (BtMany ["0C", "D4"])])
      lines written `shouldContain` ["  <delta meta=\"𝛿·0·1\">0C-D4</delta>"]
    it "writes the double a datum of eight bytes holds as the number of its delta element" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvDelta 1 (BtMany ["C0", "09", "1E", "B8", "51", "EB", "85", "1F"])])
      lines written `shouldContain` ["  <delta meta=\"𝛿·0·1\" number=\"-3.14\">C0-09-1E-B8-51-EB-85-1F</delta>"]
    it "spells the datum of an operand by the name the delta rule just gave it in the markup" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvFiring 1 "L_ж" Dataization ExRoot, EvDelta 3 (BtOne "7F"), EvData 2 "𝛿1" (ExDispatch ExXi (AtLabel "щ")) (Right (BtOne "7F"))])
      lines written `shouldContain` ["    <bind meta=\"𝛿1·1\">𝛿·1·1</bind>"]
    it "spells an object the walk computed inside by its name in a later element" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ф")))) (ExFormation [BiTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ф"))]) ExRoot, EvComputed 1 (ExFormation [BiTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ф"))]) (ExFormation [BiTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 8)])]), EvTerm 1 "𝑛1" (ExDispatch ExXi (AtLabel "z")) (ExFormation [BiTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 8)])])])
      lines written `shouldContain` ["  <bind meta=\"𝑛1·0\">𝑛·0·1</bind>"]
    it "writes a block as an element once an element stands in it" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w"))])
      lines written `shouldContain` ["  <morph at=\"Φ.w\">"]
    it "writes no element for a block nothing stands in" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvDelta 1 (BtOne "0C")])
      filter (isInfixOf "morph") (lines written) `shouldBe` []
    it "writes one element for two blocks of one judgment at one site in a row" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w")), EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w"))])
      length (filter (isInfixOf "<morph") (lines written)) `shouldBe` 1
    it "closes a block element before the element that follows beside it" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w")), EvDelta 1 (BtOne "0D")])
      lines written `shouldContain` ["  </morph>", "  <delta meta=\"𝛿·0·1\">0D-</delta>"]
    it "names the element over an answer by the element that built it" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvFiring 1 "L_ф" Dataization ExRoot, EvBuilt 2 (ExDispatch ExRoot (AtLabel "ю")), EvStarted 2 Morphing (ExDispatch ExRoot (AtLabel "ю")), EvApplied 3 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "ю"))])
      lines written `shouldContain` ["    <morph at=\"𝑛·1·1\">"]

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
