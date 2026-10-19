{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module DepsSpec where

import AST (Argument (ArTau), Attribute (AtLabel, AtPhi, AtRho), Binding (BiDelta, BiLambda, BiTau, BiVoid), Bytes (BtMany, BtOne), Expression (ExApplication, ExDispatch, ExFormation, ExRoot, ExXi), Function (FnSymbol, Function))
import Control.Exception (bracket)
import Control.Monad (replicateM_, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf, isPrefixOf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Deps (Acyclic (Proven), Evaluation (EvAnswer, EvApplied, EvBuilt, EvData, EvDeferred, EvDelta, EvFiring, EvForked, EvJoined, EvLooped, EvRun, EvStarted, EvStuck, EvStuckOn, EvTaken, EvTerm, EvWorld), Judgment (Dataization, Morphing), Nesting (..), Protocol (..), Side (LeftSide, RightSide), dontSaveEval, dontSaveStep, emptyNesting, emptyProgress, emptyProtocol, endEval, endEvalXml, perSecond, progressed, resited, saveStep)
import Fixtures (readUtf8, recorded, recorded', recordedWhole, recordedWhole', recordedXml, recordedXml')
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
    it "writes nothing for the world it is told" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvWorld (ExFormation [BiTau (AtLabel "ёж") (ExFormation [])])])
      filter (isInfixOf "ёж") (lines written) `shouldBe` []
    it "writes an application as a line binding what it made to a fresh 𝑛" $ do
      (_, written) <- recorded (\record -> record (EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w"))))
      written `shouldBe` "  𝑛·0·1 := Φ.ёж( q ↦ ⟦⟧ )  # 𝕄(Φ.w)\n"
    it "spells an object an application made by its name on a later line" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvFiring 1 "L_щ" Morphing ExRoot, EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) ExRoot, EvTerm 2 "𝑛1" Morphing (ExDispatch ExXi (AtLabel "z")) (ExFormation [BiTau (AtLabel "z") (ExFormation [BiTau (AtLabel "q") (ExFormation [])]), BiTau (AtLabel "у") (ExFormation [])])])
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
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvLooped 1 Morphing Proven (ExFormation []) ExRoot])
      last (lines written) `shouldBe` "  looped(⟦⟧)  # proven"
    it "names the block over an answer by the line that built it" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvFiring 1 "L_ф" Dataization ExRoot, EvBuilt 2 (ExDispatch ExRoot (AtLabel "ю")), EvStarted 2 Morphing (ExDispatch ExRoot (AtLabel "ю")), EvApplied 3 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "ю"))])
      lines written `shouldContain` ["    𝕄(𝑛·1·1):"]
    it "writes a deferred copy as the call of the object it was made of" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvDeferred 1 9 Morphing (ExFormation [BiTau (AtLabel "щ") (ExFormation [BiLambda (FnSymbol 4)]), BiTau AtPhi (ExDispatch (ExDispatch ExXi (AtLabel "щ")) (AtLabel "й"))]) (Just (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "щ") (ExFormation [BiLambda (FnSymbol 4)])))) ExRoot])
      last (lines written) `shouldBe` "  deferred(𝜎9) := Φ.ёж( щ ↦ 𝜎4:λ )"
    it "writes a deferred copy as it stands when no object of the world made it" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvDeferred 1 9 Morphing (ExFormation [BiTau (AtLabel "щ") (ExFormation [BiLambda (FnSymbol 4)]), BiTau AtPhi (ExDispatch (ExDispatch ExXi (AtLabel "щ")) (AtLabel "й"))]) Nothing ExRoot])
      last (lines written) `shouldBe` "  deferred(𝜎9) := ⟦ щ ↦ 𝜎4:λ, φ ↦ щ.й ⟧"
    it "writes the side a fork took beside the symbol of its condition" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvForked 2 (Left 7) LeftSide (ExDispatch ExXi (AtLabel "then"))])
      last (lines written) `shouldBe` "    forked(𝔻(𝜎7:λ)) := left  # ξ.then"
    it "writes the side a fork took beside the datum of its condition" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvForked 2 (Right (BtOne "0D")) RightSide (ExDispatch ExXi (AtLabel "else"))])
      last (lines written) `shouldBe` "    forked(0D-) := right  # ξ.else"
    it "writes the side a fork came to as the answer of its firing" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvTaken 2 [ExDispatch ExXi (AtLabel "then")] (ExFormation [BiLambda (FnSymbol 3)])])
      last (lines written) `shouldBe` "    𝑛·1·1 := 𝜎3:λ  # 𝕄(ξ.then)"
    it "writes the term both sides of a fork came to beside both of them" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvTaken 2 [ExDispatch ExXi (AtLabel "then"), ExDispatch ExXi (AtLabel "else")] (ExFormation [BiLambda (FnSymbol 3)])])
      last (lines written) `shouldBe` "    𝑛·1·1 := 𝜎3:λ  # 𝕄(ξ.then), 𝕄(ξ.else)"
    it "writes the symbol a join minted beside the symbol of its condition and the two it stands for" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvJoined 2 (Left 7) 12 (5, 9)])
      last (lines written) `shouldBe` "    joined(𝜎12) := 𝔻(𝜎7:λ) ? 𝜎5 : 𝜎9"
    it "writes the symbol a join minted beside the datum of its condition" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvJoined 2 (Right (BtOne "0D")) 14 (3, 8)])
      last (lines written) `shouldBe` "    joined(𝜎14) := 0D- ? 𝜎3 : 𝜎8"
    it "opens a heading of its own for every run of the protocol" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvDelta 1 (BtOne "0C"), EvRun Morphing "Φ", EvDelta 1 (BtOne "0D")])
      lines written `shouldBe` ["𝕄(Φ):", "  𝛿·0·1 := 0C-", "𝕄(Φ):", "  𝛿·0·2 := 0D-"]
  describe "saveEvalXml" $ do
    it "writes an application as an element naming its head and holding its argument" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ.w", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю")))) (ExFormation [BiTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю"))]) (ExDispatch ExRoot (AtLabel "w"))])
      lines written `shouldContain` ["  <applied meta=\"𝑛·0·1\" by=\"morph\" at=\"Φ.w\" of=\"Φ.ёж\"><attr name=\"q\">Φ.ю</attr></applied>"]
    it "spells an argument an earlier application made by its name" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю")))) (ExFormation [BiTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю"))]) ExRoot, EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "жук")) (ArTau (AtLabel "w") (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю")))))) (ExFormation [BiTau (AtLabel "w") (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю"))))]) ExRoot])
      lines written `shouldContain` ["  <applied meta=\"𝑛·0·2\" by=\"morph\" at=\"Φ\" of=\"Φ.жук\"><attr name=\"w\">𝑛·0·1</attr></applied>"]
    it "spells an argument that is a bare symbol as that symbol" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "цапля")) (ArTau AtPhi (ExFormation [BiLambda (FnSymbol 7)]))) (ExFormation [BiTau AtPhi (ExFormation [BiLambda (FnSymbol 7)])]) ExRoot])
      lines written `shouldContain` ["  <applied meta=\"𝑛·0·1\" by=\"morph\" at=\"Φ\" of=\"Φ.цапля\"><attr name=\"φ\">𝜎7</attr></applied>"]
    it "spells a datum an application holds with no tag" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "байты")) (ArTau AtPhi (ExFormation [BiDelta (BtOne "0C")]))) (ExFormation [BiTau AtPhi (ExFormation [BiDelta (BtOne "0C")])]) ExRoot])
      lines written `shouldContain` ["  <applied meta=\"𝑛·0·1\" by=\"morph\" at=\"Φ\" of=\"Φ.байты\"><attr name=\"φ\">0C-</attr></applied>"]
    it "names an argument that is an application by an element of its own" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "цапля")) (ArTau AtPhi (ExApplication (ExDispatch ExRoot (AtLabel "байты")) (ArTau AtPhi (ExFormation [BiDelta (BtOne "0C")]))))) (ExFormation [BiTau AtPhi (ExFormation [BiDelta (BtOne "0C")])]) ExRoot])
      lines written `shouldContain` ["  <applied meta=\"𝑛·0·1\" of=\"Φ.байты\"><attr name=\"φ\">0C-</attr></applied>"]
    it "writes an application of two arguments nothing reduced as one element" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvAnswer 1 (ExApplication (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "а") (ExFormation [BiLambda (FnSymbol 1)]))) (ArTau (AtLabel "б") (ExFormation [BiLambda (FnSymbol 2)])))])
      lines written `shouldContain` ["  <applied meta=\"𝑛·0·1\" of=\"Φ.ёж\"><attr name=\"а\">𝜎1</attr><attr name=\"б\">𝜎2</attr></applied>"]
    it "writes an answer as the name of the term it holds" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvAnswer 1 (ExApplication (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "а") (ExFormation [BiLambda (FnSymbol 1)]))) (ArTau (AtLabel "б") (ExFormation [BiLambda (FnSymbol 2)])))])
      lines written `shouldContain` ["  <answer meta=\"𝑛·0·2\">𝑛·0·1</answer>"]
    it "writes a term a firing built as the name of the term" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvBuilt 1 (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "а") (ExFormation [BiLambda (FnSymbol 1)])))])
      lines written `shouldContain` ["  <built meta=\"𝑛·0·2\">𝑛·0·1</built>"]
    it "spells an object an application made by its name in a later element" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ы" Morphing ExRoot, EvBuilt 2 (ExDispatch ExRoot (AtLabel "ю")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю")))) (ExFormation [BiTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю"))]) ExRoot, EvAnswer 2 (ExFormation [BiTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю"))])])
      lines written `shouldContain` ["    <answer meta=\"𝑛·1·3\">𝑛·1·2</answer>"]
    it "writes a formation no object of the world made as an element holding its bindings" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvAnswer 1 (ExFormation [BiLambda (Function "L_вилка"), BiVoid (AtLabel "лево"), BiTau AtPhi (ExFormation [BiLambda (FnSymbol 5)])])])
      lines written `shouldContain` ["  <formed meta=\"𝑛·0·1\" λ=\"L_вилка\"><void name=\"лево\"/><attr name=\"φ\">𝜎5</attr></formed>"]
    it "names a formation that copies an object of the world by the object and the attributes it binds" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvWorld (ExFormation [BiTau (AtLabel "ёж") (ExFormation [BiVoid (AtLabel "q"), BiTau (AtLabel "w") ExRoot])]), EvAnswer 1 (ExFormation [BiTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 3)]), BiTau (AtLabel "w") ExRoot])])
      lines written `shouldContain` ["  <formed meta=\"𝑛·0·1\" of=\"Φ.ёж\"><attr name=\"q\">𝜎3</attr></formed>"]
    it "names a formation that replaced an attribute of an object of the world by the object and that attribute" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvWorld (ExFormation [BiTau (AtLabel "ёж") (ExFormation [BiVoid (AtLabel "q"), BiTau AtPhi (ExDispatch ExXi (AtLabel "q")), BiTau (AtLabel "w") ExRoot])]), EvAnswer 1 (ExFormation [BiTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 3)]), BiTau AtPhi (ExFormation [BiLambda (FnSymbol 5)]), BiTau (AtLabel "w") ExRoot])])
      lines written `shouldContain` ["  <formed meta=\"𝑛·0·1\" of=\"Φ.ёж\"><attr name=\"q\">𝜎3</attr><attr name=\"φ\">𝜎5</attr></formed>"]
    it "writes a formation that keeps no binding of an object of the world as a formation of its own" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvWorld (ExFormation [BiTau (AtLabel "ёж") (ExFormation [BiVoid AtPhi])]), EvAnswer 1 (ExFormation [BiTau AtPhi (ExFormation [BiLambda (FnSymbol 8)])])])
      lines written `shouldContain` ["  <formed meta=\"𝑛·0·1\"><attr name=\"φ\">𝜎8</attr></formed>"]
    it "spells a formation that is an object of the world by its path" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvWorld (ExFormation [BiTau (AtLabel "ёж") (ExFormation [BiVoid (AtLabel "q"), BiTau (AtLabel "w") ExRoot])]), EvAnswer 1 (ExFormation [BiVoid (AtLabel "q"), BiTau (AtLabel "w") ExRoot])])
      lines written `shouldContain` ["  <answer meta=\"𝑛·0·1\">Φ.ёж</answer>"]
    it "names a formation that copies an object nested in the world by its path" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvWorld (ExFormation [BiTau (AtLabel "лес") (ExFormation [BiTau (AtLabel "ёж") (ExFormation [BiVoid (AtLabel "q"), BiLambda (Function "L_ёж")])])]), EvAnswer 1 (ExFormation [BiTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 7)]), BiLambda (Function "L_ёж")])])
      lines written `shouldContain` ["  <formed meta=\"𝑛·0·1\" of=\"Φ.лес.ёж\"><attr name=\"q\">𝜎7</attr></formed>"]
    it "names a method of a copy by the copy it is dispatched from" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvWorld (ExFormation [BiTau (AtLabel "ёж") (ExFormation [BiVoid (AtLabel "q"), BiTau (AtLabel "м") (ExFormation [BiVoid AtRho, BiVoid (AtLabel "z")])])]), EvAnswer 1 (ExFormation [BiTau AtRho (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 3)]))), BiTau (AtLabel "z") (ExFormation [BiLambda (FnSymbol 4)])])])
      lines written `shouldContain` ["  <formed meta=\"𝑛·0·2\" of=\"𝑛·0·1.м\"><attr name=\"z\">𝜎4</attr></formed>"]
    it "writes nothing for the world it is told" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvWorld (ExFormation [BiTau (AtLabel "ёж") (ExFormation [])])])
      filter (isInfixOf "ёж") (lines written) `shouldBe` []
    it "writes the datum the delta rule found as an element with its name" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvDelta 1 (BtMany ["0C", "D4"])])
      lines written `shouldContain` ["  <delta meta=\"𝛿·0·1\">0C-D4</delta>"]
    it "writes the double a datum of eight bytes holds as the number of its delta element" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvDelta 1 (BtMany ["C0", "09", "1E", "B8", "51", "EB", "85", "1F"])])
      lines written `shouldContain` ["  <delta meta=\"𝛿·0·1\" number=\"-3.14\">C0-09-1E-B8-51-EB-85-1F</delta>"]
    it "spells the datum of an operand by the name the delta rule just gave it in the markup" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvFiring 1 "L_ж" Dataization ExRoot, EvDelta 3 (BtOne "7F"), EvData 2 "𝛿1" (ExDispatch ExXi (AtLabel "щ")) (Right (BtOne "7F"))])
      lines written `shouldContain` ["    <bind meta=\"𝛿1·1\">𝛿·1·1</bind>"]
    it "spells the symbol a dataization stood on with no tag" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvFiring 1 "L_ж" Dataization ExRoot, EvData 2 "𝛿1" (ExDispatch ExXi (AtLabel "щ")) (Left 32)])
      lines written `shouldContain` ["    <dataize meta=\"𝛿1·1\">𝜎32</dataize>"]
    it "writes a block as an element once an element stands in it" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w"))])
      lines written `shouldContain` ["  <morph at=\"Φ.w\">"]
    it "writes no element for a block nothing stands in" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvDelta 1 (BtOne "0C")])
      filter (isInfixOf "morph") (lines written) `shouldBe` []
    it "names the site of a block that is an application by an element written before the block" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ж")) (ArTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 1)]))), EvDelta 2 (BtOne "0C")])
      lines written `shouldContain` ["  <applied meta=\"𝑛·0·1\" of=\"Φ.ж\"><attr name=\"q\">𝜎1</attr></applied>", "  <morph at=\"𝑛·0·1\">"]
    it "names nothing for the site of a block nothing stands in" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ж")) (ArTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 1)]))), EvDelta 1 (BtOne "0C")])
      filter (isInfixOf "applied") (lines written) `shouldBe` []
    it "names the site of a firing that is an application by an element written before the firing" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ж" Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ж")) (ArTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 1)])))])
      lines written `shouldContain` ["  <applied meta=\"𝑛·0·1\" of=\"Φ.ж\"><attr name=\"q\">𝜎1</attr></applied>", "  <evaluate λ=\"L_ж\" by=\"morph\" at=\"𝑛·0·1\">"]
    it "writes one element for two blocks of one judgment at one site in a row" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w")), EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w"))])
      length (filter (isInfixOf "<morph") (lines written)) `shouldBe` 1
    it "closes a block element before the element that follows beside it" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w")), EvDelta 1 (BtOne "0D")])
      lines written `shouldContain` ["  </morph>", "  <delta meta=\"𝛿·0·1\">0D-</delta>"]
    it "names the element over an answer by the element that built it" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvFiring 1 "L_ф" Dataization ExRoot, EvBuilt 2 (ExDispatch ExRoot (AtLabel "ю")), EvStarted 2 Morphing (ExDispatch ExRoot (AtLabel "ю")), EvApplied 3 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "ю"))])
      lines written `shouldContain` ["    <morph at=\"𝑛·1·1\">"]
    it "writes a deferred copy as an element naming its object and the symbols of its arguments" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvDeferred 1 9 Morphing (ExFormation [BiTau (AtLabel "щ") (ExFormation [BiLambda (FnSymbol 4)]), BiTau AtPhi (ExDispatch (ExDispatch ExXi (AtLabel "щ")) (AtLabel "й"))]) (Just (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "щ") (ExFormation [BiLambda (FnSymbol 4)])))) ExRoot])
      lines written `shouldContain` ["  <deferred symbol=\"𝜎9\" by=\"morph\" at=\"Φ\" of=\"Φ.ёж\"><with><attr name=\"щ\">𝜎4</attr></with></deferred>"]
    it "writes a deferred copy that names no call as an element naming the copy" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvDeferred 1 9 Morphing (ExFormation [BiTau (AtLabel "щ") (ExFormation [BiLambda (FnSymbol 4)])]) Nothing ExRoot])
      lines written `shouldContain` ["  <deferred symbol=\"𝜎9\" by=\"morph\" at=\"Φ\" of=\"𝑛·0·1\"/>"]
    it "names an argument of a copy nothing worked out by an expression meta" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvDeferred 1 9 Morphing (ExFormation []) (Just (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "щ") (ExApplication (ExDispatch ExRoot (AtLabel "ю")) (ArTau (AtLabel "ж") (ExFormation [BiLambda (FnSymbol 2)])))))) ExRoot])
      lines written `shouldSatisfy` any (isInfixOf "<attr name=\"щ\">𝑒1</attr>")
    it "names one argument nothing worked out by one expression meta in two elements" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvDeferred 1 9 Morphing (ExFormation []) (Just (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "щ") (ExApplication (ExDispatch ExRoot (AtLabel "ю")) (ArTau (AtLabel "ж") (ExFormation [BiLambda (FnSymbol 2)])))))) ExRoot, EvDeferred 1 10 Morphing (ExFormation []) (Just (ExApplication (ExDispatch ExRoot (AtLabel "ёрш")) (ArTau (AtLabel "ц") (ExApplication (ExDispatch ExRoot (AtLabel "ю")) (ArTau (AtLabel "ж") (ExFormation [BiLambda (FnSymbol 2)])))))) ExRoot])
      length (filter (isInfixOf ">𝑒1<") (lines written)) `shouldBe` 2
    it "writes a cut that answered no copy as an element naming the formation it entered" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvLooped 1 Morphing Proven (ExFormation [BiTau (AtLabel "ъ") ExRoot]) ExRoot])
      lines written `shouldContain` ["  <looped by=\"morph\" match=\"proven\" at=\"Φ\" of=\"𝑛·0·1\"/>"]
    it "names an argument of a deferred copy by the application that made it" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "кулик")) (ArTau (AtLabel "ш") ExRoot)) (ExFormation [BiTau (AtLabel "ш") ExRoot]) ExRoot, EvDeferred 1 31 Morphing (ExFormation [BiTau (AtLabel "щ") (ExFormation [BiTau (AtLabel "ш") ExRoot]), BiTau AtPhi (ExDispatch ExXi (AtLabel "щ"))]) (Just (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "щ") (ExFormation [BiTau (AtLabel "ш") ExRoot])))) ExRoot])
      lines written `shouldContain` ["  <deferred symbol=\"𝜎31\" by=\"morph\" at=\"Φ\" of=\"Φ.ёж\"><with><attr name=\"щ\">𝑛·0·1</attr></with></deferred>"]
    it "writes the side a fork took as an element naming the symbol of its condition" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvForked 2 (Left 7) LeftSide (ExDispatch ExXi (AtLabel "then"))])
      lines written `shouldContain` ["    <forked side=\"left\">𝜎7</forked>"]
    it "writes the side a fork took as an element holding the datum of its condition" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvForked 2 (Right (BtOne "0D")) RightSide (ExDispatch ExXi (AtLabel "else"))])
      lines written `shouldContain` ["    <forked side=\"right\">0D-</forked>"]
    it "writes the side a fork came to as the answer of its firing in the markup" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvTaken 2 [ExDispatch ExXi (AtLabel "then")] (ExFormation [BiLambda (FnSymbol 3)])])
      lines written `shouldContain` ["    <answer meta=\"𝑛·1·1\">𝜎3</answer>"]
    it "writes the symbol a join minted as an element naming the two it stands for" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvJoined 2 (Left 7) 12 (5, 9)])
      lines written `shouldContain` ["    <joined symbol=\"𝜎12\" left=\"𝜎5\" right=\"𝜎9\">𝜎7</joined>"]
    it "writes the symbol a join minted as an element holding the datum of its condition" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvJoined 2 (Right (BtOne "0D")) 14 (3, 8)])
      lines written `shouldContain` ["    <joined symbol=\"𝜎14\" left=\"𝜎3\" right=\"𝜎8\">0D-</joined>"]
    it "wraps every run of the protocol in a path element" $ do
      (_, written) <- recordedWhole (\record -> mapM_ record [EvRun Morphing "Φ", EvDelta 1 (BtOne "0C"), EvRun Morphing "Φ", EvDelta 1 (BtOne "0D")])
      filter (isInfixOf "path>") (lines written) `shouldBe` ["  <path>", "  </path>", "  <path>", "  </path>"]
    it "indents the elements of a run inside its path" $ do
      (_, written) <- recordedWhole (\record -> mapM_ record [EvRun Morphing "Φ", EvDelta 1 (BtOne "0C")])
      filter (isInfixOf "morph") (lines written) `shouldBe` ["    <morph at=\"Φ\">", "    </morph>"]
    it "writes a firing no entry answered as an element naming the term it got stuck on" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_щ" Morphing ExRoot, EvStuck 2 "L_щ" Morphing (ExFormation [BiLambda (FnSymbol 120)])])
      lines written `shouldContain` ["    <unanswered λ=\"L_щ\" by=\"morph\">𝜎120</unanswered>"]
    it "writes a stuck firing as an element of the word the text uses" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_щ" Morphing ExRoot, EvStuckOn 2 "L_жук"])
      lines written `shouldContain` ["    <stuck λ=\"L_жук\"/>"]

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

  describe "rewindEval" $ do
    it "reopens the blocks the mark stood in before the first line after the rewind" $ do
      (_, written) <- recorded' False $ \record rewind -> do
        record (EvRun Dataization "Φ.ёж")
        record (EvFiring 1 "L_ж" Dataization (ExDispatch ExRoot (AtLabel "ёж")))
        restore <- rewind
        record (EvDelta 2 (BtOne "1A"))
        restore
        record (EvDelta 2 (BtOne "2B"))
      lines written `shouldBe` ["𝔻(Φ.ёж):", "  𝔼(L_ж):", "    𝛿·1·1 := 1A-", "𝔻(Φ.ёж):", "  𝔼(L_ж):", "    𝛿·1·2 := 2B-"]
    it "writes nothing at the rewind before a line follows it" $ do
      (_, written) <- recorded' False $ \record rewind -> do
        record (EvRun Dataization "Φ.щ")
        record (EvFiring 1 "L_щ" Dataization (ExDispatch ExRoot (AtLabel "щ")))
        restore <- rewind
        record (EvDelta 2 (BtOne "3C"))
        restore
      lines written `shouldBe` ["𝔻(Φ.щ):", "  𝔼(L_щ):", "    𝛿·1·1 := 3C-"]
    it "reopens no block the mark stood in that the first line after the rewind leaves" $ do
      (_, written) <- recorded' False $ \record rewind -> do
        record (EvRun Dataization "Φ.ю")
        record (EvFiring 1 "L_ю" Dataization (ExDispatch ExRoot (AtLabel "ю")))
        record (EvFiring 2 "L_я" Dataization (ExDispatch ExRoot (AtLabel "я")))
        record (EvDelta 3 (BtOne "4D"))
        restore <- rewind
        record (EvDelta 2 (BtOne "5E"))
        restore
        record (EvDelta 2 (BtOne "6F"))
      drop 5 (lines written) `shouldBe` ["𝔻(Φ.ю):", "  𝔼(L_ю):", "    𝛿·1·2 := 6F-"]
    it "spells an object the rewound path named in full after the rewind" $ do
      (_, written) <- recorded' False $ \record rewind -> do
        record (EvRun Morphing "Φ.ёж")
        restore <- rewind
        record (EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) ExRoot)
        restore
        record (EvTerm 1 "𝑛1" Morphing (ExDispatch ExXi (AtLabel "z")) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]))
      last (lines written) `shouldBe` "  𝑛1·0 := ⟦⟧:q  # 𝕄(ξ.z)"

  describe "rewindEvalXml" $ do
    it "reopens the elements the mark stood in before the first element after the rewind" $ do
      (_, written) <- recordedXml' $ \record rewind -> do
        record (EvRun Dataization "Φ.ёж")
        record (EvFiring 1 "L_ж" Dataization (ExDispatch ExRoot (AtLabel "ёж")))
        restore <- rewind
        record (EvDelta 2 (BtOne "1A"))
        restore
        record (EvDelta 2 (BtOne "2B"))
      lines written
        `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                   , "<dataize at=\"Φ.ёж\">"
                   , "  <evaluate λ=\"L_ж\" by=\"dataize\" at=\"Φ.ёж\">"
                   , "    <delta meta=\"𝛿·1·1\">1A-</delta>"
                   , "  </evaluate>"
                   , "</dataize>"
                   , "<dataize at=\"Φ.ёж\">"
                   , "  <evaluate λ=\"L_ж\" by=\"dataize\" at=\"Φ.ёж\">"
                   , "    <delta meta=\"𝛿·1·2\">2B-</delta>"
                   , "  </evaluate>"
                   , "</dataize>"
                   ]
    it "reopens no element the mark stood in that the first element after the rewind closes" $ do
      (_, written) <- recordedXml' $ \record rewind -> do
        record (EvRun Dataization "Φ.ю")
        record (EvFiring 1 "L_ю" Dataization (ExDispatch ExRoot (AtLabel "ю")))
        record (EvFiring 2 "L_я" Dataization (ExDispatch ExRoot (AtLabel "я")))
        record (EvDelta 3 (BtOne "4D"))
        restore <- rewind
        record (EvDelta 2 (BtOne "5E"))
        restore
        record (EvDelta 2 (BtOne "6F"))
      drop 9 (lines written)
        `shouldBe` [ "<dataize at=\"Φ.ю\">"
                   , "  <evaluate λ=\"L_ю\" by=\"dataize\" at=\"Φ.ю\">"
                   , "    <delta meta=\"𝛿·1·2\">6F-</delta>"
                   , "  </evaluate>"
                   , "</dataize>"
                   ]
    it "closes the path it rewinds from before the path it reopens" $ do
      (_, written) <- recordedWhole' $ \record rewind -> do
        record (EvRun Dataization "Φ.щ")
        restore <- rewind
        record (EvDelta 1 (BtOne "3C"))
        restore
        record (EvDelta 1 (BtOne "4D"))
      filter (isInfixOf "path>") (lines written) `shouldBe` ["  <path>", "  </path>", "  <path>", "  </path>"]

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
      cursor <- newIORef emptyNesting{_fires = 3, _closing = [(1, "evaluate", "<evaluate>", True), (0, "morph", "<morph>", True), (-1, "path", "<path>", True), (-2, "protocol", "<protocol>", True)]}
      began <- getMonotonicTime
      withFile path WriteMode (\handle -> endEvalXml handle cursor began)
      content <- readUtf8 path
      take 2 (lines content) `shouldBe` ["      </evaluate>", "    </morph>"]

    it "names the firings of the run it closes" $ withScratchDir $ \dir -> do
      let path = dir </> "protocol.xml"
      createDirectoryIfMissing True dir
      cursor <- newIORef emptyNesting{_fires = 3, _closing = [(0, "morph", "<morph>", True), (-1, "path", "<path>", True), (-2, "protocol", "<protocol>", True)]}
      began <- getMonotonicTime
      withFile path WriteMode (\handle -> endEvalXml handle cursor began)
      content <- readUtf8 path
      lines content `shouldSatisfy` elem "  <firings>3</firings>"

    it "closes the document with '</protocol>' once every total is written" $ withScratchDir $ \dir -> do
      let path = dir </> "protocol.xml"
      createDirectoryIfMissing True dir
      cursor <- newIORef emptyNesting{_closing = [(-2, "protocol", "<protocol>", True)]}
      began <- getMonotonicTime
      withFile path WriteMode (\handle -> endEvalXml handle cursor began)
      content <- readUtf8 path
      last (lines content) `shouldBe` "</protocol>"

    it "writes the msec before the firings it closes with" $ withScratchDir $ \dir -> do
      let path = dir </> "protocol.xml"
      createDirectoryIfMissing True dir
      cursor <- newIORef emptyNesting{_closing = [(-2, "protocol", "<protocol>", True)]}
      began <- getMonotonicTime
      withFile path WriteMode (\handle -> endEvalXml handle cursor began)
      content <- readUtf8 path
      take 1 (lines content) `shouldSatisfy` any (isPrefixOf "  <msec>")

    it "writes the fps after the firings it closes with" $ withScratchDir $ \dir -> do
      let path = dir </> "protocol.xml"
      createDirectoryIfMissing True dir
      cursor <- newIORef emptyNesting{_closing = [(-2, "protocol", "<protocol>", True)]}
      began <- getMonotonicTime
      withFile path WriteMode (\handle -> endEvalXml handle cursor began)
      content <- readUtf8 path
      (lines content !! 2) `shouldSatisfy` isPrefixOf "  <fps>"
