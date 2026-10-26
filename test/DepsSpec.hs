{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module DepsSpec where

import AST (Argument (ArTau), Attribute (AtDelta, AtLabel, AtLambda, AtPhi, AtRho), Binding (BiDelta, BiLambda, BiTau, BiVoid), Bytes (BtMany, BtOne), Expression (ExApplication, ExBytes, ExDispatch, ExFormation, ExMeta, ExRoot, ExTermination, ExXi), Function (FnSymbol, Function))
import Control.Exception (bracket)
import Control.Monad (replicateM_, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf, isPrefixOf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Deps (Acyclic (Proven), Evaluation (EvAnswer, EvApplied, EvBound, EvBoxed, EvBuilt, EvData, EvDeferred, EvDelta, EvDiverged, EvFabricated, EvFiring, EvJoined, EvLooped, EvManufactured, EvMorphed, EvReshaped, EvRun, EvStarted, EvStuck, EvStuckOn, EvSymbolized, EvTaken, EvTerm, EvWorld), Judgment (Dataization, Morphing), Nesting (..), Protocol (..), dontSaveEval, dontSaveStep, emptyNesting, emptyProgress, emptyProtocol, endEval, endEvalXml, perSecond, progressed, resited, saveStep)
import Fixtures (readUtf8, recorded, recordedWhole, recordedXml)
import Forks (Refusal (..), Side (..))
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
    it "writes the symbol a dataization came down to under a name of its own" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvManufactured 1 12])
      lines written `shouldBe` ["𝔻(Φ):", "  𝛿·0·1 := 𝔻(𝜎12:λ)"]
    it "counts the symbols a dataization came down to with the data the delta rule found" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvFiring 1 "L_ж" Dataization ExRoot, EvDelta 3 (BtOne "7F"), EvManufactured 3 32])
      last (lines written) `shouldBe` "      𝛿·1·2 := 𝔻(𝜎32:λ)"
    it "spells the symbol of an operand by the name the dataization just gave it" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvFiring 1 "L_ж" Dataization ExRoot, EvManufactured 3 32, EvData 2 "𝛿1" (ExDispatch ExXi (AtLabel "щ")) (Left 32)])
      last (lines written) `shouldBe` "    𝛿1·1 := 𝛿·1·1  # 𝔻(ξ.щ)"
    it "spells the symbol of an operand in full when the line before it named another symbol" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvFiring 1 "L_ж" Dataization ExRoot, EvManufactured 3 31, EvData 2 "𝛿1" (ExDispatch ExXi (AtLabel "щ")) (Left 32)])
      last (lines written) `shouldBe` "    𝛿1·1 := 𝔻(𝜎32:λ)  # 𝔻(ξ.щ)"
    it "writes the term a morphing came to as a line binding it to a fresh 𝑛 under the heading of the morphing" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "жук")), EvMorphed 2 (ExDispatch ExRoot (AtLabel "щука"))])
      lines written `shouldBe` ["𝕄(Φ):", "  𝕄(Φ.жук):", "    morphed 𝑛·0·1 := Φ.щука"]
    it "spells the term a morphing came to by the name of the application that made it" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w")), EvMorphed 2 (ExFormation [BiTau (AtLabel "q") (ExFormation [])])])
      last (lines written) `shouldBe` "    morphed 𝑛·0·2 := 𝑛·0·1"
    it "numbers a box step past the term the morphing before it came to, as the markup does" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ.e", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "e")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "байт")) (ArTau AtPhi (ExFormation [BiLambda (FnSymbol 1)]))) (ExFormation [BiTau AtPhi (ExFormation [BiLambda (FnSymbol 1)])]) (ExDispatch ExRoot (AtLabel "e")), EvMorphed 2 (ExFormation [BiTau AtPhi (ExFormation [BiLambda (FnSymbol 1)])]), EvBoxed 1 (ExDispatch (ExFormation [BiTau AtPhi (ExFormation [BiLambda (FnSymbol 1)])]) AtPhi)])
      last (lines written) `shouldBe` "  boxed 𝑛·0·3 := 𝑛·0·2.φ"
    it "names the term a morphing came to in the box after it" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "жук")), EvMorphed 2 (ExFormation [BiTau AtPhi (ExDispatch ExRoot (AtLabel "щука"))]), EvBoxed 1 (ExDispatch (ExFormation [BiTau AtPhi (ExDispatch ExRoot (AtLabel "щука"))]) AtPhi)])
      last (lines written) `shouldBe` "  boxed 𝑛·0·2 := 𝑛·0·1.φ"
    it "names the term a morphing came to by its meta in the box after it, though an application named it before" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ.ю", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "ю")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "хорь")) (ArTau (AtLabel "ось") (ExFormation []))) (ExFormation [BiTau (AtLabel "ось") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "ось"))]) (ExDispatch ExRoot (AtLabel "ю")), EvMorphed 2 (ExFormation [BiTau (AtLabel "ось") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "ось"))]), EvBoxed 1 (ExDispatch (ExFormation [BiTau (AtLabel "ось") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "ось"))]) AtPhi)])
      last (lines written) `shouldBe` "  boxed 𝑛·0·3 := 𝑛·0·2.φ"
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
    it "writes the box step of a dataization as a line naming the φ it took" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "гнёт")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "щур")) (ArTau (AtLabel "ял") (ExFormation []))) (ExFormation [BiTau (AtLabel "ял") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "ял"))]) (ExDispatch ExRoot (AtLabel "гнёт")), EvBoxed 1 (ExDispatch (ExFormation [BiTau (AtLabel "ял") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "ял"))]) AtPhi)])
      lines written `shouldBe` ["𝔻(Φ):", "  𝕄(Φ.гнёт):", "    𝑛·0·1 := Φ.щур( ял ↦ ⟦⟧ )", "  boxed 𝑛·0·2 := 𝑛·0·1.φ"]
    it "writes the heading again for a morphing that follows a box step" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "гнёт")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "щур")) (ArTau (AtLabel "ял") (ExFormation []))) (ExFormation [BiTau (AtLabel "ял") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "ял"))]) (ExDispatch ExRoot (AtLabel "гнёт")), EvBoxed 1 (ExDispatch (ExFormation [BiTau (AtLabel "ял") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "ял"))]) AtPhi), EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "гнёт")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "щур")) (ArTau (AtLabel "ял") (ExBytes (BtOne "3C")))) (ExFormation [BiTau (AtLabel "ял") (ExBytes (BtOne "3C"))]) (ExDispatch ExRoot (AtLabel "гнёт"))])
      length (filter (== "  𝕄(Φ.гнёт):") (lines written)) `shouldBe` 2
    it "names the world it is told by Φ in the line of a box step" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvWorld (ExFormation [BiTau (AtLabel "вьюн") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "вьюн"))]), EvBoxed 1 (ExDispatch (ExFormation [BiTau (AtLabel "вьюн") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "вьюн"))]) AtPhi)])
      last (lines written) `shouldBe` "  boxed 𝑛·0·1 := Φ.φ"
    it "names a morphing that follows a box step by the meta the box step bound" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvWorld (ExFormation [BiTau (AtLabel "кулик") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "кулик"))]), EvBoxed 1 (ExDispatch (ExFormation [BiTau (AtLabel "кулик") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "кулик"))]) AtPhi), EvStarted 1 Morphing (ExDispatch (ExFormation [BiTau (AtLabel "кулик") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "кулик"))]) AtPhi), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "чиж")) (ArTau (AtLabel "уж") (ExBytes (BtOne "5E")))) (ExFormation [BiTau (AtLabel "уж") (ExBytes (BtOne "5E"))]) (ExDispatch (ExFormation [BiTau (AtLabel "кулик") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "кулик"))]) AtPhi)])
      lines written `shouldContain` ["  boxed 𝑛·0·1 := Φ.φ", "  𝕄(𝑛·0·1):"]
    it "spells out again the φ a box step takes once more" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvWorld (ExFormation [BiTau (AtLabel "лунь") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "лунь"))]), EvBoxed 1 (ExDispatch (ExFormation [BiTau (AtLabel "лунь") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "лунь"))]) AtPhi), EvBoxed 1 (ExDispatch (ExFormation [BiTau (AtLabel "лунь") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "лунь"))]) AtPhi)])
      last (lines written) `shouldBe` "  boxed 𝑛·0·2 := Φ.φ"
    it "names the answer of a firing by its line in the box step that takes its φ" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvFiring 1 "L_дрозд" Dataization ExRoot, EvBuilt 2 (ExFormation [BiTau AtPhi (ExApplication (ExDispatch ExRoot (AtLabel "сыч")) (ArTau AtPhi (ExFormation [BiLambda (FnSymbol 4)])))]), EvAnswer 2 (ExFormation [BiTau AtPhi (ExApplication (ExDispatch ExRoot (AtLabel "сыч")) (ArTau AtPhi (ExFormation [BiLambda (FnSymbol 4)])))]), EvBoxed 1 (ExDispatch (ExFormation [BiTau AtPhi (ExApplication (ExDispatch ExRoot (AtLabel "сыч")) (ArTau AtPhi (ExFormation [BiLambda (FnSymbol 4)])))]) AtPhi)])
      last (lines written) `shouldBe` "  boxed 𝑛·0·1 := 𝑛·1·2.φ"
    it "names the answer of a join by its line in the box step that takes its φ" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvFiring 1 "L_клёст" Dataization ExRoot, EvTaken 2 [ExDispatch ExXi (AtLabel "then")] (ExFormation [BiTau (AtLabel "жук") (ExFormation [BiLambda (FnSymbol 7)]), BiTau AtPhi (ExDispatch ExXi (AtLabel "жук"))]), EvBoxed 1 (ExDispatch (ExFormation [BiTau (AtLabel "жук") (ExFormation [BiLambda (FnSymbol 7)]), BiTau AtPhi (ExDispatch ExXi (AtLabel "жук"))]) AtPhi)])
      last (lines written) `shouldBe` "  boxed 𝑛·0·1 := 𝑛·1·1.φ"
    it "keeps the mode of a cut made at the site of its block" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvLooped 1 Morphing Proven (ExFormation []) ExRoot])
      last (lines written) `shouldBe` "  looped ⟦⟧  # proven"
    it "names the block over an answer by the line that built it" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Dataization "Φ", EvFiring 1 "L_ф" Dataization ExRoot, EvBuilt 2 (ExDispatch ExRoot (AtLabel "ю")), EvStarted 2 Morphing (ExDispatch ExRoot (AtLabel "ю")), EvApplied 3 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "ю"))])
      lines written `shouldContain` ["    𝕄(𝑛·1·1):"]
    it "writes a deferred copy as the call of the object it was made of" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvDeferred 1 9 Morphing (ExFormation [BiTau (AtLabel "щ") (ExFormation [BiLambda (FnSymbol 4)]), BiTau AtPhi (ExDispatch (ExDispatch ExXi (AtLabel "щ")) (AtLabel "й"))]) (Just (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "щ") (ExFormation [BiLambda (FnSymbol 4)])))) ExRoot])
      last (lines written) `shouldBe` "  deferred 𝜎9 := Φ.ёж( щ ↦ 𝜎4:λ )"
    it "writes a deferred copy as it stands when no object of the world made it" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvDeferred 1 9 Morphing (ExFormation [BiTau (AtLabel "щ") (ExFormation [BiLambda (FnSymbol 4)]), BiTau AtPhi (ExDispatch (ExDispatch ExXi (AtLabel "щ")) (AtLabel "й"))]) Nothing ExRoot])
      last (lines written) `shouldBe` "  deferred 𝜎9 := ⟦ щ ↦ 𝜎4:λ, φ ↦ щ.й ⟧"
    it "writes the term a join came to as the answer of its firing" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvTaken 2 [ExDispatch ExXi (AtLabel "then")] (ExFormation [BiLambda (FnSymbol 3)])])
      last (lines written) `shouldBe` "    𝑛·1·1 := 𝜎3:λ  # 𝕄(ξ.then)"
    it "writes the term both sides of a join came to beside both of them" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvTaken 2 [ExDispatch ExXi (AtLabel "then"), ExDispatch ExXi (AtLabel "else")] (ExFormation [BiLambda (FnSymbol 3)])])
      last (lines written) `shouldBe` "    𝑛·1·1 := 𝜎3:λ  # 𝕄(ξ.then), 𝕄(ξ.else)"
    it "writes the side a join read from a meta beside that meta alone" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvTaken 2 [ExMeta "n3"] (ExFormation [BiLambda (FnSymbol 3)])])
      last (lines written) `shouldBe` "    𝑛·1·1 := 𝜎3:λ  # 𝑛3"
    it "writes the term a path and a meta of a join came to beside the morph of the one and the other alone" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvTaken 2 [ExDispatch ExXi (AtLabel "then"), ExMeta "n4"] (ExFormation [BiLambda (FnSymbol 3)])])
      last (lines written) `shouldBe` "    𝑛·1·1 := 𝜎3:λ  # 𝕄(ξ.then), 𝑛4"
    it "writes the term a stay line dropped a ρ from beside the stay of the meta it read" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvReshaped 2 "𝑛3" "stay" (ExMeta "n1") (ExFormation [BiLambda (FnSymbol 3)])])
      last (lines written) `shouldBe` "    𝑛3·1 := 𝜎3:λ  # stay(𝑛1)"
    it "writes the term a symbolize line turned a datum of into a symbol beside the symbolize of the meta it read" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvReshaped 2 "𝑛4" "symbolize" (ExMeta "n2") (ExFormation [BiLambda (FnSymbol 6)])])
      last (lines written) `shouldBe` "    𝑛4·1 := 𝜎6:λ  # symbolize(𝑛2)"
    it "writes the symbol a symbolize line minted beside the datum it stands for" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvSymbolized 2 6 (BtOne "4A")])
      last (lines written) `shouldBe` "    symbolized 𝜎6 := 4A-"
    it "writes the double eight bytes hold beside the symbol a symbolize line minted for them" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvSymbolized 2 6 (BtMany ["C0", "09", "1E", "B8", "51", "EB", "85", "1F"])])
      last (lines written) `shouldBe` "    symbolized 𝜎6 := C0-09-1E-B8-51-EB-85-1F  # -3.14"
    it "writes the symbol a join minted beside the meta its condition was bound to and the two it stands for" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvJoined 2 (Left "𝛿3") 12 (5, 9)])
      last (lines written) `shouldBe` "    joined 𝜎12 := 𝛿3·1 ? 𝜎5 : 𝜎9"
    it "writes the symbol a join minted beside the path its condition was read off" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvJoined 2 (Right (ExDispatch ExXi (AtLabel "щит"))) 12 (5, 9)])
      last (lines written) `shouldBe` "    joined 𝜎12 := ξ.щит ? 𝜎5 : 𝜎9"
    it "writes two sides a join could not join by the applications that made them" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) ExRoot, EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "жук")) (ArTau (AtLabel "w") (ExFormation []))) (ExFormation [BiTau (AtLabel "w") (ExFormation [])]) ExRoot, EvDiverged 2 (ExDispatch ExXi (AtLabel "then"), ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExXi (AtLabel "else"), ExFormation [BiTau (AtLabel "w") (ExFormation [])]) [AtLabel "q"] Unnamed])
      last (lines written) `shouldBe` "    diverged 𝑛·1·1 vs 𝑛·1·2  # they differ at .q, and the join names no otherwise"
    it "writes two sides a join could not join by the metas of the lines that bound them" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvTerm 2 "𝑛1" Morphing (ExDispatch ExXi (AtLabel "a")) (ExFormation [BiTau AtPhi (ExFormation [BiDelta (BtOne "1F")])]), EvTerm 2 "𝑛2" Morphing (ExDispatch ExXi (AtLabel "b")) (ExFormation [BiTau AtPhi (ExFormation [BiDelta (BtOne "2E")])]), EvDiverged 2 (ExMeta "n1", ExFormation [BiTau AtPhi (ExFormation [BiDelta (BtOne "1F")])]) (ExMeta "n2", ExFormation [BiTau AtPhi (ExFormation [BiDelta (BtOne "2E")])]) [AtPhi, AtDelta] Unnamed])
      last (lines written) `shouldBe` "    diverged 𝑛1·1 vs 𝑛2·1  # they differ at .φ.Δ, and the join names no otherwise"
    it "writes a side of a join no line named as the term it is" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvDiverged 2 (ExDispatch ExXi (AtLabel "a"), ExDispatch ExRoot (AtLabel "ёж")) (ExDispatch ExXi (AtLabel "b"), ExDispatch ExRoot (AtLabel "щука")) [] (Unformed Lefthand)])
      last (lines written) `shouldBe` "    diverged Φ.ёж vs Φ.щука  # the left side is no formation"
    it "writes a side of a join a defer line bound to a symbol as that symbol" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvDiverged 2 (ExMeta "n7", ExFormation [BiLambda (FnSymbol 4)]) (ExDispatch ExXi (AtLabel "b"), ExDispatch ExRoot (AtLabel "щука")) [] (Holding Lefthand AtLambda)])
      last (lines written) `shouldBe` "    diverged 𝜎4:λ vs Φ.щука  # the left side has λ"
    it "writes where the two sides of a join differ beside why no formation was fabricated of them" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvDiverged 2 (ExMeta "n2", ExFormation [BiTau AtPhi (ExFormation [BiTau (AtLabel "ко") (ExDispatch ExRoot (AtLabel "уж"))])]) (ExMeta "n9", ExFormation [BiTau AtPhi (ExFormation [BiTau (AtLabel "ко") (ExDispatch ExRoot (AtLabel "ёрш"))]), BiDelta (BtOne "3C")]) [AtPhi, AtLabel "ко"] (Holding Righthand AtDelta)])
      last (lines written) `shouldBe` "    diverged 𝑛2·1 vs 𝑛9·1  # they differ at .φ.ко, and the right side has Δ"
    it "writes the formation a join fabricated beside the metas of the lines that bound its sides" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvFabricated 2 (ExMeta "n1", ExFormation [BiTau AtPhi (ExDispatch ExRoot (AtLabel "ёж"))]) (ExMeta "n2", ExFormation [BiTau (AtLabel "к") (ExDispatch ExRoot (AtLabel "щука"))]) (ExFormation [BiTau AtPhi (ExFormation [BiLambda (Function "L_ж"), BiTau (AtLabel "on") (ExFormation [BiDelta (BtOne "05")]), BiTau (AtLabel "left") (ExDispatch (ExFormation [BiTau AtPhi (ExDispatch ExRoot (AtLabel "ёж"))]) AtPhi), BiTau (AtLabel "right") (ExDispatch (ExFormation [BiTau (AtLabel "к") (ExDispatch ExRoot (AtLabel "щука"))]) AtPhi)]), BiTau (AtLabel "к") (ExFormation [BiLambda (Function "L_ж"), BiTau (AtLabel "on") (ExFormation [BiDelta (BtOne "05")]), BiTau (AtLabel "left") (ExDispatch (ExFormation [BiTau AtPhi (ExDispatch ExRoot (AtLabel "ёж"))]) (AtLabel "к")), BiTau (AtLabel "right") (ExDispatch (ExFormation [BiTau (AtLabel "к") (ExDispatch ExRoot (AtLabel "щука"))]) (AtLabel "к"))])])])
      last (lines written) `shouldSatisfy` isPrefixOf "    fabricated 𝑛·1·1 := 𝑛1·1 and 𝑛2·1  # "
    it "writes the formation a join fabricated in the comment of its line" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvFabricated 2 (ExMeta "n1", ExFormation [BiTau (AtLabel "к") (ExDispatch ExRoot (AtLabel "ёж"))]) (ExMeta "n2", ExFormation [BiTau (AtLabel "к") (ExDispatch ExRoot (AtLabel "щука"))]) (ExFormation [BiTau (AtLabel "к") (ExFormation [BiLambda (Function "L_ж"), BiTau (AtLabel "on") (ExFormation [BiDelta (BtOne "05")]), BiTau (AtLabel "left") (ExDispatch (ExFormation [BiTau (AtLabel "к") (ExDispatch ExRoot (AtLabel "ёж"))]) (AtLabel "к")), BiTau (AtLabel "right") (ExDispatch (ExFormation [BiTau (AtLabel "к") (ExDispatch ExRoot (AtLabel "щука"))]) (AtLabel "к"))])])])
      last (lines written) `shouldBe` "    fabricated 𝑛·1·1 := 𝑛1·1 and 𝑛2·1  # ⟦ λ ⤍ L_ж, on ↦ 05-:Δ, left ↦ Φ.ёж:к.к, right ↦ Φ.щука:к.к ⟧:к"
    it "spells out in the comment of its line a formation a join fabricated once more" $ do
      (_, written) <- recorded (\record -> mapM_ record (EvRun Morphing "Φ" : EvFiring 1 "L_ф" Morphing ExRoot : replicate 2 (EvFabricated 2 (ExMeta "n1", ExFormation [BiTau (AtLabel "ю") (ExDispatch ExRoot (AtLabel "ёж"))]) (ExMeta "n2", ExFormation [BiTau (AtLabel "ю") (ExDispatch ExRoot (AtLabel "щука"))]) (ExFormation [BiTau (AtLabel "ю") (ExFormation [BiLambda (Function "L_ж"), BiTau (AtLabel "on") (ExFormation [BiDelta (BtOne "3F")]), BiTau (AtLabel "left") (ExDispatch ExRoot (AtLabel "ёж")), BiTau (AtLabel "right") (ExDispatch ExRoot (AtLabel "щука"))])]))))
      last (lines written) `shouldBe` "    fabricated 𝑛·1·2 := 𝑛1·1 and 𝑛2·1  # ⟦ λ ⤍ L_ж, on ↦ 3F-:Δ, left ↦ Φ.ёж, right ↦ Φ.щука ⟧:ю"
    it "writes the formation a join fabricated by its name in a later line" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvFabricated 2 (ExMeta "n1", ExFormation [BiTau AtPhi (ExDispatch ExRoot (AtLabel "ёж"))]) (ExMeta "n2", ExFormation [BiTau (AtLabel "к") (ExDispatch ExRoot (AtLabel "щука"))]) (ExFormation [BiTau AtPhi (ExFormation [BiLambda (Function "L_ж"), BiTau (AtLabel "on") (ExFormation [BiDelta (BtOne "05")]), BiTau (AtLabel "left") (ExDispatch (ExFormation [BiTau AtPhi (ExDispatch ExRoot (AtLabel "ёж"))]) AtPhi), BiTau (AtLabel "right") (ExDispatch (ExFormation [BiTau (AtLabel "к") (ExDispatch ExRoot (AtLabel "щука"))]) AtPhi)]), BiTau (AtLabel "к") (ExFormation [BiLambda (Function "L_ж"), BiTau (AtLabel "on") (ExFormation [BiDelta (BtOne "05")]), BiTau (AtLabel "left") (ExDispatch (ExFormation [BiTau AtPhi (ExDispatch ExRoot (AtLabel "ёж"))]) (AtLabel "к")), BiTau (AtLabel "right") (ExDispatch (ExFormation [BiTau (AtLabel "к") (ExDispatch ExRoot (AtLabel "щука"))]) (AtLabel "к"))])]), EvTaken 2 [ExMeta "n7"] (ExFormation [BiTau AtPhi (ExFormation [BiLambda (Function "L_ж"), BiTau (AtLabel "on") (ExFormation [BiDelta (BtOne "05")]), BiTau (AtLabel "left") (ExDispatch (ExFormation [BiTau AtPhi (ExDispatch ExRoot (AtLabel "ёж"))]) AtPhi), BiTau (AtLabel "right") (ExDispatch (ExFormation [BiTau (AtLabel "к") (ExDispatch ExRoot (AtLabel "щука"))]) AtPhi)]), BiTau (AtLabel "к") (ExFormation [BiLambda (Function "L_ж"), BiTau (AtLabel "on") (ExFormation [BiDelta (BtOne "05")]), BiTau (AtLabel "left") (ExDispatch (ExFormation [BiTau AtPhi (ExDispatch ExRoot (AtLabel "ёж"))]) (AtLabel "к")), BiTau (AtLabel "right") (ExDispatch (ExFormation [BiTau (AtLabel "к") (ExDispatch ExRoot (AtLabel "щука"))]) (AtLabel "к"))])])])
      last (lines written) `shouldBe` "    𝑛·1·2 := 𝑛·1·1  # 𝑛7"
    it "opens a heading of its own for every run of the protocol" $ do
      (_, written) <- recorded (\record -> mapM_ record [EvRun Morphing "Φ", EvDelta 1 (BtOne "0C"), EvRun Morphing "Φ", EvDelta 1 (BtOne "0D")])
      lines written `shouldBe` ["𝕄(Φ):", "  𝛿·0·1 := 0C-", "𝕄(Φ):", "  𝛿·0·2 := 0D-"]
  describe "saveEvalXml" $ do
    it "writes an application as an element naming its head and holding its argument" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ.w", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю")))) (ExFormation [BiTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю"))]) (ExDispatch ExRoot (AtLabel "w"))])
      lines written `shouldContain` ["  <applied meta=\"𝑛·0·1\" by=\"morph\" at=\"Φ.w\" of=\"Φ.ёж\"><ref attr=\"q\">Φ.ю</ref></applied>"]
    it "spells an argument an earlier application made by its name" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю")))) (ExFormation [BiTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю"))]) ExRoot, EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "жук")) (ArTau (AtLabel "w") (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю")))))) (ExFormation [BiTau (AtLabel "w") (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю"))))]) ExRoot])
      lines written `shouldContain` ["  <applied meta=\"𝑛·0·2\" by=\"morph\" at=\"Φ\" of=\"Φ.жук\"><meta attr=\"w\">𝑛·0·1</meta></applied>"]
    it "spells an argument that is a bare symbol as that symbol" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "цапля")) (ArTau AtPhi (ExFormation [BiLambda (FnSymbol 7)]))) (ExFormation [BiTau AtPhi (ExFormation [BiLambda (FnSymbol 7)])]) ExRoot])
      lines written `shouldContain` ["  <applied meta=\"𝑛·0·1\" by=\"morph\" at=\"Φ\" of=\"Φ.цапля\"><symbol attr=\"φ\">𝜎7</symbol></applied>"]
    it "spells a datum an application holds with no tag" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "байты")) (ArTau AtPhi (ExFormation [BiDelta (BtOne "0C")]))) (ExFormation [BiTau AtPhi (ExFormation [BiDelta (BtOne "0C")])]) ExRoot])
      lines written `shouldContain` ["  <applied meta=\"𝑛·0·1\" by=\"morph\" at=\"Φ\" of=\"Φ.байты\"><datum attr=\"φ\">0C-</datum></applied>"]
    it "spells bytes an application holds as a datum" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "щука")) (ArTau (AtLabel "ё") (ExBytes (BtMany ["7E", "A1"])))) (ExFormation [BiTau (AtLabel "ё") (ExBytes (BtMany ["7E", "A1"]))]) ExRoot])
      lines written `shouldContain` ["  <applied meta=\"𝑛·0·1\" by=\"morph\" at=\"Φ\" of=\"Φ.щука\"><datum attr=\"ё\">7E-A1</datum></applied>"]
    it "writes an argument that is a termination as an empty element of its own" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёрш")) (ArTau (AtLabel "ц") ExTermination)) (ExFormation [BiTau (AtLabel "ц") ExTermination]) ExRoot])
      lines written `shouldContain` ["  <applied meta=\"𝑛·0·1\" by=\"morph\" at=\"Φ\" of=\"Φ.ёрш\"><bottom attr=\"ц\"/></applied>"]
    it "spells an argument dispatched from a named application as a reference, not a meta" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "жаба")) (ArTau (AtLabel "у") (ExDispatch (ExApplication (ExDispatch ExRoot (AtLabel "ил")) (ArTau (AtLabel "в") (ExFormation [BiLambda (FnSymbol 6)]))) (AtLabel "дно")))) (ExFormation []) ExRoot])
      lines written `shouldContain` ["  <applied meta=\"𝑛·0·1\" by=\"morph\" at=\"Φ\" of=\"Φ.жаба\"><ref attr=\"у\">𝑒·0·1.дно</ref></applied>"]
    it "names an argument that is an application by an element of its own with an expression meta" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "цапля")) (ArTau AtPhi (ExApplication (ExDispatch ExRoot (AtLabel "байты")) (ArTau AtPhi (ExFormation [BiDelta (BtOne "0C")]))))) (ExFormation [BiTau AtPhi (ExFormation [BiDelta (BtOne "0C")])]) ExRoot])
      lines written `shouldContain` ["  <applied meta=\"𝑒·0·1\" of=\"Φ.байты\"><datum attr=\"φ\">0C-</datum></applied>"]
    it "gives an application the 𝑛 the text gives it, whatever applications its argument holds" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "цапля")) (ArTau AtPhi (ExApplication (ExDispatch ExRoot (AtLabel "байты")) (ArTau AtPhi (ExFormation [BiDelta (BtOne "0C")]))))) (ExFormation [BiTau AtPhi (ExFormation [BiDelta (BtOne "0C")])]) ExRoot])
      lines written `shouldContain` ["  <applied meta=\"𝑛·0·1\" by=\"morph\" at=\"Φ\" of=\"Φ.цапля\"><meta attr=\"φ\">𝑒·0·1</meta></applied>"]
    it "gives an answer the 𝑛 the text gives it, whatever formation it holds" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvAnswer 1 (ExFormation [BiLambda (Function "L_вилка"), BiVoid (AtLabel "лево"), BiTau AtPhi (ExFormation [BiLambda (FnSymbol 5)])])])
      lines written `shouldContain` ["  <answered meta=\"𝑛·0·1\">𝑒·0·1</answered>"]
    it "writes an application of two arguments nothing reduced as one element" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvAnswer 1 (ExApplication (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "а") (ExFormation [BiLambda (FnSymbol 1)]))) (ArTau (AtLabel "б") (ExFormation [BiLambda (FnSymbol 2)])))])
      lines written `shouldContain` ["  <applied meta=\"𝑒·0·1\" of=\"Φ.ёж\"><symbol attr=\"а\">𝜎1</symbol><symbol attr=\"б\">𝜎2</symbol></applied>"]
    it "writes an answer as the name of the term it holds" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvAnswer 1 (ExApplication (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "а") (ExFormation [BiLambda (FnSymbol 1)]))) (ArTau (AtLabel "б") (ExFormation [BiLambda (FnSymbol 2)])))])
      lines written `shouldContain` ["  <answered meta=\"𝑛·0·1\">𝑒·0·1</answered>"]
    it "writes a term a firing built as the name of the term" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvBuilt 1 (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "а") (ExFormation [BiLambda (FnSymbol 1)])))])
      lines written `shouldContain` ["  <built meta=\"𝑛·0·1\">𝑒·0·1</built>"]
    it "spells an object an application made by its name in a later element" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ы" Morphing ExRoot, EvBuilt 2 (ExDispatch ExRoot (AtLabel "ю")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю")))) (ExFormation [BiTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю"))]) ExRoot, EvAnswer 2 (ExFormation [BiTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю"))])])
      lines written `shouldContain` ["    <answered meta=\"𝑛·1·3\">𝑛·1·2</answered>"]
    it "writes a formation no object of the world made as an element holding its bindings" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvAnswer 1 (ExFormation [BiLambda (Function "L_вилка"), BiVoid (AtLabel "лево"), BiTau AtPhi (ExFormation [BiLambda (FnSymbol 5)])])])
      lines written `shouldContain` ["  <formed meta=\"𝑒·0·1\" λ=\"L_вилка\"><void attr=\"лево\"/><symbol attr=\"φ\">𝜎5</symbol></formed>"]
    it "names a formation that copies an object of the world by the object and the attributes it binds" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvWorld (ExFormation [BiTau (AtLabel "ёж") (ExFormation [BiVoid (AtLabel "q"), BiTau (AtLabel "w") ExRoot])]), EvAnswer 1 (ExFormation [BiTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 3)]), BiTau (AtLabel "w") ExRoot])])
      lines written `shouldContain` ["  <formed meta=\"𝑒·0·1\" of=\"Φ.ёж\"><symbol attr=\"q\">𝜎3</symbol></formed>"]
    it "names a formation that replaced an attribute of an object of the world by the object and that attribute" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvWorld (ExFormation [BiTau (AtLabel "ёж") (ExFormation [BiVoid (AtLabel "q"), BiTau AtPhi (ExDispatch ExXi (AtLabel "q")), BiTau (AtLabel "w") ExRoot])]), EvAnswer 1 (ExFormation [BiTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 3)]), BiTau AtPhi (ExFormation [BiLambda (FnSymbol 5)]), BiTau (AtLabel "w") ExRoot])])
      lines written `shouldContain` ["  <formed meta=\"𝑒·0·1\" of=\"Φ.ёж\"><symbol attr=\"q\">𝜎3</symbol><symbol attr=\"φ\">𝜎5</symbol></formed>"]
    it "writes a formation that keeps no binding of an object of the world as a formation of its own" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvWorld (ExFormation [BiTau (AtLabel "ёж") (ExFormation [BiVoid AtPhi])]), EvAnswer 1 (ExFormation [BiTau AtPhi (ExFormation [BiLambda (FnSymbol 8)])])])
      lines written `shouldContain` ["  <formed meta=\"𝑒·0·1\"><symbol attr=\"φ\">𝜎8</symbol></formed>"]
    it "spells a formation that is an object of the world by its path" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvWorld (ExFormation [BiTau (AtLabel "ёж") (ExFormation [BiVoid (AtLabel "q"), BiTau (AtLabel "w") ExRoot])]), EvAnswer 1 (ExFormation [BiVoid (AtLabel "q"), BiTau (AtLabel "w") ExRoot])])
      lines written `shouldContain` ["  <answered meta=\"𝑛·0·1\">Φ.ёж</answered>"]
    it "names a formation that copies an object nested in the world by its path" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvWorld (ExFormation [BiTau (AtLabel "лес") (ExFormation [BiTau (AtLabel "ёж") (ExFormation [BiVoid (AtLabel "q"), BiLambda (Function "L_ёж")])])]), EvAnswer 1 (ExFormation [BiTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 7)]), BiLambda (Function "L_ёж")])])
      lines written `shouldContain` ["  <formed meta=\"𝑒·0·1\" of=\"Φ.лес.ёж\"><symbol attr=\"q\">𝜎7</symbol></formed>"]
    it "names a method of a copy by the copy it is dispatched from" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvWorld (ExFormation [BiTau (AtLabel "ёж") (ExFormation [BiVoid (AtLabel "q"), BiTau (AtLabel "м") (ExFormation [BiVoid AtRho, BiVoid (AtLabel "z")])])]), EvAnswer 1 (ExFormation [BiTau AtRho (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 3)]))), BiTau (AtLabel "z") (ExFormation [BiLambda (FnSymbol 4)])])])
      lines written `shouldContain` ["  <formed meta=\"𝑒·0·2\" of=\"𝑒·0·1.м\"><symbol attr=\"z\">𝜎4</symbol></formed>"]
    it "writes nothing for the world it is told" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvWorld (ExFormation [BiTau (AtLabel "ёж") (ExFormation [])])])
      filter (isInfixOf "ёж") (lines written) `shouldBe` []
    it "writes the datum the delta rule found as an element with its name" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvDelta 1 (BtMany ["0C", "D4"])])
      lines written `shouldContain` ["  <dataized meta=\"𝛿·0·1\">0C-D4</dataized>"]
    it "writes the double a datum of eight bytes holds as the number of its dataized element" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvDelta 1 (BtMany ["C0", "09", "1E", "B8", "51", "EB", "85", "1F"])])
      lines written `shouldContain` ["  <dataized meta=\"𝛿·0·1\" number=\"-3.14\">C0-09-1E-B8-51-EB-85-1F</dataized>"]
    it "spells the datum of an operand by the name the delta rule just gave it in the markup" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvFiring 1 "L_ж" Dataization ExRoot, EvDelta 3 (BtOne "7F"), EvData 2 "𝛿1" (ExDispatch ExXi (AtLabel "щ")) (Right (BtOne "7F"))])
      lines written `shouldContain` ["    <bound meta=\"𝛿1·1\">𝛿·1·1</bound>"]
    it "binds the term a stay line dropped a ρ from in the markup as it binds a term passed through" $ do
      (_, stayed) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ж" Morphing ExRoot, EvReshaped 2 "𝑛3" "stay" (ExMeta "n1") (ExFormation [BiLambda (FnSymbol 3)])])
      (_, passed) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ж" Morphing ExRoot, EvBound 2 "𝑛3" (ExMeta "n1") (ExFormation [BiLambda (FnSymbol 3)])])
      stayed `shouldBe` passed
    it "writes the term a line bound as a bound element in the markup" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ж" Morphing ExRoot, EvBound 2 "𝑛3" (ExMeta "n1") (ExDispatch ExRoot (AtLabel "щ"))])
      lines written `shouldContain` ["    <bound meta=\"𝑛3·1\">Φ.щ</bound>"]
    it "writes the term a morphing came to as an element with its name" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvMorphed 1 (ExDispatch ExRoot (AtLabel "щука"))])
      lines written `shouldContain` ["  <morphed meta=\"𝑛·0·1\">Φ.щука</morphed>"]
    it "writes a morphing that came to a termination as an element holding it" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvMorphed 1 ExTermination])
      lines written `shouldContain` ["  <morphed meta=\"𝑛·0·1\">⊥</morphed>"]
    it "closes a morph element with the term the morphing came to" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "жук")), EvMorphed 2 (ExDispatch ExRoot (AtLabel "ёрш")), EvDelta 1 (BtOne "0D")])
      lines written `shouldContain` ["    <morphed meta=\"𝑛·0·1\">Φ.ёрш</morphed>", "  </morph>"]
    it "writes the symbol a dataization came down to as an element with its name" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvManufactured 1 12])
      lines written `shouldContain` ["  <manufactured meta=\"𝛿·0·1\">𝜎12</manufactured>"]
    it "counts the symbols a dataization came down to with the data the delta rule found" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvFiring 1 "L_ж" Dataization ExRoot, EvDelta 3 (BtOne "7F"), EvManufactured 3 32])
      lines written `shouldContain` ["      <manufactured meta=\"𝛿·1·2\">𝜎32</manufactured>"]
    it "spells the symbol of an operand by the name the dataization just gave it in the markup" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvFiring 1 "L_ж" Dataization ExRoot, EvManufactured 3 32, EvData 2 "𝛿1" (ExDispatch ExXi (AtLabel "щ")) (Left 32)])
      lines written `shouldContain` ["    <bound meta=\"𝛿1·1\">𝛿·1·1</bound>"]
    it "spells the symbol of an operand in full in the markup when the element before it named another symbol" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvFiring 1 "L_ж" Dataization ExRoot, EvManufactured 3 31, EvData 2 "𝛿1" (ExDispatch ExXi (AtLabel "щ")) (Left 32)])
      lines written `shouldContain` ["    <bound meta=\"𝛿1·1\">𝜎32</bound>"]
    it "writes a block as an element once an element stands in it" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w"))])
      lines written `shouldContain` ["  <morph at=\"Φ.w\">"]
    it "writes no element for a block nothing stands in" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvDelta 1 (BtOne "0C")])
      filter (isInfixOf "morph") (lines written) `shouldBe` []
    it "names the site of a block that is an application by an element written before the block" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ж")) (ArTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 1)]))), EvDelta 2 (BtOne "0C")])
      lines written `shouldContain` ["  <applied meta=\"𝑒·0·1\" of=\"Φ.ж\"><symbol attr=\"q\">𝜎1</symbol></applied>", "  <morph at=\"𝑒·0·1\">"]
    it "names nothing for the site of a block nothing stands in" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ж")) (ArTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 1)]))), EvDelta 1 (BtOne "0C")])
      filter (isInfixOf "applied") (lines written) `shouldBe` []
    it "names the site of a firing that is an application by an element written before the firing" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ж" Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ж")) (ArTau (AtLabel "q") (ExFormation [BiLambda (FnSymbol 1)])))])
      lines written `shouldContain` ["  <applied meta=\"𝑒·0·1\" of=\"Φ.ж\"><symbol attr=\"q\">𝜎1</symbol></applied>", "  <evaluate λ=\"L_ж\" by=\"morph\" at=\"𝑒·0·1\">"]
    it "writes one element for two blocks of one judgment at one site in a row" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w")), EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w"))])
      length (filter (isInfixOf "<morph") (lines written)) `shouldBe` 1
    it "closes a block element before the element that follows beside it" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "w")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "w")), EvDelta 1 (BtOne "0D")])
      lines written `shouldContain` ["  </morph>", "  <dataized meta=\"𝛿·0·1\">0D-</dataized>"]
    it "writes the box step of a dataization as an element naming the φ it took" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "гнёт")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "щур")) (ArTau (AtLabel "ял") (ExFormation [BiDelta (BtOne "3C")]))) (ExFormation [BiTau (AtLabel "ял") (ExFormation [BiDelta (BtOne "3C")]), BiTau AtPhi (ExDispatch ExXi (AtLabel "ял"))]) (ExDispatch ExRoot (AtLabel "гнёт")), EvBoxed 1 (ExDispatch (ExFormation [BiTau (AtLabel "ял") (ExFormation [BiDelta (BtOne "3C")]), BiTau AtPhi (ExDispatch ExXi (AtLabel "ял"))]) AtPhi)])
      lines written `shouldContain` ["  </morph>", "  <boxed meta=\"𝑛·0·2\">𝑛·0·1.φ</boxed>"]
    it "names the term a morphing came to by its meta in the element of the box after it, though an application named it before" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ.ж", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "ж")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "рысь")) (ArTau (AtLabel "пух") (ExFormation [BiDelta (BtOne "4E")]))) (ExFormation [BiTau (AtLabel "пух") (ExFormation [BiDelta (BtOne "4E")]), BiTau AtPhi (ExDispatch ExXi (AtLabel "пух"))]) (ExDispatch ExRoot (AtLabel "ж")), EvMorphed 2 (ExFormation [BiTau (AtLabel "пух") (ExFormation [BiDelta (BtOne "4E")]), BiTau AtPhi (ExDispatch ExXi (AtLabel "пух"))]), EvBoxed 1 (ExDispatch (ExFormation [BiTau (AtLabel "пух") (ExFormation [BiDelta (BtOne "4E")]), BiTau AtPhi (ExDispatch ExXi (AtLabel "пух"))]) AtPhi)])
      lines written `shouldContain` ["  <boxed meta=\"𝑛·0·3\">𝑛·0·2.φ</boxed>"]
    it "writes a morph element again for a morphing that follows a box step" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "гнёт")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "щур")) (ArTau (AtLabel "ял") (ExFormation [BiDelta (BtOne "3C")]))) (ExFormation [BiTau (AtLabel "ял") (ExFormation [BiDelta (BtOne "3C")]), BiTau AtPhi (ExDispatch ExXi (AtLabel "ял"))]) (ExDispatch ExRoot (AtLabel "гнёт")), EvBoxed 1 (ExDispatch (ExFormation [BiTau (AtLabel "ял") (ExFormation [BiDelta (BtOne "3C")]), BiTau AtPhi (ExDispatch ExXi (AtLabel "ял"))]) AtPhi), EvStarted 1 Morphing (ExDispatch ExRoot (AtLabel "гнёт")), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "щур")) (ArTau (AtLabel "ял") (ExBytes (BtOne "3C")))) (ExFormation [BiTau (AtLabel "ял") (ExBytes (BtOne "3C"))]) (ExDispatch ExRoot (AtLabel "гнёт"))])
      length (filter (isInfixOf "<morph at=") (lines written)) `shouldBe` 2
    it "names the world it is told by Φ in the element of a box step" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvWorld (ExFormation [BiTau (AtLabel "вьюн") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "вьюн"))]), EvBoxed 1 (ExDispatch (ExFormation [BiTau (AtLabel "вьюн") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "вьюн"))]) AtPhi)])
      lines written `shouldContain` ["  <boxed meta=\"𝑛·0·1\">Φ.φ</boxed>"]
    it "names a morph element that follows a box step by the meta the box step bound" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvWorld (ExFormation [BiTau (AtLabel "кулик") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "кулик"))]), EvBoxed 1 (ExDispatch (ExFormation [BiTau (AtLabel "кулик") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "кулик"))]) AtPhi), EvStarted 1 Morphing (ExDispatch (ExFormation [BiTau (AtLabel "кулик") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "кулик"))]) AtPhi), EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "чиж")) (ArTau (AtLabel "уж") (ExBytes (BtOne "5E")))) (ExFormation [BiTau (AtLabel "уж") (ExBytes (BtOne "5E"))]) (ExDispatch (ExFormation [BiTau (AtLabel "кулик") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "кулик"))]) AtPhi)])
      lines written `shouldContain` ["  <boxed meta=\"𝑛·0·1\">Φ.φ</boxed>", "  <morph at=\"𝑛·0·1\">"]
    it "spells out again in an element the φ a box step takes once more" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvWorld (ExFormation [BiTau (AtLabel "лунь") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "лунь"))]), EvBoxed 1 (ExDispatch (ExFormation [BiTau (AtLabel "лунь") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "лунь"))]) AtPhi), EvBoxed 1 (ExDispatch (ExFormation [BiTau (AtLabel "лунь") (ExFormation []), BiTau AtPhi (ExDispatch ExXi (AtLabel "лунь"))]) AtPhi)])
      lines written `shouldContain` ["  <boxed meta=\"𝑛·0·2\">Φ.φ</boxed>"]
    it "names the element over an answer by the element that built it" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Dataization "Φ", EvFiring 1 "L_ф" Dataization ExRoot, EvBuilt 2 (ExDispatch ExRoot (AtLabel "ю")), EvStarted 2 Morphing (ExDispatch ExRoot (AtLabel "ю")), EvApplied 3 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExFormation []))) (ExFormation [BiTau (AtLabel "q") (ExFormation [])]) (ExDispatch ExRoot (AtLabel "ю"))])
      lines written `shouldContain` ["    <morph at=\"𝑛·1·1\">"]
    it "writes a deferred copy as an element naming its object and the symbols of its arguments" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvDeferred 1 9 Morphing (ExFormation [BiTau (AtLabel "щ") (ExFormation [BiLambda (FnSymbol 4)]), BiTau AtPhi (ExDispatch (ExDispatch ExXi (AtLabel "щ")) (AtLabel "й"))]) (Just (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "щ") (ExFormation [BiLambda (FnSymbol 4)])))) ExRoot])
      lines written `shouldContain` ["  <deferred symbol=\"𝜎9\" by=\"morph\" at=\"Φ\" of=\"Φ.ёж\"><with><symbol attr=\"щ\">𝜎4</symbol></with></deferred>"]
    it "writes a deferred copy that names no call as an element naming the copy" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvDeferred 1 9 Morphing (ExFormation [BiTau (AtLabel "щ") (ExFormation [BiLambda (FnSymbol 4)])]) Nothing ExRoot])
      lines written `shouldContain` ["  <deferred symbol=\"𝜎9\" by=\"morph\" at=\"Φ\" of=\"𝑒·0·1\"/>"]
    it "names an argument of a copy nothing worked out by an expression meta" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvDeferred 1 9 Morphing (ExFormation []) (Just (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "щ") (ExApplication (ExDispatch ExRoot (AtLabel "ю")) (ArTau (AtLabel "ж") (ExFormation [BiLambda (FnSymbol 2)])))))) ExRoot])
      lines written `shouldSatisfy` any (isInfixOf "<meta attr=\"щ\">𝑒1</meta>")
    it "names one argument nothing worked out by one expression meta in two elements" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvDeferred 1 9 Morphing (ExFormation []) (Just (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "щ") (ExApplication (ExDispatch ExRoot (AtLabel "ю")) (ArTau (AtLabel "ж") (ExFormation [BiLambda (FnSymbol 2)])))))) ExRoot, EvDeferred 1 10 Morphing (ExFormation []) (Just (ExApplication (ExDispatch ExRoot (AtLabel "ёрш")) (ArTau (AtLabel "ц") (ExApplication (ExDispatch ExRoot (AtLabel "ю")) (ArTau (AtLabel "ж") (ExFormation [BiLambda (FnSymbol 2)])))))) ExRoot])
      length (filter (isInfixOf ">𝑒1<") (lines written)) `shouldBe` 2
    it "writes a cut that answered no copy as an element naming the formation it entered" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvLooped 1 Morphing Proven (ExFormation [BiTau (AtLabel "ъ") ExRoot]) ExRoot])
      lines written `shouldContain` ["  <looped by=\"morph\" match=\"proven\" at=\"Φ\" of=\"𝑒·0·1\"/>"]
    it "names an argument of a deferred copy by the application that made it" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvApplied 1 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "кулик")) (ArTau (AtLabel "ш") ExRoot)) (ExFormation [BiTau (AtLabel "ш") ExRoot]) ExRoot, EvDeferred 1 31 Morphing (ExFormation [BiTau (AtLabel "щ") (ExFormation [BiTau (AtLabel "ш") ExRoot]), BiTau AtPhi (ExDispatch ExXi (AtLabel "щ"))]) (Just (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "щ") (ExFormation [BiTau (AtLabel "ш") ExRoot])))) ExRoot])
      lines written `shouldContain` ["  <deferred symbol=\"𝜎31\" by=\"morph\" at=\"Φ\" of=\"Φ.ёж\"><with><meta attr=\"щ\">𝑛·0·1</meta></with></deferred>"]
    it "writes the term a join came to as the answer of its firing in the markup" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvTaken 2 [ExDispatch ExXi (AtLabel "then")] (ExFormation [BiLambda (FnSymbol 3)])])
      lines written `shouldContain` ["    <answered meta=\"𝑛·1·1\">𝜎3</answered>"]
    it "writes the symbol a join minted as an element naming the meta its condition was bound to and the two it stands for" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvJoined 2 (Left "𝛿3") 12 (5, 9)])
      lines written `shouldContain` ["    <joined symbol=\"𝜎12\" on=\"𝛿3·1\" left=\"𝜎5\" right=\"𝜎9\"/>"]
    it "writes the symbol a join minted as an element naming the path its condition was read off" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvJoined 2 (Right (ExDispatch ExXi (AtLabel "щит"))) 12 (5, 9)])
      lines written `shouldContain` ["    <joined symbol=\"𝜎12\" on=\"ξ.щит\" left=\"𝜎5\" right=\"𝜎9\"/>"]
    it "writes two sides a join could not join as an element naming both" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "ёж")) (ArTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю")))) (ExFormation [BiTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю"))]) ExRoot, EvApplied 2 Morphing (ExApplication (ExDispatch ExRoot (AtLabel "жук")) (ArTau (AtLabel "w") (ExDispatch ExRoot (AtLabel "ю")))) (ExFormation [BiTau (AtLabel "w") (ExDispatch ExRoot (AtLabel "ю"))]) ExRoot, EvDiverged 2 (ExDispatch ExXi (AtLabel "then"), ExFormation [BiTau (AtLabel "q") (ExDispatch ExRoot (AtLabel "ю"))]) (ExDispatch ExXi (AtLabel "else"), ExFormation [BiTau (AtLabel "w") (ExDispatch ExRoot (AtLabel "ю"))]) [AtLabel "q"] Unnamed])
      lines written `shouldContain` ["    <diverged left=\"𝑛·1·1\" right=\"𝑛·1·2\" at=\".q\" reason=\"the join names no otherwise\"/>"]
    it "writes a side of a join that is a path as that path in the markup" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvDiverged 2 (ExDispatch ExXi (AtLabel "a"), ExDispatch ExRoot (AtLabel "ёж")) (ExDispatch ExXi (AtLabel "b"), ExDispatch ExRoot (AtLabel "щука")) [] (Unformed Righthand)])
      lines written `shouldContain` ["    <diverged left=\"Φ.ёж\" right=\"Φ.щука\" reason=\"the right side is no formation\"/>"]
    it "writes a side of a join read from a meta by the meta of its line in the markup" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvDiverged 2 (ExMeta "n3", ExFormation [BiTau AtPhi (ExFormation [BiDelta (BtOne "1F")])]) (ExMeta "n4", ExFormation [BiTau AtPhi (ExFormation [BiDelta (BtOne "2E")])]) [AtPhi, AtDelta] Unnamed])
      lines written `shouldContain` ["    <diverged left=\"𝑛3·1\" right=\"𝑛4·1\" at=\".φ.Δ\" reason=\"the join names no otherwise\"/>"]
    it "writes where the two sides of a join differ and why no formation was fabricated of them in the markup" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvDiverged 2 (ExMeta "n5", ExFormation [BiLambda (FnSymbol 8)]) (ExMeta "n6", ExFormation [BiTau AtPhi (ExDispatch ExRoot (AtLabel "ёрш"))]) [AtLambda] (Holding Lefthand AtLambda)])
      lines written `shouldContain` ["    <diverged left=\"𝜎8\" right=\"𝑛6·1\" at=\".λ\" reason=\"the left side has λ\"/>"]
    it "writes the formation a join fabricated as an element naming both sides in the markup" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvFabricated 2 (ExMeta "n3", ExFormation [BiTau AtPhi (ExDispatch ExRoot (AtLabel "ёж"))]) (ExMeta "n4", ExFormation [BiTau (AtLabel "к") (ExDispatch ExRoot (AtLabel "щука"))]) (ExFormation [BiTau AtPhi (ExFormation [BiLambda (Function "L_ж"), BiTau (AtLabel "on") (ExFormation [BiDelta (BtOne "05")]), BiTau (AtLabel "left") (ExDispatch (ExFormation [BiTau AtPhi (ExDispatch ExRoot (AtLabel "ёж"))]) AtPhi), BiTau (AtLabel "right") (ExDispatch (ExFormation [BiTau (AtLabel "к") (ExDispatch ExRoot (AtLabel "щука"))]) AtPhi)]), BiTau (AtLabel "к") (ExFormation [BiLambda (Function "L_ж"), BiTau (AtLabel "on") (ExFormation [BiDelta (BtOne "05")]), BiTau (AtLabel "left") (ExDispatch (ExFormation [BiTau AtPhi (ExDispatch ExRoot (AtLabel "ёж"))]) (AtLabel "к")), BiTau (AtLabel "right") (ExDispatch (ExFormation [BiTau (AtLabel "к") (ExDispatch ExRoot (AtLabel "щука"))]) (AtLabel "к"))])])])
      lines written `shouldContain` ["    <fabricated meta=\"𝑛·1·1\" left=\"𝑛3·1\" right=\"𝑛4·1\">𝑒·1·5</fabricated>"]
    it "writes the symbol a symbolize line minted as an element holding the datum it stands for" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvSymbolized 2 6 (BtOne "4A")])
      lines written `shouldContain` ["    <symbolized symbol=\"𝜎6\">4A-</symbolized>"]
    it "writes the double eight bytes hold as the number of the element of the symbol a symbolize line minted" $ do
      (_, written) <- recordedXml (\record -> mapM_ record [EvRun Morphing "Φ", EvFiring 1 "L_ф" Morphing ExRoot, EvSymbolized 2 6 (BtMany ["C0", "09", "1E", "B8", "51", "EB", "85", "1F"])])
      lines written `shouldContain` ["    <symbolized symbol=\"𝜎6\" number=\"-3.14\">C0-09-1E-B8-51-EB-85-1F</symbolized>"]
    it "wraps the run in no path element" $ do
      (_, written) <- recordedWhole (\record -> mapM_ record [EvRun Morphing "Φ", EvDelta 1 (BtOne "0C")])
      filter (isInfixOf "path>") (lines written) `shouldBe` []
    it "indents the elements of a run inside the root" $ do
      (_, written) <- recordedWhole (\record -> mapM_ record [EvRun Morphing "Φ", EvDelta 1 (BtOne "0C")])
      filter (isInfixOf "morph") (lines written) `shouldBe` ["  <morph at=\"Φ\">", "  </morph>"]
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
      map (takeWhile (/= ' ')) (lines content) `shouldBe` ["msec", "firings", "fps"]

    it "names the firings of the run it closes" $ withScratchDir $ \dir -> do
      let path = dir </> "protocol.txt"
      createDirectoryIfMissing True dir
      cursor <- newIORef emptyProtocol{_begun = True, _fired = 5}
      began <- getMonotonicTime
      withFile path WriteMode (\handle -> endEval handle cursor began)
      content <- readUtf8 path
      lines content `shouldSatisfy` elem "firings 5"

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
