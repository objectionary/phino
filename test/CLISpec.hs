{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLISpec (spec) where

import CLI (runCLI)
import CLI.Types (CmdException (..), IOFormat (..))
import Control.Exception
import Control.Monad (forM_, unless)
import Data.Char (isDigit)
import Data.List (intercalate, isInfixOf, isPrefixOf, sort)
import Data.Text qualified as T
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Version (showVersion)
import Files (allPathsIn)
import Fixtures (explainPack, lambdasFile, loopingLambdas, readProtocol, readUtf8, withLambdasOf)
import GHC.IO.Handle
import Paths_phino (version)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getTemporaryDirectory, listDirectory, makeAbsolute, removeDirectoryRecursive, removeFile, removePathForcibly, setModificationTime, withCurrentDirectory)
import System.Exit (ExitCode (ExitFailure))
import System.FilePath ((</>))
import System.IO
import System.Timeout (timeout)
import Test.Hspec
import Text.Printf (printf)
import Text.XML qualified as X

withStdin :: String -> IO a -> IO a
withStdin input action =
  bracket (openTempFile "." "stdinXXXXXX.tmp") cleanup $ \(filePath, h) -> do
    hSetEncoding h utf8
    hPutStr h input
    hFlush h
    hClose h
    withFile filePath ReadMode $ \hIn -> do
      hSetEncoding hIn utf8
      bracket (hDuplicate stdin) restoreStdin $ \_ -> do
        hDuplicateTo hIn stdin
        hSetEncoding stdin utf8
        action
  where
    restoreStdin orig = hDuplicateTo orig stdin >> hClose orig
    cleanup (fp, _) = removeFile fp

withStdout :: IO a -> IO (String, a)
withStdout action =
  bracket
    (openTempFile "." "stdoutXXXXXX.tmp")
    cleanup
    ( \(path, hTmp) -> do
        hSetEncoding hTmp utf8
        oldOut <- hDuplicate stdout
        oldErr <- hDuplicate stderr
        hDuplicateTo hTmp stdout
        hDuplicateTo hTmp stderr

        result <-
          action `finally` do
            hFlush stdout
            hFlush stderr
            hDuplicateTo oldOut stdout >> hClose oldOut
            hDuplicateTo oldErr stderr >> hClose oldErr
            hClose hTmp

        captured <- readFile path
        _ <- evaluate (length captured)
        return (captured, result)
    )
  where
    cleanup (fp, _) = removeFile fp

withTempFile :: String -> ((FilePath, Handle) -> IO a) -> IO a
withTempFile pattern =
  bracket
    (openTempFile "." pattern)
    (\(path, _) -> removeFile path)

withTempFileContent :: String -> String -> (FilePath -> IO a) -> IO a
withTempFileContent pattern content action =
  withTempFile pattern $ \(path, h) -> do
    hPutStr h content
    hClose h
    action path

withTempDirectory :: String -> (FilePath -> IO a) -> IO a
withTempDirectory prefix action = do
  tmp <- getTemporaryDirectory
  stamp <- getPOSIXTime
  let dir = tmp </> (prefix ++ "-" ++ show (round (stamp * 1000000) :: Integer))
  bracket (pure dir) removePathForcibly action

testCLI' :: [String] -> [String] -> Either ExitCode () -> Expectation
testCLI' args outputs exit = do
  (out, result) <- withStdout (try (runCLI args) :: IO (Either ExitCode ()))
  if null outputs
    then
      unless (null out) $
        expectationFailure ("Expected that output is empty, but got:\n" ++ out)
    else
      forM_
        outputs
        ( \output ->
            unless (output `isInfixOf` out) $
              expectationFailure
                ("Expected that output contains:\n" ++ output ++ "\nbut got:\n" ++ out)
        )
  result `shouldBe` exit

testCLISucceeded :: [String] -> [String] -> Expectation
testCLISucceeded args outputs = testCLI' args outputs (Right ())

symbolic :: String
symbolic = "--symbolic=" ++ lambdasFile

testCLIFailed :: [String] -> [String] -> Expectation
testCLIFailed args outputs = testCLI' args outputs (Left (ExitFailure 1))

resource :: String -> String
resource file = "test-resources/cli/expressions/" <> file

rule :: String -> String
rule file = "--rule=test-resources/cli/rules/" <> file

spec :: Spec
spec = do
  it "prints version" $
    testCLISucceeded ["--version"] [showVersion version]

  it "prints help" $
    testCLISucceeded
      ["--help"]
      ["Phino - CLI Manipulator of 𝜑-Calculus Expressions", "Usage:"]

  describe "--pin" $
    forM_
      [
        ( "succeeds when --pin matches actual version"
        , ["--pin=" ++ showVersion version, "rewrite", "--sweet"]
        , testCLISucceeded
        , ["⟦⟧"]
        )
      ,
        ( "fails when --pin doesn't match actual version"
        , ["--pin=9.9.9.9", "rewrite"]
        , testCLIFailed
        , ["Version mismatch: --pin requires '9.9.9.9', but this is phino " ++ showVersion version]
        )
      ,
        ( "fails when --pin is empty"
        , ["--pin=", "rewrite"]
        , testCLIFailed
        , ["Version mismatch: --pin requires ''"]
        )
      ]
      (\(desc, args, test, expected) -> it desc (withStdin "[[ ]]" (test args expected)))

  describe "--pin-file" $
    forM_
      [
        ( "succeeds when --pin-file holds actual version among spaces"
        , Just ("  \n" ++ showVersion version ++ " \t\n\n")
        , testCLISucceeded
        , ["⟦⟧"]
        )
      ,
        ( "fails when --pin-file holds another version"
        , Just "7.3.0.41\n"
        , testCLIFailed
        , ["Version mismatch: --pin requires '7.3.0.41', but this is phino " ++ showVersion version]
        )
      ,
        ( "fails when --pin-file is empty"
        , Just " \n"
        , testCLIFailed
        , ["Version mismatch: --pin requires ''"]
        )
      ,
        ( "fails when --pin-file is absent"
        , Nothing
        , testCLIFailed
        , ["does not exist"]
        )
      ]
      ( \(desc, content, test, expected) ->
          it desc $
            withTempDirectory "phino-pin-file" $ \dir -> do
              createDirectoryIfMissing True dir
              let file = dir </> "vérsion.txt"
              forM_ content (writeFile file)
              withStdin "[[ ]]" (test ["--pin-file=" ++ file, "rewrite", "--sweet"] expected)
      )

  it "fails when both --pin and --pin-file are given" $
    withStdin "[[ ]]" $
      testCLIFailed
        ["--pin=" ++ showVersion version, "--pin-file=pinned-version.txt", "rewrite"]
        ["[ERROR]"]

  describe "--hide-rho" $
    forM_
      [
        ( "drops every rho binding from the default salty output"
        , "[[ foo -> [[ x -> [[ ]], ^ -> $.y ]], y -> [[ ]] ]]"
        , ["rewrite", "--flat", "--hide-rho"]
        , ["⟦ foo ↦ ⟦ x ↦ ⟦⟧ ⟧, y ↦ ⟦⟧ ⟧"]
        )
      ,
        ( "also drops the rho that --sweet leaves behind"
        , "[[ foo -> [[ x -> [[ ]], ^ -> $.y ]], y -> [[ ]] ]]"
        , ["rewrite", "--flat", "--sweet", "--hide-rho"]
        , ["⟦ foo ↦ ⟦⟧:x, y ↦ ⟦⟧ ⟧"]
        )
      ,
        ( "keeps sweet numeric literals intact"
        , "[[ a -> 42 ]]"
        , ["rewrite", "--flat", "--sweet", "--hide-rho"]
        , ["42:a"]
        )
      ,
        ( "keeps the one-binding sugar after inline voids"
        , "[[ x(y) -> [[ a -> 42, ^ -> ? ]] ]]"
        , ["rewrite", "--flat", "--sweet", "--hide-rho"]
        , ["⟦ x(y) ↦ 42:a ⟧"]
        )
      ]
      (\(desc, input, args, expected) -> it desc (withStdin input (testCLISucceeded args expected)))

  it "prints the one-binding sugar after inline voids with --sweet" $
    withStdin "[[ x(y) -> [[ a -> 42 ]] ]]" $
      testCLISucceeded ["rewrite", "--sweet"] ["⟦ x(y) ↦ 42:a ⟧"]

  it "prints debug info with --log-level=DEBUG" $
    withStdin "[[]]" $
      testCLISucceeded ["rewrite", "--log-level=DEBUG"] ["[DEBUG]:"]

  describe "--log-level accepts every named level" $
    forM_
      ["INFO", "info", "ERROR", "ERR", "error", "NONE", "none"]
      ( \flagValue ->
          it ("--log-level=" ++ flagValue) $
            withStdin "[[]]" $
              testCLISucceeded ["rewrite", "--log-level=" ++ flagValue] ["⟧"]
      )

  describe "--log-level prints nothing below its level" $
    forM_
      [("NONE", ["[DEBUG]", "[INFO]"]), ("ERROR", ["[DEBUG]", "[INFO]"]), ("INFO", ["[DEBUG]"])]
      ( \(level, hidden) ->
          it ("--log-level=" ++ level) $
            withStdin "[[]]" $ do
              (out, _) <- withStdout (try (runCLI ["rewrite", "--log-level=" ++ level]) :: IO (Either ExitCode ()))
              forM_ hidden (out `shouldNotContain`)
      )

  it "fails on an unrecognized --log-level value" $
    withStdin "[[]]" $
      testCLIFailed ["rewrite", "--log-level=verbose"] ["unknown log-level: verbose"]

  describe "rewriting" $ do
    describe "fails" $ do
      forM_
        [ ("with --input=latex", "", ["rewrite", "--input=latex"], ["The value 'latex' can't be used for '--input' option"])
        , ("with negative --log-lines", "", ["rewrite", "--log-lines=-2"], ["--log-lines must be >= -1"])
        , ("with negative --max-depth", "", ["rewrite", "--max-depth=-1"], ["--max-depth must be positive"])
        , ("with zero --max-cycles", "", ["rewrite", "--max-cycles=0"], ["--max-cycles must be positive"])
        , ("with zero --meet-length", "", ["rewrite", "--output=latex", "--meet-length=0"], ["--meet-length must be positive"])
        ,
          ( "with --normalize and --must=1"
          , "[[ x -> [[ y -> 5 ]].y ]].x"
          , ["rewrite", "--max-cycles=2", "--max-depth=1", "--normalize", "--must=1"]
          , ["it's expected rewriting cycles to be in range [1], but rewriting has already reached 2"]
          )
        , ("when --in-place is used without input file", "[[ ]]", ["rewrite", "--in-place"], ["--in-place requires an input file"])
        ,
          ( "with --output=xmir on a non-top-level expression"
          , "⟦ x ↦ 1, ρ ↦ 2 ⟧"
          , ["rewrite", "--output=xmir"]
          , ["[ERROR]:", "its top level must be a single binding"]
          )
        ]
        (\(desc, input, args, expected) -> it desc (withStdin input (testCLIFailed args expected)))

      it "when --in-place is used with --target" $
        withTempFile "inplaceXXXXXX.phi" $ \(path, h) -> do
          hPutStr h "[[ ]]"
          hClose h
          testCLIFailed
            ["rewrite", "--in-place", "--target=output.phi", path]
            ["--in-place and --target cannot be used together"]

      it "fails when --in-place is used with a non-phi output format" $
        withTempFile "inplaceXXXXXX.phi" $ \(path, h) -> do
          hPutStr h "[[ ]]"
          hClose h
          testCLIFailed
            ["rewrite", "--in-place", "--output=latex", path]
            ["--in-place can only be used together with --output=phi"]

      it "does not leak a HasCallStack backtrace into errors" $ do
        (out, _) <- withStdout (try (runCLI ["rewrite", "--in-place"]) :: IO (Either ExitCode ()))
        out `shouldNotContain` "HasCallStack backtrace"
        out `shouldNotContain` "ExitFailure 1"
        out `shouldContain` "[ERROR]:"

      it "prints optparse errors once, without a backtrace" $ do
        (out, _) <- withStdout (try (runCLI ["rewrite", "--badopt"]) :: IO (Either ExitCode ()))
        out `shouldNotContain` "HasCallStack backtrace"
        out `shouldNotContain` "ExitFailure 1"
        out `shouldContain` "[ERROR]:"

      forM_
        [ ("when --update is used without --target", "[[ ]]", ["rewrite", "--update"], ["--update requires --target"])
        ,
          ( "when --update is used without an input file"
          , "[[ ]]"
          , ["rewrite", "--update", "--target=output.phi"]
          , ["--update requires an input file"]
          )
        ,
          ( "when --sequence is used with --in-place"
          , "[[ ]]"
          , ["rewrite", "--sequence", "--in-place", "input.phi"]
          , ["--in-place and --sequence cannot be used together"]
          )
        ,
          ( "when --focus is used with --in-place"
          , "[[ ]]"
          , ["rewrite", "--focus=Q.y", "--in-place", "input.phi"]
          , ["--in-place and --focus cannot be used together"]
          )
        ,
          ( "when --show is used with --in-place"
          , "[[ ]]"
          , ["rewrite", "--show=Q.y", "--in-place", "input.phi"]
          , ["--in-place and --show cannot be used together"]
          )
        ,
          ( "when --update is used with --in-place"
          , "[[ ]]"
          , ["rewrite", "--update", "--in-place", "input.phi"]
          , ["--update and --in-place cannot be used together"]
          )
        ,
          ( "with --depth-sensitive"
          , "[[ x -> \"x\"]]"
          , ["rewrite", "--depth-sensitive", "--max-depth=1", "--max-cycles=1", rule "infinite.yaml"]
          , ["[ERROR]: With option --depth-sensitive it's expected rewriting iterations amount does not reach the limit: --max-depth=1"]
          )
        ,
          ( "with looping rules"
          , "[[ x -> \"0\" ]]"
          , ["rewrite", rule "first.yaml", rule "second.yaml", "--max-depth=1", "--max-cycles=3"]
          , ["it seems rewriting is looping"]
          )
        ]
        (\(desc, input, args, expected) -> it desc (withStdin input (testCLIFailed args expected)))

      it "with wrong attribute and valid error message" $
        testCLIFailed
          ["rewrite", resource "with-$this-attribute.phi"]
          [ "[ERROR]: Couldn't parse given phi expression, cause:"
          , "unexpected"
          ]

      forM_
        [
          ( "with --output != latex and --nonumber"
          , ["rewrite", "--nonumber", "--output=xmir"]
          , ["The --nonumber option can stay together with --output=latex only"]
          )
        , ("with --omit-listing and --output != xmir", ["rewrite", "--omit-listing", "--output=phi"], ["--omit-listing"])
        , ("with --omit-comments and --output != xmir", ["rewrite", "--omit-comments", "--output=phi"], ["--omit-comments"])
        ,
          ( "with --expression and --output != latex"
          , ["rewrite", "--expression=foo", "--output=phi"]
          , ["--expression option can stay together with --output=latex only"]
          )
        ,
          ( "with --label and --output != latex"
          , ["rewrite", "--label=foo", "--output=phi"]
          , ["--label option can stay together with --output=latex only"]
          )
        ,
          ( "with --compress and --output != latex"
          , ["rewrite", "--compress", "--output=phi"]
          , ["--compress option can stay together with --output=latex only"]
          )
        ,
          ( "with --meet-prefix and --output != latex"
          , ["rewrite", "--meet-prefix=foo", "--output=phi"]
          , ["--meet-prefix option can stay together with --output=latex only"]
          )
        ,
          ( "with wrong --hide option"
          , ["rewrite", "--hide=Q.x(Q.y)"]
          , ["[ERROR]: Invalid set of arguments: Only dispatch expression", "but given: Φ.x( Φ.y )"]
          )
        , ("with many --show options", ["rewrite", "--show=Q.x.y", "--show=hello"], ["The option --show can be used only once"])
        ,
          ( "with wrong --show option"
          , ["rewrite", "--show=Q.x(Q.y)"]
          , ["[ERROR]:", "Only dispatch expression started with Φ (or Q) can be used in --show"]
          )
        , ("with --show overlapping --hide", ["rewrite", "--show=Q.x", "--hide=Q.x"], ["[ERROR]:", "The --show locator 'Φ.x' is also listed in --hide"])
        , ("with --hide of an ancestor of --show", ["rewrite", "--show=Q.y.z", "--hide=Q.y"], ["[ERROR]:", "The --show locator 'Φ.y.z' lies inside the --hide locator 'Φ.y'"])
        , ("with --meet-popularity < 0", ["rewrite", "--meet-popularity=-1"], ["[ERROR]:", "--meet-popularity must be positive"])
        , ("with --meet-popularity > 100", ["rewrite", "--meet-popularity=102"], ["[ERROR]:", "--meet-popularity must be <= 100"])
        ,
          ( "with --meet-popularity and output != latex"
          , ["rewrite", "--meet-popularity=51", "--output=phi"]
          , ["[ERROR]:", "--meet-popularity option can stay together with --output=latex only"]
          )
        ,
          ( "with --meet-length and output != latex"
          , ["rewrite", "--meet-length=4", "--output=phi"]
          , ["[ERROR]:", "--meet-length option can stay together with --output=latex only"]
          )
        , ("with non-dispatch --focus", ["rewrite", "--focus=Q.x(Q.y)"], ["[ERROR]"])
        , ("with --focus!=Q and --output=XMIR", ["rewrite", "--focus=Q.x", "--output=xmir"], ["[ERROR]"])
        , ("with --margin < 0", ["rewrite", "--margin=-1"], ["[ERROR]"])
        , ("with --breakpoint which does not exist across the rules", ["rewrite", "--breakpoint=hello", "--normalize"], ["[ERROR]"])
        ]
        (\(desc, args, expected) -> it desc (withStdin "" (testCLIFailed args expected)))

    it "prints help" $
      testCLISucceeded
        ["rewrite", "--help"]
        ["Rewrite the 𝜑-expression", "--seed SEED"]

    it "accepts --seed flag" $
      withStdin "[[ x -> 5 ]]" $
        testCLISucceeded
          ["rewrite", "--seed=42", "--sweet"]
          ["5:x"]

    it "defaults --seed to 0 in help" $
      testCLISucceeded
        ["rewrite", "--help"]
        ["default: 0"]

    it "reproduces the same shuffle order for the same --seed" $ do
      let args =
            [ "rewrite"
            , "--shuffle"
            , "--seed=42"
            , "--sweet"
            , "--sequence"
            , "--max-depth=1"
            , "--max-cycles=1"
            , rule "swap-a.yaml"
            , rule "swap-b.yaml"
            ]
      (firstRun, _) <- withStdin "[[ x -> 5 ]]" $ withStdout (runCLI args)
      (secondRun, _) <- withStdin "[[ x -> 5 ]]" $ withStdout (runCLI args)
      firstRun `shouldBe` secondRun

    it "fails with a non-integer --seed" $
      withStdin "[[ ]]" $
        testCLIFailed
          ["rewrite", "--seed=abc"]
          ["[ERROR]"]

    it "saves steps to dir with --steps-dir" $
      withTempDirectory "phino-steps" $ \dir ->
        withStdin "[[ x -> \"hello\"]]" $ do
          testCLISucceeded
            ["rewrite", rule "infinite.yaml", "--max-cycles=2", "--max-depth=2", "--steps-dir=" ++ dir, "--sweet"]
            ["hello_hi_hi"]
          doesDirectoryExist dir `shouldReturn` True
          files <- listDirectory dir
          length files `shouldBe` 4
          doesFileExist (dir ++ "/00001.phi") `shouldReturn` True
          doesFileExist (dir ++ "/00003.phi") `shouldReturn` True

    it "gives the saved steps the --canonize and --hide of the printed ones" $
      withTempDirectory "phino-steps-filtered" $ \dir ->
        withStdin "[[ m -> [[ x -> [[ L> Plus ]], y -> $.x ]].y, k -> [[ L> Minus ]] ]]" $ do
          testCLISucceeded
            ["rewrite", "--normalize", "--hide=Q.k", "--canonize", "--steps-dir=" ++ dir, "--flat"]
            ["Fn1"]
          files <- listDirectory dir
          null files `shouldBe` False
          saved <- mapM (\file -> readFile (dir ++ "/" ++ file)) files
          concat saved `shouldNotContain` "Minus"
          concat saved `shouldNotContain` "Plus"

    it "saves dataize steps to dir with --steps-dir" $
      withTempDirectory "phino-steps-dataize" $ \dir ->
        withStdin "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(^, x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6).plus(7) ]]" $ do
          testCLISucceeded
            ["dataize", symbolic, "--steps-dir=" ++ dir, "--sweet"]
            ["40-45"]
          doesDirectoryExist dir `shouldReturn` True
          files <- listDirectory dir
          let steps = sort files
          steps `shouldBe` map (\n -> printf "%05d.phi" (n :: Int)) [1 .. length steps]
          length steps `shouldSatisfy` (> 18)

    it "saves steps with a .tex extension when --output=latex is used with --steps-dir" $
      withTempDirectory "phino-steps-latex" $ \dir ->
        withStdin "[[ x -> \"hello\"]]" $ do
          testCLISucceeded
            ["rewrite", rule "infinite.yaml", "--max-cycles=2", "--max-depth=2", "--steps-dir=" ++ dir, "--output=latex", "--sweet"]
            ["\\begin{phiquation}"]
          doesDirectoryExist dir `shouldReturn` True
          files <- listDirectory dir
          length files `shouldBe` 4
          doesFileExist (dir ++ "/00001.tex") `shouldReturn` True
          doesFileExist (dir ++ "/00003.tex") `shouldReturn` True

    it "desugares without any rules flag from file" $
      testCLISucceeded
        ["rewrite", resource "desugar.phi"]
        ["⟦ foo ↦ ξ.x ⟧"]

    it "desugares with without any rules flag from stdin" $
      withStdin "[[foo ↦ x]]" $
        testCLISucceeded ["rewrite"] ["⟦ foo ↦ ξ.x ⟧"]

    it "keeps the bytes of a string intact while desugaring it" $
      withStdin "⟦ φ ↦ Φ.string(as-bytes ↦ Φ.bytes(data ↦ ⟦ Δ ⤍ 65-0A-65, ρ ↦ ∅ ⟧)), ρ ↦ ∅ ⟧" $
        testCLISucceeded ["rewrite", "--flat"] ["Δ ⤍ 65-0A-65"]

    it "rewrites with single rule" $
      withStdin "T(x -> Q.y)" $
        testCLISucceeded ["rewrite", "--rule=resources/normalize/dc.yaml"] ["⊥"]

    it "fails when a rewriting rule uses a dataization-only function" $
      withStdin "⟦⟧" $
        testCLIFailed
          ["rewrite", rule "evaluate-in-rewrite.yaml"]
          ["Function 'evaluate' in rule 'uses-evaluate' is available only for dataization and morphing, not for rewriting"]

    it "names the join function in the error message" $
      withStdin "⟦⟧" $
        testCLIFailed
          ["rewrite", rule "join-broken.yaml"]
          ["Function join() can work with bindings only"]

    it "normalizes with --normalize flag" $
      testCLISucceeded
        ["rewrite", "--normalize", resource "normalize.phi", "--margin=25"]
        [ unlines
            [ "⟦"
            , "  x ↦ ⟦"
            , "    ρ ↦ ⟦ y ↦ ⟦ ρ ↦ ∅ ⟧ ⟧"
            , "  ⟧"
            , "⟧"
            ]
        ]

    it "normalizes and applies --rule at the same time" $
      withStdin "⟦ k ↦ ⟦ m ↦ ⟦ Δ ⤍ 01- ⟧ ⟧.m, j ↦ ⟦ λ ⤍ Marker ⟧ ⟧" $
        testCLISucceeded
          ["rewrite", "--normalize", rule "marker.yaml", "--sweet"]
          ["⟦ k ↦ 01-:Δ, j ↦ FF-:Δ ⟧"]

    it "normalizes from stdin" $
      withStdin "⟦ a ↦ ⟦ b ↦ ∅ ⟧ (b ↦ [[ ]]) ⟧" $
        testCLISucceeded
          ["rewrite", "--normalize", "--margin=20"]
          ["⟦ a ↦ ⟦ b ↦ ⟦⟧ ⟧ ⟧"]

    it "rewrites with --sweet flag" $
      withStdin "[[ x -> 5]]" $
        testCLISucceeded
          ["rewrite", "--sweet"]
          ["5:x"]

    it "rewrites as XMIR" $
      withStdin "[[ x -> Q.y ]]" $
        testCLISucceeded
          ["rewrite", "--output=xmir"]
          ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<object", "  <o base=\"Φ.y\" name=\"x\"/>"]

    it "emits a real revision and ms in XMIR" $ do
      (output, _) <- withStdin "[[ x -> Q.y ]]" $ withStdout (runCLI ["rewrite", "--output=xmir"])
      let attrValue :: String -> String -> String
          attrValue name text =
            let needle = name ++ "=\""
                breakOn :: String -> Maybe String
                breakOn haystack
                  | needle `isPrefixOf` haystack = Just (drop (length needle) haystack)
                  | null haystack = Nothing
                  | otherwise = breakOn (drop 1 haystack)
             in case breakOn text of
                  Just afterNeedle -> takeWhile (/= '"') afterNeedle
                  Nothing -> ""
          revision = attrValue "revision" output
          ms = attrValue "ms" output
      revision `shouldSatisfy` (\sha -> length sha == 7 && all (`elem` "0123456789abcdef") sha)
      revision `shouldNotBe` "1234567"
      ms `shouldSatisfy` (all isDigit)

    it "rewrites as LaTeX" $
      withStdin "[[ x_o -> Q.z(y -> 5), q$ -> T, w -> $, ^ -> Q, @ -> 1, y -> \"H$@^M\", L> Fu_nc ]]" $
        testCLISucceeded
          ["rewrite", "--output=latex", "--sweet"]
          [ unlines
              [ "\\begin{phiquation}"
              , "[["
              , "  |x\\char95{}o| -> Q . |z| ( |y| -> 5 ),"
              , "  |q\\char36{}| -> T,"
              , "  |w| -> \\phiTerminal{\\xi},"
              , "  \\phiTerminal{\\rho} -> Q,"
              , "  @ -> 1,"
              , "  |y| -> \"H\\char36{}\\char64{}\\char94{}M\","
              , "  L> |Fu\\char95{}nc|"
              , "]]{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "rewrites as LaTeX without numeration" $
      withStdin "[[ x -> 5 ]]" $
        testCLISucceeded
          ["rewrite", "--output=latex", "--sweet", "--nonumber", "--flat"]
          [ unlines
              [ "\\begin{phiquation*}"
              , "5 : |x|{.}"
              , "\\end{phiquation*}"
              ]
          ]

    it "rewrites an alpha-index argument as \\alpha subscript in LaTeX" $
      withStdin "Q.foo(~1 -> Q.y)" $
        testCLISucceeded
          ["rewrite", "--output=latex", "--flat", "--nonumber"]
          [ unlines
              [ "\\begin{phiquation*}"
              , "Q . |foo| ( \\phiTerminal{\\alpha_{1}} -> Q . |y| ){.}"
              , "\\end{phiquation*}"
              ]
          ]

    it "rewrite as LaTeX with expression name" $
      withStdin "[[ x -> 5 ]]" $
        testCLISucceeded
          ["rewrite", "--output=latex", "--sweet", "--flat", "--expression=foo"]
          [ unlines
              [ "\\begin{phiquation}"
              , "\\phiExpression{foo} 5 : |x|{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "rewrite as LaTeX with label name" $
      withStdin "[[ x -> 5 ]]" $
        testCLISucceeded
          ["rewrite", "--output=latex", "--sweet", "--flat", "--label=foo"]
          [ unlines
              [ "\\begin{phiquation}\n\\label{foo}"
              , "5 : |x|{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "rewrites with XMIR as input" $
      withStdin "<object><o name=\"app\"><o name=\"x\" base=\"Φ.number\"/></o></object>" $
        testCLISucceeded
          ["rewrite", "--input=xmir", "--sweet"]
          ["Φ.number:x:app"]

    it "rewrites and prints with XMIR as input and output" $
      withStdin
        ( intercalate
            ""
            [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            , "<object><o name=\"app\"><o name=\"x\" base=\"Φ.number\"/></o></object>"
            ]
        )
        ( testCLISucceeded
            ["rewrite", "--input=xmir", "--output=xmir", "--sweet", "--flat"]
            [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            , "<listing>Φ.number:x:app</listing>"
            ]
        )

    it "rewrites as XMIR with omit-listing flag" $
      withStdin "[[ x -> Q.y ]]" $
        testCLISucceeded
          ["rewrite", "--output=xmir", "--omit-listing"]
          ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<object", "<listing>1 line(s)</listing>", "  <o base=\"Φ.y\" name=\"x\"/>"]

    it "does not fail on exactly 1 rewriting" $
      withStdin "⟦ t ↦ ⟦ x ↦ \"foo\" ⟧ ⟧" $
        testCLISucceeded
          ["rewrite", rule "simple.yaml", "--must=1", "--sweet"]
          ["\"bar\":x"]

    it "prints many expressions with --sequence" $
      withStdin "[[ x -> \"foo\" ]]" $
        testCLISucceeded
          [ "rewrite"
          , rule "first.yaml"
          , rule "second.yaml"
          , "--max-depth=1"
          , "--max-cycles=2"
          , "--sequence"
          , "--sweet"
          , "--flat"
          ]
          [ unlines
              [ "\"foo\":x"
              , "Φ.x( y ↦ \"foo\" )"
              , "\"foo\":x"
              ]
          ]

    it "prefixes every step with a header when --headers is on" $
      withStdin "[[ x -> \"foo\" ]]" $
        testCLISucceeded
          [ "rewrite"
          , rule "first.yaml"
          , rule "second.yaml"
          , "--max-depth=1"
          , "--max-cycles=2"
          , "--sequence"
          , "--headers"
          , "--sweet"
          , "--flat"
          ]
          [ intercalate
              "\n"
              [ ""
              , "=== Step #1"
              , "\"foo\":x"
              , ""
              , "=== Step #2, Rule 'first', 23t -> 26t"
              , "Φ.x( y ↦ \"foo\" )"
              , ""
              , "=== Step #3, Rule 'second', 26t -> 23t"
              , "\"foo\":x"
              ]
          ]

    it "ignores --headers without --sequence" $
      withStdin "[[ x -> \"foo\" ]]" $
        testCLISucceeded
          ["rewrite", rule "simple.yaml", "--headers", "--sweet", "--flat"]
          ["\"bar\":x"]

    it "emits step headers as LaTeX comments with --headers" $
      withStdin "[[ x -> \"foo\" ]]" $
        testCLISucceeded
          [ "rewrite"
          , rule "first.yaml"
          , rule "second.yaml"
          , "--max-depth=1"
          , "--max-cycles=2"
          , "--sequence"
          , "--headers"
          , "--sweet"
          , "--flat"
          , "--output=latex"
          ]
          [ unlines
              [ "\\begin{phiquation}"
              , "% === Step #1"
              , "\"foo\" : |x| \\phiNormalize[\\nameref{r:first}]"
              , "% === Step #2, Rule 'first', 23t -> 26t"
              , "  \\phiNormalize Q . |x| ( |y| -> \"foo\" ) \\phiNormalize[\\nameref{r:second}]"
              , "% === Step #3, Rule 'second', 26t -> 23t"
              , "  \\phiNormalize \"foo\" : |x|{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "prints only one latex preamble with --sequence" $
      withStdin "[[ x -> \"foo\" ]]" $
        testCLISucceeded
          [ "rewrite"
          , rule "first.yaml"
          , rule "second.yaml"
          , "--max-depth=1"
          , "--max-cycles=2"
          , "--sequence"
          , "--sweet"
          , "--flat"
          , "--output=latex"
          ]
          [ unlines
              [ "\\begin{phiquation}"
              , "\"foo\" : |x| \\phiNormalize[\\nameref{r:first}]"
              , "  \\phiNormalize Q . |x| ( |y| -> \"foo\" ) \\phiNormalize[\\nameref{r:second}]"
              , "  \\phiNormalize \"foo\" : |x|{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "prints meet prefix with --meet-prefix=foo in LaTeX" $
      withStdin "[[ x -> ?, y -> $.x ]](x -> [[ D> 42- ]]).y" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sweet", "--sequence", "--output=latex", "--flat", "--compress", "--meet-prefix=foo"]
          [ unlines
              [ "\\begin{phiquation}"
              , "[[ |x| -> ?, |y| -> |x| ]] ( |x| -> |42-| : D ) . |y| \\phiNormalize[\\nameref{r:copy}]"
              , "  \\phiNormalize \\phinoMeet{foo:1}{ [[ |x| -> |42-| : D, |y| -> |x| ]] } . |y| \\phiNormalize[\\nameref{r:dot}]"
              , "  \\phiNormalize |42-| : D : |x| . |x| ( \\phiTerminal{\\rho} -> \\phinoAgain{foo:1} ) \\phiNormalize[\\nameref{r:dot}]"
              , "  \\phiNormalize |42-| : D ( \\phiTerminal{\\rho} -> |42-| : D : |x|, \\phiTerminal{\\rho} -> \\phinoAgain{foo:1} ) \\phiNormalize[\\nameref{r:skip}]"
              , "  \\phiNormalize |42-| : D ( \\phiTerminal{\\rho} -> \\phinoAgain{foo:1} ) \\phiNormalize[\\nameref{r:skip}]"
              , "  \\phiNormalize |42-| : D{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "prints with compressed expressions in LaTeX" $
      withStdin "[[ x -> ?, y -> $.x ]](x -> [[ D> 42- ]]).y" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sweet", "--sequence", "--output=latex", "--flat", "--compress"]
          [ unlines
              [ "\\begin{phiquation}"
              , "[[ |x| -> ?, |y| -> |x| ]] ( |x| -> |42-| : D ) . |y| \\phiNormalize[\\nameref{r:copy}]"
              , "  \\phiNormalize \\phinoMeet{1}{ [[ |x| -> |42-| : D, |y| -> |x| ]] } . |y| \\phiNormalize[\\nameref{r:dot}]"
              , "  \\phiNormalize |42-| : D : |x| . |x| ( \\phiTerminal{\\rho} -> \\phinoAgain{1} ) \\phiNormalize[\\nameref{r:dot}]"
              , "  \\phiNormalize |42-| : D ( \\phiTerminal{\\rho} -> |42-| : D : |x|, \\phiTerminal{\\rho} -> \\phinoAgain{1} ) \\phiNormalize[\\nameref{r:skip}]"
              , "  \\phiNormalize |42-| : D ( \\phiTerminal{\\rho} -> \\phinoAgain{1} ) \\phiNormalize[\\nameref{r:skip}]"
              , "  \\phiNormalize |42-| : D{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "should not print \\phinoMeet{} twice" $
      withStdin "[[ ex -> [[ x -> [[ y -> ?, k -> [[ t -> 42]]  ]]( y -> [[ t -> 42 ]]) ]].i ]]" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sequence", "--flat", "--compress", "--output=latex", "--sweet"]
          [ unlines
              [ "\\begin{phiquation}"
              , "[[ |y| -> ?, |k| -> \\phinoMeet{1}{ 42 : |t| } ]] ( |y| -> \\phinoAgain{1} ) : |x| . |i| : |ex| \\phiNormalize[\\nameref{r:copy}]"
              , "  \\phiNormalize [[ |y| -> \\phinoAgain{1}, |k| -> \\phinoAgain{1} ]] : |x| . |i| : |ex| \\phiNormalize[\\nameref{r:stop}]"
              , "  \\phiNormalize T : |ex|{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "should not meet expression with high --meet-popularity" $
      withStdin "[[ ex -> [[ x -> [[ y -> ?, k -> [[ t -> 42]]  ]]( y -> [[ t -> 42 ]]) ]].i ]]" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sequence", "--flat", "--compress", "--output=latex", "--sweet", "--meet-popularity=70"]
          [ unlines
              [ "\\begin{phiquation}"
              , "[[ |y| -> ?, |k| -> 42 : |t| ]] ( |y| -> 42 : |t| ) : |x| . |i| : |ex| \\phiNormalize[\\nameref{r:copy}]"
              , "  \\phiNormalize [[ |y| -> 42 : |t|, |k| -> 42 : |t| ]] : |x| . |i| : |ex| \\phiNormalize[\\nameref{r:stop}]"
              , "  \\phiNormalize T : |ex|{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "meets with --meet-length=32" $
      withStdin "[[ ex -> [[ x -> [[ y -> ?, k -> [[ t -> 42]]  ]]( y -> [[ t -> 42 ]]) ]].i ]]" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sequence", "--flat", "--compress", "--output=latex", "--sweet", "--meet-length=32"]
          [ unlines
              [ "\\begin{phiquation}"
              , "[[ |y| -> ?, |k| -> 42 : |t| ]] ( |y| -> 42 : |t| ) : |x| . |i| : |ex| \\phiNormalize[\\nameref{r:copy}]"
              , "  \\phiNormalize [[ |y| -> 42 : |t|, |k| -> 42 : |t| ]] : |x| . |i| : |ex| \\phiNormalize[\\nameref{r:stop}]"
              , "  \\phiNormalize T : |ex|{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "focuses expression in latex with sequence" $
      withStdin "[[ ex -> [[ x -> [[ y -> ?, k -> [[ t -> 42]]  ]]( y -> [[ t -> 42 ]]) ]].i ]]" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sequence", "--flat", "--output=latex", "--sweet", "--focus=Q.ex"]
          [ unlines
              [ "\\begin{phiquation}"
              , "[[ |y| -> ?, |k| -> 42 : |t| ]] ( |y| -> 42 : |t| ) : |x| . |i| \\phiNormalize[\\nameref{r:copy}]"
              , "  \\phiNormalize [[ |y| -> 42 : |t|, |k| -> 42 : |t| ]] : |x| . |i| \\phiNormalize[\\nameref{r:stop}]"
              , "  \\phiNormalize T{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "focuses expression in latex without sequence" $
      withStdin "[[ ex -> [[ x -> [[ y -> ?, k -> [[ t -> 42]]  ]]( y -> [[ t -> 42 ]]) ]].i ]]" $
        testCLISucceeded
          ["rewrite", "--normalize", "--flat", "--output=latex", "--sweet", "--focus=Q.ex"]
          [ unlines
              [ "\\begin{phiquation}"
              , "T{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "shows exceeding of limits in latex" $
      withStdin "[[ x -> $.y, y -> $.x ]].x" $
        testCLISucceeded
          ["rewrite", "--normalize", "--flat", "--sequence", "--output=latex", "--sweet", "--max-depth=1", "--max-cycles=1"]
          [ unlines
              [ "\\begin{phiquation}"
              , "[[ |x| -> |y|, |y| -> |x| ]] . |x| \\phiNormalize[\\nameref{r:dot}]"
              , "  \\phiNormalize |x| : |y| . |y| ( \\phiTerminal{\\rho} -> [[ |x| -> |y|, |y| -> |x| ]] ) \\phiNormalize"
              , "  \\phiNormalize \\dots"
              , "\\end{phiquation}"
              ]
          ]

    it "focuses expression in phi without sequence" $
      withStdin "[[ ex -> [[ x -> [[ y -> ?, k -> [[ t -> 42]]  ]]( y -> [[ t -> 42 ]]) ]].i ]]" $
        testCLISucceeded
          ["rewrite", "--normalize", "--flat", "--output=phi", "--sweet", "--focus=Q.ex"]
          ["⊥"]

    it "focuses expression in phi with sequence" $
      withStdin "[[ ex -> [[ x -> [[ y -> ?, k -> [[ t -> 42]]  ]]( y -> [[ t -> 42 ]]) ]].i ]]" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sequence", "--flat", "--output=phi", "--sweet", "--focus=Q.ex"]
          [ unlines
              [ "⟦ y ↦ ∅, k ↦ 42:t ⟧( y ↦ 42:t ):x.i"
              , "⟦ y ↦ 42:t, k ↦ 42:t ⟧:x.i"
              , "⊥"
              ]
          ]

    it "prints input as listing in XMIR" $
      withStdin "[[ app -> [[]] ]]" $
        testCLISucceeded
          ["rewrite", "--output=xmir", "--omit-comments", "--sweet", "--flat"]
          ["  <listing>[[ app -> [[]] ]]</listing>"]

    it "print expression in listing in XMIRs with --sequence" $
      withStdin "[[ x -> \"foo\" ]]" $
        testCLISucceeded
          ["rewrite", "--output=xmir", "--omit-comments", "--sweet", "--flat", "--sequence", rule "simple.yaml"]
          ["  <listing>\"foo\":x</listing>", "  <listing>\"bar\":x</listing>"]

    describe "must range tests" $ do
      describe "fails" $ do
        it "when cycles exceed range ..1" $
          withStdin "[[ x -> [[ y -> 5 ]].y ]].x" $
            testCLIFailed
              ["rewrite", "--max-depth=1", "--max-cycles=2", "--normalize", "--must=..1"]
              ["it's expected rewriting cycles to be in range [..1], but rewriting has already reached 2"]

        it "when cycles below range 2.." $
          withStdin "⟦ t ↦ ⟦ x ↦ \"foo\" ⟧ ⟧" $
            testCLIFailed
              ["rewrite", rule "simple.yaml", "--must=2.."]
              ["it's expected rewriting cycles to be in range [2..], but rewriting stopped after 1"]

        it "with invalid range 5..3" $
          withStdin "[[ ]]" $
            testCLIFailed
              ["rewrite", "--must=5..3"]
              ["cannot parse value `5..3'"]

        it "with negative in range -1..5" $
          withStdin "[[ ]]" $
            testCLIFailed
              ["rewrite", "--must=-1..5"]
              ["cannot parse value `-1..5'"]

        it "with malformed range syntax" $
          withStdin "[[ ]]" $
            testCLIFailed
              ["rewrite", "--must=3...5"]
              ["cannot parse value `3...5'"]

      it "accepts range ..5 (0 to 5 cycles)" $
        withStdin "[[ ]]" $
          testCLISucceeded ["rewrite", "--must=..5", "--sweet"] ["⟦⟧"]

      it "accepts range 0..0 (exactly 0 cycles)" $
        withStdin "[[ ]]" $
          testCLISucceeded ["rewrite", "--must=0..0", "--sweet"] ["⟦⟧"]

      it "accepts range 1..1 (exactly 1 cycle)" $
        withStdin "⟦ t ↦ ⟦ x ↦ \"foo\" ⟧ ⟧" $
          testCLISucceeded
            ["rewrite", rule "simple.yaml", "--must=1..1", "--sweet"]
            ["\"bar\":x"]

      it "accepts range 1..3 when 1 cycle happens" $
        withStdin "⟦ t ↦ ⟦ x ↦ \"foo\" ⟧ ⟧" $
          testCLISucceeded
            ["rewrite", rule "simple.yaml", "--must=1..3", "--sweet"]
            ["\"bar\":x"]

      it "accepts range 0.. (0 or more)" $
        withStdin "[[ ]]" $
          testCLISucceeded ["rewrite", "--must=0..", "--sweet"] ["⟦⟧"]

    it "prints to target file" $
      withStdin "[[ ]]" $
        withTempFile "targetXXXXXX.tmp" $ \(path, h) -> do
          hClose h
          testCLISucceeded ["rewrite", "--sweet", printf "--target=%s" path] []
          content <- readFile path
          content `shouldBe` "⟦⟧"

    it "modifies file in-place" $
      withTempFile "inplaceXXXXXX.phi" $ \(path, h) -> do
        hPutStr h "[[ x -> \"foo\" ]]"
        hClose h
        testCLISucceeded ["rewrite", rule "simple.yaml", "--in-place", "--sweet", path] []
        content <- readFile path
        content `shouldBe` "\"bar\":x"

    it "skips rewriting with --update when target is newer than source" $
      withTempFileContent "src-XXXXXX.phi" "[[ x -> \"foo\" ]]" $ \src ->
        withTempFileContent "tgt-XXXXXX.phi" "ORIGINAL" $ \tgt -> do
          now <- getCurrentTime
          setModificationTime src (addUTCTime (-60) now)
          setModificationTime tgt now
          testCLISucceeded
            ["rewrite", rule "simple.yaml", "--update", "--sweet", "--target=" ++ tgt, src]
            []
          content <- readFile tgt
          content `shouldBe` "ORIGINAL"

    it "logs the skip reason at debug level when --update finds a newer target" $
      withTempFileContent "src-XXXXXX.phi" "[[ x -> \"foo\" ]]" $ \src ->
        withTempFileContent "tgt-XXXXXX.phi" "ORIGINAL" $ \tgt -> do
          now <- getCurrentTime
          setModificationTime src (addUTCTime (-60) now)
          setModificationTime tgt now
          testCLISucceeded
            ["rewrite", rule "simple.yaml", "--update", "--sweet", "--log-level=DEBUG", "--target=" ++ tgt, src]
            ["is newer than source", "skipping rewriting (--update)"]

    it "logs progress at debug level when printing to --target" $
      withStdin "[[ ]]" $
        withTempFile "targetXXXXXX.tmp" $ \(path, h) -> do
          hClose h
          testCLISucceeded
            ["rewrite", "--sweet", "--log-level=DEBUG", printf "--target=%s" path]
            ["The option '--target' is specified, printing to", "The command result was saved in"]

    it "logs progress at debug level when modifying a file in-place" $
      withTempFile "inplaceXXXXXX.phi" $ \(path, h) -> do
        hPutStr h "[[ x -> \"foo\" ]]"
        hClose h
        testCLISucceeded
          ["rewrite", rule "simple.yaml", "--in-place", "--sweet", "--log-level=DEBUG", path]
          ["The option '--in-place' is specified, writing back to", "was modified in-place"]

    it "rewrites with --update when source is newer than target" $
      withTempFileContent "src-XXXXXX.phi" "[[ x -> \"foo\" ]]" $ \src ->
        withTempFileContent "tgt-XXXXXX.phi" "ORIGINAL" $ \tgt -> do
          now <- getCurrentTime
          setModificationTime tgt (addUTCTime (-60) now)
          setModificationTime src now
          testCLISucceeded
            ["rewrite", rule "simple.yaml", "--update", "--sweet", "--target=" ++ tgt, src]
            []
          content <- readFile tgt
          content `shouldBe` "\"bar\":x"

    it "rewrites with cycles" $
      withStdin "[[ x -> \"x\" ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", rule "infinite.yaml", "--max-depth=1", "--max-cycles=2"]
          ["\"x_hi_hi\":x"]

    it "hides default package" $
      withStdin "[[ org -> [[ eolang -> [[ number -> [[]] ]]]], x -> 42 ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--hide=Q.org"]
          ["42:x"]

    it "hides several FQNs" $
      withStdin "[[ org -> [[ eolang -> Q.x, yegor256 -> Q.y ]], x -> 42 ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--hide=Q.org.eolang", "--hide=Q.org.yegor256"]
          ["⟦ org ↦ ⟦⟧, x ↦ 42 ⟧"]

    it "shows and hides" $
      withStdin "[[ org -> [[ eolang -> Q.x, yegor256 -> Q.y ]], x -> 42 ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--show=Q.org", "--hide=Q.org.eolang"]
          ["Φ.y:yegor256:org"]

    it "fails on a --show locator that matches nothing" $
      withStdin "[[ a -> [[ b -> Q, c -> Q ]], d -> Q ]]" $
        testCLIFailed
          ["rewrite", "--flat", "--show=Q.zzz"]
          ["[ERROR]:", "Can't find object by locator: 'Φ.zzz'"]

    it "shows the whole program with --show=Q" $
      withStdin "[[ a -> [[ b -> Q, c -> Q ]], d -> Q ]]" $
        testCLISucceeded
          ["rewrite", "--flat", "--show=Q"]
          ["⟦ a ↦ ⟦ b ↦ Φ, c ↦ Φ ⟧, d ↦ Φ ⟧"]

    it "prints in line with --flat" $
      withStdin "[[ x -> 5, y -> \"hey\", z -> [[ w -> [[ ]] ]] ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat"]
          ["⟦ x ↦ 5, y ↦ \"hey\", z ↦ ⟦⟧:w ⟧"]

    it "removes unnecessary rho bindings in primitive applications" $
      withStdin
        ( unlines
            [ "[["
            , "  z -> [[ x -> [[ t -> 42 ]].t ]].x,"
            , "  org -> [[ eolang -> [[ bytes -> [[ data -> ? ]], number -> [[ as-bytes -> ? ]] ]] ]]"
            , "]]"
            ]
        )
        ( testCLISucceeded
            ["rewrite", "--sweet", "--normalize", "--flat"]
            ["⟦ z ↦ 42, org ↦ ⟦ bytes(data) ↦ ⟦⟧, number(as-bytes) ↦ ⟦⟧ ⟧:eolang ⟧"]
        )

    it "reduces log message" $
      withStdin "[[ x -> [[ y -> ? ]](y -> 5) ]]" $
        testCLISucceeded
          ["rewrite", "--log-level=debug", "--log-lines=1", "--normalize"]
          [ intercalate
              "\n"
              [ "[DEBUG]: Applied 'copy' (32 nodes -> 27 nodes)"
              , "---| log is limited by --log-lines=1 option |---"
              ]
          ]

    it "reports a condition that raised while being evaluated" $
      withStdin "[[ x -> [[ y -> ∅ ]] ]]" $
        testCLISucceeded
          ["rewrite", rule "raising-condition.yaml", "--log-level=debug", "--flat"]
          [ "raised and was treated as not met: user error (Only data objects and bytes are supported"
          , "⟦ x ↦ ⟦ y ↦ ∅ ⟧ ⟧"
          ]

    it "canonizes expression" $
      withStdin "[[ x -> [[ y -> [[ L> Func ]].q, z -> Q.x(a -> [[ w -> [[ L> Atom ]], L> Hello ]]) ]], L> Package ]]" $
        testCLISucceeded
          ["rewrite", "--canonize", "--sweet", "--flat"]
          ["⟦ x ↦ ⟦ y ↦ Fn1:λ.q, z ↦ Φ.x( a ↦ ⟦ w ↦ Fn2:λ, λ ⤍ Fn3 ⟧ ) ⟧, λ ⤍ Package ⟧"]

    it "rewrites by locator" $
      withStdin "[[ ex -> [[ x -> [[ y -> 5 ]].y ]], abc -> [[ x -> ? ]](x -> 5) ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--locator=Q.ex", "--normalize"]
          ["⟦ ex ↦ 5:x, abc ↦ ∅:x( x ↦ 5 ) ⟧"]

    it "returns original expression on --breakpoint" $
      withStdin "[[ x -> ?, y -> $.x ]](x -> [[ D> 42- ]]).y" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--normalize", "--breakpoint=stop", "--log-level=debug"]
          [ "Applied 'copy' (22 nodes -> 17 nodes)"
          , "Rule 'stop' is a breakpoint, dropping down all the previous rewritings..."
          , "⟦ x ↦ ∅, y ↦ x ⟧( x ↦ 42-:Δ ).y"
          ]

    it "finishes under --depth-sensitive when the only match left would not change the term" $
      withTempFileContent "phino-fixpoint.yaml" "name: fix\npattern: '[[ x -> !e1, !B1 ]]'\nresult: '[[ x -> Q, !B1 ]]'\n" $ \fix ->
        withStdin "[[ x -> $ ]]" $
          testCLISucceeded ["rewrite", "--rule=" ++ fix, "--max-depth=1", "--depth-sensitive", "--flat"] ["⟦ x ↦ Φ ⟧"]

  describe "dataize" $ do
    it "prints help" $
      testCLISucceeded ["dataize", "--help"] ["Dataize the 𝜑-expression"]

    it "names every block of a --symbolic entry in its help" $
      testCLISucceeded ["dataize", "--help"] ["\"dataize\"", "\"morph\"", "\"rewrite\"", "\"symbolize\"", "\"join\""]

    it "dataizes simple expression" $
      withStdin "[[ D> 01- ]]" $
        testCLISucceeded ["dataize"] ["01-"]

    it "accepts --seed flag" $
      withStdin "[[ D> 01- ]]" $
        testCLISucceeded ["dataize", "--seed=7"] ["01-"]

    it "fails to dataize an empty object, which dataizes the terminator ⊥" $
      withStdin "[[ ]]" $
        testCLIFailed ["dataize"] ["terminator ⊥"]

    it "fails with negative --max-steps" $
      withStdin "[[ D> 01- ]]" $
        testCLIFailed ["dataize", "--max-steps=-1"] ["--max-steps must be positive"]

    it "fails on --max-steps instead of dataizing forever" $
      loopingLambdas $ \endless ->
        withStdin "⟦ @ ↦ ⟦ λ ⤍ L_loop ⟧ ⟧" $
          testCLIFailed
            ["dataize", "--symbolic=" ++ endless, "--max-steps=40"]
            ["[ERROR]: Dataization did not finish before reaching the limit of steps: --max-steps=40"]

    it "parks --max-steps on a residual with --partial" $
      loopingLambdas $ \endless ->
        withStdin "⟦ @ ↦ ⟦ λ ⤍ L_loop ⟧ ⟧" $
          testCLISucceeded
            ["dataize", "--symbolic=" ++ endless, "--max-steps=40", "--partial", "--flat", "--hide-rho"]
            ["⟦ λ ⤍ L_loop ⟧"]

    it "fails on --max-firings before --max-steps is spent" $
      loopingLambdas $ \endless ->
        withStdin "⟦ @ ↦ ⟦ λ ⤍ L_loop ⟧ ⟧" $
          testCLIFailed
            ["dataize", "--symbolic=" ++ endless, "--max-steps=400", "--max-firings=5"]
            ["[ERROR]: Evaluation did not finish before reaching the limit of firings: --max-firings=5"]

    describe "--acyclic=proven" $ do
      let circling = "⟦ cyc ↦ ⟦ x ↦ ∅, φ ↦ Φ.cyc( ξ.x ) ⟧, t ↦ Φ.cyc( ⟦⟧ ) ⟧"
      it "spends the whole budget and fails on the limit without the flag" $
        withStdin circling $
          testCLIFailed
            ["dataize", "--locator=Q.t", "--max-steps=40"]
            ["[ERROR]: Dataization did not finish before reaching the limit of steps: --max-steps=40"]

      it "names the term it came back to with the flag" $
        withStdin circling $
          testCLIFailed
            ["dataize", "--locator=Q.t", "--acyclic=proven", "--max-steps=4000"]
            ["[ERROR]: Reduction entered a formation it is already inside:"]

      it "prints the residue and exits successfully with --partial" $
        withStdin circling $
          testCLISucceeded
            ["dataize", "--locator=Q.t", "--acyclic=proven", "--partial", "--max-steps=4000", "--flat", "--hide-rho"]
            ["Φ.cyc( α0 ↦ ⟦⟧ )"]

      it "writes the cut to the protocol where the formation would have opened" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin circling $
            testCLISucceeded
              ["dataize", "--locator=Q.t", "--acyclic=proven", "--partial", "--protocol=" ++ path, "--sweet", "--hide-rho", "--flat", "--quiet"]
              []
          records <- readProtocol path
          lines records
            `shouldBe` [ "𝔻(Φ.t)"
                       , "  formation(⟦ x ↦ ⟦⟧, φ ↦ Φ.cyc( x ) ⟧)  # 𝔻(Φ.t)"
                       , "    looped(⟦ x ↦ ⟦⟧, φ ↦ Φ.cyc( x ) ⟧)  # 𝔻(Φ.t), proven"
                       ]

      it "writes the cut to the XML protocol as a self-closing element" $
        withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
          hClose stream
          withStdin circling $
            testCLISucceeded
              ["dataize", "--locator=Q.t", "--acyclic=proven", "--partial", "--protocol=" ++ path, "--sweet", "--hide-rho", "--flat", "--quiet"]
              []
          records <- readProtocol path
          lines records
            `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                       , "<dataize at=\"Φ.t\">"
                       , "  <formation at=\"Φ.t\" term=\"⟦ x ↦ ⟦⟧, φ ↦ Φ.cyc( x ) ⟧\">"
                       , "    <looped by=\"dataize\" match=\"proven\" at=\"Φ.t\" term=\"⟦ x ↦ ⟦⟧, φ ↦ Φ.cyc( x ) ⟧\"/>"
                       , "  </formation>"
                       , "</dataize>"
                       ]

      it "writes a plausible cut to the protocol as plausible" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin circling $
            testCLISucceeded
              ["dataize", "--locator=Q.t", "--acyclic=plausible", "--partial", "--protocol=" ++ path, "--sweet", "--hide-rho", "--flat", "--quiet"]
              []
          records <- readProtocol path
          lines records `shouldContain` ["    looped(⟦ x ↦ ⟦⟧, φ ↦ Φ.cyc( x ) ⟧)  # 𝔻(Φ.t), plausible"]

      it "refuses the flag without a mode" $
        withStdin "⟦ t ↦ ⟦ Δ ⤍ 01-02 ⟧ ⟧" $
          testCLIFailed ["dataize", "--locator=Q.t", "--acyclic"] ["The option `--acyclic` expects an argument"]

      it "refuses a mode it does not know" $
        withStdin "⟦ t ↦ ⟦ Δ ⤍ 01-02 ⟧ ⟧" $
          testCLIFailed ["dataize", "--locator=Q.t", "--acyclic=sure"] ["The value 'sure' can't be used for '--acyclic' option"]

      it "answers a terminating program the same way with the flag" $
        withStdin "⟦ t ↦ ⟦ Δ ⤍ 01-02 ⟧ ⟧" $
          testCLISucceeded ["dataize", "--locator=Q.t", "--acyclic=proven"] ["01-02"]

    it "dataizes with --sequence" $
      withStdin "[[ @ -> [[ x -> [[ D> 01-, y -> ? ]](y -> [[ ]]) ]].x ]]" $
        testCLISucceeded
          ["dataize", "--sequence", "--output=latex", "--flat", "--sweet"]
          [ intercalate
              "\n"
              [ "\\begin{phiquation}"
              , "[[ D> |01-|, |y| -> ? ]] ( |y| -> [[]] ) : |x| . |x| : @ \\phiContextualize[\\nameref{r:contextualize}]"
              , "  \\phiContextualize [[ D> |01-|, |y| -> ? ]] ( |y| -> [[]] ) : |x| . |x| \\phiNormalize[\\nameref{r:copy}]"
              , "  \\phiNormalize [[ D> |01-|, |y| -> [[]] ]] : |x| . |x| \\phiNormalize[\\nameref{r:dot}]"
              , "  \\phiNormalize [[ D> |01-|, |y| -> [[]] ]] ( \\phiTerminal{\\rho} -> [[ D> |01-|, |y| -> [[]] ]] : |x| ) \\phiNormalize[\\nameref{r:skip}]"
              , "  \\phiNormalize [[ D> |01-|, |y| -> [[]] ]] \\phiDataize[\\nameref{r:delta}]"
              , "  \\phiDataize |01-|{.}"
              , "\\end{phiquation}"
              , "01-"
              ]
          ]

    it "keeps the delta step in --sequence under --quiet" $
      withStdin "[[ D> 01- ]]" $
        testCLISucceeded
          ["dataize", "--sequence", "--quiet", "--output=latex", "--flat", "--sweet"]
          [ intercalate
              "\n"
              [ "|01-| : D \\phiDataize[\\nameref{r:delta}]"
              , "  \\phiDataize |01-|{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "ends the phi --sequence at the bare data" $
      withStdin "[[ D> 01- ]]" $
        testCLISucceeded
          ["dataize", "--sequence", "--quiet", "--flat", "--sweet"]
          ["01-:Δ\n01-"]

    it "focuses a compressed sequence whose meet replaces a step root" $
      withStdin "[[ @ -> [[ @ -> $.c.plus( 32.0 ), c -> 25.0 ]], bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus -> [[ ^ -> ?, x -> ?, L> L_number_plus ]] ]] ]]" $
        testCLISucceeded
          ["dataize", symbolic, "--output=latex", "--sweet", "--nonumber", "--compress", "--canonize", "--meet-prefix=dataization", "--sequence", "--flat", "--quiet", "--hide=Q.bytes", "--hide=Q.number", "--locator=Q.@", "--focus=Q.@", "--meet-length=5", "--meet-popularity=1"]
          ["\\phinoMeet{dataization:1}{ [[ @ -> |c| . |plus| ( 32 ), |c| -> 25 ]] } \\phiContextualize[\\nameref{r:contextualize}]"]

    it "compresses a canonized whole-expression sequence into a meet" $
      withStdin "[[ @ -> [[ @ -> $.c.plus( 32.0 ), c -> 25.0 ]], bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus -> [[ ^ -> ?, x -> ?, L> L_number_plus ]] ]] ]]" $
        testCLISucceeded
          ["dataize", symbolic, "--output=latex", "--sweet", "--nonumber", "--compress", "--canonize", "--meet-prefix=dataization", "--sequence", "--flat", "--quiet", "--meet-length=5", "--meet-popularity=1"]
          ["\\phinoMeet{dataization:1}"]

    it "canonizes the residue it prints with --partial" $
      withStdin "[[ @ -> [[ L> Foo ]] ]]" $
        testCLISucceeded ["dataize", "--partial", "--canonize", "--flat", "--sweet"] ["Fn1:λ"]

    it "dataizes with --locator" $
      withStdin "[[ ex -> [[ @ -> Q.x ]], x -> [[ D> 42- ]] ]]" $
        testCLISucceeded ["dataize", "--locator=Q.ex"] ["42-"]

    it "does not print bytes with --quiet" $
      withStdin "[[ D> 01- ]]" $
        testCLISucceeded ["dataize", "--quiet"] []

    describe "--abridged" $ do
      let wide = "⟦ t ↦ ⟦ φ ↦ ⟦ Δ ⤍ 01-02 ⟧, anfang ↦ ξ.schluss, mitte ↦ ξ.anfang, schluss ↦ ξ.mitte, rand ↦ ξ.schluss ⟧ ⟧"
      it "folds a long formation in the text protocol" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin wide $
            testCLISucceeded ["dataize", "--locator=Q.t", "--protocol=" ++ path, "--abridged", "--sweet", "--hide-rho", "--quiet"] []
          records <- readProtocol path
          lines records `shouldContain` ["  formation(⟦ φ ↦ 01-02:Δ, +4 ⟧)  # 𝔻(Φ.t)"]
      it "folds a long formation in the XML protocol" $
        withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
          hClose stream
          withStdin wide $
            testCLISucceeded ["dataize", "--locator=Q.t", "--protocol=" ++ path, "--abridged", "--sweet", "--hide-rho", "--quiet"] []
          records <- readProtocol path
          lines records `shouldContain` ["  <formation at=\"Φ.t\" term=\"⟦ φ ↦ 01-02:Δ, +4 ⟧\">"]
      it "folds a long formation under the width given as the value" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin wide $
            testCLISucceeded ["dataize", "--locator=Q.t", "--protocol=" ++ path, "--abridged=64", "--sweet", "--hide-rho", "--quiet"] []
          records <- readProtocol path
          lines records `shouldContain` ["  formation(⟦ φ ↦ 01-02:Δ, +4 ⟧)  # 𝔻(Φ.t)"]
      it "keeps a formation whole under a width it fits in" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin wide $
            testCLISucceeded ["dataize", "--locator=Q.t", "--protocol=" ++ path, "--abridged=200", "--sweet", "--hide-rho", "--quiet"] []
          records <- readProtocol path
          lines records `shouldContain` ["  formation(⟦ φ ↦ 01-02:Δ, anfang ↦ schluss, mitte ↦ anfang, schluss ↦ mitte, rand ↦ schluss ⟧)  # 𝔻(Φ.t)"]
      it "refuses a width that is not a number" $
        withStdin wide $
          testCLIFailed ["dataize", "--locator=Q.t", "--protocol=breit.txt", "--abridged=breit"] ["cannot parse value `breit'"]
      it "leaves the printed result whole" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin wide $
            testCLISucceeded ["morph", "--locator=Q.t", "--protocol=" ++ path, "--abridged", "--sweet", "--hide-rho", "--flat"] ["anfang ↦ schluss, mitte ↦ anfang"]
      it "refuses the flag without a protocol" $
        withStdin wide $
          testCLIFailed ["dataize", "--locator=Q.t", "--abridged"] ["The option --abridged requires --protocol"]

    describe "--protocol" $ do
      let sum' = "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(^, x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6) ]]"
          chained = "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(^, x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6).plus(7) ]]"
          nested = "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(^, x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6.plus(7)) ]]"
          mixed = "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(^, x) -> [[ L> L_number_plus ]], times(^, x) -> [[ L> L_number_times ]] ]], @ -> 5.plus(6).times(7) ]]"
      it "opens the protocol with the run it is the protocol of" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin "[[ D> 01- ]]" $
            testCLISucceeded ["dataize", "--protocol=" ++ path, "--quiet"] []
          records <- readProtocol path
          records `shouldBe` "𝔻(Φ)\n"

      it "closes the protocol with its msec on the third line from the end" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin sum' $
            testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet"] []
          records <- readUtf8 path
          (lines records !! (length (lines records) - 3)) `shouldSatisfy` isPrefixOf "msec("

      it "closes the protocol with the firings it counted" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin sum' $
            testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet"] []
          records <- readUtf8 path
          lines records `shouldContain` ["firings(1)"]

      it "closes the protocol with its fps on the last line" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin sum' $
            testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet"] []
          records <- readUtf8 path
          last (lines records) `shouldSatisfy` isPrefixOf "fps("

      it "closes the protocol with zero firings when the run fires nothing" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin "[[ D> 01- ]]" $
            testCLISucceeded ["dataize", "--protocol=" ++ path, "--quiet"] []
          records <- readUtf8 path
          lines records `shouldContain` ["firings(0)"]

      it "writes one line per operand and one per answer of a firing" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin sum' $
            testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
          records <- readProtocol path
          lines records
            `shouldBe` [ "𝔻(Φ)"
                       , "  formation(⟦ bytes(φ) ↦ ⟦⟧, number(φ) ↦ ⟦ plus(x) ↦ L_number_plus:λ ⟧, φ ↦ 5.plus( 6 ) ⟧)  # 𝔻(Φ)"
                       , "    𝔼(L_number_plus)  # 𝔻(Φ)"
                       , "      formation(⟦ φ ↦ Φ.bytes( φ ↦ 40-14-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ ⟧)  # 𝔻(Φ.a🌵0)"
                       , "        formation(40-14-00-00-00-00-00-00:Δ:φ)  # 𝔻(Φ.a🌵0)"
                       , "      𝛿1.1 := 40-14-00-00-00-00-00-00  # 𝔻(ξ.ρ)"
                       , "      formation(⟦ φ ↦ Φ.bytes( φ ↦ 40-18-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ ⟧)  # 𝔻(Φ.a🌵1)"
                       , "        formation(40-18-00-00-00-00-00-00:Δ:φ)  # 𝔻(Φ.a🌵1)"
                       , "      𝛿2.1 := 40-18-00-00-00-00-00-00  # 𝔻(ξ.x)"
                       , "      𝑛.1.1 := Φ.number( φ ↦ 𝜎1:λ )  # 𝑛"
                       , "      𝑛.1.2 := ⟦ φ ↦ 𝜎1:λ, plus(x) ↦ L_number_plus:λ ⟧  # 𝕄(𝑛.1.1)"
                       , "    formation(⟦ φ ↦ 𝜎1:λ, plus(x) ↦ L_number_plus:λ ⟧)  # 𝔻(Φ)"
                       ]

      it "numbers the firings of one entry apart and names the symbol between them" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin chained $
            testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
          records <- readProtocol path
          lines records
            `shouldBe` [ "𝔻(Φ)"
                       , "  formation(⟦ bytes(φ) ↦ ⟦⟧, number(φ) ↦ ⟦ plus(x) ↦ L_number_plus:λ ⟧, φ ↦ 5.plus( 6 ).plus( 7 ) ⟧)  # 𝔻(Φ)"
                       , "    𝔼(L_number_plus)  # 𝕄(Φ)"
                       , "      formation(⟦ φ ↦ Φ.bytes( φ ↦ 40-14-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ ⟧)  # 𝔻(Φ.a🌵0)"
                       , "        formation(40-14-00-00-00-00-00-00:Δ:φ)  # 𝔻(Φ.a🌵0)"
                       , "      𝛿1.1 := 40-14-00-00-00-00-00-00  # 𝔻(ξ.ρ)"
                       , "      formation(⟦ φ ↦ Φ.bytes( φ ↦ 40-18-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ ⟧)  # 𝔻(Φ.a🌵1)"
                       , "        formation(40-18-00-00-00-00-00-00:Δ:φ)  # 𝔻(Φ.a🌵1)"
                       , "      𝛿2.1 := 40-18-00-00-00-00-00-00  # 𝔻(ξ.x)"
                       , "      𝑛.1.1 := Φ.number( φ ↦ 𝜎1:λ )  # 𝑛"
                       , "      𝑛.1.2 := ⟦ φ ↦ 𝜎1:λ, plus(x) ↦ L_number_plus:λ ⟧  # 𝕄(𝑛.1.1)"
                       , "    𝔼(L_number_plus)  # 𝔻(Φ)"
                       , "      formation(⟦ φ ↦ 𝜎1:λ, plus(x) ↦ L_number_plus:λ ⟧)  # 𝔻(Φ.a🌵2)"
                       , "      𝛿1.2 := 𝔻(𝜎1:λ)  # 𝔻(ξ.ρ)"
                       , "      formation(⟦ φ ↦ Φ.bytes( φ ↦ 40-1C-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ ⟧)  # 𝔻(Φ.a🌵3)"
                       , "        formation(40-1C-00-00-00-00-00-00:Δ:φ)  # 𝔻(Φ.a🌵3)"
                       , "      𝛿2.2 := 40-1C-00-00-00-00-00-00  # 𝔻(ξ.x)"
                       , "      𝑛.2.1 := Φ.number( φ ↦ 𝜎2:λ )  # 𝑛"
                       , "      𝑛.2.2 := ⟦ φ ↦ 𝜎2:λ, plus(x) ↦ L_number_plus:λ ⟧  # 𝕄(𝑛.2.1)"
                       , "    formation(⟦ φ ↦ 𝜎2:λ, plus(x) ↦ L_number_plus:λ ⟧)  # 𝔻(Φ)"
                       ]

      it "numbers the firings of different entries apart" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin mixed $
            testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
          records <- readProtocol path
          lines records
            `shouldBe` [ "𝔻(Φ)"
                       , "  formation(⟦ bytes(φ) ↦ ⟦⟧, number(φ) ↦ ⟦ plus(x) ↦ L_number_plus:λ, times(x) ↦ L_number_times:λ ⟧, φ ↦ 5.plus( 6 ).times( 7 ) ⟧)  # 𝔻(Φ)"
                       , "    𝔼(L_number_plus)  # 𝕄(Φ)"
                       , "      formation(⟦ φ ↦ Φ.bytes( φ ↦ 40-14-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ, times(x) ↦ L_number_times:λ ⟧)  # 𝔻(Φ.a🌵0)"
                       , "        formation(40-14-00-00-00-00-00-00:Δ:φ)  # 𝔻(Φ.a🌵0)"
                       , "      𝛿1.1 := 40-14-00-00-00-00-00-00  # 𝔻(ξ.ρ)"
                       , "      formation(⟦ φ ↦ Φ.bytes( φ ↦ 40-18-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ, times(x) ↦ L_number_times:λ ⟧)  # 𝔻(Φ.a🌵1)"
                       , "        formation(40-18-00-00-00-00-00-00:Δ:φ)  # 𝔻(Φ.a🌵1)"
                       , "      𝛿2.1 := 40-18-00-00-00-00-00-00  # 𝔻(ξ.x)"
                       , "      𝑛.1.1 := Φ.number( φ ↦ 𝜎1:λ )  # 𝑛"
                       , "      𝑛.1.2 := ⟦ φ ↦ 𝜎1:λ, plus(x) ↦ L_number_plus:λ, times(x) ↦ L_number_times:λ ⟧  # 𝕄(𝑛.1.1)"
                       , "    𝔼(L_number_times)  # 𝔻(Φ)"
                       , "      formation(⟦ φ ↦ 𝜎1:λ, plus(x) ↦ L_number_plus:λ, times(x) ↦ L_number_times:λ ⟧)  # 𝔻(Φ.a🌵2)"
                       , "      𝛿1.2 := 𝔻(𝜎1:λ)  # 𝔻(ξ.ρ)"
                       , "      formation(⟦ φ ↦ Φ.bytes( φ ↦ 40-1C-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ, times(x) ↦ L_number_times:λ ⟧)  # 𝔻(Φ.a🌵3)"
                       , "        formation(40-1C-00-00-00-00-00-00:Δ:φ)  # 𝔻(Φ.a🌵3)"
                       , "      𝛿2.2 := 40-1C-00-00-00-00-00-00  # 𝔻(ξ.x)"
                       , "      𝑛.2.1 := Φ.number( φ ↦ 𝜎2:λ )  # 𝑛"
                       , "      𝑛.2.2 := ⟦ φ ↦ 𝜎2:λ, plus(x) ↦ L_number_plus:λ, times(x) ↦ L_number_times:λ ⟧  # 𝕄(𝑛.2.1)"
                       , "    formation(⟦ φ ↦ 𝜎2:λ, plus(x) ↦ L_number_plus:λ, times(x) ↦ L_number_times:λ ⟧)  # 𝔻(Φ)"
                       ]

      it "nests the firing an operand of another firing brought down" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin nested $
            testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
          records <- readProtocol path
          lines records
            `shouldBe` [ "𝔻(Φ)"
                       , "  formation(⟦ bytes(φ) ↦ ⟦⟧, number(φ) ↦ ⟦ plus(x) ↦ L_number_plus:λ ⟧, φ ↦ 5.plus( 6.plus( 7 ) ) ⟧)  # 𝔻(Φ)"
                       , "    𝔼(L_number_plus)  # 𝔻(Φ)"
                       , "      formation(⟦ φ ↦ Φ.bytes( φ ↦ 40-14-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ ⟧)  # 𝔻(Φ.a🌵0)"
                       , "        formation(40-14-00-00-00-00-00-00:Δ:φ)  # 𝔻(Φ.a🌵0)"
                       , "      𝛿1.1 := 40-14-00-00-00-00-00-00  # 𝔻(ξ.ρ)"
                       , "      𝔼(L_number_plus)  # 𝔻(Φ.a🌵1)"
                       , "        formation(⟦ φ ↦ Φ.bytes( φ ↦ 40-18-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ ⟧)  # 𝔻(Φ.a🌵2)"
                       , "          formation(40-18-00-00-00-00-00-00:Δ:φ)  # 𝔻(Φ.a🌵2)"
                       , "        𝛿1.2 := 40-18-00-00-00-00-00-00  # 𝔻(ξ.ρ)"
                       , "        formation(⟦ φ ↦ Φ.bytes( φ ↦ 40-1C-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ ⟧)  # 𝔻(Φ.a🌵3)"
                       , "          formation(40-1C-00-00-00-00-00-00:Δ:φ)  # 𝔻(Φ.a🌵3)"
                       , "        𝛿2.2 := 40-1C-00-00-00-00-00-00  # 𝔻(ξ.x)"
                       , "        𝑛.2.1 := Φ.number( φ ↦ 𝜎1:λ )  # 𝑛"
                       , "        𝑛.2.2 := ⟦ φ ↦ 𝜎1:λ, plus(x) ↦ L_number_plus:λ ⟧  # 𝕄(𝑛.2.1)"
                       , "      formation(⟦ φ ↦ 𝜎1:λ, plus(x) ↦ L_number_plus:λ ⟧)  # 𝔻(Φ.a🌵1)"
                       , "      𝛿2.1 := 𝔻(𝜎1:λ)  # 𝔻(ξ.x)"
                       , "      𝑛.1.1 := Φ.number( φ ↦ 𝜎2:λ )  # 𝑛"
                       , "      𝑛.1.2 := ⟦ φ ↦ 𝜎2:λ, plus(x) ↦ L_number_plus:λ ⟧  # 𝕄(𝑛.1.1)"
                       , "    formation(⟦ φ ↦ 𝜎2:λ, plus(x) ↦ L_number_plus:λ ⟧)  # 𝔻(Φ)"
                       ]

      it "writes what is known about every symbol a 'symbolize' line minted" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withLambdasOf (T.pack "- λ: L_stand\n  morph:\n    𝑛1: $.x\n  symbolize:\n    𝑛2: 𝑛1\n  𝑛: ⟦ z ↦ 𝑛2 ⟧\n") $ \stands ->
            withStdin "⟦ y ↦ ⟦ x ↦ ⟦ Δ ⤍ 01- ⟧, λ ⤍ L_stand ⟧.z ⟧" $
              testCLISucceeded ["morph", "--symbolic=" ++ stands, "--locator=Q.y", "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
          records <- readProtocol path
          lines records
            `shouldBe` [ "𝕄(Φ.y)"
                       , "  𝔼(L_stand)  # 𝕄(Φ.y)"
                       , "    𝑛1.1 := 01-:Δ  # 𝕄(ξ.x)"
                       , "    𝔻(𝜎1:λ) == 01-"
                       , "    𝑛2.1 := 𝜎1:λ  # 𝑛1"
                       , "    𝑛.1.1 := 𝜎1:λ:z  # 𝑛"
                       , "    𝑛.1.2 := 𝜎1:λ:z  # 𝕄(𝑛.1.1)"
                       ]

      it "writes a told stall to the XML protocol" $
        withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
          hClose stream
          withLambdasOf (T.pack "- λ: L_outer\n  dataize:\n    𝛿1: ξ.arg\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n") $ \outer ->
            withStdin "⟦ x ↦ ⟦ arg ↦ ⟦ λ ⤍ L_none ⟧, λ ⤍ L_outer ⟧, y ↦ ⟦ arg ↦ ⟦ λ ⤍ L_none ⟧, λ ⤍ L_outer ⟧ ⟧" $
              testCLISucceeded ["morph", "--symbolic=" ++ outer, "--deep", "--partial", "--acyclic=plausible", "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
          records <- readProtocol path
          lines records `shouldContain` ["    <stall λ=\"L_none\"/>"]

      it "writes a stuck firing to the XML protocol" $
        withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
          hClose stream
          withLambdasOf (T.pack "- λ: L_outer\n  dataize:\n    𝛿1: ξ.arg\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n") $ \outer ->
            withStdin "⟦ x ↦ ⟦ arg ↦ ⟦ λ ⤍ L_absent ⟧, λ ⤍ L_outer ⟧ ⟧" $
              testCLISucceeded ["morph", "--symbolic=" ++ outer, "--deep", "--partial", "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
          records <- readProtocol path
          lines records `shouldContain` ["    <unfinished λ=\"L_absent\"/>"]

      it "writes a starved step budget to the XML protocol" $
        withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
          hClose stream
          withLambdasOf (T.pack "- λ: L_outer\n  dataize:\n    𝛿1: ξ.arg\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n") $ \outer ->
            withStdin "⟦ x ↦ ⟦ arg ↦ ⟦ φ ↦ ⟦ φ ↦ ⟦ Δ ⤍ 07- ⟧ ⟧ ⟧, λ ⤍ L_outer ⟧ ⟧" $
              testCLISucceeded ["dataize", "--symbolic=" ++ outer, "--locator=Q.x", "--partial", "--max-steps=3", "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho", "--flat"] []
          records <- readProtocol path
          lines records `shouldContain` ["        <starved limit=\"3\" by=\"dataize\" at=\"Φ.a🌵0\"/>"]

      forM_
        [("XMLXXXXXX.xml", "<spent limit=\"5\" by="), ("textXXXXXX.txt", "spent(5)  # ")]
        ( \(template, record) ->
            it ("writes a spent firing budget to the protocol as " ++ record) $
              withTempFile template $ \(path, stream) -> do
                hClose stream
                loopingLambdas $ \endless ->
                  withStdin "⟦ @ ↦ ⟦ λ ⤍ L_loop ⟧ ⟧" $
                    testCLIFailed ["dataize", "--symbolic=" ++ endless, "--max-steps=400", "--max-firings=5", "--protocol=" ++ path] ["--max-firings=5"]
                records <- readProtocol path
                any (record `isInfixOf`) (lines records) `shouldBe` True
        )

      it "keeps the lines of a run that fails" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(^, x) -> [[ L> L_number_plus ]], nope -> [[ ^ -> ?, L> L_number_nope ]] ]], @ -> 5.plus(6).nope ]]" $
            testCLIFailed
              ["dataize", symbolic, "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"]
              ["No entry of --symbolic answers the λ function 'L_number_nope'"]
          records <- readProtocol path
          lines records
            `shouldBe` [ "𝔻(Φ)"
                       , "  formation(⟦ bytes(φ) ↦ ⟦⟧, number(φ) ↦ ⟦ plus(x) ↦ L_number_plus:λ, nope ↦ L_number_nope:λ ⟧, φ ↦ 5.plus( 6 ).nope ⟧)  # 𝔻(Φ)"
                       , "    𝔼(L_number_plus)  # 𝕄(Φ)"
                       , "      formation(⟦ φ ↦ Φ.bytes( φ ↦ 40-14-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ, nope ↦ L_number_nope:λ ⟧)  # 𝔻(Φ.a🌵0)"
                       , "        formation(40-14-00-00-00-00-00-00:Δ:φ)  # 𝔻(Φ.a🌵0)"
                       , "      𝛿1.1 := 40-14-00-00-00-00-00-00  # 𝔻(ξ.ρ)"
                       , "      formation(⟦ φ ↦ Φ.bytes( φ ↦ 40-18-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ, nope ↦ L_number_nope:λ ⟧)  # 𝔻(Φ.a🌵1)"
                       , "        formation(40-18-00-00-00-00-00-00:Δ:φ)  # 𝔻(Φ.a🌵1)"
                       , "      𝛿2.1 := 40-18-00-00-00-00-00-00  # 𝔻(ξ.x)"
                       , "      𝑛.1.1 := Φ.number( φ ↦ 𝜎1:λ )  # 𝑛"
                       , "      𝑛.1.2 := ⟦ φ ↦ 𝜎1:λ, plus(x) ↦ L_number_plus:λ, nope ↦ L_number_nope:λ ⟧  # 𝕄(𝑛.1.1)"
                       , "    unanswered(L_number_nope)  # 𝔻(L_number_nope:λ)"
                       ]

      it "truncates the lines left over from the previous run" $
        withTempFileContent "protocolXXXXXX.txt" "𝔼(L_number_gt)\n" $ \path -> do
          withStdin "[[ D> 01- ]]" $
            testCLISucceeded ["dataize", "--protocol=" ++ path, "--quiet"] []
          records <- readProtocol path
          records `shouldBe` "𝔻(Φ)\n"

      it "writes the lines in 𝜑 even with --output=xmir" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin sum' $
            testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--output=xmir", "--quiet", "--sweet", "--hide-rho"] []
          records <- readProtocol path
          records `shouldEndWith` "    formation(⟦ φ ↦ 𝜎1:λ, plus(x) ↦ L_number_plus:λ ⟧)  # 𝔻(Φ)\n"

      describe "as XML" $ do
        it "writes the document when the file is named .xml" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withStdin sum' $
              testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
            records <- readProtocol path
            lines records
              `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                         , "<dataize at=\"Φ\">"
                         , "  <formation at=\"Φ\" term=\"⟦ bytes(φ) ↦ ⟦⟧, number(φ) ↦ ⟦ plus(x) ↦ L_number_plus:λ ⟧, φ ↦ 5.plus( 6 ) ⟧\">"
                         , "    <evaluate λ=\"L_number_plus\" by=\"dataize\" at=\"Φ\">"
                         , "      <formation at=\"Φ.a🌵0\" term=\"⟦ φ ↦ Φ.bytes( φ ↦ 40-14-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ ⟧\">"
                         , "        <formation at=\"Φ.a🌵0\" term=\"40-14-00-00-00-00-00-00:Δ:φ\">"
                         , "        </formation>"
                         , "      </formation>"
                         , "      <bind meta=\"𝛿1.1\">40-14-00-00-00-00-00-00</bind>"
                         , "      <formation at=\"Φ.a🌵1\" term=\"⟦ φ ↦ Φ.bytes( φ ↦ 40-18-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ ⟧\">"
                         , "        <formation at=\"Φ.a🌵1\" term=\"40-18-00-00-00-00-00-00:Δ:φ\">"
                         , "        </formation>"
                         , "      </formation>"
                         , "      <bind meta=\"𝛿2.1\">40-18-00-00-00-00-00-00</bind>"
                         , "      <minted symbol=\"𝜎1\">40-14-00-00-00-00-00-00 40-18-00-00-00-00-00-00</minted>"
                         , "      <built meta=\"𝑛.1.1\">Φ.number( φ ↦ 𝜎1:λ )</built>"
                         , "      <answer meta=\"𝑛.1.2\">⟦ φ ↦ 𝜎1:λ, plus(x) ↦ L_number_plus:λ ⟧</answer>"
                         , "    </evaluate>"
                         , "    <formation at=\"Φ\" term=\"⟦ φ ↦ 𝜎1:λ, plus(x) ↦ L_number_plus:λ ⟧\">"
                         , "    </formation>"
                         , "  </formation>"
                         , "</dataize>"
                         ]

        it "nests the judgment one level inside a '<protocol>' root" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withStdin "[[ D> 01- ]]" $
              testCLISucceeded ["dataize", "--protocol=" ++ path, "--quiet"] []
            records <- readUtf8 path
            take 4 (lines records)
              `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                         , "<protocol>"
                         , "  <dataize at=\"Φ\">"
                         , "  </dataize>"
                         ]

        it "closes the '<protocol>' root with its msec" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withStdin "[[ D> 01- ]]" $
              testCLISucceeded ["dataize", "--protocol=" ++ path, "--quiet"] []
            records <- readUtf8 path
            (lines records !! 4) `shouldSatisfy` isPrefixOf "  <msec>"

        it "closes the '<protocol>' root with its firings" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withStdin "[[ D> 01- ]]" $
              testCLISucceeded ["dataize", "--protocol=" ++ path, "--quiet"] []
            records <- readUtf8 path
            lines records `shouldContain` ["  <firings>0</firings>"]

        it "closes the '<protocol>' root with its fps, then '</protocol>' itself" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withStdin "[[ D> 01- ]]" $
              testCLISucceeded ["dataize", "--protocol=" ++ path, "--quiet"] []
            records <- readUtf8 path
            drop 6 (lines records) `shouldBe` ["  <fps>0</fps>", "</protocol>"]

        it "nests what a φ body fires inside the formation element it was boxed from" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withStdin sum' $
              testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
            records <- readProtocol path
            lines records
              `shouldContain` [ "  <formation at=\"Φ\" term=\"⟦ bytes(φ) ↦ ⟦⟧, number(φ) ↦ ⟦ plus(x) ↦ L_number_plus:λ ⟧, φ ↦ 5.plus( 6 ) ⟧\">"
                              , "    <evaluate λ=\"L_number_plus\" by=\"dataize\" at=\"Φ\">"
                              ]

        it "closes the document even when nothing fires" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withStdin "[[ D> 01- ]]" $
              testCLISucceeded ["dataize", "--protocol=" ++ path, "--quiet"] []
            records <- readProtocol path
            lines records
              `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                         , "<dataize at=\"Φ\">"
                         , "</dataize>"
                         ]

        it "tells a manufactured datum from data by the name of the element" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withStdin chained $
              testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
            records <- readProtocol path
            lines records
              `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                         , "<dataize at=\"Φ\">"
                         , "  <formation at=\"Φ\" term=\"⟦ bytes(φ) ↦ ⟦⟧, number(φ) ↦ ⟦ plus(x) ↦ L_number_plus:λ ⟧, φ ↦ 5.plus( 6 ).plus( 7 ) ⟧\">"
                         , "    <evaluate λ=\"L_number_plus\" by=\"morph\" at=\"Φ\">"
                         , "      <formation at=\"Φ.a🌵0\" term=\"⟦ φ ↦ Φ.bytes( φ ↦ 40-14-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ ⟧\">"
                         , "        <formation at=\"Φ.a🌵0\" term=\"40-14-00-00-00-00-00-00:Δ:φ\">"
                         , "        </formation>"
                         , "      </formation>"
                         , "      <bind meta=\"𝛿1.1\">40-14-00-00-00-00-00-00</bind>"
                         , "      <formation at=\"Φ.a🌵1\" term=\"⟦ φ ↦ Φ.bytes( φ ↦ 40-18-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ ⟧\">"
                         , "        <formation at=\"Φ.a🌵1\" term=\"40-18-00-00-00-00-00-00:Δ:φ\">"
                         , "        </formation>"
                         , "      </formation>"
                         , "      <bind meta=\"𝛿2.1\">40-18-00-00-00-00-00-00</bind>"
                         , "      <minted symbol=\"𝜎1\">40-14-00-00-00-00-00-00 40-18-00-00-00-00-00-00</minted>"
                         , "      <built meta=\"𝑛.1.1\">Φ.number( φ ↦ 𝜎1:λ )</built>"
                         , "      <answer meta=\"𝑛.1.2\">⟦ φ ↦ 𝜎1:λ, plus(x) ↦ L_number_plus:λ ⟧</answer>"
                         , "    </evaluate>"
                         , "    <evaluate λ=\"L_number_plus\" by=\"dataize\" at=\"Φ\">"
                         , "      <formation at=\"Φ.a🌵2\" term=\"⟦ φ ↦ 𝜎1:λ, plus(x) ↦ L_number_plus:λ ⟧\">"
                         , "      </formation>"
                         , "      <dataize meta=\"𝛿1.2\">𝜎1:λ</dataize>"
                         , "      <formation at=\"Φ.a🌵3\" term=\"⟦ φ ↦ Φ.bytes( φ ↦ 40-1C-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ ⟧\">"
                         , "        <formation at=\"Φ.a🌵3\" term=\"40-1C-00-00-00-00-00-00:Δ:φ\">"
                         , "        </formation>"
                         , "      </formation>"
                         , "      <bind meta=\"𝛿2.2\">40-1C-00-00-00-00-00-00</bind>"
                         , "      <minted symbol=\"𝜎2\">𝜎1 40-1C-00-00-00-00-00-00</minted>"
                         , "      <built meta=\"𝑛.2.1\">Φ.number( φ ↦ 𝜎2:λ )</built>"
                         , "      <answer meta=\"𝑛.2.2\">⟦ φ ↦ 𝜎2:λ, plus(x) ↦ L_number_plus:λ ⟧</answer>"
                         , "    </evaluate>"
                         , "    <formation at=\"Φ\" term=\"⟦ φ ↦ 𝜎2:λ, plus(x) ↦ L_number_plus:λ ⟧\">"
                         , "    </formation>"
                         , "  </formation>"
                         , "</dataize>"
                         ]

        it "writes what is known about a symbol as an element of its own" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withLambdasOf (T.pack "- λ: L_stand\n  morph:\n    𝑛1: $.x\n  symbolize:\n    𝑛2: 𝑛1\n  𝑛: ⟦ z ↦ 𝑛2 ⟧\n") $ \stands ->
              withStdin "⟦ y ↦ ⟦ x ↦ ⟦ Δ ⤍ 01- ⟧, λ ⤍ L_stand ⟧.z ⟧" $
                testCLISucceeded ["morph", "--symbolic=" ++ stands, "--locator=Q.y", "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
            records <- readProtocol path
            lines records
              `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                         , "<morph at=\"Φ.y\">"
                         , "  <evaluate λ=\"L_stand\" by=\"morph\" at=\"Φ.y\">"
                         , "    <bind meta=\"𝑛1.1\">01-:Δ</bind>"
                         , "    <known symbol=\"𝜎1\">01-</known>"
                         , "    <bind meta=\"𝑛2.1\">𝜎1:λ</bind>"
                         , "    <built meta=\"𝑛.1.1\">𝜎1:λ:z</built>"
                         , "    <answer meta=\"𝑛.1.2\">𝜎1:λ:z</answer>"
                         , "  </evaluate>"
                         , "</morph>"
                         ]

        it "writes what a 'join' line knows as an element of its own" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withLambdasOf (T.pack "- λ: L_fork\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  join:\n    𝑛3: [𝑛1, 𝑛2]\n  𝑛: 𝑛3\n") $ \forks ->
              withStdin "⟦ y ↦ ⟦ a ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧, b ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ⟧, λ ⤍ L_fork ⟧.φ ⟧" $
                testCLISucceeded ["morph", "--symbolic=" ++ forks, "--locator=Q.y", "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
            records <- readProtocol path
            lines records
              `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                         , "<morph at=\"Φ.y\">"
                         , "  <evaluate λ=\"L_fork\" by=\"morph\" at=\"Φ.y\">"
                         , "    <bind meta=\"𝑛1.1\">𝜎1:λ:φ</bind>"
                         , "    <bind meta=\"𝑛2.1\">𝜎2:λ:φ</bind>"
                         , "    <joined symbol=\"𝜎3\">𝜎1 𝜎2</joined>"
                         , "    <bind meta=\"𝑛3.1\">𝜎3:λ:φ</bind>"
                         , "    <built meta=\"𝑛.1.1\">𝜎3:λ:φ</built>"
                         , "    <answer meta=\"𝑛.1.2\">𝜎3:λ:φ</answer>"
                         , "  </evaluate>"
                         , "</morph>"
                         ]

        it "writes on which side of the condition a fork raises" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withLambdasOf (T.pack "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  join:\n    𝑛3: [𝑛1, 𝑛2]\n  𝑛: 𝑛3\n") $ \forks ->
              withStdin "⟦ y ↦ ⟦ c ↦ ⟦ λ ⤍ 𝜎1 ⟧, a ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ⟧, b ↦ ⊥, λ ⤍ L_fork ⟧.φ ⟧" $
                testCLISucceeded ["morph", "--symbolic=" ++ forks, "--locator=Q.y", "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
            records <- readProtocol path
            lines records
              `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                         , "<morph at=\"Φ.y\">"
                         , "  <evaluate λ=\"L_fork\" by=\"morph\" at=\"Φ.y\">"
                         , "    <dataize meta=\"𝛿1.1\">𝜎1:λ</dataize>"
                         , "    <bind meta=\"𝑛1.1\">𝜎2:λ:φ</bind>"
                         , "    <bind meta=\"𝑛2.1\">⊥</bind>"
                         , "    <terminate symbol=\"𝜎1\" branch=\"right\"/>"
                         , "    <bind meta=\"𝑛3.1\">𝜎2:λ:φ</bind>"
                         , "    <built meta=\"𝑛.1.1\">𝜎2:λ:φ</built>"
                         , "    <answer meta=\"𝑛.1.2\">𝜎2:λ:φ</answer>"
                         , "  </evaluate>"
                         , "</morph>"
                         ]

        it "writes one 'minted' element per symbol the answer asked for" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withLambdasOf (T.pack "- λ: L_pair\n  morph:\n    𝑛1: $.x\n  𝑛: ⟦ left ↦ ⟦ λ ⤍ 𝜎 ⟧, right ↦ ⟦ λ ⤍ 𝜎 ⟧ ⟧\n") $ \pairs ->
              withStdin "⟦ y ↦ ⟦ x ↦ ⟦ Δ ⤍ 01- ⟧, λ ⤍ L_pair ⟧.left ⟧" $
                testCLISucceeded ["morph", "--symbolic=" ++ pairs, "--locator=Q.y", "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
            records <- readProtocol path
            lines records
              `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                         , "<morph at=\"Φ.y\">"
                         , "  <evaluate λ=\"L_pair\" by=\"morph\" at=\"Φ.y\">"
                         , "    <bind meta=\"𝑛1.1\">01-:Δ</bind>"
                         , "    <minted symbol=\"𝜎1\"/>"
                         , "    <minted symbol=\"𝜎2\"/>"
                         , "    <built meta=\"𝑛.1.1\">⟦ left ↦ 𝜎1:λ, right ↦ 𝜎2:λ ⟧</built>"
                         , "    <answer meta=\"𝑛.1.2\">⟦ left ↦ 𝜎1:λ, right ↦ 𝜎2:λ ⟧</answer>"
                         , "  </evaluate>"
                         , "</morph>"
                         ]

        it "writes no 'minted' element for a firing minting nothing" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withLambdasOf (T.pack "- λ: L_keep\n  morph:\n    𝑛1: $.x\n  𝑛: ⟦ z ↦ 𝑛1 ⟧\n") $ \keeps ->
              withStdin "⟦ y ↦ ⟦ x ↦ ⟦ Δ ⤍ 01- ⟧, λ ⤍ L_keep ⟧.z ⟧" $
                testCLISucceeded ["morph", "--symbolic=" ++ keeps, "--locator=Q.y", "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
            records <- readProtocol path
            lines records
              `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                         , "<morph at=\"Φ.y\">"
                         , "  <evaluate λ=\"L_keep\" by=\"morph\" at=\"Φ.y\">"
                         , "    <bind meta=\"𝑛1.1\">01-:Δ</bind>"
                         , "    <built meta=\"𝑛.1.1\">01-:Δ:z</built>"
                         , "    <answer meta=\"𝑛.1.2\">01-:Δ:z</answer>"
                         , "  </evaluate>"
                         , "</morph>"
                         ]

        it "nests a firing an operand took inside the firing that asked" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withStdin nested $
              testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
            records <- readProtocol path
            lines records
              `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                         , "<dataize at=\"Φ\">"
                         , "  <formation at=\"Φ\" term=\"⟦ bytes(φ) ↦ ⟦⟧, number(φ) ↦ ⟦ plus(x) ↦ L_number_plus:λ ⟧, φ ↦ 5.plus( 6.plus( 7 ) ) ⟧\">"
                         , "    <evaluate λ=\"L_number_plus\" by=\"dataize\" at=\"Φ\">"
                         , "      <formation at=\"Φ.a🌵0\" term=\"⟦ φ ↦ Φ.bytes( φ ↦ 40-14-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ ⟧\">"
                         , "        <formation at=\"Φ.a🌵0\" term=\"40-14-00-00-00-00-00-00:Δ:φ\">"
                         , "        </formation>"
                         , "      </formation>"
                         , "      <bind meta=\"𝛿1.1\">40-14-00-00-00-00-00-00</bind>"
                         , "      <evaluate λ=\"L_number_plus\" by=\"dataize\" at=\"Φ.a🌵1\">"
                         , "        <formation at=\"Φ.a🌵2\" term=\"⟦ φ ↦ Φ.bytes( φ ↦ 40-18-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ ⟧\">"
                         , "          <formation at=\"Φ.a🌵2\" term=\"40-18-00-00-00-00-00-00:Δ:φ\">"
                         , "          </formation>"
                         , "        </formation>"
                         , "        <bind meta=\"𝛿1.2\">40-18-00-00-00-00-00-00</bind>"
                         , "        <formation at=\"Φ.a🌵3\" term=\"⟦ φ ↦ Φ.bytes( φ ↦ 40-1C-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ ⟧\">"
                         , "          <formation at=\"Φ.a🌵3\" term=\"40-1C-00-00-00-00-00-00:Δ:φ\">"
                         , "          </formation>"
                         , "        </formation>"
                         , "        <bind meta=\"𝛿2.2\">40-1C-00-00-00-00-00-00</bind>"
                         , "        <minted symbol=\"𝜎1\">40-18-00-00-00-00-00-00 40-1C-00-00-00-00-00-00</minted>"
                         , "        <built meta=\"𝑛.2.1\">Φ.number( φ ↦ 𝜎1:λ )</built>"
                         , "        <answer meta=\"𝑛.2.2\">⟦ φ ↦ 𝜎1:λ, plus(x) ↦ L_number_plus:λ ⟧</answer>"
                         , "      </evaluate>"
                         , "      <formation at=\"Φ.a🌵1\" term=\"⟦ φ ↦ 𝜎1:λ, plus(x) ↦ L_number_plus:λ ⟧\">"
                         , "      </formation>"
                         , "      <dataize meta=\"𝛿2.1\">𝜎1:λ</dataize>"
                         , "      <minted symbol=\"𝜎2\">40-14-00-00-00-00-00-00 𝜎1</minted>"
                         , "      <built meta=\"𝑛.1.1\">Φ.number( φ ↦ 𝜎2:λ )</built>"
                         , "      <answer meta=\"𝑛.1.2\">⟦ φ ↦ 𝜎2:λ, plus(x) ↦ L_number_plus:λ ⟧</answer>"
                         , "    </evaluate>"
                         , "    <formation at=\"Φ\" term=\"⟦ φ ↦ 𝜎2:λ, plus(x) ↦ L_number_plus:λ ⟧\">"
                         , "    </formation>"
                         , "  </formation>"
                         , "</dataize>"
                         ]

        it "records a λ function no entry answers as a childless element" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withStdin "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ times(^, x) -> [[ L> L_number_times ]], nope -> [[ ^ -> ?, L> L_number_nope ]] ]], @ -> 2.times(3).nope ]]" $
              testCLISucceeded ["dataize", symbolic, "--partial", "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
            records <- readProtocol path
            lines records
              `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                         , "<dataize at=\"Φ\">"
                         , "  <formation at=\"Φ\" term=\"⟦ bytes(φ) ↦ ⟦⟧, number(φ) ↦ ⟦ times(x) ↦ L_number_times:λ, nope ↦ L_number_nope:λ ⟧, φ ↦ 2.times( 3 ).nope ⟧\">"
                         , "    <evaluate λ=\"L_number_times\" by=\"morph\" at=\"Φ\">"
                         , "      <formation at=\"Φ.a🌵0\" term=\"⟦ φ ↦ Φ.bytes( φ ↦ 40-00-00-00-00-00-00-00:Δ ), times(x) ↦ L_number_times:λ, nope ↦ L_number_nope:λ ⟧\">"
                         , "        <formation at=\"Φ.a🌵0\" term=\"40-00-00-00-00-00-00-00:Δ:φ\">"
                         , "        </formation>"
                         , "      </formation>"
                         , "      <bind meta=\"𝛿1.1\">40-00-00-00-00-00-00-00</bind>"
                         , "      <formation at=\"Φ.a🌵1\" term=\"⟦ φ ↦ Φ.bytes( φ ↦ 40-08-00-00-00-00-00-00:Δ ), times(x) ↦ L_number_times:λ, nope ↦ L_number_nope:λ ⟧\">"
                         , "        <formation at=\"Φ.a🌵1\" term=\"40-08-00-00-00-00-00-00:Δ:φ\">"
                         , "        </formation>"
                         , "      </formation>"
                         , "      <bind meta=\"𝛿2.1\">40-08-00-00-00-00-00-00</bind>"
                         , "      <minted symbol=\"𝜎1\">40-00-00-00-00-00-00-00 40-08-00-00-00-00-00-00</minted>"
                         , "      <built meta=\"𝑛.1.1\">Φ.number( φ ↦ 𝜎1:λ )</built>"
                         , "      <answer meta=\"𝑛.1.2\">⟦ φ ↦ 𝜎1:λ, times(x) ↦ L_number_times:λ, nope ↦ L_number_nope:λ ⟧</answer>"
                         , "    </evaluate>"
                         , "    <unanswered λ=\"L_number_nope\" by=\"dataize\">L_number_nope:λ</unanswered>"
                         , "  </formation>"
                         , "</dataize>"
                         ]

        it "names the root after the judgment a morphing ran" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withStdin "[[ x -> [[ L> L_number_nope ]].foo ]]" $
              testCLISucceeded ["morph", "--locator=Q.x", "--partial", "--protocol=" ++ path, "--quiet"] []
            records <- readProtocol path
            lines records
              `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                         , "<morph at=\"Φ.x\">"
                         , "  <unanswered λ=\"L_number_nope\" by=\"morph\">⟦ λ ⤍ L_number_nope ⟧</unanswered>"
                         , "</morph>"
                         ]

        it "closes the document even when the run fails" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withStdin "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ times(^, x) -> [[ L> L_number_times ]], nope -> [[ ^ -> ?, L> L_number_nope ]] ]], @ -> 2.times(3).nope ]]" $
              testCLIFailed ["dataize", symbolic, "--protocol=" ++ path] ["No entry of --symbolic answers"]
            records <- readProtocol path
            lines records
              `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                         , "<dataize at=\"Φ\">"
                         , "  <formation at=\"Φ\" term=\"⟦ bytes ↦ ⟦ φ ↦ ∅ ⟧, number ↦ ⟦ φ ↦ ∅, times ↦ ⟦ ρ ↦ ∅, x ↦ ∅, λ ⤍ L_number_times ⟧, nope ↦ ⟦ ρ ↦ ∅, λ ⤍ L_number_nope ⟧ ⟧, φ ↦ Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-00-00-00-00-00-00-00 ⟧ ) ).times( α0 ↦ Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-08-00-00-00-00-00-00 ⟧ ) ) ).nope ⟧\">"
                         , "    <evaluate λ=\"L_number_times\" by=\"morph\" at=\"Φ\">"
                         , "      <formation at=\"Φ.a🌵0\" term=\"⟦ φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-00-00-00-00-00-00-00 ⟧ ), times ↦ ⟦ ρ ↦ ∅, x ↦ ∅, λ ⤍ L_number_times ⟧, nope ↦ ⟦ ρ ↦ ∅, λ ⤍ L_number_nope ⟧ ⟧\">"
                         , "        <formation at=\"Φ.a🌵0\" term=\"⟦ φ ↦ ⟦ Δ ⤍ 40-00-00-00-00-00-00-00 ⟧ ⟧\">"
                         , "        </formation>"
                         , "      </formation>"
                         , "      <bind meta=\"𝛿1.1\">40-00-00-00-00-00-00-00</bind>"
                         , "      <formation at=\"Φ.a🌵1\" term=\"⟦ φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-08-00-00-00-00-00-00 ⟧ ), times ↦ ⟦ ρ ↦ ∅, x ↦ ∅, λ ⤍ L_number_times ⟧, nope ↦ ⟦ ρ ↦ ∅, λ ⤍ L_number_nope ⟧ ⟧\">"
                         , "        <formation at=\"Φ.a🌵1\" term=\"⟦ φ ↦ ⟦ Δ ⤍ 40-08-00-00-00-00-00-00 ⟧ ⟧\">"
                         , "        </formation>"
                         , "      </formation>"
                         , "      <bind meta=\"𝛿2.1\">40-08-00-00-00-00-00-00</bind>"
                         , "      <minted symbol=\"𝜎1\">40-00-00-00-00-00-00-00 40-08-00-00-00-00-00-00</minted>"
                         , "      <built meta=\"𝑛.1.1\">Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ )</built>"
                         , "      <answer meta=\"𝑛.1.2\">⟦ φ ↦ ⟦ λ ⤍ 𝜎1 ⟧, times ↦ ⟦ ρ ↦ ∅, x ↦ ∅, λ ⤍ L_number_times ⟧, nope ↦ ⟦ ρ ↦ ∅, λ ⤍ L_number_nope ⟧ ⟧</answer>"
                         , "    </evaluate>"
                         , "    <unanswered λ=\"L_number_nope\" by=\"dataize\">⟦ ρ ↦ Φ.number( φ ↦ ⟦ λ ⤍ 𝜎1 ⟧ ), λ ⤍ L_number_nope ⟧</unanswered>"
                         , "  </formation>"
                         , "</dataize>"
                         ]

        it "writes the terminator as the term a meta was bound to" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withLambdasOf (T.pack "- λ: L_pick\n  morph:\n    𝑛1: ξ.absent\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n") $ \picks ->
              withStdin "[[ x -> [[ here -> [[ ]], L> L_pick ]].foo ]]" $
                testCLIFailed ["morph", "--symbolic=" ++ picks, "--locator=Q.x", "--protocol=" ++ path, "--quiet", "--hide-rho"] ["No entry of --symbolic answers the λ function '𝜎1'"]
            records <- readProtocol path
            lines records
              `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                         , "<morph at=\"Φ.x\">"
                         , "  <evaluate λ=\"L_pick\" by=\"morph\" at=\"Φ.x\">"
                         , "    <bind meta=\"𝑛1.1\">⊥</bind>"
                         , "    <minted symbol=\"𝜎1\"/>"
                         , "    <built meta=\"𝑛.1.1\">⟦ λ ⤍ 𝜎1 ⟧</built>"
                         , "    <answer meta=\"𝑛.1.2\">⟦ λ ⤍ 𝜎1 ⟧</answer>"
                         , "  </evaluate>"
                         , "  <unanswered λ=\"𝜎1\" by=\"morph\">⟦ λ ⤍ 𝜎1 ⟧</unanswered>"
                         , "</morph>"
                         ]

        it "keeps writing text when the file is named anything else" $
          withTempFile "protocolXXXXXX.xmir" $ \(path, stream) -> do
            hClose stream
            withStdin sum' $
              testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
            records <- readProtocol path
            take 1 (lines records) `shouldBe` ["𝔻(Φ)"]

    describe "--partial" $ do
      let stuck = "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ times(^, x) -> [[ L> L_number_times ]], nope -> [[ ^ -> ?, L> L_number_nope ]] ]], @ -> 2.times(3).nope ]]"
          dispatched = "[[ foo -> [[ bar -> [[ ^ -> ?, L> L_number_nope ]] ]], @ -> Q.foo.bar ]]"
          wrapped = "[[ app -> [[ foo -> [[ bar -> [[ ^ -> ?, L> L_number_nope ]] ]], @ -> Q.app.foo.bar ]] ]]"
      it "fails on a λ function that cannot fire without the flag" $
        withStdin stuck $
          testCLIFailed
            ["dataize", symbolic, "--sweet", "--hide-rho"]
            ["No entry of --symbolic answers the λ function 'L_number_nope'"]

      it "prints the residue with the stuck application intact and exits successfully" $
        withStdin stuck $
          testCLISucceeded
            ["dataize", symbolic, "--partial", "--sweet", "--hide-rho"]
            ["L_number_nope:λ"]

      it "keeps what was evaluated before the stuck site in the residue" $
        withStdin stuck $
          testCLISucceeded
            ["dataize", symbolic, "--partial", "--sweet"]
            ["φ ↦ 𝜎1:λ"]

      it "records every firing before the stuck site in --protocol" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin stuck $
            testCLISucceeded ["dataize", symbolic, "--partial", "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
          records <- readProtocol path
          lines records
            `shouldBe` [ "𝔻(Φ)"
                       , "  formation(⟦ bytes(φ) ↦ ⟦⟧, number(φ) ↦ ⟦ times(x) ↦ L_number_times:λ, nope ↦ L_number_nope:λ ⟧, φ ↦ 2.times( 3 ).nope ⟧)  # 𝔻(Φ)"
                       , "    𝔼(L_number_times)  # 𝕄(Φ)"
                       , "      formation(⟦ φ ↦ Φ.bytes( φ ↦ 40-00-00-00-00-00-00-00:Δ ), times(x) ↦ L_number_times:λ, nope ↦ L_number_nope:λ ⟧)  # 𝔻(Φ.a🌵0)"
                       , "        formation(40-00-00-00-00-00-00-00:Δ:φ)  # 𝔻(Φ.a🌵0)"
                       , "      𝛿1.1 := 40-00-00-00-00-00-00-00  # 𝔻(ξ.ρ)"
                       , "      formation(⟦ φ ↦ Φ.bytes( φ ↦ 40-08-00-00-00-00-00-00:Δ ), times(x) ↦ L_number_times:λ, nope ↦ L_number_nope:λ ⟧)  # 𝔻(Φ.a🌵1)"
                       , "        formation(40-08-00-00-00-00-00-00:Δ:φ)  # 𝔻(Φ.a🌵1)"
                       , "      𝛿2.1 := 40-08-00-00-00-00-00-00  # 𝔻(ξ.x)"
                       , "      𝑛.1.1 := Φ.number( φ ↦ 𝜎1:λ )  # 𝑛"
                       , "      𝑛.1.2 := ⟦ φ ↦ 𝜎1:λ, times(x) ↦ L_number_times:λ, nope ↦ L_number_nope:λ ⟧  # 𝕄(𝑛.1.1)"
                       , "    unanswered(L_number_nope)  # 𝔻(L_number_nope:λ)"
                       ]

      it "still prints bytes when nothing gets stuck" $
        withStdin "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(^, x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6) ]]" $
          testCLISucceeded ["dataize", symbolic, "--partial"] ["40-45-00-00-00-00-00-00"]

      it "prints the residual at --locator, which XMIR has no top level for" $
        withStdin wrapped $
          testCLIFailed
            ["dataize", symbolic, "--partial", "--locator=Q.app", "--output=xmir", "--hide-rho"]
            ["[ERROR]:", "its top level must be a single binding"]

      it "prints the residual at --locator, not the whole program" $
        withStdin wrapped $ do
          (out, _) <- withStdout (runCLI ["dataize", symbolic, "--partial", "--locator=Q.app", "--hide-rho", "--flat"])
          lines out `shouldBe` ["⟦ λ ⤍ L_number_nope ⟧"]

      it "cannot print a residual of several top bindings as XMIR" $
        withStdin dispatched $
          testCLIFailed
            ["dataize", symbolic, "--partial", "--output=xmir"]
            ["[ERROR]:", "its top level must be a single binding"]

      it "prints the chain of steps ending in the residue with --sequence" $
        withStdin stuck $
          testCLISucceeded
            ["dataize", symbolic, "--partial", "--sequence", "--sweet", "--hide-rho", "--flat"]
            ["2.times( 3 ).nope", "L_number_nope:λ"]

      it "still stops on the terminator ⊥, since a wrong operand is not a stuck λ function" $
        withStdin "[[ ]]" $
          testCLIFailed ["dataize", "--partial"] ["terminator ⊥"]

    describe "--symbolic" $ do
      let sum' = "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(^, x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6) ]]"
      it "fires the λ function an entry of the file answers" $
        withStdin sum' $
          testCLISucceeded ["dataize", symbolic] ["40-45-00-00-00-00-00-00"]

      it "reports the progress of the run with --log-level=INFO" $
        withStdin sum' $
          testCLISucceeded ["dataize", symbolic, "--log-level=INFO", "--quiet"] ["[INFO]: Entered "]

      it "gets stuck on every λ function when it is not given" $
        withStdin sum' $
          testCLIFailed ["dataize"] ["No entry of --symbolic answers the λ function 'L_number_plus'"]

      it "fails when the file is not there" $
        withStdin sum' $
          testCLIFailed ["dataize", "--symbolic=no-such-file.yaml"] ["no-such-file.yaml"]

      it "fails on a file that carries no entries at all, before dataizing anything" $
        withTempFileContent "symbolicXXXXXX.yaml" "nope: true\n" $ \path ->
          withStdin sum' $
            testCLIFailed ["dataize", "--symbolic=" ++ path] ["cannot be read"]

    describe "--inside" $ do
      let universe = "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(^, x) -> [[ L> L_number_plus ]] ]], @ -> [[ D> 01- ]] ]]"
      it "dataizes an expression the input does not contain" $
        withStdin universe $
          testCLISucceeded ["dataize", symbolic, "--inside=5.plus( 6 )"] ["40-45-00-00-00-00-00-00"]

      it "normalizes what it is handed before dataizing it" $
        withStdin universe $
          testCLISucceeded ["dataize", "--inside=[[ x -> [[ D> 2A- ]] ]].x"] ["2A-"]

      it "morphs inside the universe just as it dataizes inside it" $
        withStdin universe $
          testCLISucceeded ["morph", symbolic, "--inside=5.plus( 6 )", "--sweet", "--hide-rho", "--flat"] ["⟦ x ↦ 6, λ ⤍ L_number_plus ⟧"]

      it "cannot be used together with --locator" $
        withStdin universe $
          testCLIFailed ["dataize", "--inside=Q.@", "--locator=Q.@"] ["--inside and --locator cannot be used together"]

      it "fails when the input expression is not a formation" $
        withStdin "Q.x" $
          testCLIFailed ["dataize", "--inside=Q.x"] ["--inside requires the input expression to be a formation"]

    describe "fails" $ do
      it "with --output != latex and --nonumber" $
        withStdin "" $
          testCLIFailed
            ["dataize", "--nonumber", "--output=xmir"]
            ["The --nonumber option can stay together with --output=latex only"]

      it "with --omit-listing and --output != xmir" $
        withStdin "" $
          testCLIFailed
            ["dataize", "--omit-listing", "--output=phi"]
            ["--omit-listing"]

      it "with --omit-comments and --output != xmir" $
        withStdin "" $
          testCLIFailed
            ["dataize", "--omit-comments", "--output=phi"]
            ["--omit-comments"]

      it "with --expression and --output != latex" $
        withStdin "" $
          testCLIFailed
            ["dataize", "--expression=foo", "--output=phi"]
            ["--expression option can stay together with --output=latex only"]

      it "with --label and --output != latex" $
        withStdin "" $
          testCLIFailed
            ["dataize", "--label=foo", "--output=phi"]
            ["--label option can stay together with --output=latex only"]

      it "with wrong --hide option" $
        withStdin "" $
          testCLIFailed
            ["dataize", "--hide=Q.x(Q.y)"]
            ["[ERROR]: Invalid set of arguments: Only dispatch expression", "but given: Φ.x( Φ.y )"]

      it "with wrong --show option" $
        withStdin "" $
          testCLIFailed
            ["dataize", "--show=Q.x(Q.y)"]
            ["[ERROR]:", "Only dispatch expression started with Φ (or Q) can be used in --show"]

      it "with wrong --locator option" $
        withStdin "" $
          testCLIFailed
            ["dataize", "--locator=Q.x(Q.y)"]
            ["[ERROR]:", "Only dispatch expression started with Φ (or Q) can be used in --locator"]

      it "with wrong --focus option" $
        withStdin "" $
          testCLIFailed
            ["dataize", "--focus=Q.x(Q.y)"]
            ["[ERROR]:", "Only dispatch expression started with Φ (or Q) can be used in --focus"]

    it "accepts --depth-sensitive" $
      withStdin "[[ D> 01- ]]" $
        testCLISucceeded ["dataize", "--depth-sensitive"] ["01-"]

  describe "morph" $ do
    let chained = "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(^, x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6).plus(7) ]]"
    it "prints help" $
      testCLISucceeded ["morph", "--help"] ["Morph the 𝜑-expression"]

    it "hands the top formation back untouched under the default locator" $
      withStdin "[[ D> 01- ]]" $
        testCLISucceeded ["morph", "--flat", "--hide-rho"] ["⟦ Δ ⤍ 01- ⟧"]

    it "stops at the bare saturated λ-formation" $
      withStdin chained $
        testCLISucceeded
          ["morph", symbolic, "--locator=Q.@", "--sweet", "--hide-rho", "--flat"]
          ["⟦ x ↦ 7, λ ⤍ L_number_plus ⟧"]

    it "leaves to dataize the firing that takes the same term to bytes" $
      withStdin chained $
        testCLISucceeded ["dataize", symbolic] ["40-45-00-00-00-00-00-00"]

    it "morphs the subterm --locator aims at" $
      withStdin "[[ ex -> Q.x, x -> [[ D> 42- ]] ]]" $
        testCLISucceeded ["morph", "--locator=Q.ex", "--flat", "--hide-rho"] ["⟦ Δ ⤍ 42- ⟧"]

    it "canonizes the answer it prints" $
      withStdin "[[ x -> [[ L> Foo ]], y -> [[ L> Bar ]] ]]" $
        testCLISucceeded ["morph", "--canonize", "--flat", "--sweet"] ["⟦ x ↦ Fn1:λ, y ↦ Fn2:λ ⟧"]

    it "hides a binding of the answer it prints" $
      withStdin "[[ x -> [[ L> Foo ]], y -> [[ L> Bar ]] ]]" $
        testCLISucceeded ["morph", "--hide=Q.x", "--flat", "--sweet"] ["Bar:λ:y"]

    it "shows only one binding of the answer it prints" $
      withStdin "[[ x -> [[ L> Foo ]], y -> [[ L> Bar ]] ]]" $
        testCLISucceeded ["morph", "--show=Q.x", "--flat", "--sweet"] ["Foo:λ:x"]

    it "prints ⊥ instead of failing the run" $
      withStdin "[[ x -> $ ]]" $
        testCLISucceeded ["morph", "--locator=Q.x"] ["⊥"]

    it "fails to dataize what it morphs to ⊥" $
      withStdin "[[ x -> $ ]]" $
        testCLIFailed ["dataize", "--locator=Q.x"] ["terminator ⊥"]

    it "prints the chain of morphing steps with --sequence" $
      withStdin chained $
        testCLISucceeded
          ["morph", symbolic, "--locator=Q.@", "--sequence", "--headers", "--sweet", "--hide-rho", "--flat"]
          [ "Rule 'maa'"
          , "Rule 'alpha'"
          , "Rule 'copy'"
          , "Rule 'mf'"
          , "⟦ x ↦ 7, λ ⤍ L_number_plus ⟧"
          ]

    it "writes every step of a LaTeX --sequence with the arrow of its judgment" $
      withStdin "[[ q -> [[ ]], k -> Q.q ]]" $
        testCLISucceeded
          ["morph", "--locator=Q.k", "--sequence", "--output=latex", "--flat", "--sweet", "--quiet"]
          [ intercalate
              "\n"
              [ "\\begin{phiquation}"
              , "[[ |q| -> [[]], |k| -> Q . |q| ]] \\phiMorph[\\nameref{r:md}]"
              , "  \\phiMorph [[ |q| -> [[]], |k| -> [[ |q| -> [[]], |k| -> Q . |q| ]] . |q| ]] \\phiNormalize[\\nameref{r:dot}]"
              , "  \\phiNormalize [[ |q| -> [[]], |k| -> [[]] ( \\phiTerminal{\\rho} -> Q ) ]] \\phiNormalize[\\nameref{r:skip}]"
              , "  \\phiNormalize [[ |q| -> [[]], |k| -> [[]] ]] \\phiMorph[\\nameref{r:mf}]"
              , "  \\phiMorph [[ |q| -> [[]], |k| -> [[]] ]]{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "does not print the result with --quiet" $
      withStdin "[[ D> 01- ]]" $
        testCLISucceeded ["morph", "--quiet"] []

    it "records the λ functions it fires with --protocol" $
      withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
        hClose stream
        withStdin chained $
          testCLISucceeded ["morph", symbolic, "--locator=Q.@", "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
        records <- readProtocol path
        lines records
          `shouldBe` [ "𝕄(Φ.φ)"
                     , "  𝔼(L_number_plus)  # 𝕄(Φ.φ)"
                     , "    formation(⟦ φ ↦ Φ.bytes( φ ↦ 40-14-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ ⟧)  # 𝔻(Φ.a🌵0)"
                     , "      formation(40-14-00-00-00-00-00-00:Δ:φ)  # 𝔻(Φ.a🌵0)"
                     , "    𝛿1.1 := 40-14-00-00-00-00-00-00  # 𝔻(ξ.ρ)"
                     , "    formation(⟦ φ ↦ Φ.bytes( φ ↦ 40-18-00-00-00-00-00-00:Δ ), plus(x) ↦ L_number_plus:λ ⟧)  # 𝔻(Φ.a🌵1)"
                     , "      formation(40-18-00-00-00-00-00-00:Δ:φ)  # 𝔻(Φ.a🌵1)"
                     , "    𝛿2.1 := 40-18-00-00-00-00-00-00  # 𝔻(ξ.x)"
                     , "    𝑛.1.1 := Φ.number( φ ↦ 𝜎1:λ )  # 𝑛"
                     , "    𝑛.1.2 := ⟦ φ ↦ 𝜎1:λ, plus(x) ↦ L_number_plus:λ ⟧  # 𝕄(𝑛.1.1)"
                     ]

    it "saves morphing steps to dir with --steps-dir" $
      withTempDirectory "phino-steps-morph" $ \dir ->
        withStdin chained $ do
          testCLISucceeded
            ["morph", symbolic, "--locator=Q.@", "--steps-dir=" ++ dir, "--sweet", "--hide-rho", "--flat"]
            ["⟦ x ↦ 7, λ ⤍ L_number_plus ⟧"]
          steps <- sort <$> listDirectory dir
          steps `shouldBe` map (\n -> printf "%05d.phi" (n :: Int)) [1 .. length steps]
          length steps `shouldSatisfy` (> 0)

    it "accepts --seed, --shuffle and --depth-sensitive" $
      withStdin "[[ D> 01- ]]" $
        testCLISucceeded ["morph", "--seed=7", "--shuffle", "--depth-sensitive", "--flat", "--hide-rho"] ["⟦ Δ ⤍ 01- ⟧"]

    it "returns the λ-formation dataize cannot finish on" $
      withStdin "⟦ @ ↦ ⟦ λ ⤍ L_number_div, ρ ↦ ⟦ Δ ⤍ 40-45-00-00-00-00-00-00 ⟧, x ↦ ⟦ Δ ⤍ 40-00-00-00-00-00-00-00 ⟧ ⟧ ⟧" $
        testCLISucceeded
          ["morph", "--locator=Q.@", "--max-steps=40", "--flat", "--hide-rho"]
          ["⟦ λ ⤍ L_number_div"]

    it "fails once the --max-steps budget is spent" $
      withStdin chained $
        testCLIFailed
          ["morph", "--locator=Q.@", "--max-steps=3"]
          ["[ERROR]: Dataization did not finish before reaching the limit of steps: --max-steps=3"]

    describe "--max-firings" $ do
      let splitting = withLambdasOf (T.pack "- λ: L_split\n  morph:\n    𝑛1: Φ.s.foo\n    𝑛2: Φ.s.foo\n  𝑛: ⟦ l ↦ 𝑛1, r ↦ 𝑛2 ⟧\n")
          split = "⟦ s ↦ ⟦ λ ⤍ L_split ⟧, x ↦ Φ.s.foo ⟧"
      it "fails with non-positive --max-firings" $
        withStdin split $
          testCLIFailed ["morph", "--max-firings=0"] ["--max-firings must be positive"]

      it "fails once the --max-firings budget is spent on a widening recursion" $
        splitting $ \table ->
          withStdin split $
            testCLIFailed
              ["morph", "--symbolic=" ++ table, "--locator=Q.x", "--max-firings=64"]
              ["[ERROR]: Evaluation did not finish before reaching the limit of firings: --max-firings=64"]

      it "ends the widening recursion with --partial" $
        splitting $ \table ->
          withStdin split $
            testCLISucceeded
              ["morph", "--symbolic=" ++ table, "--locator=Q.x", "--max-firings=64", "--partial", "--flat", "--hide-rho", "--sweet"]
              ["⊥"]

      it "parks the spent --max-firings budget with --deep and --partial" $
        splitting $ \table ->
          withStdin split $
            testCLISucceeded
              ["morph", "--symbolic=" ++ table, "--deep", "--max-firings=64", "--partial", "--flat", "--hide-rho", "--sweet"]
              ["x ↦ Φ.s.foo"]

      it "fires no more λ functions than --max-firings allows" $
        splitting $ \table ->
          withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
            hClose stream
            withStdin split $
              testCLISucceeded
                ["morph", "--symbolic=" ++ table, "--deep", "--max-firings=64", "--partial", "--protocol=" ++ path, "--quiet"]
                []
            records <- readProtocol path
            length (filter (isInfixOf "𝔼(L_split)") (lines records)) `shouldBe` 64

    describe "--max-seconds" $ do
      let ladder = withLambdasOf (T.pack "- λ: L_split\n  morph:\n    𝑛1: ξ.n.foo\n    𝑛2: ξ.n.foo\n  𝑛: ⟦ l ↦ 𝑛1, r ↦ 𝑛2 ⟧\n")
          rungs = "⟦ " ++ intercalate ", " [printf "l%d ↦ ⟦ λ ⤍ L_split, n ↦ Φ.l%d ⟧" rung (rung + 1) | rung <- [0 .. 23 :: Int]] ++ ", l24 ↦ ⟦⟧, x ↦ Φ.l0.foo ⟧"
          bounded :: Expectation -> Expectation
          bounded check = timeout 60000000 check >>= (`shouldBe` Just ())
      it "fails with non-positive --max-seconds" $
        withStdin rungs $
          testCLIFailed ["morph", "--max-seconds=0"] ["--max-seconds must be positive"]

      it "fails once the --max-seconds budget is spent" $
        ladder $ \table ->
          bounded $
            withStdin rungs $
              testCLIFailed
                ["morph", "--symbolic=" ++ table, "--locator=Q.x", "--max-seconds=1"]
                ["[ERROR]: Evaluation did not finish before reaching the limit of seconds: --max-seconds=1"]

      it "fails dataize once the --max-seconds budget is spent" $
        ladder $ \table ->
          bounded $
            withStdin rungs $
              testCLIFailed
                ["dataize", "--symbolic=" ++ table, "--locator=Q.x", "--max-seconds=1"]
                ["[ERROR]: Evaluation did not finish before reaching the limit of seconds: --max-seconds=1"]

      forM_ [["--locator=Q.x", "--partial"], ["--deep", "--partial"]] $ \opts ->
        it ("fails once the --max-seconds budget is spent with " ++ unwords opts) $
          ladder $ \table ->
            bounded $
              withStdin rungs $
                testCLIFailed
                  (["morph", "--symbolic=" ++ table, "--max-seconds=1"] ++ opts)
                  ["[ERROR]: Evaluation did not finish before reaching the limit of seconds: --max-seconds=1"]

      forM_ [["--locator=Q.x"], ["--locator=Q.x", "--partial"], ["--deep", "--partial"], ["--deep", "--partial", "--jobs=4"]] $ \opts ->
        it ("writes the timeout as the last line of the protocol with " ++ unwords opts) $
          ladder $ \table ->
            withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
              hClose stream
              bounded $
                withStdin rungs $
                  testCLIFailed
                    (["morph", "--symbolic=" ++ table, "--max-seconds=1", "--protocol=" ++ path, "--quiet"] ++ opts)
                    ["--max-seconds=1"]
              records <- readProtocol path
              dropWhile (== ' ') (last (lines records)) `shouldStartWith` "timeout(1)  # 𝕄("

      it "writes the timeout once to the XML protocol of a deep run" $
        ladder $ \table ->
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            bounded $
              withStdin rungs $
                testCLIFailed
                  ["morph", "--symbolic=" ++ table, "--deep", "--partial", "--max-seconds=1", "--protocol=" ++ path, "--quiet"]
                  ["--max-seconds=1"]
            records <- readProtocol path
            length (filter (isInfixOf "<timeout limit=\"1\" by=\"morph\" at=\"") (lines records)) `shouldBe` 1

      it "closes the XML protocol of a run out of time" $
        ladder $ \table ->
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            bounded $
              withStdin rungs $
                testCLIFailed
                  ["morph", "--symbolic=" ++ table, "--locator=Q.x", "--max-seconds=1", "--protocol=" ++ path, "--quiet"]
                  ["--max-seconds=1"]
            document <- X.readFile X.def path
            X.nameLocalName (X.elementName (X.documentRoot document)) `shouldBe` T.pack "protocol"

    describe "--jobs" $ do
      let twins = "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(^, x) -> [[ L> L_number_plus ]] ]], a -> 7.plus( 5.plus( 6 ) ), b -> 7.plus( 5.plus( 6 ) ) ]]"
          recorded :: [String] -> IO [String]
          recorded extra =
            withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
              hClose stream
              withStdin twins $
                testCLISucceeded (["morph", symbolic, "--deep", "--protocol=" ++ path, "--quiet"] ++ extra) []
              lines <$> readProtocol path
          untaued :: String -> String
          untaued [] = []
          untaued text
            | "a🌵" `isPrefixOf` text = "a🌵" ++ untaued (dropWhile (\ch -> isDigit ch || ch == '-') (drop 2 text))
          untaued (ch : rest) = ch : untaued rest
      it "prints the answer one walk over the bindings prints" $
        withStdin twins $
          testCLISucceeded
            ["morph", symbolic, "--deep", "--acyclic=proven", "--jobs=3", "--flat", "--hide-rho", "--sweet"]
            ["a ↦ ⟦ φ ↦ 𝜎2:λ, plus(x) ↦ L_number_plus:λ ⟧, b ↦ ⟦ φ ↦ 𝜎4:λ, plus(x) ↦ L_number_plus:λ ⟧"]

      it "writes the firings of a binding after those of the bindings before it" $
        recorded ["--jobs=2"]
          >>= (`shouldBe` ["# 𝕄(Φ.a)", "# 𝕄(Φ.a)", "# 𝕄(Φ.b)", "# 𝕄(Φ.b)"]) . map (dropWhile (/= '#')) . filter (isPrefixOf "  𝔼(")

      it "numbers the symbols of the protocol the way the answer numbers them" $
        recorded ["--jobs=2"] >>= (`shouldSatisfy` elem "    𝑛.4.1 := Φ.number( φ ↦ ⟦ λ ⤍ 𝜎4 ⟧ )  # 𝑛")

      it "names what a binding mints after the binding" $
        recorded ["--jobs=2"] >>= (`shouldSatisfy` any (isInfixOf "# 𝔻(Φ.a🌵4-0)"))

      it "writes the protocol one walk writes, the names a binding mints apart" $ do
        one <- recorded ["--jobs=1"]
        many <- recorded ["--jobs=4"]
        map untaued many `shouldBe` map untaued one

      it "writes the same protocol however many workers it is given" $ do
        few <- recorded ["--jobs=2"]
        many <- recorded ["--jobs=5"]
        many `shouldBe` few

      it "keeps a memo of its own for every binding under plausible" $
        withStdin twins $
          testCLISucceeded
            ["morph", symbolic, "--deep", "--acyclic=plausible", "--jobs=2", "--flat", "--hide-rho", "--sweet"]
            ["b ↦ ⟦ φ ↦ 𝜎4:λ, plus(x) ↦ L_number_plus:λ ⟧"]

      it "fails with non-positive --jobs" $
        withStdin twins $
          testCLIFailed ["morph", "--deep", "--jobs=0"] ["--jobs must be positive"]

      it "fails with --jobs above one and no --deep" $
        withStdin twins $
          testCLIFailed ["morph", "--jobs=2"] ["The option --jobs requires --deep, since only the deep walk runs on several workers"]

    it "parks the spent budget as a residual with --partial" $
      withStdin "⟦ φ ↦ 5.gt(Φ.nan) ⟧" $
        testCLISucceeded
          ["morph", "--locator=Q.@", "--max-steps=10", "--partial", "--flat", "--hide-rho", "--sweet"]
          ["5.gt( Φ.nan )"]

    describe "--partial" $ do
      let stuck = "[[ @ -> [[ L> Sym_arg_0 ]].foo ]]"
      it "fails on a λ function that cannot fire without the flag" $
        withStdin stuck $
          testCLIFailed ["morph", "--locator=Q.@"] ["No entry of --symbolic answers the λ function 'Sym_arg_0'"]

      it "prints the residue with the stuck application intact and exits successfully" $
        withStdin stuck $
          testCLISucceeded
            ["morph", "--locator=Q.@", "--partial", "--flat", "--hide-rho"]
            ["⟦ λ ⤍ Sym_arg_0 ⟧.foo"]

    describe "--deep" $ do
      let program =
            "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, \
            \number(φ) -> [[ times(^, x) -> [[ L> L_number_times ]] ]], \
            \bar(x) -> [[ L> L_bar ]], \
            \demo -> [[ foo -> [[ n -> 3, @ -> Q.bar( $.n.times( 5 ).times( 7 ) ) ]] ]] ]]"
      it "answers the formation as it was written without the flag" $
        withStdin program $
          testCLISucceeded
            ["morph", symbolic, "--inside=Q.demo.foo", "--sweet", "--hide-rho", "--flat"]
            ["⟦ n ↦ 3, φ ↦ Φ.bar( n.times( 5 ).times( 7 ) ) ⟧"]

      it "reduces every binding it can and leaves the rest in place" $
        withStdin program $
          testCLISucceeded
            ["morph", symbolic, "--deep", "--inside=Q.demo.foo", "--sweet", "--hide-rho", "--flat"]
            ["⟦ n ↦ 3, φ ↦ Φ.bar( ⟦ φ ↦ 𝜎2:λ, times(x) ↦ L_number_times:λ ⟧ ) ⟧"]

      it "fires the bare saturated λ-formation mf hands back" $
        withStdin chained $
          testCLISucceeded
            ["morph", symbolic, "--deep", "--locator=Q.@", "--sweet", "--hide-rho", "--flat"]
            ["⟦ φ ↦ 𝜎2:λ, plus(x) ↦ L_number_plus:λ ⟧"]

      it "keeps the object model intact while it folds the program" $
        withStdin program $
          testCLISucceeded
            ["morph", symbolic, "--deep", "--sweet", "--hide-rho", "--flat"]
            [ "number(φ) ↦ ⟦ times(x) ↦ L_number_times:λ ⟧"
            , "demo ↦ ⟦ n ↦ 3, φ ↦ Φ.bar( ⟦ φ ↦ 𝜎2:λ, times(x) ↦ L_number_times:λ ⟧ ) ⟧:foo"
            ]

      it "keeps a binding whose spine got stuck with --partial" $
        withStdin "[[ x -> [[ L> Sym_arg_0 ]].foo ]]" $
          testCLISucceeded
            ["morph", "--deep", "--partial", "--sweet", "--hide-rho", "--flat"]
            ["Sym_arg_0:λ.foo:x"]

      it "fails on that same spine without --partial" $
        withStdin "[[ x -> [[ L> Sym_arg_0 ]].foo ]]" $
          testCLIFailed ["morph", "--deep"] ["No entry of --symbolic answers the λ function 'Sym_arg_0'"]

    describe "--acyclic=plausible" $ do
      let twins = "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(^, x) -> [[ L> L_number_plus ]] ]], a -> 7.plus( 5.plus( 6 ) ), b -> 7.plus( 5.plus( 6 ) ) ]]"
          recorded :: String -> IO [String]
          recorded mode =
            withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
              hClose stream
              withStdin twins $
                testCLISucceeded
                  ["morph", symbolic, "--deep", "--acyclic=" ++ mode, "--protocol=" ++ path, "--quiet"]
                  []
              lines <$> readProtocol path
      it "charges a formation once per binding spelling it under proven" $
        withStdin twins $
          testCLIFailed
            ["morph", symbolic, "--deep", "--acyclic=proven", "--max-firings=3"]
            ["[ERROR]: Evaluation did not finish before reaching the limit of firings: --max-firings=3"]

      it "charges a formation once for the run under plausible" $
        withStdin twins $
          testCLISucceeded
            ["morph", symbolic, "--deep", "--acyclic=plausible", "--max-firings=2", "--flat", "--hide-rho", "--sweet"]
            ["b ↦ ⟦ φ ↦ 𝜎2:λ, plus(x) ↦ L_number_plus:λ ⟧"]

      it "reduces the operands of a formation once per binding spelling it under proven" $
        recorded "proven" >>= (`shouldSatisfy` ((== 4) . length . filter (isInfixOf "𝛿1.")))

      it "reduces the operands of a formation once for the run under plausible" $
        recorded "plausible" >>= (`shouldSatisfy` ((== 2) . length . filter (isInfixOf "𝛿1.")))

      it "writes a recalled firing at its own site under plausible" $
        recorded "plausible" >>= (`shouldSatisfy` ((== 4) . length . filter (isInfixOf "𝔼(L_number_plus)")))

      it "answers a recalled firing with the line of the first under plausible" $
        recorded "plausible" >>= (`shouldContain` ["    𝑛.4.2 := 𝑛.2.2  # 𝕄(𝑛.4.1)"])

      it "dataizes to the same datum under plausible" $
        withStdin chained $
          testCLISucceeded
            ["dataize", symbolic, "--acyclic=plausible", "--locator=Q.@"]
            ["40-45-00-00-00-00-00-00"]

    describe "--acyclic=proven" $ do
      let looping = "⟦ x ↦ ⟦ λ ⤍ L_loop ⟧.foo ⟧"
      it "spends the whole budget and fails on the limit without the flag" $
        loopingLambdas $ \endless ->
          withStdin looping $
            testCLIFailed
              ["morph", "--symbolic=" ++ endless, "--locator=Q.x", "--max-steps=40"]
              ["[ERROR]: Dataization did not finish before reaching the limit of steps: --max-steps=40"]

      it "prints the residue and exits successfully with the flag" $
        loopingLambdas $ \endless ->
          withStdin looping $
            testCLISucceeded
              ["morph", "--symbolic=" ++ endless, "--locator=Q.x", "--acyclic=proven", "--max-steps=4000", "--flat", "--hide-rho"]
              ["⟦ λ ⤍ L_loop ⟧.foo"]

      it "answers a terminating program the same way with the flag" $
        withStdin chained $
          testCLISucceeded
            ["morph", symbolic, "--acyclic=proven", "--locator=Q.@", "--sweet", "--hide-rho", "--flat"]
            ["⟦ x ↦ 7, λ ⤍ L_number_plus ⟧"]

      it "parks the looping binding and keeps walking with --deep" $
        loopingLambdas $ \endless ->
          withStdin "⟦ x ↦ ⟦ λ ⤍ L_loop, ρ ↦ ∅ ⟧.foo, y ↦ ⟦ z ↦ ⟦⟧ ⟧ ⟧" $
            testCLISucceeded
              ["morph", "--symbolic=" ++ endless, "--deep", "--acyclic=proven", "--max-steps=4000", "--flat", "--hide-rho"]
              ["⟦ x ↦ ⟦ λ ⤍ L_loop ⟧.foo, y ↦ ⟦ z ↦ ⟦⟧ ⟧ ⟧"]

    describe "fails" $ do
      it "with --output=xmir on a top formation of several bindings" $
        withStdin "[[ x -> [[ D> 01- ]], y -> [[ D> 02- ]] ]]" $
          testCLIFailed
            ["morph", "--output=xmir"]
            ["[ERROR]:", "its top level must be a single binding"]

      it "with --output != latex and --nonumber" $
        withStdin "" $
          testCLIFailed
            ["morph", "--nonumber", "--output=xmir"]
            ["The --nonumber option can stay together with --output=latex only"]

      it "with --show used more than once" $
        withStdin "" $
          testCLIFailed
            ["morph", "--show=Q.a", "--show=Q.b"]
            ["The option --show can be used only once"]

      it "with wrong --locator option" $
        withStdin "" $
          testCLIFailed
            ["morph", "--locator=Q.x(Q.y)"]
            ["[ERROR]:", "Only dispatch expression started with Φ (or Q) can be used in --locator"]

  describe "explain" $ do
    forM_
      ["--morph", "--dataize", "--contextualize"]
      ( \judgment ->
          it ("refuses --normalize together with " ++ judgment) $
            testCLIFailed ["explain", judgment, "--normalize"] ["The --normalize option cannot be used together with"]
      )

    it "prints help" $
      testCLISucceeded
        ["explain", "--help"]
        ["Explain built-in morphing rules", "Explain built-in dataization rules", "Explain built-in contextualization rules"]

    it "explains single rule" $ do
      latex <- explainPack "test-resources/explain-packs/normalize/copy.yaml"
      testCLISucceeded ["explain", "--rule=resources/normalize/copy.yaml"] [latex <> "\n"]

    it "explains single rule with a label" $
      testCLISucceeded
        ["explain", rule "labeled.yaml"]
        [ unlines
            [ "\\phinoNormalizationRule[\\lambda]{copy}"
            , "  { [[ B_1, \\tau -> ?, B_2 ]] ( \\tau -> k ) }"
            , "  { [[ B_1, \\tau -> k, B_2 ]] }"
            , "  { }"
            , "  { }"
            ]
        ]

    it "explains multiple rules" $
      testCLISucceeded
        ["explain", "--rule=resources/normalize/copy.yaml", "--rule=resources/normalize/alpha.yaml"]
        ["\\phinoNormalizationRule{copy}", "\\phinoNormalizationRule{alpha}"]

    it "reproduces the same shuffle order for the same --seed" $ do
      let args =
            [ "explain"
            , "--shuffle"
            , "--seed=42"
            , rule "swap-a.yaml"
            , rule "swap-b.yaml"
            ]
      (firstRun, _) <- withStdout (runCLI args)
      (secondRun, _) <- withStdout (runCLI args)
      firstRun `shouldBe` secondRun

    it "accepts --seed flag" $
      testCLISucceeded
        ["explain", "--seed=7", "--normalize"]
        ["\\phinoNormalizationRule{alpha}"]

    forM_
      [ ("normalization", "--normalize", "normalize")
      , ("morphing", "--morph", "morphing")
      , ("dataization", "--dataize", "dataization")
      , ("contextualization", "--contextualize", "contextualization")
      ]
      ( \(judgment, option, dir) -> it ("explains " <> judgment <> " rules") $ do
          packs <- allPathsIn ("test-resources/explain-packs" </> dir)
          latex <- mapM explainPack (sort packs)
          testCLISucceeded ["explain", option] [unlines latex]
      )

    it "fails with no rules specified" $
      testCLIFailed
        ["explain"]
        ["Either --rule, --normalize, --morph, --dataize or --contextualize must be specified"]

    it "fails when more than one rule set is specified" $
      testCLIFailed
        ["explain", "--morph", "--dataize"]
        ["Only one of --morph, --dataize or --contextualize can be specified"]

    it "allows --normalize together with --rule" $
      testCLISucceeded
        ["explain", "--normalize", "--rule=resources/normalize/copy.yaml"]
        ["\\phinoNormalizationRule{copy}"]

    it "allows --shuffle together with --morph" $
      testCLISucceeded
        ["explain", "--morph", "--shuffle"]
        ["\\begin{phinoMorphingInference}"]

    it "writes to target file" $
      bracket
        ( do
            tmp <- getTemporaryDirectory
            stamp <- getPOSIXTime
            let dir = tmp </> ("phino-test-" ++ show (floor stamp :: Integer))
            createDirectoryIfMissing True dir
            pure (dir </> "explain.tex", dir)
        )
        (\(_, dir) -> removeDirectoryRecursive dir)
        ( \(path, _) -> do
            testCLISucceeded ["explain", "--normalize", printf "--target=%s" path] []
            content <- readFile path
            _ <- evaluate (length content)
            content `shouldContain` "\\phinoNormalizationRule{alpha}"
        )

  describe "merge" $ do
    it "prints help" $
      testCLISucceeded ["merge", "--help"] ["Paths to input files"]

    it "merges single expression" $
      testCLISucceeded
        ["merge", resource "desugar.phi", "--sweet", "--flat"]
        ["x:foo"]

    it "merges EO expressions" $
      testCLISucceeded
        ["merge", "--sweet", resource "number.phi", resource "bytes.phi", resource "string.phi", "--margin=25"]
        [ unlines
            [ "⟦"
            , "  eolang ↦ ⟦"
            , "    number(φ) ↦ ⟦⟧,"
            , "    bytes(data) ↦ ⟦⟧,"
            , "    string(φ) ↦ ⟦⟧,"
            , "    λ ⤍ Package"
            , "  ⟧,"
            , "  λ ⤍ Package"
            , "⟧:org"
            ]
        ]

    it "fails on merging non formations" $
      testCLIFailed
        ["merge", resource "dispatch.phi", resource "number.phi"]
        ["Invalid expression format, only expressions with top level formations are supported for 'merge' command"]

    it "fails on merging conflicted bindings" $
      testCLIFailed
        ["merge", resource "foo.phi", resource "desugar.phi"]
        ["Can't merge two bindings, conflict found"]

    it "fails on merging empty list of expressions" $
      testCLIFailed
        ["merge"]
        ["At least one input file must be specified for 'merge' command"]

    it "merges and prints as XMIR, with the listing rendered from the merged expression" $
      testCLISucceeded
        ["merge", resource "desugar.phi", "--output=xmir"]
        ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<listing>⟦ foo ↦ ξ.x ⟧</listing>", "<o base=\"ξ.x\" name=\"foo\"/>"]

    it "names an atom of XMIR after its locator and keeps its type" $ do
      let xmir = "<object><o name=\"number\"><o name=\"plus\"><o base=\"∅\" name=\"b\"/><o atom=\"Φ.number\" name=\"λ\"/></o></o></object>"
      withTempFileContent "phino-atom.xmir" xmir $ \file -> do
        testCLISucceeded
          ["merge", "--input=xmir", "--sweet", "--flat", file]
          ["L_number_plus:λ"]
        testCLISucceeded
          ["merge", "--input=xmir", "--output=xmir", file]
          ["<o atom=\"Φ.number\" name=\"λ\">L_number_plus</o>"]

    it "reproduces the same output for the same --seed" $ do
      let args =
            [ "merge"
            , "--seed=42"
            , "--sweet"
            , resource "number.phi"
            , resource "bytes.phi"
            ]
      (firstRun, _) <- withStdout (runCLI args)
      (secondRun, _) <- withStdout (runCLI args)
      firstRun `shouldBe` secondRun

  describe "compile" $ do
    it "writes the module to the target" $
      withTempDirectory "phino-compile" $ \dir -> do
        createDirectoryIfMissing True dir
        withCurrentDirectory dir (runCLI ["compile", "--target=gen/Compiled.hs"])
        doesFileExist (dir </> "gen" </> "Compiled.hs") `shouldReturn` True
    it "writes the rules of --rule into the module" $
      withTempDirectory "phino-compile" $ \dir -> do
        createDirectoryIfMissing True dir
        simple <- makeAbsolute "test-resources/cli/rules/simple.yaml"
        withCurrentDirectory dir (runCLI ["compile", "--rule=" ++ simple, "--target=Compiled.hs"])
        readFile' (dir </> "Compiled.hs") >>= (`shouldSatisfy` ("R.direct \"foo\"" `isInfixOf`))
    it "turns the flag on in a new cabal.project.local" $
      withTempDirectory "phino-compile" $ \dir -> do
        createDirectoryIfMissing True dir
        withCurrentDirectory dir (runCLI ["compile", "--target=Compiled.hs"])
        readFile (dir </> "cabal.project.local") `shouldReturn` "package phino\n  flags: +compiled\n"
    it "prints the lines an existing cabal.project.local lacks" $
      withTempDirectory "phino-compile" $ \dir -> do
        createDirectoryIfMissing True dir
        writeFile (dir </> "cabal.project.local") "tests: True\n"
        withCurrentDirectory dir (testCLISucceeded ["compile", "--target=Compiled.hs"] ["package phino\n  flags: +compiled"])
    it "leaves an existing cabal.project.local as it is" $
      withTempDirectory "phino-compile" $ \dir -> do
        createDirectoryIfMissing True dir
        writeFile (dir </> "cabal.project.local") "tests: True\n"
        withStdout (withCurrentDirectory dir (runCLI ["compile", "--target=Compiled.hs"]))
        readFile (dir </> "cabal.project.local") `shouldReturn` "tests: True\n"
    it "refuses a rule it cannot compile" $
      withTempDirectory "phino-compile" $ \dir -> do
        createDirectoryIfMissing True dir
        writeFile (dir </> "having.yaml") "name: hv\npattern: '[[ x -> !e1, !B1 ]]'\nresult: '[[ !B1 ]]'\nhaving:\n  eq: ['!e1', 'Q']\n"
        withCurrentDirectory dir (testCLIFailed ["compile", "--rule=having.yaml", "--target=Compiled.hs"] ["The rule 'hv' cannot be compiled, since it has a 'having' condition"])

  describe "match" $ do
    it "prints help" $
      testCLISucceeded
        ["match", "--help"]
        ["Pattern expression to match against", "Predicate for matched substitutions"]

    it "takes from stdin" $
      withStdin "[[]]" $
        testCLISucceeded ["match", "--log-level=debug"] ["[DEBUG]"]

    it "takes from file" $
      testCLISucceeded ["match", resource "foo.phi", "--log-level=debug"] ["[DEBUG]"]

    it "does not print substitutions without pattern" $
      withStdin "[[]]" $
        testCLISucceeded ["match", "--log-level=debug"] ["[DEBUG]: The --pattern is not provided, no substitutions are built"]

    it "reproduces the same output for the same --seed" $ do
      dir <- getTemporaryDirectory
      let file = dir ++ "/phino-match-seed-test.phi"
      writeFile file "[[ x -> Q.x, y -> Q.y, z -> Q.z ]]"
      let args =
            [ "match"
            , "--seed=42"
            , "--sweet"
            , "--flat"
            , "--pattern=Q.!t"
            , file
            ]
      (firstRun, _) <- withStdout (runCLI args)
      (secondRun, _) <- withStdout (runCLI args)
      firstRun `shouldBe` secondRun
      removeFile file

    it "prints many substitutions" $
      withStdin "[[ x -> Q.x, y -> Q.y ]]" $
        testCLISucceeded ["match", "--pattern=Q.!t"] ["t >> x\n------\nt >> y"]

    it "builds substitutions with conditions" $
      withStdin "[[ x -> Q.y ]].x" $
        testCLISucceeded
          ["match", "--pattern=[[ !t1 -> Q.y, !B1 ]].!t1", "--when=eq(length(!B1),0)"]
          ["B1 >> ⟦⟧\nt1 >> x"]

    it "builds with condition from file" $
      testCLISucceeded
        ["match", "--pattern=[[ !B1 ]]", "--when=eq(length(!B1),1)", resource "foo.phi"]
        ["B1 >> ⟦ foo ↦ Φ.org.eolang.x ⟧"]

    it "rejects an anonymous meta in --when" $
      withStdin "[[ x -> Q.y ]]" $
        testCLIFailed
          ["match", "--pattern=[[ !B ]]", "--when=eq(length(!B),1)"]
          ["[ERROR]: Anonymous meta '!B' cannot be referenced in --when"]

    it "fails on parsing --when condition" $
      withStdin "[[]]" $
        testCLIFailed
          ["match", "--pattern=[[!B]]", "--when=hello"]
          ["[ERROR]: Couldn't parse given condition"]

    it "fails on empty substitutions" $
      withStdin "Q.x.y" $
        testCLIFailed
          ["match", "--pattern=$.!t"]
          ["[ERROR]"]

  describe "CmdException Show instance" $
    forM_
      [ ("InvalidCLIArguments", InvalidCLIArguments "bad flag", "Invalid set of arguments: bad flag")
      , ("CouldNotReadFromStdin", CouldNotReadFromStdin "broken pipe", "Could not read input from stdin\nReason: broken pipe")
      , ("CouldNotDataize", CouldNotDataize, "Could not dataize given expression")
      ,
        ( "CouldNotPrintExpressionInXMIR"
        , CouldNotPrintExpressionInXMIR
        , "Could not print expression with --output=xmir, only expression printing is allowed"
        )
      , ("EmptySubstsOnMatch", EmptySubstsOnMatch, "Provided pattern was not matched, no substitutions are built")
      ,
        ( "VersionMismatch"
        , VersionMismatch "1.2.3" "4.5.6"
        , "Version mismatch: --pin requires '1.2.3', but this is phino 4.5.6"
        )
      , ("CouldNotCompile", CouldNotCompile "The rule 'q' cannot be compiled, since it is odd", "The rule 'q' cannot be compiled, since it is odd")
      ,
        ( "StaleEngine"
        , StaleEngine
        , "The compiled rules are stale, since the rules of phino changed after 'phino compile', so run it again and rebuild"
        )
      ]
      ( \(desc, exception, expected) ->
          it (desc ++ " renders its message") $ do
            show exception `shouldBe` expected
            displayException exception `shouldBe` expected
      )

  describe "IOFormat Show instance" $
    forM_
      [ ("XMIR", XMIR, "xmir")
      , ("PHI", PHI, "phi")
      , ("LATEX", LATEX, "latex")
      ]
      ( \(desc, format, expected) ->
          it (desc ++ " renders as " ++ expected) $
            show format `shouldBe` expected
      )
