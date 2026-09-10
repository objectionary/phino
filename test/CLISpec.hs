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
import Fixtures (withAskingRegistry, withFixtureRegistry, withLoopingAskRegistry, withNode, withServing, withShell)
import GHC.IO.Handle
import Paths_phino (version)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getTemporaryDirectory, listDirectory, removeDirectoryRecursive, removeFile, removePathForcibly, setModificationTime)
import System.Exit (ExitCode (ExitFailure))
import System.FilePath ((</>))
import System.IO
import Test.Hspec
import Text.Printf (printf)

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

-- A fresh, uniquely-named directory under the system temp directory, removed
-- afterwards even when the action throws (an assertion failure included), so a
-- red run never leaves it behind for the next run to depend on.
withTempDirectory :: String -> (FilePath -> IO a) -> IO a
withTempDirectory prefix action = do
  tmp <- getTemporaryDirectory
  stamp <- getPOSIXTime
  let dir = tmp </> (prefix ++ "-" ++ show (round (stamp * 1000000) :: Integer))
  bracket (pure dir) removePathForcibly action

readUtf8 :: FilePath -> IO String
readUtf8 path =
  withFile path ReadMode $ \stream -> do
    hSetEncoding stream utf8
    content <- hGetContents stream
    _ <- evaluate (length content)
    pure content

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

-- phino implements no λ function of its own, so a case that needs an atom to
-- fire brings the fixture registry in and hands its path to the command as
-- '--atoms' (see 'Fixtures'). Every such atom runs under 'node', so the case is
-- pending where 'node' is not installed.
withAtoms :: (String -> Expectation) -> Expectation
withAtoms action = withNode (withFixtureRegistry (action . ("--atoms=" ++)))

-- The same, for the fixture that reduces no operand of its own and asks phino
-- for every one of them instead (see 'Fixtures')
withAsking :: (String -> Expectation) -> Expectation
withAsking action = withNode (withAskingRegistry (action . ("--atoms=" ++)))

withLoopingAsk :: (String -> Expectation) -> Expectation
withLoopingAsk action = withNode (withLoopingAskRegistry (action . ("--atoms=" ++)))

-- A resident program that cannot answer its request before phino reduces
-- 'Q.nope' for it, a dispatch on an atom the registry does not carry and
-- '--partial' parks: it answers 'FF-' when phino said the parked node back
-- alone and '00-' when the answer carried the whole universe that node was
-- reduced inside, which the other atom of the universe is named in
parking :: T.Text
parking =
  T.pack $
    unlines
      [ "printf '{\"id\": 7, \"ask\": \"Q.nope\"}\\n'"
      , "IFS= read -r reply"
      , "case \"$reply\" in"
      , "  *L_answer*) " ++ answering "00-" ++ ";;"
      , "  *) " ++ answering "FF-" ++ ";;"
      , "esac"
      ]
  where
    answering :: String -> String
    answering bytes = "printf '{\"id\": %s, \"𝑛\": \"⟦ Δ ⤍ " ++ bytes ++ " ⟧\"}\\n' \"$id\""

testCLIFailed :: [String] -> [String] -> Expectation
testCLIFailed args outputs = testCLI' args outputs (Left (ExitFailure 1))

resource :: String -> String
resource file = "test-resources/cli/" <> file

rule :: String -> String
rule file = "--rule=" <> resource file

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
        , ["⟦ foo ↦ ⟦ x ↦ ⟦⟧ ⟧, y ↦ ⟦⟧ ⟧"]
        )
      ,
        ( "keeps sweet numeric literals intact"
        , "[[ a -> 42 ]]"
        , ["rewrite", "--flat", "--sweet", "--hide-rho"]
        , ["⟦ a ↦ 42 ⟧"]
        )
      ]
      (\(desc, input, args, expected) -> it desc (withStdin input (testCLISucceeded args expected)))

  it "prints debug info with --log-level=DEBUG" $
    withStdin "[[]]" $
      testCLISucceeded ["rewrite", "--log-level=DEBUG"] ["[DEBUG]:"]

  describe "--log-level accepts every named level" $
    forM_
      ["ERROR", "ERR", "error", "NONE", "none"]
      ( \flagValue ->
          it ("--log-level=" ++ flagValue) $
            withStdin "[[]]" $
              testCLISucceeded ["rewrite", "--log-level=" ++ flagValue] ["⟧"]
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
          , ["[ERROR]:", "its top level must be a single binding followed by ρ ↦ ∅"]
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

      -- Only assert the stable parts of the parse error: phino's envelope and
      -- that megaparsec reports an 'unexpected' token. The exact line:column and
      -- offending token depend on megaparsec's internal try/longest-match error
      -- merging, which shifts between megaparsec releases (deps are unpinned), so
      -- pinning them here makes the test brittle without testing anything extra.
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
          ["⟦ x ↦ 5 ⟧"]

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

    -- A served atom is asked over the streams of one resident program that
    -- 'phino' starts on the first fire and stops when the run is over, so the
    -- whole of it goes through the command line here: registry, program and
    -- the bytes it answers with
    it "dataizes with an atom served by a resident program" $
      withShell $
        withServing (T.pack "printf '{\"id\": %s, \"𝑛\": \"⟦ Δ ⤍ 2A- ⟧\"}\\n' \"$id\"") $ \registry ->
          withStdin "⟦ @ ↦ ⟦ λ ⤍ L_answer ⟧ ⟧" $
            testCLISucceeded ["dataize", "--atoms=" ++ registry] ["2A-"]

    it "saves dataize steps to dir with --steps-dir" $
      withAtoms $ \atoms ->
        withTempDirectory "phino-steps-dataize" $ \dir ->
          withStdin "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6).plus(7) ]]" $ do
            testCLISucceeded
              ["dataize", atoms, "--steps-dir=" ++ dir, "--sweet"]
              ["40-32"]
            doesDirectoryExist dir `shouldReturn` True
            files <- listDirectory dir
            let steps = sort files
            -- The fix is about numbering, not about a specific rule set: the file
            -- names must be distinct and contiguous from 00001, and there must be
            -- more of them than a single normalization pass produces (this input
            -- runs several normalizations, so a global counter yields more steps).
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
        ["⟦ foo ↦ ξ.x, ρ ↦ ∅ ⟧"]

    it "desugares with without any rules flag from stdin" $
      withStdin "[[foo ↦ x]]" $
        testCLISucceeded ["rewrite"] ["⟦ foo ↦ ξ.x, ρ ↦ ∅ ⟧"]

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
            , "    ρ ↦ ⟦"
            , "      y ↦ ⟦ ρ ↦ ∅ ⟧,"
            , "      ρ ↦ ∅"
            , "    ⟧"
            , "  ⟧,"
            , "  ρ ↦ ∅"
            , "⟧"
            ]
        ]

    it "normalizes and applies --rule at the same time" $
      withStdin "⟦ k ↦ ⟦ m ↦ ⟦ Δ ⤍ 01- ⟧ ⟧.m, j ↦ ⟦ λ ⤍ Marker ⟧ ⟧" $
        testCLISucceeded
          ["rewrite", "--normalize", rule "marker.yaml", "--sweet"]
          ["⟦ k ↦ ⟦ Δ ⤍ 01-, ρ ↦ ⟦ m ↦ ⟦ Δ ⤍ 01- ⟧ ⟧ ⟧, j ↦ ⟦ Δ ⤍ FF- ⟧ ⟧"]

    it "normalizes from stdin" $
      withStdin "⟦ a ↦ ⟦ b ↦ ∅ ⟧ (b ↦ [[ ]]) ⟧" $
        testCLISucceeded
          ["rewrite", "--normalize", "--margin=20"]
          [ unlines
              [ "⟦"
              , "  a ↦ ⟦"
              , "    b ↦ ⟦ ρ ↦ ∅ ⟧,"
              , "    ρ ↦ ∅"
              , "  ⟧,"
              , "  ρ ↦ ∅"
              , "⟧"
              ]
          ]

    it "rewrites with --sweet flag" $
      withStdin "[[ x -> 5]]" $
        testCLISucceeded
          ["rewrite", "--sweet"]
          ["⟦ x ↦ 5 ⟧"]

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
              , "  |y| -> \"H$@^M\","
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
              , "[[ |x| -> 5 ]]{.}"
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
              , "\\phiExpression{foo} [[ |x| -> 5 ]]{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "rewrite as LaTeX with label name" $
      withStdin "[[ x -> 5 ]]" $
        testCLISucceeded
          ["rewrite", "--output=latex", "--sweet", "--flat", "--label=foo"]
          [ unlines
              [ "\\begin{phiquation}\n\\label{foo}"
              , "[[ |x| -> 5 ]]{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "rewrites with XMIR as input" $
      withStdin "<object><o name=\"app\"><o name=\"x\" base=\"Φ.number\"/></o></object>" $
        testCLISucceeded
          ["rewrite", "--input=xmir", "--sweet"]
          ["⟦ app ↦ ⟦ x ↦ Φ.number ⟧ ⟧"]

    it "rewrites and prints with XMIR as input and output" $
      withStdin
        ( intercalate
            ""
            [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            , "<object><o name=\"app\"><o name=\"x\" base=\"Φ.number\"/></o></object>"
            ]
        )
        ( testCLISucceeded
            ["rewrite", "--input=xmir", "--output=xmir", "--sweet"]
            [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            , "<listing>&lt;?xml version=&quot;1.0&quot; encoding=&quot;UTF-8&quot;?&gt;&lt;object&gt;&lt;o name=&quot;app&quot;&gt;&lt;o name=&quot;x&quot; base=&quot;Φ.number&quot;/&gt;&lt;/o&gt;&lt;/object&gt;</listing>"
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
          ["x ↦ \"bar\""]

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
              [ "⟦ x ↦ \"foo\" ⟧"
              , "Φ.x( y ↦ \"foo\" )"
              , "⟦ x ↦ \"foo\" ⟧"
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
              , "⟦ x ↦ \"foo\" ⟧"
              , ""
              , "=== Step #2, Rule 'first', 31t -> 30t"
              , "Φ.x( y ↦ \"foo\" )"
              , ""
              , "=== Step #3, Rule 'second', 30t -> 31t"
              , "⟦ x ↦ \"foo\" ⟧"
              ]
          ]

    it "ignores --headers without --sequence" $
      withStdin "[[ x -> \"foo\" ]]" $
        testCLISucceeded
          ["rewrite", rule "simple.yaml", "--headers", "--sweet", "--flat"]
          ["⟦ x ↦ \"bar\" ⟧"]

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
              , "[[ |x| -> \"foo\" ]] \\leadsto_{\\nameref{r:first}}"
              , "% === Step #2, Rule 'first', 31t -> 30t"
              , "  \\leadsto Q . |x| ( |y| -> \"foo\" ) \\leadsto_{\\nameref{r:second}}"
              , "% === Step #3, Rule 'second', 30t -> 31t"
              , "  \\leadsto [[ |x| -> \"foo\" ]]{.}"
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
              , "[[ |x| -> \"foo\" ]] \\leadsto_{\\nameref{r:first}}"
              , "  \\leadsto Q . |x| ( |y| -> \"foo\" ) \\leadsto_{\\nameref{r:second}}"
              , "  \\leadsto [[ |x| -> \"foo\" ]]{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "prints meet prefix with --meet-prefix=foo in LaTeX" $
      withStdin "[[ x -> ?, y -> $.x ]](x -> [[ D> 42- ]]).y" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sweet", "--sequence", "--output=latex", "--flat", "--compress", "--meet-prefix=foo"]
          [ unlines
              [ "\\begin{phiquation}"
              , "[[ |x| -> ?, |y| -> |x| ]] ( |x| -> \\phinoMeet{foo:1}{ [[ D> |42-| ]] } ) . |y| \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto \\phinoMeet{foo:2}{ [[ |x| -> \\phinoAgain{foo:1}, |y| -> |x| ]] } . |y| \\leadsto_{\\nameref{r:dot}}"
              , "  \\leadsto \\phinoMeet{foo:3}{ [[ |x| -> \\phinoAgain{foo:1} ]] } . |x| ( \\phiTerminal{\\rho} -> \\phinoAgain{foo:2} ) \\leadsto_{\\nameref{r:dot}}"
              , "  \\leadsto \\phinoAgain{foo:1} ( \\phiTerminal{\\rho} -> \\phinoAgain{foo:3}, \\phiTerminal{\\rho} -> \\phinoAgain{foo:2} ) \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto [[ D> |42-|, \\phiTerminal{\\rho} -> \\phinoAgain{foo:3} ]] ( \\phiTerminal{\\rho} -> \\phinoAgain{foo:2} ) \\leadsto_{\\nameref{r:stay}}"
              , "  \\leadsto [[ D> |42-|, \\phiTerminal{\\rho} -> \\phinoAgain{foo:3} ]]{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "prints with compressed expressions in LaTeX" $
      withStdin "[[ x -> ?, y -> $.x ]](x -> [[ D> 42- ]]).y" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sweet", "--sequence", "--output=latex", "--flat", "--compress"]
          [ unlines
              [ "\\begin{phiquation}"
              , "[[ |x| -> ?, |y| -> |x| ]] ( |x| -> \\phinoMeet{1}{ [[ D> |42-| ]] } ) . |y| \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto \\phinoMeet{2}{ [[ |x| -> \\phinoAgain{1}, |y| -> |x| ]] } . |y| \\leadsto_{\\nameref{r:dot}}"
              , "  \\leadsto \\phinoMeet{3}{ [[ |x| -> \\phinoAgain{1} ]] } . |x| ( \\phiTerminal{\\rho} -> \\phinoAgain{2} ) \\leadsto_{\\nameref{r:dot}}"
              , "  \\leadsto \\phinoAgain{1} ( \\phiTerminal{\\rho} -> \\phinoAgain{3}, \\phiTerminal{\\rho} -> \\phinoAgain{2} ) \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto [[ D> |42-|, \\phiTerminal{\\rho} -> \\phinoAgain{3} ]] ( \\phiTerminal{\\rho} -> \\phinoAgain{2} ) \\leadsto_{\\nameref{r:stay}}"
              , "  \\leadsto [[ D> |42-|, \\phiTerminal{\\rho} -> \\phinoAgain{3} ]]{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "should not print \\phinoMeet{} twice" $
      withStdin "[[ ex -> [[ x -> [[ y -> ?, k -> [[ t -> 42]]  ]]( y -> [[ t -> 42 ]]) ]].i ]]" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sequence", "--flat", "--compress", "--output=latex", "--sweet"]
          [ unlines
              [ "\\begin{phiquation}"
              , "[[ |ex| -> [[ |x| -> [[ |y| -> ?, |k| -> \\phinoMeet{1}{ [[ |t| -> 42 ]] } ]] ( |y| -> \\phinoAgain{1} ) ]] . |i| ]] \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto [[ |ex| -> [[ |x| -> [[ |y| -> \\phinoAgain{1}, |k| -> \\phinoAgain{1} ]] ]] . |i| ]] \\leadsto_{\\nameref{r:stop}}"
              , "  \\leadsto [[ |ex| -> T ]]{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "should not meet expression with high --meet-popularity" $
      withStdin "[[ ex -> [[ x -> [[ y -> ?, k -> [[ t -> 42]]  ]]( y -> [[ t -> 42 ]]) ]].i ]]" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sequence", "--flat", "--compress", "--output=latex", "--sweet", "--meet-popularity=70"]
          [ unlines
              [ "\\begin{phiquation}"
              , "[[ |ex| -> [[ |x| -> [[ |y| -> ?, |k| -> [[ |t| -> 42 ]] ]] ( |y| -> [[ |t| -> 42 ]] ) ]] . |i| ]] \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto [[ |ex| -> [[ |x| -> [[ |y| -> [[ |t| -> 42 ]], |k| -> [[ |t| -> 42 ]] ]] ]] . |i| ]] \\leadsto_{\\nameref{r:stop}}"
              , "  \\leadsto [[ |ex| -> T ]]{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "meets with --meet-length=32" $
      withStdin "[[ ex -> [[ x -> [[ y -> ?, k -> [[ t -> 42]]  ]]( y -> [[ t -> 42 ]]) ]].i ]]" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sequence", "--flat", "--compress", "--output=latex", "--sweet", "--meet-length=32"]
          [ unlines
              [ "\\begin{phiquation}"
              , "[[ |ex| -> [[ |x| -> [[ |y| -> ?, |k| -> [[ |t| -> 42 ]] ]] ( |y| -> [[ |t| -> 42 ]] ) ]] . |i| ]] \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto [[ |ex| -> [[ |x| -> [[ |y| -> [[ |t| -> 42 ]], |k| -> [[ |t| -> 42 ]] ]] ]] . |i| ]] \\leadsto_{\\nameref{r:stop}}"
              , "  \\leadsto [[ |ex| -> T ]]{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "focuses expression in latex with sequence" $
      withStdin "[[ ex -> [[ x -> [[ y -> ?, k -> [[ t -> 42]]  ]]( y -> [[ t -> 42 ]]) ]].i ]]" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sequence", "--flat", "--output=latex", "--sweet", "--focus=Q.ex"]
          [ unlines
              [ "\\begin{phiquation}"
              , "[[ |x| -> [[ |y| -> ?, |k| -> [[ |t| -> 42 ]] ]] ( |y| -> [[ |t| -> 42 ]] ) ]] . |i| \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto [[ |x| -> [[ |y| -> [[ |t| -> 42 ]], |k| -> [[ |t| -> 42 ]] ]] ]] . |i| \\leadsto_{\\nameref{r:stop}}"
              , "  \\leadsto T{.}"
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
              , "[[ |x| -> |y|, |y| -> |x| ]] . |x| \\leadsto_{\\nameref{r:dot}}"
              , "  \\leadsto [[ |y| -> |x| ]] . |y| ( \\phiTerminal{\\rho} -> [[ |x| -> |y|, |y| -> |x| ]] ) \\leadsto"
              , "  \\leadsto \\dots"
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
              [ "⟦ x ↦ ⟦ y ↦ ∅, k ↦ ⟦ t ↦ 42 ⟧ ⟧( y ↦ ⟦ t ↦ 42 ⟧ ) ⟧.i"
              , "⟦ x ↦ ⟦ y ↦ ⟦ t ↦ 42 ⟧, k ↦ ⟦ t ↦ 42 ⟧ ⟧ ⟧.i"
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
          ["  <listing>⟦ x ↦ \"foo\" ⟧</listing>", "  <listing>⟦ x ↦ \"bar\" ⟧</listing>"]

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
            ["x ↦ \"bar\""]

      it "accepts range 1..3 when 1 cycle happens" $
        withStdin "⟦ t ↦ ⟦ x ↦ \"foo\" ⟧ ⟧" $
          testCLISucceeded
            ["rewrite", rule "simple.yaml", "--must=1..3", "--sweet"]
            ["x ↦ \"bar\""]

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
        content `shouldBe` "⟦ x ↦ \"bar\" ⟧"

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
          content `shouldBe` "⟦ x ↦ \"bar\" ⟧"

    it "rewrites with cycles" $
      withStdin "[[ x -> \"x\" ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", rule "infinite.yaml", "--max-depth=1", "--max-cycles=2"]
          ["⟦ x ↦ \"x_hi_hi\" ⟧"]

    it "hides default package" $
      withStdin "[[ org -> [[ eolang -> [[ number -> [[]] ]]]], x -> 42 ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--hide=Q.org"]
          ["⟦ x ↦ 42 ⟧"]

    it "hides several FQNs" $
      withStdin "[[ org -> [[ eolang -> Q.x, yegor256 -> Q.y ]], x -> 42 ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--hide=Q.org.eolang", "--hide=Q.org.yegor256"]
          ["⟦ org ↦ ⟦⟧, x ↦ 42 ⟧"]

    it "shows and hides" $
      withStdin "[[ org -> [[ eolang -> Q.x, yegor256 -> Q.y ]], x -> 42 ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--show=Q.org", "--hide=Q.org.eolang"]
          ["⟦ org ↦ ⟦ yegor256 ↦ Φ.y ⟧ ⟧"]

    it "prints in line with --flat" $
      withStdin "[[ x -> 5, y -> \"hey\", z -> [[ w -> [[ ]] ]] ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat"]
          ["⟦ x ↦ 5, y ↦ \"hey\", z ↦ ⟦ w ↦ ⟦⟧ ⟧ ⟧"]

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
            ["⟦ z ↦ 42, org ↦ ⟦ eolang ↦ ⟦ bytes(data) ↦ ⟦⟧, number(as-bytes) ↦ ⟦⟧ ⟧ ⟧ ⟧"]
        )

    it "reduces log message" $
      withStdin "[[ x -> [[ y -> ? ]](y -> 5) ]]" $
        testCLISucceeded
          ["rewrite", "--log-level=debug", "--log-lines=1", "--normalize"]
          [ intercalate
              "\n"
              [ "[DEBUG]: Applied 'copy' (44 nodes -> 39 nodes)"
              , "---| log is limited by --log-lines=1 option |---"
              ]
          ]

    -- 'matches' inside 'when' raises while dataizing a formation: the
    -- substitution is still dropped (the policy #1079 questions), but the
    -- reason surfaces in the debug log instead of vanishing
    it "reports a condition that raised while being evaluated" $
      withStdin "[[ x -> [[ y -> ∅ ]] ]]" $
        testCLISucceeded
          ["rewrite", rule "raising-condition.yaml", "--log-level=debug", "--flat"]
          [ "raised and was treated as not met: user error (Only data objects and bytes are supported"
          , "⟦ x ↦ ⟦ y ↦ ∅, ρ ↦ ∅ ⟧, ρ ↦ ∅ ⟧"
          ]

    it "canonizes expression" $
      withStdin "[[ x -> [[ y -> [[ L> Func ]].q, z -> Q.x(a -> [[ w -> [[ L> Atom ]], L> Hello ]]) ]], L> Package ]]" $
        testCLISucceeded
          ["rewrite", "--canonize", "--sweet", "--flat"]
          ["⟦ x ↦ ⟦ y ↦ ⟦ λ ⤍ Fn1 ⟧.q, z ↦ Φ.x( a ↦ ⟦ w ↦ ⟦ λ ⤍ Fn2 ⟧, λ ⤍ Fn3 ⟧ ) ⟧, λ ⤍ Fn4 ⟧"]

    it "rewrites by locator" $
      withStdin "[[ ex -> [[ x -> [[ y -> 5 ]].y ]], abc -> [[ x -> ? ]](x -> 5) ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--locator=Q.ex", "--normalize"]
          ["⟦ ex ↦ ⟦ x ↦ 5 ⟧, abc ↦ ⟦ x ↦ ∅ ⟧( x ↦ 5 ) ⟧"]

    it "returns original expression on --breakpoint" $
      withStdin "[[ x -> ?, y -> $.x ]](x -> [[ D> 42- ]]).y" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--normalize", "--breakpoint=stop", "--log-level=debug"]
          [ "Applied 'copy' (30 nodes -> 25 nodes)"
          , "Rule 'stop' is a breakpoint, dropping down all the previous rewritings..."
          , "⟦ x ↦ ∅, y ↦ x ⟧( x ↦ ⟦ Δ ⤍ 42- ⟧ ).y"
          ]

  describe "dataize" $ do
    it "prints help" $
      testCLISucceeded ["dataize", "--help"] ["Dataize the 𝜑-expression"]

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

    -- The 𝕄/𝔻 recursion used to be unbounded, so this division kept morphing
    -- forever and no option could stop it (#1052)
    it "fails on --max-steps instead of morphing forever" $
      withAtoms $ \atoms ->
        withStdin "⟦ @ ↦ ⟦ λ ⤍ L_number_div, ρ ↦ ⟦ Δ ⤍ 40-45-00-00-00-00-00-00 ⟧, x ↦ ⟦ Δ ⤍ 40-00-00-00-00-00-00-00 ⟧ ⟧ ⟧" $
          testCLIFailed
            ["dataize", atoms, "--max-steps=40"]
            ["[ERROR]: Dataization did not finish before reaching the limit of steps: --max-steps=40"]

    -- Under '--partial' the same term does not fail: the spent budget is a
    -- stuck site too, and the run ends on the residual the spine reached (#1078)
    it "parks --max-steps on a residual with --partial" $
      withAtoms $ \atoms ->
        withStdin "⟦ @ ↦ ⟦ λ ⤍ L_number_div, ρ ↦ ⟦ Δ ⤍ 40-45-00-00-00-00-00-00 ⟧, x ↦ ⟦ Δ ⤍ 40-00-00-00-00-00-00-00 ⟧ ⟧ ⟧" $
          testCLISucceeded
            ["dataize", atoms, "--max-steps=40", "--partial", "--flat"]
            ["Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-35-00-00-00-00-00-00"]

    it "dataizes with --sequence" $
      withStdin "[[ @ -> [[ x -> [[ D> 01-, y -> ? ]](y -> [[ ]]) ]].x ]]" $
        testCLISucceeded
          ["dataize", "--sequence", "--output=latex", "--flat", "--sweet"]
          [ intercalate
              "\n"
              [ "\\begin{phiquation}"
              , "[[ @ -> [[ |x| -> [[ D> |01-|, |y| -> ? ]] ( |y| -> [[]] ) ]] . |x| ]] \\leadsto_{\\nameref{r:contextualize}}"
              , "  \\leadsto [[ |x| -> [[ D> |01-|, |y| -> ? ]] ( |y| -> [[]] ) ]] . |x| \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto [[ |x| -> [[ D> |01-|, |y| -> [[]] ]] ]] . |x| \\leadsto_{\\nameref{r:dot}}"
              , "  \\leadsto [[ D> |01-|, |y| -> [[]] ]] ( \\phiTerminal{\\rho} -> [[ |x| -> [[ D> |01-|, |y| -> [[]] ]] ]] ) \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto [[ D> |01-|, |y| -> [[]], \\phiTerminal{\\rho} -> [[ |x| -> [[ D> |01-|, |y| -> [[]] ]] ]] ]] \\leadsto_{\\nameref{r:delta}}"
              , "  \\leadsto |01-|{.}"
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
              [ "[[ D> |01-| ]] \\leadsto_{\\nameref{r:delta}}"
              , "  \\leadsto |01-|{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "ends the phi --sequence at the bare data" $
      withStdin "[[ D> 01- ]]" $
        testCLISucceeded
          ["dataize", "--sequence", "--quiet", "--flat", "--sweet"]
          ["⟦ Δ ⤍ 01- ⟧\n01-"]

    it "focuses a compressed sequence whose meet replaces a step root" $
      withAtoms $ \atoms ->
        withStdin "[[ @ -> [[ @ -> $.c.plus( 32.0 ), c -> 25.0 ]], bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus -> [[ x -> ?, L> L_number_plus ]] ]] ]]" $
          testCLISucceeded
            ["dataize", atoms, "--output=latex", "--sweet", "--nonumber", "--compress", "--canonize", "--meet-prefix=dataization", "--sequence", "--flat", "--quiet", "--hide=Q.bytes", "--hide=Q.number", "--locator=Q.@", "--focus=Q.@", "--meet-length=5", "--meet-popularity=1"]
            ["\\phinoMeet{dataization:1}{ [[ @ -> |c| . |plus| ( 32 ), |c| -> 25 ]] } \\leadsto_{\\nameref{r:contextualize}}"]

    it "compresses a canonized whole-expression sequence into a meet" $
      withAtoms $ \atoms ->
        withStdin "[[ @ -> [[ @ -> $.c.plus( 32.0 ), c -> 25.0 ]], bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus -> [[ x -> ?, L> L_number_plus ]] ]] ]]" $
          testCLISucceeded
            ["dataize", atoms, "--output=latex", "--sweet", "--nonumber", "--compress", "--canonize", "--meet-prefix=dataization", "--sequence", "--flat", "--quiet", "--meet-length=5", "--meet-popularity=1"]
            ["\\phinoMeet{dataization:1}"]

    it "dataizes with --locator" $
      withStdin "[[ ex -> [[ @ -> Q.x ]], x -> [[ D> 42- ]] ]]" $
        testCLISucceeded ["dataize", "--locator=Q.ex"] ["42-"]

    it "does not print bytes with --quiet" $
      withStdin "[[ D> 01- ]]" $
        testCLISucceeded ["dataize", "--quiet"] []

    describe "--evaluations" $ do
      it "writes one tab-separated record per fired atom" $
        withAtoms $ \atoms ->
          withTempFile "evaluationsXXXXXX.txt" $ \(path, stream) -> do
            hClose stream
            withStdin "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6) ]]" $
              testCLISucceeded ["dataize", atoms, "--evaluations=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
            records <- readUtf8 path
            records `shouldBe` "L_number_plus\t⟦ x ↦ 6 ⟧\t11\n"

      it "writes a record for every firing" $
        withAtoms $ \atoms ->
          withTempFile "evaluationsXXXXXX.txt" $ \(path, stream) -> do
            hClose stream
            withStdin "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6).plus(7) ]]" $
              testCLISucceeded ["dataize", atoms, "--evaluations=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
            records <- readUtf8 path
            lines records `shouldBe` ["L_number_plus\t⟦ x ↦ 6 ⟧\t11", "L_number_plus\t⟦ x ↦ 7 ⟧\t18"]

      it "writes records in canonical syntax without --sweet" $
        withAtoms $ \atoms ->
          withTempFile "evaluationsXXXXXX.txt" $ \(path, stream) -> do
            hClose stream
            withStdin "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6) ]]" $
              testCLISucceeded ["dataize", atoms, "--evaluations=" ++ path, "--quiet", "--hide-rho"] []
            records <- readUtf8 path
            records `shouldEndWith` "\tΦ.number( φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-26-00-00-00-00-00-00 ⟧ ) )\n"

      it "keeps the records of a run that fails" $
        withAtoms $ \atoms ->
          withTempFile "evaluationsXXXXXX.txt" $ \(path, stream) -> do
            hClose stream
            withStdin "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(x) -> [[ L> L_number_plus ]], nope -> [[ L> L_number_nope ]] ]], @ -> 5.plus(6).nope ]]" $
              testCLIFailed
                ["dataize", atoms, "--evaluations=" ++ path, "--quiet", "--sweet", "--hide-rho"]
                ["Atom 'L_number_nope' does not exist"]
            records <- readUtf8 path
            records `shouldBe` "L_number_plus\t⟦ x ↦ 6 ⟧\t11\n"

      it "truncates the records left over from the previous run" $
        withTempFileContent "evaluationsXXXXXX.txt" "L_number_gt\t[[ ]]\t01-\n" $ \path -> do
          withStdin "[[ D> 01- ]]" $
            testCLISucceeded ["dataize", "--evaluations=" ++ path, "--quiet"] []
          records <- readUtf8 path
          records `shouldBe` ""

      it "fails with --output=xmir" $
        withStdin "[[ D> 01- ]]" $
          testCLIFailed
            ["dataize", "--evaluations=evaluations.txt", "--output=xmir"]
            ["The --evaluations option can stay together with --output=phi only"]

      it "fails with --output=latex" $
        withStdin "[[ D> 01- ]]" $
          testCLIFailed
            ["dataize", "--evaluations=evaluations.txt", "--output=latex"]
            ["The --evaluations option can stay together with --output=phi only"]

    -- A λ function the '--atoms' registry does not carry cannot fire — a
    -- placeholder such as ⟦ λ ⤍ Sym_arg_0 ⟧ standing in for a data input, or
    -- an operation the caller left out of its registry on purpose. The run used
    -- to die on it, discarding what it had already evaluated (#1060)
    describe "--partial" $ do
      let stuck = "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ times(x) -> [[ L> L_number_times ]], nope -> [[ L> L_number_nope ]] ]], @ -> 2.times(3).nope ]]"
      it "fails on an atom that cannot fire without the flag" $
        withAtoms $ \atoms ->
          withStdin stuck $
            testCLIFailed ["dataize", atoms, "--sweet", "--hide-rho"] ["Atom 'L_number_nope' does not exist"]

      it "prints the residue with the stuck application intact and exits successfully" $
        withAtoms $ \atoms ->
          withStdin stuck $
            testCLISucceeded
              ["dataize", atoms, "--partial", "--sweet", "--hide-rho"]
              ["⟦ λ ⤍ L_number_nope ⟧"]

      it "keeps what was evaluated before the stuck site in the residue" $
        withAtoms $ \atoms ->
          withStdin stuck $
            testCLISucceeded
              ["dataize", atoms, "--partial", "--sweet"]
              ["φ ↦ Φ.bytes( φ ↦ ⟦ Δ ⤍ 40-18-00-00-00-00-00-00 ⟧ )"]

      it "records every stuck site in --evaluations with no result" $
        withAtoms $ \atoms ->
          withTempFile "evaluationsXXXXXX.txt" $ \(path, stream) -> do
            hClose stream
            withStdin stuck $
              testCLISucceeded ["dataize", atoms, "--partial", "--evaluations=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
            records <- readUtf8 path
            lines records
              `shouldBe` [ "L_number_times\t⟦ x ↦ 3 ⟧\t6"
                         , "L_number_nope\t⟦⟧"
                         ]

      it "still prints bytes when nothing gets stuck" $
        withAtoms $ \atoms ->
          withStdin "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6) ]]" $
            testCLISucceeded ["dataize", atoms, "--partial"] ["40-26-00-00-00-00-00-00"]

      -- The residual is an arbitrary formation, and a multi-binding <object>
      -- is exactly what XMIR now carries: one <o> per binding (#1076)
      it "prints the residual to XMIR, with its real listing by default" $
        withAtoms $ \atoms ->
          withStdin stuck $
            testCLISucceeded
              ["dataize", atoms, "--partial", "--output=xmir"]
              ["<o name=\"λ\">L_number_nope</o>", "<o name=\"ρ\">", "<listing>⟦"]

      it "honors --hide-rho and --omit-listing when printing the residual to XMIR" $
        withAtoms $ \atoms ->
          withStdin stuck $
            testCLISucceeded
              ["dataize", atoms, "--partial", "--output=xmir", "--hide-rho", "--omit-listing"]
              ["<o name=\"λ\">L_number_nope</o>", "line(s)</listing>"]

      it "prints the chain of steps ending in the residue with --sequence" $
        withAtoms $ \atoms ->
          withStdin stuck $
            testCLISucceeded
              ["dataize", atoms, "--partial", "--sequence", "--sweet", "--hide-rho", "--flat"]
              ["2.times( 3 ).nope", "⟦ λ ⤍ L_number_nope ⟧"]

      it "still stops on the terminator ⊥, since a wrong operand is not a stuck atom" $
        withStdin "[[ ]]" $
          testCLIFailed ["dataize", "--partial"] ["terminator ⊥"]

    -- Which λ functions exist is not phino's business any more: the registry
    -- given with '--atoms' decides, and phino carries none of its own
    describe "--atoms" $ do
      let sum' = "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6) ]]"
      it "fires the λ function the registry carries" $
        withAtoms $ \atoms ->
          withStdin sum' $
            testCLISucceeded ["dataize", atoms] ["40-26-00-00-00-00-00-00"]

      it "gets stuck on every atom when it is not given" $
        withStdin sum' $
          testCLIFailed ["dataize"] ["Atom 'L_number_plus' does not exist"]

      it "fails when the registry file is not there" $
        withStdin sum' $
          testCLIFailed ["dataize", "--atoms=no-such-registry.json"] ["no-such-registry.json"]

      -- An unknown runtime is refused where the registry is read, which is
      -- before the input is even parsed, rather than when an atom of it fires
      it "fails on a runtime phino cannot run, before dataizing anything" $
        withTempFileContent "atomsXXXXXX.json" "{\"L_number_plus\": {\"rt\": \"ruby\", \"script\": \"puts 1\"}}" $ \path ->
          withStdin sum' $
            testCLIFailed ["dataize", "--atoms=" ++ path] ["unknown runtime 'ruby'"]

      it "fails on a registry that is not JSON" $
        withTempFileContent "atomsXXXXXX.json" "L_number_plus: js" $ \path ->
          withStdin sum' $
            testCLIFailed ["dataize", "--atoms=" ++ path] ["cannot be read"]

      -- An operand reaches an atom as it was written, so 'x' arrives here as
      -- '6.plus( 7 )': a program that needs it reduced asks phino for it over
      -- the very channel it answers on, and serving that question costs
      -- another fire of the same program, which arrives while the question is
      -- still open
      it "reduces the operand a program asks it about" $
        withAsking $ \atoms ->
          withStdin "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6.plus(7)) ]]" $
            testCLISucceeded ["dataize", atoms] ["40-32-00-00-00-00-00-00"]

      -- A question about a term the universe cannot finish reducing does not
      -- kill a '--partial' run: phino parks the cycle the question walks into
      -- and answers with the residual, so the program still replies and the
      -- bytes arrive (#1078, over the ask channel of #1160)
      it "answers a looping question with a parked residual under --partial" $
        withLoopingAsk $ \atoms ->
          withStdin "⟦ bytes ↦ ⟦ φ ↦ ∅ ⟧, number ↦ ⟦ φ ↦ ∅, gt(x) ↦ ⟦ λ ⤍ L_number_gt ⟧ ⟧, φ ↦ 5.gt(1) ⟧" $
            testCLISucceeded
              ["dataize", atoms, "--partial", "--max-steps=200"]
              ["2A-"]

      -- A question is answered with the node the program asked about, and
      -- never with the universe that node was reduced inside: a parked
      -- question used to hand the residue back whole, so a program reading a
      -- seventeen-byte node paid for a print of the entire universe, once per
      -- question (#1167)
      it "answers a parked question with the node alone" $
        withShell $
          withServing parking $ \registry ->
            withStdin "⟦ nope ↦ ⟦ λ ⤍ L_nope ⟧, φ ↦ ⟦ λ ⤍ L_answer ⟧ ⟧" $
              testCLISucceeded ["dataize", "--atoms=" ++ registry, "--partial"] ["FF-"]

      -- Without '--partial' the exhausted budget fails the run through a
      -- question just as it fails it anywhere else (#1052's message)
      it "fails a looping question without --partial" $
        withLoopingAsk $ \atoms ->
          withStdin "⟦ bytes ↦ ⟦ φ ↦ ∅ ⟧, number ↦ ⟦ φ ↦ ∅, gt(x) ↦ ⟦ λ ⤍ L_number_gt ⟧ ⟧, φ ↦ 5.gt(1) ⟧" $
            testCLIFailed
              ["dataize", atoms, "--max-steps=200"]
              ["--max-steps=200"]

    -- An atom script cannot reduce the operands it was handed by itself, so it
    -- asks phino for them: '--inside' binds an expression to a synthetic
    -- attribute of the universe and aims the run at it
    describe "--inside" $ do
      let universe = "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(x) -> [[ L> L_number_plus ]] ]], @ -> [[ D> 01- ]] ]]"
      it "dataizes an expression the input does not contain" $
        withAtoms $ \atoms ->
          withStdin universe $
            testCLISucceeded ["dataize", atoms, "--inside=5.plus( 6 )"] ["40-26-00-00-00-00-00-00"]

      -- The expression is normalized first, so a dispatch off a formation — the
      -- very shape a script asks about, '⟦ x ↦ 6, ρ ↦ 5 ⟧.x' — reduces too
      it "normalizes what it is handed before dataizing it" $
        withStdin universe $
          testCLISucceeded ["dataize", "--inside=[[ x -> [[ D> 2A- ]] ]].x"] ["2A-"]

      it "morphs inside the universe just as it dataizes inside it" $
        withAtoms $ \atoms ->
          withStdin universe $
            testCLISucceeded ["morph", atoms, "--inside=5.plus( 6 )", "--sweet", "--hide-rho", "--flat"] ["⟦ x ↦ 6, λ ⤍ L_number_plus ⟧"]

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

  -- 𝕄 was reachable only from inside 𝔻, through the 'norm' rule of the
  -- dataization relation, so there was no way to ask phino for 𝕄(n, Φ) on its
  -- own (#1114)
  describe "morph" $ do
    -- Two chained atom calls: the inner one fires under 'ml', because '.plus'
    -- is dispatched on its result, while the outer application is saturated but
    -- bare, so 'mf' hands it back and firing it is 𝔻's job
    let chained = "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6).plus(7) ]]"
    it "prints help" $
      testCLISucceeded ["morph", "--help"] ["Morph the 𝜑-expression"]

    it "hands the top formation back untouched under the default locator" $
      withStdin "[[ D> 01- ]]" $
        testCLISucceeded ["morph", "--flat", "--hide-rho"] ["⟦ Δ ⤍ 01- ⟧"]

    it "stops at the bare saturated λ-formation" $
      withAtoms $ \atoms ->
        withStdin chained $
          testCLISucceeded
            ["morph", atoms, "--locator=Q.@", "--sweet", "--hide-rho", "--flat"]
            ["⟦ x ↦ 7, λ ⤍ L_number_plus ⟧"]

    -- The same term under 𝔻, which insists on bytes and fires what 𝕄 left bare
    it "leaves to dataize the firing that takes the same term to bytes" $
      withAtoms $ \atoms ->
        withStdin chained $
          testCLISucceeded ["dataize", atoms] ["40-32-00-00-00-00-00-00"]

    -- 'mf' hands a formation back as it is, so '--locator' is how one aims 𝕄 at
    -- a subterm worth navigating: here it resolves Φ against the universe and
    -- peels the dispatch through 𝒩
    it "morphs the subterm --locator aims at" $
      withStdin "[[ ex -> Q.x, x -> [[ D> 42- ]] ]]" $
        testCLISucceeded ["morph", "--locator=Q.ex", "--flat", "--hide-rho"] ["⟦ Δ ⤍ 42- ⟧"]

    -- 𝕄 is total and 𝔻 is not: where the derivation dies, 𝕄 answers ⊥ ('xi'
    -- here) and the run succeeds, while 𝔻 has no bytes to give and fails
    it "prints ⊥ instead of failing the run" $
      withStdin "[[ x -> $ ]]" $
        testCLISucceeded ["morph", "--locator=Q.x"] ["⊥"]

    it "fails to dataize what it morphs to ⊥" $
      withStdin "[[ x -> $ ]]" $
        testCLIFailed ["dataize", "--locator=Q.x"] ["terminator ⊥"]

    -- The chain carries the spine: the morphing rules that reduced the term
    -- ('maa', then the terminal 'mf') with the normalization steps they spliced
    -- in ('alpha', 'copy'). The 'ml' firing of the inner call is not there by
    -- design — it happens in a side premise, which reduces on a chain of its
    -- own and discards it
    it "prints the chain of morphing steps with --sequence" $
      withAtoms $ \atoms ->
        withStdin chained $
          testCLISucceeded
            ["morph", atoms, "--locator=Q.@", "--sequence", "--headers", "--sweet", "--hide-rho", "--flat"]
            [ "Rule 'maa'"
            , "Rule 'alpha'"
            , "Rule 'copy'"
            , "Rule 'mf'"
            , "⟦ x ↦ 7, λ ⤍ L_number_plus ⟧"
            ]

    it "does not print the result with --quiet" $
      withStdin "[[ D> 01- ]]" $
        testCLISucceeded ["morph", "--quiet"] []

    it "records the atoms it fires with --evaluations" $
      withAtoms $ \atoms ->
        withTempFile "evaluationsXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin chained $
            testCLISucceeded ["morph", atoms, "--locator=Q.@", "--evaluations=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
          records <- readUtf8 path
          lines records `shouldBe` ["L_number_plus\t⟦ x ↦ 6 ⟧\t11"]

    it "saves morphing steps to dir with --steps-dir" $
      withAtoms $ \atoms ->
        withTempDirectory "phino-steps-morph" $ \dir ->
          withStdin chained $ do
            testCLISucceeded
              ["morph", atoms, "--locator=Q.@", "--steps-dir=" ++ dir, "--sweet", "--hide-rho", "--flat"]
              ["⟦ x ↦ 7, λ ⤍ L_number_plus ⟧"]
            steps <- sort <$> listDirectory dir
            steps `shouldBe` map (\n -> printf "%05d.phi" (n :: Int)) [1 .. length steps]
            length steps `shouldSatisfy` (> 0)

    it "accepts --seed, --shuffle and --depth-sensitive" $
      withStdin "[[ D> 01- ]]" $
        testCLISucceeded ["morph", "--seed=7", "--shuffle", "--depth-sensitive", "--flat", "--hide-rho"] ["⟦ Δ ⤍ 01- ⟧"]

    -- The division 𝔻 cannot finish, whatever '--max-steps' it is given (#1052),
    -- is no work at all for 𝕄: the term is already a formation, so 'mf' hands
    -- it back and the atom is never fired
    it "returns the λ-formation dataize cannot finish on" $
      withStdin "⟦ @ ↦ ⟦ λ ⤍ L_number_div, ρ ↦ ⟦ Δ ⤍ 40-45-00-00-00-00-00-00 ⟧, x ↦ ⟦ Δ ⤍ 40-00-00-00-00-00-00-00 ⟧ ⟧ ⟧" $
        testCLISucceeded
          ["morph", "--locator=Q.@", "--max-steps=40", "--flat", "--hide-rho"]
          ["⟦ λ ⤍ L_number_div"]

    -- '--max-steps' bounds the 𝕄 recursion just as it bounds the 𝕄/𝔻 one
    it "fails once the --max-steps budget is spent" $
      withStdin chained $
        testCLIFailed
          ["morph", "--locator=Q.@", "--max-steps=3"]
          ["[ERROR]: Dataization did not finish before reaching the limit of steps: --max-steps=3"]

    -- '--partial' parks a spent 𝕄 budget the same way it parks a stuck atom:
    -- the answer is the term the walk had reached, dispatch intact (#1078)
    it "parks the spent budget as a residual with --partial" $
      withStdin "⟦ φ ↦ 5.gt(Φ.nan) ⟧" $
        testCLISucceeded
          ["morph", "--locator=Q.@", "--max-steps=10", "--partial", "--flat", "--hide-rho", "--sweet"]
          ["5.gt( Φ.nan )"]

    -- 𝕄 never fires a bare λ-formation, so only the atoms sitting under a
    -- dispatch ('ml') can get stuck; '--partial' parks them exactly as under 𝔻
    describe "--partial" $ do
      let stuck = "[[ @ -> [[ L> Sym_arg_0 ]].foo ]]"
      it "fails on an atom that cannot fire without the flag" $
        withStdin stuck $
          testCLIFailed ["morph", "--locator=Q.@"] ["Atom 'Sym_arg_0' does not exist"]

      it "prints the residue with the stuck application intact and exits successfully" $
        withStdin stuck $
          testCLISucceeded
            ["morph", "--locator=Q.@", "--partial", "--flat", "--hide-rho"]
            ["⟦ λ ⤍ Sym_arg_0 ⟧.foo"]

    -- 𝕄 stops at the first formation and hands its bindings back as they were
    -- written, so a program whose parts nothing demands is never reduced;
    -- '--deep' enters every binding and finishes what 'mf' left, while what no
    -- atom touched keeps its name and the answer stays a program (#1124)
    describe "--deep" $ do
      let program =
            "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, \
            \number(φ) -> [[ times(x) -> [[ L> L_number_times ]] ]], \
            \bar(x) -> [[ L> L_bar ]], \
            \demo -> [[ foo -> [[ n -> 3, @ -> Q.bar( $.n.times( 5 ).times( 7 ) ) ]] ]] ]]"
      it "answers the formation as it was written without the flag" $
        withAtoms $ \atoms ->
          withStdin program $
            testCLISucceeded
              ["morph", atoms, "--inside=Q.demo.foo", "--sweet", "--hide-rho", "--flat"]
              ["⟦ n ↦ 3, φ ↦ Φ.bar( n.times( 5 ).times( 7 ) ) ⟧"]

      -- 'L_bar' is not in the registry, so the call to it stays as written and
      -- keeps its name, while the arithmetic in the argument nothing demands
      -- folds into the number it makes
      it "reduces every binding it can and leaves the rest in place" $
        withAtoms $ \atoms ->
          withStdin program $
            testCLISucceeded
              ["morph", atoms, "--deep", "--inside=Q.demo.foo", "--sweet", "--hide-rho", "--flat"]
              ["⟦ n ↦ 3, φ ↦ Φ.bar( 105 ) ⟧"]

      -- The same term the run above stops at as a bare λ-formation: 'mf' leaves
      -- it to 𝔻, and the walk fires it instead of demanding bytes
      it "fires the bare saturated λ-formation mf hands back" $
        withAtoms $ \atoms ->
          withStdin chained $
            testCLISucceeded
              ["morph", atoms, "--deep", "--locator=Q.@", "--sweet", "--hide-rho", "--flat"]
              ["18"]

      -- The default locator walks the whole program: the method table of the
      -- object model keeps every one of its λ-formations, since not one of them
      -- is saturated, while the one place that can be computed is
      it "keeps the object model intact while it folds the program" $
        withAtoms $ \atoms ->
          withStdin program $
            testCLISucceeded
              ["morph", atoms, "--deep", "--sweet", "--hide-rho", "--flat"]
              [ "number(φ) ↦ ⟦ times(x) ↦ ⟦ λ ⤍ L_number_times ⟧ ⟧"
              , "demo ↦ ⟦ foo ↦ ⟦ n ↦ 3, φ ↦ Φ.bar( 105 ) ⟧ ⟧"
              ]

      it "keeps a binding whose spine got stuck with --partial" $
        withStdin "[[ x -> [[ L> Sym_arg_0 ]].foo ]]" $
          testCLISucceeded
            ["morph", "--deep", "--partial", "--sweet", "--hide-rho", "--flat"]
            ["⟦ x ↦ ⟦ λ ⤍ Sym_arg_0 ⟧.foo ⟧"]

      it "fails on that same spine without --partial" $
        withStdin "[[ x -> [[ L> Sym_arg_0 ]].foo ]]" $
          testCLIFailed ["morph", "--deep"] ["Atom 'Sym_arg_0' does not exist"]

    describe "fails" $ do
      it "with --output != latex and --nonumber" $
        withStdin "" $
          testCLIFailed
            ["morph", "--nonumber", "--output=xmir"]
            ["The --nonumber option can stay together with --output=latex only"]

      it "with --evaluations and --output != phi" $
        withStdin "[[ D> 01- ]]" $
          testCLIFailed
            ["morph", "--evaluations=evaluations.txt", "--output=latex"]
            ["The --evaluations option can stay together with --output=phi only"]

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
    it "prints help" $
      testCLISucceeded
        ["explain", "--help"]
        ["Explain built-in morphing rules", "Explain built-in dataization rules", "Explain built-in contextualization rules"]

    it "explains single rule" $
      testCLISucceeded
        ["explain", "--rule=resources/normalize/copy.yaml"]
        [ unlines
            [ "\\phinoNormalizationRule{copy}"
            , "  { [[ B_1, \\tau_1 -> ?, B_2 ]] ( \\tau_1 -> k_1 ) }"
            , "  { [[ B_1, \\tau_1 -> k_1, B_2 ]] }"
            , "  { }"
            , "  { }"
            ]
        ]

    it "explains single rule with a label" $
      testCLISucceeded
        ["explain", "--rule=test-resources/cli/labeled.yaml"]
        [ unlines
            [ "\\phinoNormalizationRule[\\lambda]{copy}"
            , "  { [[ B_1, \\tau_1 -> ?, B_2 ]] ( \\tau_1 -> k_1 ) }"
            , "  { [[ B_1, \\tau_1 -> k_1, B_2 ]] }"
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

    it "explains normalization rules" $
      testCLISucceeded
        ["explain", "--normalize"]
        [ unlines
            [ "\\phinoNormalizationRule{alpha}"
            , "  { [[ B_1, \\tau_1 -> ?, B_2 ]] ( \\phiTerminal{\\alpha_{i1}} -> e_1 ) }"
            , "  { [[ B_1, \\tau_1 -> ?, B_2 ]] ( \\tau_1 -> e_1 ) }"
            , "  { i_1 = \\vert \\overline{ B_1 } \\vert \\;\\text{and}\\; \\tau_1 \\not= \\phiTerminal{\\rho} }"
            , "  { }"
            , "\\phinoNormalizationRule{amiss}"
            , "  { [[ B_1 ]] ( \\phiTerminal{\\alpha_{i1}} -> e ) }"
            , "  { T }"
            , "  { \\vert \\overline{ B_1 } \\vert \\leq i_1 }"
            , "  { }"
            , "\\phinoNormalizationRule{copy}"
            , "  { [[ B_1, \\tau_1 -> ?, B_2 ]] ( \\tau_1 -> k_1 ) }"
            , "  { [[ B_1, \\tau_1 -> k_1, B_2 ]] }"
            , "  { }"
            , "  { }"
            , "\\phinoNormalizationRule{dc}"
            , "  { T ( \\tau -> e ) }"
            , "  { T }"
            , "  { }"
            , "  { }"
            , "\\phinoNormalizationRule{dca}"
            , "  { T ( \\phiTerminal{\\alpha_{i}} -> e ) }"
            , "  { T }"
            , "  { }"
            , "  { }"
            , "\\phinoNormalizationRule{dd}"
            , "  { T . \\tau }"
            , "  { T }"
            , "  { }"
            , "  { }"
            , "\\phinoNormalizationRule{dl}"
            , "  { [[ B_1, L> F, B_2 ]] }"
            , "  { T }"
            , "  { D \\in B_1 \\;\\text{or}\\; D \\in B_2 }"
            , "  { }"
            , "\\phinoNormalizationRule{dot}"
            , "  { [[ B_1, \\tau_1 -> n_1, B_2 ]] . \\tau_1 }"
            , "  { e_1 ( \\phiTerminal{\\rho} -> [[ B_1, \\tau_1 -> n_1, B_2 ]] ) }"
            , "  { }"
            , "  { \\phinoContextualize{ n_1 }{ [[ B_1, B_2 ]] }{ e_1 } }"
            , "\\phinoNormalizationRule{miss}"
            , "  { [[ B_1 ]] ( \\tau_1 -> e ) }"
            , "  { T }"
            , "  { \\tau_1 \\notin B_1 }"
            , "  { }"
            , "\\phinoNormalizationRule{null}"
            , "  { [[ B_1, \\tau_1 -> ?, B_2 ]] . \\tau_1 }"
            , "  { T }"
            , "  { }"
            , "  { }"
            , "\\phinoNormalizationRule{over}"
            , "  { [[ B_1, \\tau_1 -> e_1, B_2 ]] ( \\tau_1 -> e_2 ) }"
            , "  { T }"
            , "  { \\tau_1 \\not= \\phiTerminal{\\rho} }"
            , "  { }"
            , "\\phinoNormalizationRule{overa}"
            , "  { [[ B_1, \\tau_1 -> e_1, B_2 ]] ( \\phiTerminal{\\alpha_{i1}} -> e_2 ) }"
            , "  { T }"
            , "  { i_1 = \\vert \\overline{ B_1 } \\vert \\;\\text{and}\\; \\tau_1 \\not= \\phiTerminal{\\rho} }"
            , "  { }"
            , "\\phinoNormalizationRule{stay}"
            , "  { [[ B_1, \\phiTerminal{\\rho} -> e_1, B_2 ]] ( \\phiTerminal{\\rho} -> e_2 ) }"
            , "  { [[ B_1, \\phiTerminal{\\rho} -> e_1, B_2 ]] }"
            , "  { }"
            , "  { }"
            , "\\phinoNormalizationRule{stop}"
            , "  { [[ B_1 ]] . \\tau_1 }"
            , "  { T }"
            , "  { \\tau_1 \\notin B_1 \\;\\text{and}\\; @ \\notin B_1 \\;\\text{and}\\; L \\notin B_1 }"
            , "  { }"
            ]
        ]

    it "explains morphing rules" $
      testCLISucceeded
        ["explain", "--morph"]
        [ unlines
            [ "\\begin{phinoMorphingInference}"
            , "  \\phinoName{mf}"
            , "  \\phinoConclusion{ \\phinoMorph{ [[ B_0 ]] }{ e_0 }{ s }{ [[ B_0 ]] }{ s } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{ml}"
            , "  \\phinoLabel{\\lambda}"
            , "  \\phinoPremise{ \\phinoEvaluate{ [[ B_1, L> F_0, B_2 ]] }{ e_0 }{ s_1 }{ n_1 }{ s_2 } }"
            , "  \\phinoPremise{ \\phinoNormalize{ n_1 . \\tau_0 }{ n_2 } }"
            , "  \\phinoPremise{ \\phinoMorph{ n_2 }{ e_0 }{ s_2 }{ n_3 }{ s_3 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ [[ B_1, L> F_0, B_2 ]] . \\tau_0 }{ e_0 }{ s_1 }{ n_3 }{ s_3 } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{mphi}"
            , "  \\phinoLabel{\\varphi}"
            , "  \\phinoCondition{ @ \\in B_0 \\;\\text{and}\\; \\tau_0 \\notin B_0 \\;\\text{and}\\; L \\notin B_0 }"
            , "  \\phinoPremise{ \\phinoNormalize{ [[ B_0 ]] . @ . \\tau_0 }{ n_1 } }"
            , "  \\phinoPremise{ \\phinoMorph{ n_1 }{ e_0 }{ s_1 }{ n_2 }{ s_2 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ [[ B_0 ]] . \\tau_0 }{ e_0 }{ s_1 }{ n_2 }{ s_2 } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{md}"
            , "  \\phinoCondition{ \\phinoNotFormation{ n_0 } }"
            , "  \\phinoPremise{ \\phinoMorph{ n_0 }{ e_0 }{ s_1 }{ n_1 }{ s_2 } }"
            , "  \\phinoPremise{ \\phinoNormalize{ n_1 . \\tau_0 }{ n_2 } }"
            , "  \\phinoPremise{ \\phinoMorph{ n_2 }{ e_0 }{ s_2 }{ n_3 }{ s_3 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ n_0 . \\tau_0 }{ e_0 }{ s_1 }{ n_3 }{ s_3 } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{ma}"
            , "  \\phinoPremise{ \\phinoMorph{ n_0 }{ e_0 }{ s_1 }{ n_1 }{ s_2 } }"
            , "  \\phinoPremise{ \\phinoNormalize{ n_1 ( \\tau_0 -> k_1 ) }{ n_2 } }"
            , "  \\phinoPremise{ \\phinoMorph{ n_2 }{ e_0 }{ s_2 }{ n_3 }{ s_3 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ n_0 ( \\tau_0 -> k_1 ) }{ e_0 }{ s_1 }{ n_3 }{ s_3 } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{maa}"
            , "  \\phinoPremise{ \\phinoMorph{ n_0 }{ e_0 }{ s_1 }{ n_1 }{ s_2 } }"
            , "  \\phinoPremise{ \\phinoNormalize{ n_1 ( \\phiTerminal{\\alpha_{i0}} -> k_1 ) }{ n_2 } }"
            , "  \\phinoPremise{ \\phinoMorph{ n_2 }{ e_0 }{ s_2 }{ n_3 }{ s_3 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ n_0 ( \\phiTerminal{\\alpha_{i0}} -> k_1 ) }{ e_0 }{ s_1 }{ n_3 }{ s_3 } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{mad}"
            , "  \\phinoCondition{ \\phinoNotAbsolute{ n_1 } }"
            , "  \\phinoPremise{ \\phinoMorph{ T }{ e_0 }{ s_1 }{ n_2 }{ s_2 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ n ( \\tau -> n_1 ) }{ e_0 }{ s_1 }{ n_2 }{ s_2 } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{maad}"
            , "  \\phinoCondition{ \\phinoNotAbsolute{ n_1 } }"
            , "  \\phinoPremise{ \\phinoMorph{ T }{ e_0 }{ s_1 }{ n_2 }{ s_2 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ n ( \\phiTerminal{\\alpha_{i}} -> n_1 ) }{ e_0 }{ s_1 }{ n_2 }{ s_2 } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{universe}"
            , "  \\phinoLabel{\\Phi}"
            , "  \\phinoCondition{ e_0 \\not= Q }"
            , "  \\phinoPremise{ \\phinoNormalize{ e_0 }{ n_1 } }"
            , "  \\phinoPremise{ \\phinoMorph{ n_1 }{ e_0 }{ s_1 }{ n_2 }{ s_2 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ Q }{ e_0 }{ s_1 }{ n_2 }{ s_2 } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{dead}"
            , "  \\phinoConclusion{ \\phinoMorph{ T }{ e_0 }{ s }{ T }{ s } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{xi}"
            , "  \\phinoPremise{ \\phinoMorph{ T }{ e_0 }{ s_1 }{ n_1 }{ s_2 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ \\phiTerminal{\\xi} }{ e_0 }{ s_1 }{ n_1 }{ s_2 } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{mg}"
            , "  \\phinoPremise{ \\phinoMorph{ T }{ Q }{ s_1 }{ n_1 }{ s_2 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ Q }{ Q }{ s_1 }{ n_1 }{ s_2 } }"
            , "\\end{phinoMorphingInference}"
            ]
        ]

    it "explains dataization rules" $
      testCLISucceeded
        ["explain", "--dataize"]
        [ unlines
            [ "\\begin{phinoDataizationInference}"
            , "  \\phinoName{delta}"
            , "  \\phinoLabel{\\Delta}"
            , "  \\phinoConclusion{ \\phinoDataize{ [[ B_1, D> \\delta_0, B_2 ]] }{ e_0 }{ s }{ \\delta_0 }{ s } }"
            , "\\end{phinoDataizationInference}"
            , "\\begin{phinoDataizationInference}"
            , "  \\phinoName{box}"
            , "  \\phinoCondition{ [ D \\char44{} L ] \\cap \\lparen B_1 \\cup B_2 \\rparen = \\emptyset }"
            , "  \\phinoPremise{ \\phinoContextualize{ e_1 }{ [[ B_1, @ -> e_1, B_2 ]] }{ e_2 } }"
            , "  \\phinoPremise{ \\phinoNormalize{ e_2 }{ n_1 } }"
            , "  \\phinoPremise{ \\phinoDataize{ n_1 }{ e_0 }{ s_1 }{ \\delta_0 }{ s_2 } }"
            , "  \\phinoConclusion{ \\phinoDataize{ [[ B_1, @ -> e_1, B_2 ]] }{ e_0 }{ s_1 }{ \\delta_0 }{ s_2 } }"
            , "\\end{phinoDataizationInference}"
            , "\\begin{phinoDataizationInference}"
            , "  \\phinoName{fire}"
            , "  \\phinoPremise{ \\phinoEvaluate{ [[ B_1, L> F_0, B_2 ]] }{ e_0 }{ s_1 }{ n_1 }{ s_2 } }"
            , "  \\phinoPremise{ \\phinoDataize{ n_1 }{ e_0 }{ s_2 }{ \\delta_0 }{ s_3 } }"
            , "  \\phinoConclusion{ \\phinoDataize{ [[ B_1, L> F_0, B_2 ]] }{ e_0 }{ s_1 }{ \\delta_0 }{ s_3 } }"
            , "\\end{phinoDataizationInference}"
            , "\\begin{phinoDataizationInference}"
            , "  \\phinoName{none}"
            , "  \\phinoCondition{ [ D \\char44{} L \\char44{} @ ] \\cap B_0 = \\emptyset }"
            , "  \\phinoPremise{ \\phinoDataize{ T }{ e_0 }{ s_1 }{ \\delta_0 }{ s_2 } }"
            , "  \\phinoConclusion{ \\phinoDataize{ [[ B_0 ]] }{ e_0 }{ s_1 }{ \\delta_0 }{ s_2 } }"
            , "\\end{phinoDataizationInference}"
            , "\\begin{phinoDataizationInference}"
            , "  \\phinoName{norm}"
            , "  \\phinoCondition{ \\phinoNotFormation{ n_0 } \\;\\text{and}\\; n_0 \\not= T }"
            , "  \\phinoPremise{ \\phinoMorph{ n_0 }{ e_0 }{ s_1 }{ n_1 }{ s_2 } }"
            , "  \\phinoPremise{ \\phinoDataize{ n_1 }{ e_0 }{ s_2 }{ \\delta_0 }{ s_3 } }"
            , "  \\phinoConclusion{ \\phinoDataize{ n_0 }{ e_0 }{ s_1 }{ \\delta_0 }{ s_3 } }"
            , "\\end{phinoDataizationInference}"
            ]
        ]

    it "explains contextualization rules" $
      testCLISucceeded
        ["explain", "--contextualize"]
        [ unlines
            [ "\\begin{phinoContextualizationInference}"
            , "  \\phinoName{cg}"
            , "  \\phinoConclusion{ \\phinoContextualize{ Q }{ k_0 }{ Q } }"
            , "\\end{phinoContextualizationInference}"
            , "\\begin{phinoContextualizationInference}"
            , "  \\phinoName{cxi}"
            , "  \\phinoConclusion{ \\phinoContextualize{ \\phiTerminal{\\xi} }{ k_0 }{ k_0 } }"
            , "\\end{phinoContextualizationInference}"
            , "\\begin{phinoContextualizationInference}"
            , "  \\phinoName{ct}"
            , "  \\phinoConclusion{ \\phinoContextualize{ T }{ k_0 }{ T } }"
            , "\\end{phinoContextualizationInference}"
            , "\\begin{phinoContextualizationInference}"
            , "  \\phinoName{cf}"
            , "  \\phinoConclusion{ \\phinoContextualize{ [[ B_0 ]] }{ k_0 }{ [[ B_0 ]] } }"
            , "\\end{phinoContextualizationInference}"
            , "\\begin{phinoContextualizationInference}"
            , "  \\phinoName{cd}"
            , "  \\phinoPremise{ \\phinoContextualize{ n_0 }{ k_0 }{ n_1 } }"
            , "  \\phinoConclusion{ \\phinoContextualize{ n_0 . \\tau_0 }{ k_0 }{ n_1 . \\tau_0 } }"
            , "\\end{phinoContextualizationInference}"
            , "\\begin{phinoContextualizationInference}"
            , "  \\phinoName{ca}"
            , "  \\phinoPremise{ \\phinoContextualize{ n_0 }{ k_0 }{ n_1 } }"
            , "  \\phinoPremise{ \\phinoContextualize{ e_1 }{ k_0 }{ n_2 } }"
            , "  \\phinoConclusion{ \\phinoContextualize{ n_0 ( \\tau_0 -> e_1 ) }{ k_0 }{ n_1 ( \\tau_0 -> n_2 ) } }"
            , "\\end{phinoContextualizationInference}"
            , "\\begin{phinoContextualizationInference}"
            , "  \\phinoName{caa}"
            , "  \\phinoPremise{ \\phinoContextualize{ n_0 }{ k_0 }{ n_1 } }"
            , "  \\phinoPremise{ \\phinoContextualize{ e_1 }{ k_0 }{ n_2 } }"
            , "  \\phinoConclusion{ \\phinoContextualize{ n_0 ( \\phiTerminal{\\alpha_{i0}} -> e_1 ) }{ k_0 }{ n_1 ( \\phiTerminal{\\alpha_{i0}} -> n_2 ) } }"
            , "\\end{phinoContextualizationInference}"
            ]
        ]

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
        ["⟦ foo ↦ x ⟧"]

    it "merges EO expressions" $
      testCLISucceeded
        ["merge", "--sweet", resource "number.phi", resource "bytes.phi", resource "string.phi", "--margin=25"]
        [ unlines
            [ "⟦"
            , "  org ↦ ⟦"
            , "    eolang ↦ ⟦"
            , "      number(φ) ↦ ⟦⟧,"
            , "      bytes(data) ↦ ⟦⟧,"
            , "      string(φ) ↦ ⟦⟧,"
            , "      λ ⤍ Package"
            , "    ⟧,"
            , "    λ ⤍ Package"
            , "  ⟧"
            , "⟧"
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
        ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<listing>⟦ foo ↦ ξ.x, ρ ↦ ∅ ⟧</listing>", "<o base=\"ξ.x\" name=\"foo\"/>"]

  describe "match" $ do
    it "prints help" $
      testCLISucceeded
        ["match", "--help"]
        ["Pattern expression to match against", "Predicate for matched substitutions"]

    it "takes from stdin" $
      withStdin "[[]]" $
        testCLISucceeded ["match", "--log-level=debug"] ["[DEBUG]"]

    it "takes from file" $
      testCLISucceeded ["match", "test-resources/cli/foo.phi", "--log-level=debug"] ["[DEBUG]"]

    it "does not print substitutions without pattern" $
      withStdin "[[]]" $
        testCLISucceeded ["match", "--log-level=debug"] ["[DEBUG]: The --pattern is not provided, no substitutions are built"]

    it "prints one substitution" $
      withStdin "[[ x -> Q.x ]]" $
        testCLISucceeded ["match", "--pattern=Q.!t"] ["t >> x"]

    it "does not accept a --seed flag (matching has nothing random)" $
      withStdin "[[ x -> Q.x ]]" $
        testCLIFailed ["match", "--seed=3", "--pattern=Q.!t"] ["Invalid option `--seed=3'"]

    it "prints many substitutions" $
      withStdin "[[ x -> Q.x, y -> Q.y ]]" $
        testCLISucceeded ["match", "--pattern=Q.!t"] ["t >> x\n------\nt >> y"]

    it "builds substitutions with conditions" $
      withStdin "[[ x -> Q.y ]].x" $
        testCLISucceeded
          ["match", "--pattern=[[ !t1 -> Q.y, !B1 ]].!t1", "--when=eq(length(!B1),1)"]
          ["B1 >> ⟦ ρ ↦ ∅ ⟧\nt1 >> x"]

    it "builds with condition from file" $
      testCLISucceeded
        ["match", "--pattern=[[ !B1 ]]", "--when=eq(length(!B1),2)", "test-resources/cli/foo.phi"]
        ["B1 >> ⟦ foo ↦ Φ.org.eolang.x, ρ ↦ ∅ ⟧"]

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
