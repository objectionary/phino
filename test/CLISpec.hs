{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLISpec (spec) where

import CLI (runCLI)
import CLI.Types (CmdException (..), IOFormat (..))
import Control.Concurrent (threadDelay)
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
  bracket (pure dir) removedEventually action
  where
    removedEventually :: FilePath -> IO ()
    removedEventually = retrying (10 :: Int)
      where
        retrying 0 dir = removePathForcibly dir
        retrying left dir = removePathForcibly dir `catch` \(_ :: IOException) -> threadDelay 200000 >> retrying (left - 1) dir

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
    withTempFileContent "pin-versionXXXXXX.txt" (showVersion version ++ "\n") $ \file ->
      withStdin "[[ ]]" $
        testCLIFailed
          ["--pin=" ++ showVersion version, "--pin-file=" ++ file, "rewrite"]
          ["[ERROR]: Invalid option `--pin-file=" ++ file ++ "'"]

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
        , ["42.0:a"]
        )
      ,
        ( "keeps the one-binding sugar after inline voids"
        , "[[ x(y) -> [[ a -> 42, ^ -> ? ]] ]]"
        , ["rewrite", "--flat", "--sweet", "--hide-rho"]
        , ["⟦ x(y) ↦ 42.0:a ⟧"]
        )
      ,
        ( "prints a lone void as the term without the rho would be printed"
        , "⟦ a ↦ ⟦ x ↦ ⟦ ρ ↦ ξ, y ↦ ∅ ⟧ ⟧ ⟧"
        , ["rewrite", "--sweet", "--hide-rho"]
        , ["⟦ x(y) ↦ ⟦⟧ ⟧:a"]
        )
      ,
        ( "indents the body as the term without the rho would be indented"
        , "⟦ a ↦ ⟦ x ↦ ⟦ ρ ↦ ∅, b ↦ ⟦ c ↦ ∅, ρ ↦ ∅ ⟧ ⟧ ⟧ ⟧"
        , ["rewrite", "--sweet", "--hide-rho", "--margin=3"]
        , ["⟦\n  b(c) ↦ ⟦⟧\n⟧:x:a"]
        )
      ,
        ( "prints positional arguments as such once the rho is gone"
        , "⟦ a ↦ ξ.b(ρ ↦ ξ, α0 ↦ ξ.c, α1 ↦ ξ.d) ⟧"
        , ["rewrite", "--flat", "--sweet", "--hide-rho"]
        , ["b( c, d ):a"]
        )
      ,
        ( "drops the rho from the LaTeX output too"
        , "[[ n -> [[ ^ -> ?, k -> $ ]] ]]"
        , ["rewrite", "--flat", "--hide-rho", "--output=latex"]
        , ["[[ |n| -> [[ |k| -> \\phiTerminal{\\xi} ]] ]]"]
        )
      ,
        ( "drops the rho argument of a number literal from the LaTeX output"
        , "42(ρ ↦ ξ)"
        , ["rewrite", "--flat", "--hide-rho", "--output=latex"]
        , ["Q . |number| ( @ -> Q . |bytes| ( @ -> [[ D> |40-45-00-00-00-00-00-00| ]] ) ){.}"]
        )
      ,
        ( "drops only the rho argument of a string literal from the LaTeX output"
        , "\"s\"(ρ ↦ ⊥, b ↦ Φ)"
        , ["rewrite", "--flat", "--hide-rho", "--output=latex"]
        , ["Q . |string| ( @ -> Q . |bytes| ( @ -> [[ D> |73-| ]] ) ) ( |b| -> Q ){.}"]
        )
      ,
        ( "drops the rho argument of a non-finite literal from the LaTeX output"
        , "Φ.nan(ρ ↦ ξ)"
        , ["rewrite", "--flat", "--hide-rho", "--output=latex"]
        , ["Q . |number| ( @ -> Q . |bytes| ( @ -> [[ D> |7F-F8-00-00-00-00-00-00| ]] ) ){.}"]
        )
      ]
      (\(desc, input, args, expected) -> it desc (withStdin input (testCLISucceeded args expected)))

  it "keeps a data literal sugared when it is applied to more arguments" $
    withStdin "⟦ i ↦ 42(z ↦ ξ.f), s ↦ \"Hello\"(z ↦ ξ.f) ⟧" $
      testCLISucceeded ["rewrite", "--sweet", "--flat"] ["⟦ i ↦ 42.0( z ↦ f ), s ↦ \"Hello\"( z ↦ f ) ⟧"]

  it "prints the one-binding sugar after inline voids with --sweet" $
    withStdin "[[ x(y) -> [[ a -> 42 ]] ]]" $
      testCLISucceeded ["rewrite", "--sweet"] ["⟦ x(y) ↦ 42.0:a ⟧"]

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
    let xmir =
          unlines
            [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            , "<object>"
            , "  <o name=\"bar\">"
            , "    <o base=\"∅\" name=\"x\"/>"
            , "  </o>"
            , "</object>"
            ]
    describe "fails" $ do
      forM_
        [ ("with --input=latex", "", ["rewrite", "--input=latex"], ["The value 'latex' can't be used for '--input' option"])
        , ("with negative --log-lines", "", ["rewrite", "--log-lines=-2"], ["--log-lines must be >= -1"])
        , ("with negative --max-depth", "", ["rewrite", "--max-depth=-1"], ["--max-depth must be positive"])
        , ("with zero --max-cycles", "", ["rewrite", "--max-cycles=0"], ["--max-cycles must be positive"])
        , ("with zero --meet-length", "", ["rewrite", "--output=latex", "--meet-length=0"], ["--meet-length must be positive"])
        , ("with a --max-cycles value above maxBound Int", "", ["rewrite", "--max-cycles=18446744073709551617"], ["value must be between", "9223372036854775807"])
        ,
          ( "with --normalize and --must=1"
          , "[[ x -> [[ y -> 5 ]].y ]].x"
          , ["rewrite", "--max-cycles=2", "--max-depth=1", "--normalize", "--must=1"]
          , ["it's expected rewriting cycles to be in range [1], but rewriting has already reached 2"]
          )
        , ("when --in-place is used without input file", "[[ ]]", ["rewrite", "--in-place"], ["--in-place requires an input file"])
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
            ["The option --in-place requires the output format to match the input format"]

      it "requires an XMIR input to be written back as XMIR" $
        withTempFileContent "inplaceXXXXXX.xmir" xmir $ \path ->
          testCLIFailed
            ["rewrite", "--input=xmir", "--in-place", path]
            ["The option --in-place requires the output format to match the input format"]

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
          ( "when --hide is used with --in-place"
          , "[[ ]]"
          , ["rewrite", "--hide=Q.y", "--in-place", "input.phi"]
          , ["--in-place and --hide cannot be used together"]
          )
        ,
          ( "when --hide-rho is used with --in-place"
          , "[[ ]]"
          , ["rewrite", "--hide-rho", "--in-place", "input.phi"]
          , ["--in-place and --hide-rho cannot be used together"]
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
          ( "with an unsafe --label and --output=latex"
          , ["rewrite", "--label=eq:bad%tag", "--output=latex"]
          , ["The --label option must contain only letters, numbers, colons, periods, underscores, and hyphens"]
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
        ,
          ( "with non-dispatch --focus"
          , ["rewrite", "--focus=Q.x(Q.y)"]
          , ["Invalid set of arguments: Only dispatch expression started with Φ (or Q) can be used in --focus, but given: Φ.x( Φ.y )"]
          )
        , ("with --focus!=Q and --output=XMIR", ["rewrite", "--focus=Q.x", "--output=xmir"], ["Only --focus=Q is allowed to be used with --output=xmir"])
        , ("with --margin < 0", ["rewrite", "--margin=-1"], ["--margin must be positive"])
        ,
          ( "with --breakpoint which does not exist across the rules"
          , ["rewrite", "--breakpoint=hello", "--normalize"]
          , ["The rule 'hello' provided in '--breakpoint' option is absent across given rewriting rules: alpha"]
          )
        ]
        (\(desc, args, expected) -> it desc (withStdin "[[ x -> 5 ]]" (testCLIFailed args expected)))

    it "prints help" $
      testCLISucceeded
        ["rewrite", "--help"]
        ["Rewrite the 𝜑-expression", "--seed SEED"]

    it "accepts --seed flag" $
      withStdin "[[ x -> 5 ]]" $
        testCLISucceeded
          ["rewrite", "--seed=42", "--sweet"]
          ["5.0:x"]

    it "defaults --seed to 0 in help" $
      testCLISucceeded
        ["rewrite", "--help"]
        ["default: 0"]

    it "describes --must as counting rewriting cycles, not rules applied" $
      testCLISucceeded
        ["rewrite", "--help"]
        ["Stops execution if number of rewriting cycles is not"]

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

    it "shuffles the rules away from their file order" $ do
      let base =
            [ "--sweet"
            , "--sequence"
            , "--max-depth=1"
            , "--max-cycles=1"
            , rule "swap-a.yaml"
            , rule "swap-b.yaml"
            ]
      (plain, _) <- withStdin "[[ x -> 5 ]]" $ withStdout (runCLI ("rewrite" : base))
      (mixed, _) <- withStdin "[[ x -> 5 ]]" $ withStdout (runCLI ("rewrite" : "--shuffle" : "--seed=1" : base))
      mixed `shouldNotBe` plain

    it "fails with a non-integer --seed" $
      withStdin "[[ ]]" $
        testCLIFailed
          ["rewrite", "--seed=abc"]
          ["[ERROR]"]

    it "refuses a --seed outside the Int range" $
      withStdin "[[ ]]" $
        testCLIFailed
          ["rewrite", "--seed=18446744073709551616"]
          ["value must be between"]

    it "refuses a non-positive --abridged width" $
      withStdin "[[ t -> 01- ]]" $
        testCLIFailed
          ["dataize", "--locator=Q.t", "--protocol=p.txt", "--abridged=0"]
          ["--abridged must be positive"]

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

    it "clears step files left from an earlier, longer run with --steps-dir" $
      withTempDirectory "phino-steps-stale" $ \dir -> do
        withStdin "[[ x -> \"hello\"]]" $
          testCLISucceeded
            ["rewrite", rule "infinite.yaml", "--max-cycles=2", "--max-depth=2", "--steps-dir=" ++ dir, "--sweet"]
            ["hello_hi_hi"]
        files <- listDirectory dir
        length files `shouldBe` 4
        withStdin "[[ x -> \"hello\"]]" $
          testCLISucceeded
            ["rewrite", rule "infinite.yaml", "--max-cycles=1", "--max-depth=2", "--steps-dir=" ++ dir, "--sweet"]
            ["hello_hi"]
        files2 <- listDirectory dir
        sort files2 `shouldBe` ["00001.phi", "00002.phi"]

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

    it "gives the saved steps the --focus of the printed ones" $
      withTempDirectory "phino-steps-focused" $ \dir ->
        withStdin "⟦ x ↦ ⟦ k ↦ ⟦ q ↦ ⟦⟧ ⟧ ⟧.k, y ↦ ⟦ z ↦ ⟦⟧ ⟧ ⟧" $ do
          testCLISucceeded
            ["rewrite", "--normalize", "--sweet", "--flat", "--focus=Q.y", "--sequence", "--steps-dir=" ++ dir]
            ["⟦⟧:z"]
          files <- listDirectory dir
          null files `shouldBe` False
          saved <- mapM (\file -> readFile (dir ++ "/" ++ file)) (sort files)
          map (filter (/= '\n')) saved `shouldSatisfy` all (== "⟦⟧:z")

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

    it "reports a broken rule file in plain text with its path" $
      withStdin "⟦⟧" $
        testCLIFailed
          ["rewrite", rule "malformed-pattern.yaml"]
          [ "Could not load rule from 'test-resources/cli/rules/malformed-pattern.yaml'"
          , "unexpected '⟧'"
          ]

    it "counts the position of a YAML error from one" $
      withTempFileContent "badruleXXXXXX.yaml" "name: a\npattern: x: y\n" $ \path ->
        withStdin "⟦⟧" $
          testCLIFailed
            ["rewrite", "--rule=" ++ path]
            ["YAML parse exception at line 2, column 11"]

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
          ["5.0:x"]

    it "rewrites as XMIR" $
      withStdin "[[ x -> Q.y ]]" $
        testCLISucceeded
          ["rewrite", "--output=xmir"]
          ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<object", "  <o base=\"Φ.y\" name=\"x\"/>"]

    it "accepts --focus=Φ together with --output=xmir, since Φ and Q are the same locator" $
      withStdin "[[ x -> Q.y ]]" $
        testCLISucceeded
          ["rewrite", "--focus=Φ", "--output=xmir"]
          ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<object", "  <o base=\"Φ.y\" name=\"x\"/>"]

    it "rewrites a formation of several bindings at the root as XMIR" $
      withStdin "⟦ x ↦ 1, ρ ↦ 2 ⟧" $
        testCLISucceeded
          ["rewrite", "--output=xmir"]
          ["<o base=\"Φ.number\" name=\"x\">", "<o base=\"Φ.number\" name=\"ρ\">"]

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
              , "  |x\\char95{}o| -> Q . |z| ( |y| -> |5.0| ),"
              , "  |q\\char36{}| -> T,"
              , "  |w| -> \\phiTerminal{\\xi},"
              , "  \\phiTerminal{\\rho} -> Q,"
              , "  @ -> |1.0|,"
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
              , "|5.0| : |x|{.}"
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
              , "\\phiExpression{foo} |5.0| : |x|{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "escapes special characters in the LaTeX expression name" $
      withStdin "[[ x -> 5 ]]" $
        testCLISucceeded
          ["rewrite", "--output=latex", "--sweet", "--flat", "--expression=bad%tag"]
          [ unlines
              [ "\\begin{phiquation}"
              , "\\phiExpression{bad\\char37{}tag} |5.0| : |x|{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "rewrite as LaTeX with label name" $
      withStdin "[[ x -> 5 ]]" $
        testCLISucceeded
          ["rewrite", "--output=latex", "--sweet", "--flat", "--label=foo"]
          [ unlines
              [ "\\begin{phiquation}\n\\label{foo}"
              , "|5.0| : |x|{.}"
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
              , "[[ |y| -> ?, |k| -> \\phinoMeet{1}{ |42.0| : |t| } ]] ( |y| -> \\phinoAgain{1} ) : |x| . |i| : |ex| \\phiNormalize[\\nameref{r:copy}]"
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
              , "[[ |y| -> ?, |k| -> |42.0| : |t| ]] ( |y| -> |42.0| : |t| ) : |x| . |i| : |ex| \\phiNormalize[\\nameref{r:copy}]"
              , "  \\phiNormalize [[ |y| -> |42.0| : |t|, |k| -> |42.0| : |t| ]] : |x| . |i| : |ex| \\phiNormalize[\\nameref{r:stop}]"
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
              , "[[ |y| -> ?, |k| -> |42.0| : |t| ]] ( |y| -> |42.0| : |t| ) : |x| . |i| : |ex| \\phiNormalize[\\nameref{r:copy}]"
              , "  \\phiNormalize [[ |y| -> |42.0| : |t|, |k| -> |42.0| : |t| ]] : |x| . |i| : |ex| \\phiNormalize[\\nameref{r:stop}]"
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
              , "[[ |y| -> ?, |k| -> |42.0| : |t| ]] ( |y| -> |42.0| : |t| ) : |x| . |i| \\phiNormalize[\\nameref{r:copy}]"
              , "  \\phiNormalize [[ |y| -> |42.0| : |t|, |k| -> |42.0| : |t| ]] : |x| . |i| \\phiNormalize[\\nameref{r:stop}]"
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

    it "ends the last step with a period when --max-cycles lands on the normal form" $
      withStdin "⟦ a ↦ ⟦ x ↦ ∅ ⟧( x ↦ ⟦⟧ ).x ⟧" $
        testCLISucceeded
          ["rewrite", "--normalize", "--max-cycles=1", "--sequence", "--output=latex"]
          ["  \\phiNormalize [[ |a| -> [[]] ]]{.}"]

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
              [ "⟦ y ↦ ∅, k ↦ 42.0:t ⟧( y ↦ 42.0:t ):x.i"
              , "⟦ y ↦ 42.0:t, k ↦ 42.0:t ⟧:x.i"
              , "⊥"
              ]
          ]

    it "prints input as listing in XMIR" $
      withStdin "[[ app -> [[]] ]]" $
        testCLISucceeded
          ["rewrite", "--output=xmir", "--omit-comments", "--sweet", "--flat"]
          ["  <listing>[[ app -&gt; [[]] ]]</listing>"]

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
          content `shouldBe` "⟦⟧\n"

    it "modifies file in-place" $
      withTempFile "inplaceXXXXXX.phi" $ \(path, h) -> do
        hPutStr h "[[ x -> \"foo\" ]]"
        hClose h
        testCLISucceeded ["rewrite", rule "simple.yaml", "--in-place", "--sweet", path] []
        content <- readFile path
        content `shouldBe` "\"bar\":x\n"

    it "writes the same bytes to --target as it prints to the console" $
      withTempFile "targetXXXXXX.tmp" $ \(path, h) -> do
        hClose h
        (console, _) <- withStdout (withStdin "[[ ]]" (runCLI ["rewrite", "--sweet"]))
        withStdin "[[ ]]" (testCLISucceeded ["rewrite", "--sweet", printf "--target=%s" path] [])
        content <- readFile path
        content `shouldBe` console

    it "keeps the trailing newline of the source file when nothing is rewritten by --in-place" $
      withTempFile "inplaceXXXXXX.phi" $ \(path, h) -> do
        hPutStr h "⟦⟧\n"
        hClose h
        testCLISucceeded ["rewrite", "--in-place", "--sweet", path] []
        content <- readFile path
        content `shouldBe` "⟦⟧\n"

    it "modifies file in-place with --focus=Φ, since Φ and Q are the same locator" $
      withTempFile "inplaceXXXXXX.phi" $ \(path, h) -> do
        hPutStr h "[[ x -> \"foo\" ]]"
        hClose h
        testCLISucceeded ["rewrite", rule "simple.yaml", "--focus=Φ", "--in-place", "--sweet", path] []
        content <- readFile path
        content `shouldBe` "\"bar\":x\n"

    it "keeps the XMIR format when rewriting an XMIR file in-place" $
      withTempFileContent "inplaceXXXXXX.xmir" xmir $ \path -> do
        testCLISucceeded ["rewrite", "--input=xmir", "--output=xmir", "--in-place", path] []
        content <- readFile path
        _ <- evaluate (length content)
        content `shouldContain` "<object"

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
          content `shouldBe` "\"bar\":x\n"

    it "rewrites with cycles" $
      withStdin "[[ x -> \"x\" ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", rule "infinite.yaml", "--max-depth=1", "--max-cycles=2"]
          ["\"x_hi_hi\":x"]

    it "hides default package" $
      withStdin "[[ org -> [[ eolang -> [[ number -> [[]] ]]]], x -> 42 ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--hide=Q.org"]
          ["42.0:x"]

    it "hides several FQNs" $
      withStdin "[[ org -> [[ eolang -> Q.x, yegor256 -> Q.y ]], x -> 42 ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--hide=Q.org.eolang", "--hide=Q.org.yegor256"]
          ["⟦ org ↦ ⟦⟧, x ↦ 42.0 ⟧"]

    it "shows and hides" $
      withStdin "[[ org -> [[ eolang -> Q.x, yegor256 -> Q.y ]], x -> 42 ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--show=Q.org", "--hide=Q.org.eolang"]
          ["Φ.y:yegor256:org"]

    it "hides an object outside the --show locator" $
      withStdin "[[ org -> [[ eolang -> Q.x ]], x -> 42 ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--show=Q.x", "--hide=Q.org.eolang"]
          ["42.0:x"]

    it "hides an object inside an earlier --hide locator" $
      withStdin "[[ org -> [[ eolang -> [[ number -> [[]] ]] ]], x -> 42 ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--hide=Q.org.eolang", "--hide=Q.org.eolang.number"]
          ["⟦ org ↦ ⟦⟧, x ↦ 42.0 ⟧"]

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
          ["⟦ x ↦ 5.0, y ↦ \"hey\", z ↦ ⟦⟧:w ⟧"]

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
            ["⟦ z ↦ 42.0, org ↦ ⟦ bytes(data) ↦ ⟦⟧, number(as-bytes) ↦ ⟦⟧ ⟧:eolang ⟧"]
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
          ["⟦ ex ↦ 5.0:x, abc ↦ ∅:x( x ↦ 5.0 ) ⟧"]

    it "returns original expression on --breakpoint" $
      withStdin "[[ x -> ?, y -> $.x ]](x -> [[ D> 42- ]]).y" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--normalize", "--breakpoint=stop", "--log-level=debug"]
          [ "Applied 'copy' (22 nodes -> 17 nodes)"
          , "Rule 'stop' is a breakpoint, dropping down all the previous rewritings..."
          , "⟦ x ↦ ∅, y ↦ x ⟧( x ↦ 42-:Δ ).y"
          ]

    it "leaves no step files of the rewritings --breakpoint dropped" $
      withTempDirectory "phino-steps-breakpoint" $ \dir ->
        withStdin "[[ x -> ?, y -> $.x ]](x -> [[ D> 42- ]]).y" $ do
          testCLISucceeded
            ["rewrite", "--sweet", "--flat", "--normalize", "--breakpoint=stop", "--steps-dir=" ++ dir]
            ["⟦ x ↦ ∅, y ↦ x ⟧( x ↦ 42-:Δ ).y"]
          listDirectory dir `shouldReturn` []

    it "finishes under --depth-sensitive when the only match left would not change the term" $
      withTempFileContent "phino-fixpoint.yaml" "name: fix\npattern: '[[ x -> !e1, !B1 ]]'\nresult: '[[ x -> Q, !B1 ]]'\n" $ \fix ->
        withStdin "[[ x -> $ ]]" $
          testCLISucceeded ["rewrite", "--rule=" ++ fix, "--max-depth=1", "--depth-sensitive", "--flat"] ["⟦ x ↦ Φ ⟧"]

    it "keeps the rewritten expression when the --breakpoint rule fired" $
      withStdin "⟦ a ↦ ⟦ b ↦ Φ ⟧.b ⟧" $
        testCLISucceeded
          ["rewrite", "--flat", "--normalize", "--breakpoint=dot"]
          ["⟦ a ↦ Φ( ρ ↦ ⟦ b ↦ Φ ⟧ ) ⟧"]

  describe "morph --focus under --locator" $ do
    it "finds the same object for the steps and for the answer" $
      withStdin "⟦ t ↦ ⟦ a ↦ ⟦ Δ ⤍ 01- ⟧ ⟧, a ↦ ⟦ Δ ⤍ 02- ⟧ ⟧" $ do
        (out, _) <- withStdout (runCLI ["morph", "--locator=Q.t", "--focus=Q.a", "--flat", "--sequence"])
        filter (not . null) (lines out) `shouldSatisfy` all (== "⟦ Δ ⤍ 02- ⟧")
    it "prints the answer when --focus names the object at --locator" $
      withStdin "⟦ t ↦ ⟦ a ↦ ⟦ Δ ⤍ 01- ⟧ ⟧ ⟧" $
        testCLISucceeded ["morph", "--locator=Q.t", "--focus=Q.t", "--flat", "--sequence"] ["⟦ a ↦ ⟦ Δ ⤍ 01- ⟧ ⟧"]
    it "fails on a --focus it cannot find before it prints any step" $
      withStdin "⟦ t ↦ ⟦ a ↦ ⟦ Δ ⤍ 01- ⟧ ⟧ ⟧" $ do
        (out, _) <- withStdout (try (runCLI ["morph", "--locator=Q.t", "--focus=Q.nope", "--flat", "--sequence"]) :: IO (Either ExitCode ()))
        out `shouldNotContain` "⟦ t ↦"

  it "fails on a --hide locator that matches nothing, as --show does" $
    withStdin "[[ x -> Q.y ]]" $
      testCLIFailed ["rewrite", "--hide=Q.nope"] ["Can't find object by locator: 'Φ.nope'"]

  it "names the term dataization cannot take, and says a normal form is expected" $
    withStdin "⟦ x ↦ ⟦ b ↦ ⟦ Δ ⤍ 01- ⟧ ⟧.b ⟧" $
      testCLIFailed ["dataize", "--locator=Q.x"] ["expects a normal form: 01-:Δ:b.b"]

  describe "--partial parks a universe that --max-cycles stops" $ do
    let heavy = "⟦ simple ↦ ⟦ Δ ⤍ 01- ⟧, heavy ↦ ⟦ a ↦ ⟦ b ↦ ⟦ c ↦ ⟦ Δ ⤍ 02- ⟧ ⟧ ⟧ ⟧.a.b.c ⟧"
    it "in dataize" $
      withStdin heavy $
        testCLISucceeded ["dataize", "--locator=Q.simple", "--max-cycles=1", "--partial", "--flat"] ["⟦ Δ ⤍ 01- ⟧"]
    it "in morph" $
      withStdin heavy $
        testCLISucceeded ["morph", "--locator=Q.simple", "--max-cycles=1", "--partial", "--flat"] ["⟦ Δ ⤍ 01- ⟧"]

  describe "dataize" $ do
    it "prints help" $
      testCLISucceeded ["dataize", "--help"] ["Dataize the 𝜑-expression"]

    it "names every block of a --symbolic entry in its help" $
      testCLISucceeded ["dataize", "--help"] ["\"dataize\"", "\"morph\"", "\"evaluate\"", "\"defer\"", "\"stay\"", "\"rewrite\"", "\"symbolize\"", "\"join\""]

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
            ["[ERROR]: Morphing did not finish before reaching the limit of steps: --max-steps=40"]

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
            ["[ERROR]: Morphing did not finish before reaching the limit of steps: --max-steps=40"]

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
            `shouldBe` [ "𝔻(Φ.t):"
                       , "  𝕄(Φ.t):"
                       , "    𝑛·0·1 := Φ.cyc( x ↦ ⟦⟧ )"
                       , "  boxed 𝑛·0·2 := 𝑛·0·1.φ"
                       , "  𝕄(𝑛·0·2):"
                       , "    𝑛·0·3 := Φ.cyc( x ↦ ⟦⟧ )"
                       , "  looped 𝑛·0·3  # 𝔻(𝑛·0·2), proven"
                       ]

      it "writes the cut to the XML protocol with the formation in an element of its own" $
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
                       , "  <morph at=\"Φ.t\">"
                       , "    <formed meta=\"𝑛·0·1\"/>"
                       , "    <applied meta=\"𝑛·0·2\" by=\"morph\" at=\"Φ.t\" of=\"Φ.cyc\"><meta attr=\"x\">𝑛·0·1</meta></applied>"
                       , "    <morphed meta=\"𝑛·0·3\">𝑛·0·2</morphed>"
                       , "  </morph>"
                       , "  <boxed meta=\"𝑛·0·4\">𝑛·0·2.φ</boxed>"
                       , "  <morph at=\"𝑛·0·4\">"
                       , "    <applied meta=\"𝑛·0·5\" by=\"morph\" at=\"𝑛·0·4\" of=\"Φ.cyc\"><meta attr=\"x\">𝑛·0·1</meta></applied>"
                       , "    <morphed meta=\"𝑛·0·6\">𝑛·0·5</morphed>"
                       , "  </morph>"
                       , "  <looped by=\"dataize\" match=\"proven\" at=\"𝑛·0·4\" of=\"𝑛·0·5\"/>"
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
          lines records `shouldContain` ["  looped 𝑛·0·3  # 𝔻(𝑛·0·2), plausible"]

      it "refuses the flag without a mode" $
        withStdin "⟦ t ↦ ⟦ Δ ⤍ 01-02 ⟧ ⟧" $
          testCLIFailed ["dataize", "--locator=Q.t", "--acyclic"] ["The option `--acyclic` expects an argument"]

      it "refuses a mode it does not know" $
        withStdin "⟦ t ↦ ⟦ Δ ⤍ 01-02 ⟧ ⟧" $
          testCLIFailed ["dataize", "--locator=Q.t", "--acyclic=sure"] ["The value 'sure' can't be used for '--acyclic' option"]

      it "answers a terminating program the same way with the flag" $
        withStdin "⟦ t ↦ ⟦ Δ ⤍ 01-02 ⟧ ⟧" $
          testCLISucceeded ["dataize", "--locator=Q.t", "--acyclic=proven"] ["01-02"]

    describe "--max-loops" $ do
      let once = "⟦ f ↦ ⟦ x ↦ ∅, φ ↦ ξ.x.again ⟧, start ↦ ⟦ again ↦ Φ.f( x ↦ Φ.stop( v ↦ Φ.start ) ) ⟧, stop ↦ ⟦ v ↦ ∅, again ↦ ⟦ Δ ⤍ 01- ⟧ ⟧, t ↦ Φ.f( x ↦ Φ.start ) ⟧"
      it "cuts a recursion that stops after one round without the flag" $
        withStdin once $
          testCLIFailed
            ["dataize", "--locator=Q.t", "--acyclic=plausible"]
            ["[ERROR]: Reduction entered a formation it is already inside:"]

      it "dataizes a recursion that stops after one round with --max-loops=1" $
        withStdin once $
          testCLISucceeded ["dataize", "--locator=Q.t", "--acyclic=plausible", "--max-loops=1"] ["01-"]

      it "cuts a recursion that never stops with --max-loops=1" $
        withStdin "⟦ cyc ↦ ⟦ x ↦ ∅, φ ↦ Φ.cyc( ξ.x ) ⟧, t ↦ Φ.cyc( ⟦⟧ ) ⟧" $
          testCLIFailed
            ["dataize", "--locator=Q.t", "--acyclic=proven", "--max-loops=1", "--max-steps=4000"]
            ["[ERROR]: Reduction entered a formation it is already inside:"]

      it "refuses a negative count" $
        withStdin once $
          testCLIFailed ["dataize", "--locator=Q.t", "--acyclic=plausible", "--max-loops=-1"] ["--max-loops must be >= 0"]

      it "refuses the flag without --acyclic" $
        withStdin once $
          testCLIFailed ["dataize", "--locator=Q.t", "--max-loops=1"] ["The option --max-loops requires --acyclic, since only --acyclic cuts a loop"]

    it "dataizes with --sequence" $
      withStdin "[[ @ -> [[ x -> [[ D> 01-, y -> ? ]](y -> [[ ]]) ]].x ]]" $
        testCLISucceeded
          ["dataize", "--sequence", "--output=latex", "--flat", "--sweet"]
          [ intercalate
              "\n"
              [ "\\begin{phiquation}"
              , "[[ D> |01-|, |y| -> ? ]] ( |y| -> [[]] ) : |x| . |x| : @ \\phiDataize[\\nameref{r:box}]"
              , "  \\phiDataize [[ D> |01-|, |y| -> ? ]] ( |y| -> [[]] ) : |x| . |x| : @ . @ \\phiNormalize[\\nameref{r:copy}]"
              , "  \\phiNormalize [[ D> |01-|, |y| -> [[]] ]] : |x| . |x| : @ . @ \\phiNormalize[\\nameref{r:dot}]"
              , "  \\phiNormalize [[ D> |01-|, |y| -> [[]] ]] ( \\phiTerminal{\\rho} -> [[ D> |01-|, |y| -> [[]] ]] : |x| ) : @ . @ \\phiNormalize[\\nameref{r:skip}]"
              , "  \\phiNormalize [[ D> |01-|, |y| -> [[]] ]] : @ . @ \\phiNormalize[\\nameref{r:dot}]"
              , "  \\phiNormalize [[ D> |01-|, |y| -> [[]] ]] ( \\phiTerminal{\\rho} -> Q ) \\phiNormalize[\\nameref{r:skip}]"
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
          ["\\phinoMeet{dataization:1}{ [[ @ -> |c| . |plus| ( |32.0| ), |c| -> |25.0| ]] } \\phiDataize[\\nameref{r:box}]"]

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
          stuck = "⟦ t ↦ ⟦ λ ⤍ L_weit, anfang ↦ ξ.schluss, mitte ↦ ξ.anfang, schluss ↦ ξ.mitte, rand ↦ ξ.schluss ⟧ ⟧"
      it "folds a long formation in the text protocol" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin stuck $
            testCLISucceeded ["dataize", "--locator=Q.t", "--partial", "--protocol=" ++ path, "--abridged", "--sweet", "--hide-rho", "--quiet"] []
          records <- readProtocol path
          lines records `shouldContain` ["  unanswered L_weit  # 𝔻(⟦ λ ⤍ L_weit, +4 ⟧)"]
      it "names a long formation in the XML protocol by its path rather than folding it" $
        withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
          hClose stream
          withStdin stuck $
            testCLISucceeded ["dataize", "--locator=Q.t", "--partial", "--protocol=" ++ path, "--abridged", "--sweet", "--hide-rho", "--quiet"] []
          records <- readProtocol path
          lines records `shouldContain` ["  <unanswered λ=\"L_weit\" by=\"dataize\">Φ.t</unanswered>"]
      forM_
        [ ("textXXXXXX.txt", "      𝛿·1·1 := 01-02-..(8b)..-0B-0C")
        , ("XMLXXXXXX.xml", "      <dataized meta=\"𝛿·1·1\">01-02-..(8b)..-0B-0C</dataized>")
        ]
        ( \(template, line) ->
            it ("cuts a long datum a firing came down to under --abridged-data, as " ++ line) $
              withTempFile template $ \(path, stream) -> do
                hClose stream
                withLambdasOf (T.pack "- λ: L_outer\n  dataize:\n    𝛿1: ξ.arg\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n") $ \outer ->
                  withStdin "⟦ x ↦ ⟦ arg ↦ ⟦ Δ ⤍ 01-02-03-04-05-06-07-08-09-0A-0B-0C ⟧, λ ⤍ L_outer ⟧ ⟧" $
                    testCLISucceeded ["dataize", "--symbolic=" ++ outer, "--locator=Q.x", "--partial", "--protocol=" ++ path, "--abridged", "--abridged-data", "--sweet", "--hide-rho", "--quiet"] []
                records <- readProtocol path
                lines records `shouldContain` [line]
        )
      forM_
        [ ("textXXXXXX.txt", "      𝛿·1·1 := 30-31-32-33-34-35-36-37-38-39-41-42-43-44-45-46")
        , ("XMLXXXXXX.xml", "      <dataized meta=\"𝛿·1·1\">30-31-32-33-34-35-36-37-38-39-41-42-43-44-45-46</dataized>")
        ]
        ( \(template, line) ->
            it ("keeps a long datum a firing came down to whole without --abridged-data, as " ++ line) $
              withTempFile template $ \(path, stream) -> do
                hClose stream
                withLambdasOf (T.pack "- λ: L_hex\n  dataize:\n    𝛿1: ξ.arg\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n") $ \hex ->
                  withStdin "⟦ x ↦ ⟦ arg ↦ ⟦ Δ ⤍ 30-31-32-33-34-35-36-37-38-39-41-42-43-44-45-46 ⟧, λ ⤍ L_hex ⟧ ⟧" $
                    testCLISucceeded ["dataize", "--symbolic=" ++ hex, "--locator=Q.x", "--partial", "--protocol=" ++ path, "--abridged", "--sweet", "--hide-rho", "--quiet"] []
                records <- readProtocol path
                lines records `shouldContain` [line]
        )
      it "folds a long formation under the width given as the value" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin stuck $
            testCLISucceeded ["dataize", "--locator=Q.t", "--partial", "--protocol=" ++ path, "--abridged=64", "--sweet", "--hide-rho", "--quiet"] []
          records <- readProtocol path
          lines records `shouldContain` ["  unanswered L_weit  # 𝔻(⟦ λ ⤍ L_weit, +4 ⟧)"]
      it "keeps a formation whole under a width it fits in" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin stuck $
            testCLISucceeded ["dataize", "--locator=Q.t", "--partial", "--protocol=" ++ path, "--abridged=200", "--sweet", "--hide-rho", "--quiet"] []
          records <- readProtocol path
          lines records `shouldContain` ["  unanswered L_weit  # 𝔻(⟦ λ ⤍ L_weit, anfang ↦ schluss, mitte ↦ anfang, schluss ↦ mitte, rand ↦ schluss ⟧)"]
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
      it "refuses to cut the data in dataize without --abridged" $
        withStdin wide $
          testCLIFailed ["dataize", "--locator=Q.t", "--protocol=daten.txt", "--abridged-data"] ["The option --abridged-data requires --abridged"]
      it "refuses to cut the data in morph without --abridged" $
        withStdin wide $
          testCLIFailed ["morph", "--locator=Q.t", "--protocol=daten.xml", "--abridged-data"] ["The option --abridged-data requires --abridged"]

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
          records `shouldBe` "𝔻(Φ):\n  𝛿·0·1 := 01-\n"

      it "closes the protocol with its msec on the third line from the end" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin sum' $
            testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet"] []
          records <- readUtf8 path
          (lines records !! (length (lines records) - 3)) `shouldSatisfy` isPrefixOf "msec "

      it "closes the protocol with the firings it counted" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin sum' $
            testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet"] []
          records <- readUtf8 path
          lines records `shouldContain` ["firings 1"]

      it "closes the protocol with its fps on the last line" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin sum' $
            testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet"] []
          records <- readUtf8 path
          last (lines records) `shouldSatisfy` isPrefixOf "fps "

      it "closes the protocol with zero firings when the run fires nothing" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin "[[ D> 01- ]]" $
            testCLISucceeded ["dataize", "--protocol=" ++ path, "--quiet"] []
          records <- readUtf8 path
          lines records `shouldContain` ["firings 0"]

      it "writes one line per operand and one per answer of a firing" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin sum' $
            testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
          records <- readProtocol path
          lines records
            `shouldBe` [ "𝔻(Φ):"
                       , "  boxed 𝑛·0·1 := Φ.φ"
                       , "  𝕄(𝑛·0·1):"
                       , "    𝑛·0·2 := 5.0"
                       , "    𝑛·0·3 := 𝑛·0·2.plus( x ↦ 6.0 )"
                       , "  𝔼(L_number_plus):  # 𝔻(𝑛·0·1)"
                       , "    𝔻(ξ.ρ):"
                       , "      𝕄(ξ.ρ):"
                       , "        𝑛·1·1 := 5.0"
                       , "      boxed 𝑛·1·2 := 𝑛·1·1.φ"
                       , "      𝕄(𝑛·1·2):"
                       , "        𝑛·1·3 := Φ.bytes( φ ↦ 40-14-00-00-00-00-00-00:Δ )"
                       , "      boxed 𝑛·1·4 := 𝑛·1·3.φ"
                       , "      𝛿·1·1 := 40-14-00-00-00-00-00-00  # 5.0"
                       , "    𝛿1·1 := 𝛿·1·1  # 𝔻(ξ.ρ)"
                       , "    𝔻(ξ.x):"
                       , "      𝕄(ξ.x):"
                       , "        𝑛·1·5 := 6.0"
                       , "      boxed 𝑛·1·6 := 𝑛·1·5.φ"
                       , "      𝕄(𝑛·1·6):"
                       , "        𝑛·1·7 := Φ.bytes( φ ↦ 40-18-00-00-00-00-00-00:Δ )"
                       , "      boxed 𝑛·1·8 := 𝑛·1·7.φ"
                       , "      𝛿·1·2 := 40-18-00-00-00-00-00-00  # 6.0"
                       , "    𝛿2·1 := 𝛿·1·2  # 𝔻(ξ.x)"
                       , "    𝑛·1·9 := Φ.number( φ ↦ 𝜎1:λ )  # 𝑛"
                       , "    𝕄(𝑛·1·9):"
                       , "      𝑛·1·10 := Φ.number( φ ↦ 𝜎1:λ )"
                       , "    𝑛·1·11 := 𝑛·1·10  # 𝕄(𝑛·1·9)"
                       , "  boxed 𝑛·0·4 := 𝑛·1·10.φ"
                       , "  𝛿·0·1 := 𝔻(𝜎1:λ)"
                       ]

      it "writes what every morphing a dataization ran came to" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withLambdasOf (T.pack "- λ: L_make\n  𝑛: ⟦ φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ 𝜎 ⟧ ) ⟧\n") $ \makes ->
            withStdin "⟦ bytes ↦ ⟦ φ ↦ ∅ ⟧, make ↦ ⟦ λ ⤍ L_make ⟧, foo ↦ ⟦ φ ↦ Φ.make ⟧, e ↦ Φ.foo ⟧" $
              testCLISucceeded ["dataize", "--symbolic=" ++ makes, "--locator=Q.e", "--partial", "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
          records <- readProtocol path
          lines records
            `shouldBe` [ "𝔻(Φ.e):"
                       , "  𝕄(Φ.e):"
                       , "    𝑛·0·1 := Φ.make:φ"
                       , "  boxed 𝑛·0·2 := 𝑛·0·1.φ"
                       , "  𝕄(𝑛·0·2):"
                       , "    𝑛·0·3 := L_make:λ"
                       , "  𝔼(L_make):  # 𝔻(𝑛·0·2)"
                       , "    𝑛·1·1 := Φ.bytes( φ ↦ 𝜎1:λ ):φ  # 𝑛"
                       , "    𝑛·1·2 := Φ.bytes( φ ↦ 𝜎1:λ ):φ  # 𝕄(𝑛·1·1)"
                       , "  boxed 𝑛·0·4 := 𝑛·1·2.φ"
                       , "  𝕄(𝑛·0·4):"
                       , "    𝑛·0·5 := Φ.bytes( φ ↦ 𝜎1:λ )"
                       , "  boxed 𝑛·0·6 := 𝑛·0·5.φ"
                       , "  𝛿·0·1 := 𝔻(𝜎1:λ)"
                       ]

      it "numbers the firings of one entry apart and names the symbol between them" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin chained $
            testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
          records <- readProtocol path
          lines records
            `shouldBe` [ "𝔻(Φ):"
                       , "  boxed 𝑛·0·1 := Φ.φ"
                       , "  𝕄(𝑛·0·1):"
                       , "    𝑛·0·2 := 5.0"
                       , "    𝑛·0·3 := 𝑛·0·2.plus( x ↦ 6.0 )"
                       , "    𝔼(L_number_plus):"
                       , "      𝔻(ξ.ρ):"
                       , "        𝕄(ξ.ρ):"
                       , "          𝑛·1·1 := 5.0"
                       , "        boxed 𝑛·1·2 := 𝑛·1·1.φ"
                       , "        𝕄(𝑛·1·2):"
                       , "          𝑛·1·3 := Φ.bytes( φ ↦ 40-14-00-00-00-00-00-00:Δ )"
                       , "        boxed 𝑛·1·4 := 𝑛·1·3.φ"
                       , "        𝛿·1·1 := 40-14-00-00-00-00-00-00  # 5.0"
                       , "      𝛿1·1 := 𝛿·1·1  # 𝔻(ξ.ρ)"
                       , "      𝔻(ξ.x):"
                       , "        𝕄(ξ.x):"
                       , "          𝑛·1·5 := 6.0"
                       , "        boxed 𝑛·1·6 := 𝑛·1·5.φ"
                       , "        𝕄(𝑛·1·6):"
                       , "          𝑛·1·7 := Φ.bytes( φ ↦ 40-18-00-00-00-00-00-00:Δ )"
                       , "        boxed 𝑛·1·8 := 𝑛·1·7.φ"
                       , "        𝛿·1·2 := 40-18-00-00-00-00-00-00  # 6.0"
                       , "      𝛿2·1 := 𝛿·1·2  # 𝔻(ξ.x)"
                       , "      𝑛·1·9 := Φ.number( φ ↦ 𝜎1:λ )  # 𝑛"
                       , "      𝕄(𝑛·1·9):"
                       , "        𝑛·1·10 := Φ.number( φ ↦ 𝜎1:λ )"
                       , "      𝑛·1·11 := 𝑛·1·10  # 𝕄(𝑛·1·9)"
                       , "    𝑛·0·4 := 𝑛·1·10.plus( x ↦ 7.0 )"
                       , "  𝔼(L_number_plus):  # 𝔻(𝑛·0·1)"
                       , "    𝔻(ξ.ρ):"
                       , "      𝕄(ξ.ρ):"
                       , "        𝑛·2·1 := Φ.number( φ ↦ 𝜎1:λ )"
                       , "      boxed 𝑛·2·2 := 𝑛·2·1.φ"
                       , "      𝛿·2·1 := 𝔻(𝜎1:λ)"
                       , "    𝛿1·2 := 𝛿·2·1  # 𝔻(ξ.ρ)"
                       , "    𝔻(ξ.x):"
                       , "      𝕄(ξ.x):"
                       , "        𝑛·2·3 := 7.0"
                       , "      boxed 𝑛·2·4 := 𝑛·2·3.φ"
                       , "      𝕄(𝑛·2·4):"
                       , "        𝑛·2·5 := Φ.bytes( φ ↦ 40-1C-00-00-00-00-00-00:Δ )"
                       , "      boxed 𝑛·2·6 := 𝑛·2·5.φ"
                       , "      𝛿·2·2 := 40-1C-00-00-00-00-00-00  # 7.0"
                       , "    𝛿2·2 := 𝛿·2·2  # 𝔻(ξ.x)"
                       , "    𝑛·2·7 := Φ.number( φ ↦ 𝜎2:λ )  # 𝑛"
                       , "    𝕄(𝑛·2·7):"
                       , "      𝑛·2·8 := Φ.number( φ ↦ 𝜎2:λ )"
                       , "    𝑛·2·9 := 𝑛·2·8  # 𝕄(𝑛·2·7)"
                       , "  boxed 𝑛·0·5 := 𝑛·2·8.φ"
                       , "  𝛿·0·1 := 𝔻(𝜎2:λ)"
                       ]

      it "numbers the firings of different entries apart" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin mixed $
            testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
          records <- readProtocol path
          lines records
            `shouldBe` [ "𝔻(Φ):"
                       , "  boxed 𝑛·0·1 := Φ.φ"
                       , "  𝕄(𝑛·0·1):"
                       , "    𝑛·0·2 := 5.0"
                       , "    𝑛·0·3 := 𝑛·0·2.plus( x ↦ 6.0 )"
                       , "    𝔼(L_number_plus):"
                       , "      𝔻(ξ.ρ):"
                       , "        𝕄(ξ.ρ):"
                       , "          𝑛·1·1 := 5.0"
                       , "        boxed 𝑛·1·2 := 𝑛·1·1.φ"
                       , "        𝕄(𝑛·1·2):"
                       , "          𝑛·1·3 := Φ.bytes( φ ↦ 40-14-00-00-00-00-00-00:Δ )"
                       , "        boxed 𝑛·1·4 := 𝑛·1·3.φ"
                       , "        𝛿·1·1 := 40-14-00-00-00-00-00-00  # 5.0"
                       , "      𝛿1·1 := 𝛿·1·1  # 𝔻(ξ.ρ)"
                       , "      𝔻(ξ.x):"
                       , "        𝕄(ξ.x):"
                       , "          𝑛·1·5 := 6.0"
                       , "        boxed 𝑛·1·6 := 𝑛·1·5.φ"
                       , "        𝕄(𝑛·1·6):"
                       , "          𝑛·1·7 := Φ.bytes( φ ↦ 40-18-00-00-00-00-00-00:Δ )"
                       , "        boxed 𝑛·1·8 := 𝑛·1·7.φ"
                       , "        𝛿·1·2 := 40-18-00-00-00-00-00-00  # 6.0"
                       , "      𝛿2·1 := 𝛿·1·2  # 𝔻(ξ.x)"
                       , "      𝑛·1·9 := Φ.number( φ ↦ 𝜎1:λ )  # 𝑛"
                       , "      𝕄(𝑛·1·9):"
                       , "        𝑛·1·10 := Φ.number( φ ↦ 𝜎1:λ )"
                       , "      𝑛·1·11 := 𝑛·1·10  # 𝕄(𝑛·1·9)"
                       , "    𝑛·0·4 := 𝑛·1·10.times( x ↦ 7.0 )"
                       , "  𝔼(L_number_times):  # 𝔻(𝑛·0·1)"
                       , "    𝔻(ξ.ρ):"
                       , "      𝕄(ξ.ρ):"
                       , "        𝑛·2·1 := Φ.number( φ ↦ 𝜎1:λ )"
                       , "      boxed 𝑛·2·2 := 𝑛·2·1.φ"
                       , "      𝛿·2·1 := 𝔻(𝜎1:λ)"
                       , "    𝛿1·2 := 𝛿·2·1  # 𝔻(ξ.ρ)"
                       , "    𝔻(ξ.x):"
                       , "      𝕄(ξ.x):"
                       , "        𝑛·2·3 := 7.0"
                       , "      boxed 𝑛·2·4 := 𝑛·2·3.φ"
                       , "      𝕄(𝑛·2·4):"
                       , "        𝑛·2·5 := Φ.bytes( φ ↦ 40-1C-00-00-00-00-00-00:Δ )"
                       , "      boxed 𝑛·2·6 := 𝑛·2·5.φ"
                       , "      𝛿·2·2 := 40-1C-00-00-00-00-00-00  # 7.0"
                       , "    𝛿2·2 := 𝛿·2·2  # 𝔻(ξ.x)"
                       , "    𝑛·2·7 := Φ.number( φ ↦ 𝜎2:λ )  # 𝑛"
                       , "    𝕄(𝑛·2·7):"
                       , "      𝑛·2·8 := Φ.number( φ ↦ 𝜎2:λ )"
                       , "    𝑛·2·9 := 𝑛·2·8  # 𝕄(𝑛·2·7)"
                       , "  boxed 𝑛·0·5 := 𝑛·2·8.φ"
                       , "  𝛿·0·1 := 𝔻(𝜎2:λ)"
                       ]

      it "nests the firing an operand of another firing brought down" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin nested $
            testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
          records <- readProtocol path
          lines records
            `shouldBe` [ "𝔻(Φ):"
                       , "  boxed 𝑛·0·1 := Φ.φ"
                       , "  𝕄(𝑛·0·1):"
                       , "    𝑛·0·2 := 5.0"
                       , "    𝑛·0·3 := 𝑛·0·2.plus( x ↦ 6.0.plus( 7.0 ) )"
                       , "  𝔼(L_number_plus):  # 𝔻(𝑛·0·1)"
                       , "    𝔻(ξ.ρ):"
                       , "      𝕄(ξ.ρ):"
                       , "        𝑛·1·1 := 5.0"
                       , "      boxed 𝑛·1·2 := 𝑛·1·1.φ"
                       , "      𝕄(𝑛·1·2):"
                       , "        𝑛·1·3 := Φ.bytes( φ ↦ 40-14-00-00-00-00-00-00:Δ )"
                       , "      boxed 𝑛·1·4 := 𝑛·1·3.φ"
                       , "      𝛿·1·1 := 40-14-00-00-00-00-00-00  # 5.0"
                       , "    𝛿1·1 := 𝛿·1·1  # 𝔻(ξ.ρ)"
                       , "    𝔻(ξ.x):"
                       , "      𝕄(ξ.x):"
                       , "        𝑛·1·5 := 6.0"
                       , "        𝑛·1·6 := 𝑛·1·5.plus( x ↦ 7.0 )"
                       , "      𝔼(L_number_plus):"
                       , "        𝔻(ξ.ρ):"
                       , "          𝕄(ξ.ρ):"
                       , "            𝑛·2·1 := 6.0"
                       , "          boxed 𝑛·2·2 := 𝑛·2·1.φ"
                       , "          𝕄(𝑛·2·2):"
                       , "            𝑛·2·3 := Φ.bytes( φ ↦ 40-18-00-00-00-00-00-00:Δ )"
                       , "          boxed 𝑛·2·4 := 𝑛·2·3.φ"
                       , "          𝛿·2·1 := 40-18-00-00-00-00-00-00  # 6.0"
                       , "        𝛿1·2 := 𝛿·2·1  # 𝔻(ξ.ρ)"
                       , "        𝔻(ξ.x):"
                       , "          𝕄(ξ.x):"
                       , "            𝑛·2·5 := 7.0"
                       , "          boxed 𝑛·2·6 := 𝑛·2·5.φ"
                       , "          𝕄(𝑛·2·6):"
                       , "            𝑛·2·7 := Φ.bytes( φ ↦ 40-1C-00-00-00-00-00-00:Δ )"
                       , "          boxed 𝑛·2·8 := 𝑛·2·7.φ"
                       , "          𝛿·2·2 := 40-1C-00-00-00-00-00-00  # 7.0"
                       , "        𝛿2·2 := 𝛿·2·2  # 𝔻(ξ.x)"
                       , "        𝑛·2·9 := Φ.number( φ ↦ 𝜎1:λ )  # 𝑛"
                       , "        𝕄(𝑛·2·9):"
                       , "          𝑛·2·10 := Φ.number( φ ↦ 𝜎1:λ )"
                       , "        𝑛·2·11 := 𝑛·2·10  # 𝕄(𝑛·2·9)"
                       , "      boxed 𝑛·1·7 := 𝑛·2·10.φ"
                       , "      𝛿·1·2 := 𝔻(𝜎1:λ)"
                       , "    𝛿2·1 := 𝛿·1·2  # 𝔻(ξ.x)"
                       , "    𝑛·1·8 := Φ.number( φ ↦ 𝜎2:λ )  # 𝑛"
                       , "    𝕄(𝑛·1·8):"
                       , "      𝑛·1·9 := Φ.number( φ ↦ 𝜎2:λ )"
                       , "    𝑛·1·10 := 𝑛·1·9  # 𝕄(𝑛·1·8)"
                       , "  boxed 𝑛·0·4 := 𝑛·1·9.φ"
                       , "  𝛿·0·1 := 𝔻(𝜎2:λ)"
                       ]

      it "writes a stuck firing to the XML protocol" $
        withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
          hClose stream
          withLambdasOf (T.pack "- λ: L_outer\n  dataize:\n    𝛿1: ξ.arg\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n") $ \outer ->
            withStdin "⟦ x ↦ ⟦ arg ↦ ⟦ λ ⤍ L_absent ⟧, λ ⤍ L_outer ⟧.φ ⟧" $
              testCLISucceeded ["morph", "--symbolic=" ++ outer, "--locator=Q.x", "--partial", "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
          records <- readProtocol path
          lines records `shouldContain` ["    <stuck λ=\"L_absent\"/>"]

      it "writes a starved step budget to the XML protocol" $
        withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
          hClose stream
          withLambdasOf (T.pack "- λ: L_outer\n  dataize:\n    𝛿1: ξ.arg\n  𝑛: ⟦ λ ⤍ 𝜎 ⟧\n") $ \outer ->
            withStdin "⟦ x ↦ ⟦ arg ↦ ⟦ φ ↦ ⟦ φ ↦ ⟦ Δ ⤍ 07- ⟧ ⟧ ⟧, λ ⤍ L_outer ⟧ ⟧" $
              testCLISucceeded ["dataize", "--symbolic=" ++ outer, "--locator=Q.x", "--partial", "--max-steps=3", "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho", "--flat"] []
          records <- readProtocol path
          lines records `shouldContain` ["      <starved limit=\"3\" by=\"dataize\" at=\"𝑛·1·2\"/>"]

      forM_
        [("XMLXXXXXX.xml", "<spent limit=\"5\" by="), ("textXXXXXX.txt", "  spent 5")]
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
            `shouldBe` [ "𝔻(Φ):"
                       , "  boxed 𝑛·0·1 := Φ.φ"
                       , "  𝕄(𝑛·0·1):"
                       , "    𝑛·0·2 := 5.0"
                       , "    𝑛·0·3 := 𝑛·0·2.plus( x ↦ 6.0 )"
                       , "    𝔼(L_number_plus):"
                       , "      𝔻(ξ.ρ):"
                       , "        𝕄(ξ.ρ):"
                       , "          𝑛·1·1 := 5.0"
                       , "        boxed 𝑛·1·2 := 𝑛·1·1.φ"
                       , "        𝕄(𝑛·1·2):"
                       , "          𝑛·1·3 := Φ.bytes( φ ↦ 40-14-00-00-00-00-00-00:Δ )"
                       , "        boxed 𝑛·1·4 := 𝑛·1·3.φ"
                       , "        𝛿·1·1 := 40-14-00-00-00-00-00-00  # 5.0"
                       , "      𝛿1·1 := 𝛿·1·1  # 𝔻(ξ.ρ)"
                       , "      𝔻(ξ.x):"
                       , "        𝕄(ξ.x):"
                       , "          𝑛·1·5 := 6.0"
                       , "        boxed 𝑛·1·6 := 𝑛·1·5.φ"
                       , "        𝕄(𝑛·1·6):"
                       , "          𝑛·1·7 := Φ.bytes( φ ↦ 40-18-00-00-00-00-00-00:Δ )"
                       , "        boxed 𝑛·1·8 := 𝑛·1·7.φ"
                       , "        𝛿·1·2 := 40-18-00-00-00-00-00-00  # 6.0"
                       , "      𝛿2·1 := 𝛿·1·2  # 𝔻(ξ.x)"
                       , "      𝑛·1·9 := Φ.number( φ ↦ 𝜎1:λ )  # 𝑛"
                       , "      𝕄(𝑛·1·9):"
                       , "        𝑛·1·10 := Φ.number( φ ↦ 𝜎1:λ )"
                       , "      𝑛·1·11 := 𝑛·1·10  # 𝕄(𝑛·1·9)"
                       , "    𝑛·0·4 := L_number_nope:λ"
                       , "  unanswered L_number_nope  # 𝔻(L_number_nope:λ)"
                       ]

      it "truncates the lines left over from the previous run" $
        withTempFileContent "protocolXXXXXX.txt" "𝔼(L_number_gt)\n" $ \path -> do
          withStdin "[[ D> 01- ]]" $
            testCLISucceeded ["dataize", "--protocol=" ++ path, "--quiet"] []
          records <- readProtocol path
          records `shouldBe` "𝔻(Φ):\n  𝛿·0·1 := 01-\n"

      it "writes the lines in 𝜑 even with --output=xmir" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin sum' $
            testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--output=xmir", "--quiet", "--sweet", "--hide-rho"] []
          records <- readProtocol path
          records `shouldEndWith` "    𝕄(𝑛·1·9):\n      𝑛·1·10 := Φ.number( φ ↦ 𝜎1:λ )\n    𝑛·1·11 := 𝑛·1·10  # 𝕄(𝑛·1·9)\n  boxed 𝑛·0·4 := 𝑛·1·10.φ\n  𝛿·0·1 := 𝔻(𝜎1:λ)\n"

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
                         , "  <boxed meta=\"𝑛·0·1\">Φ.φ</boxed>"
                         , "  <morph at=\"𝑛·0·1\">"
                         , "    <applied meta=\"𝑛·0·2\" of=\"Φ.bytes\"><datum attr=\"φ\">40-14-00-00-00-00-00-00</datum></applied>"
                         , "    <applied meta=\"𝑛·0·3\" by=\"morph\" at=\"𝑛·0·1\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·2</meta></applied>"
                         , "    <applied meta=\"𝑛·0·4\" of=\"Φ.bytes\"><datum attr=\"φ\">40-18-00-00-00-00-00-00</datum></applied>"
                         , "    <applied meta=\"𝑛·0·5\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·4</meta></applied>"
                         , "    <applied meta=\"𝑛·0·6\" by=\"morph\" at=\"𝑛·0·1\" of=\"𝑛·0·3.plus\"><meta attr=\"x\">𝑛·0·5</meta></applied>"
                         , "    <morphed meta=\"𝑛·0·7\">𝑛·0·6</morphed>"
                         , "  </morph>"
                         , "  <evaluate λ=\"L_number_plus\" by=\"dataize\" at=\"𝑛·0·1\">"
                         , "    <dataize at=\"ξ.ρ\">"
                         , "      <morph at=\"ξ.ρ\">"
                         , "        <applied meta=\"𝑛·1·1\" by=\"morph\" at=\"ξ.ρ\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·2</meta></applied>"
                         , "        <morphed meta=\"𝑛·1·2\">𝑛·1·1</morphed>"
                         , "      </morph>"
                         , "      <boxed meta=\"𝑛·1·3\">𝑛·1·1.φ</boxed>"
                         , "      <morph at=\"𝑛·1·3\">"
                         , "        <applied meta=\"𝑛·1·4\" by=\"morph\" at=\"𝑛·1·3\" of=\"Φ.bytes\"><datum attr=\"φ\">40-14-00-00-00-00-00-00</datum></applied>"
                         , "        <morphed meta=\"𝑛·1·5\">𝑛·1·4</morphed>"
                         , "      </morph>"
                         , "      <boxed meta=\"𝑛·1·6\">𝑛·1·4.φ</boxed>"
                         , "      <dataized meta=\"𝛿·1·1\" number=\"5.0\">40-14-00-00-00-00-00-00</dataized>"
                         , "    </dataize>"
                         , "    <bound meta=\"𝛿1·1\">𝛿·1·1</bound>"
                         , "    <dataize at=\"ξ.x\">"
                         , "      <morph at=\"ξ.x\">"
                         , "        <applied meta=\"𝑛·1·7\" by=\"morph\" at=\"ξ.x\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·4</meta></applied>"
                         , "        <morphed meta=\"𝑛·1·8\">𝑛·1·7</morphed>"
                         , "      </morph>"
                         , "      <boxed meta=\"𝑛·1·9\">𝑛·1·7.φ</boxed>"
                         , "      <morph at=\"𝑛·1·9\">"
                         , "        <applied meta=\"𝑛·1·10\" by=\"morph\" at=\"𝑛·1·9\" of=\"Φ.bytes\"><datum attr=\"φ\">40-18-00-00-00-00-00-00</datum></applied>"
                         , "        <morphed meta=\"𝑛·1·11\">𝑛·1·10</morphed>"
                         , "      </morph>"
                         , "      <boxed meta=\"𝑛·1·12\">𝑛·1·10.φ</boxed>"
                         , "      <dataized meta=\"𝛿·1·2\" number=\"6.0\">40-18-00-00-00-00-00-00</dataized>"
                         , "    </dataize>"
                         , "    <bound meta=\"𝛿2·1\">𝛿·1·2</bound>"
                         , "    <minted symbol=\"𝜎1\">40-14-00-00-00-00-00-00 40-18-00-00-00-00-00-00</minted>"
                         , "    <applied meta=\"𝑛·1·13\" of=\"Φ.number\"><symbol attr=\"φ\">𝜎1</symbol></applied>"
                         , "    <built meta=\"𝑛·1·14\">𝑛·1·13</built>"
                         , "    <morph at=\"𝑛·1·14\">"
                         , "      <applied meta=\"𝑛·1·15\" by=\"morph\" at=\"𝑛·1·14\" of=\"Φ.number\"><symbol attr=\"φ\">𝜎1</symbol></applied>"
                         , "      <morphed meta=\"𝑛·1·16\">𝑛·1·15</morphed>"
                         , "    </morph>"
                         , "    <answered meta=\"𝑛·1·17\">𝑛·1·15</answered>"
                         , "  </evaluate>"
                         , "  <boxed meta=\"𝑛·0·8\">𝑛·1·15.φ</boxed>"
                         , "  <manufactured meta=\"𝛿·0·1\">𝜎1</manufactured>"
                         , "</dataize>"
                         ]

        it "nests the judgment right inside the '<protocol>' root" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withStdin "[[ D> 01- ]]" $
              testCLISucceeded ["dataize", "--protocol=" ++ path, "--quiet"] []
            records <- readUtf8 path
            take 4 (lines records)
              `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                         , "<protocol>"
                         , "  <dataize at=\"Φ\">"
                         , "    <dataized meta=\"𝛿·0·1\">01-</dataized>"
                         ]

        it "closes the '<protocol>' root with its msec" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withStdin "[[ D> 01- ]]" $
              testCLISucceeded ["dataize", "--protocol=" ++ path, "--quiet"] []
            records <- readUtf8 path
            (lines records !! 5) `shouldSatisfy` isPrefixOf "  <msec>"

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
            drop 7 (lines records) `shouldBe` ["  <fps>0</fps>", "</protocol>"]

        it "nests what a φ body dataizes inside the element of the box rule that entered it" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withStdin sum' $
              testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
            records <- readProtocol path
            filter (\line -> not (any (`isInfixOf` line) ["<applied ", "<morph ", "<morphed ", "</morph>"])) (lines records)
              `shouldContain` [ "    <dataize at=\"ξ.ρ\">"
                              , "      <boxed meta=\"𝑛·1·3\">𝑛·1·1.φ</boxed>"
                              , "      <boxed meta=\"𝑛·1·6\">𝑛·1·4.φ</boxed>"
                              , "      <dataized meta=\"𝛿·1·1\" number=\"5.0\">40-14-00-00-00-00-00-00</dataized>"
                              , "    </dataize>"
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
                         , "  <dataized meta=\"𝛿·0·1\">01-</dataized>"
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
                         , "  <boxed meta=\"𝑛·0·1\">Φ.φ</boxed>"
                         , "  <morph at=\"𝑛·0·1\">"
                         , "    <applied meta=\"𝑛·0·2\" of=\"Φ.bytes\"><datum attr=\"φ\">40-14-00-00-00-00-00-00</datum></applied>"
                         , "    <applied meta=\"𝑛·0·3\" by=\"morph\" at=\"𝑛·0·1\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·2</meta></applied>"
                         , "    <applied meta=\"𝑛·0·4\" of=\"Φ.bytes\"><datum attr=\"φ\">40-18-00-00-00-00-00-00</datum></applied>"
                         , "    <applied meta=\"𝑛·0·5\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·4</meta></applied>"
                         , "    <applied meta=\"𝑛·0·6\" by=\"morph\" at=\"𝑛·0·1\" of=\"𝑛·0·3.plus\"><meta attr=\"x\">𝑛·0·5</meta></applied>"
                         , "    <evaluate λ=\"L_number_plus\" by=\"morph\" at=\"𝑛·0·1\">"
                         , "      <dataize at=\"ξ.ρ\">"
                         , "        <morph at=\"ξ.ρ\">"
                         , "          <applied meta=\"𝑛·1·1\" by=\"morph\" at=\"ξ.ρ\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·2</meta></applied>"
                         , "          <morphed meta=\"𝑛·1·2\">𝑛·1·1</morphed>"
                         , "        </morph>"
                         , "        <boxed meta=\"𝑛·1·3\">𝑛·1·1.φ</boxed>"
                         , "        <morph at=\"𝑛·1·3\">"
                         , "          <applied meta=\"𝑛·1·4\" by=\"morph\" at=\"𝑛·1·3\" of=\"Φ.bytes\"><datum attr=\"φ\">40-14-00-00-00-00-00-00</datum></applied>"
                         , "          <morphed meta=\"𝑛·1·5\">𝑛·1·4</morphed>"
                         , "        </morph>"
                         , "        <boxed meta=\"𝑛·1·6\">𝑛·1·4.φ</boxed>"
                         , "        <dataized meta=\"𝛿·1·1\" number=\"5.0\">40-14-00-00-00-00-00-00</dataized>"
                         , "      </dataize>"
                         , "      <bound meta=\"𝛿1·1\">𝛿·1·1</bound>"
                         , "      <dataize at=\"ξ.x\">"
                         , "        <morph at=\"ξ.x\">"
                         , "          <applied meta=\"𝑛·1·7\" by=\"morph\" at=\"ξ.x\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·4</meta></applied>"
                         , "          <morphed meta=\"𝑛·1·8\">𝑛·1·7</morphed>"
                         , "        </morph>"
                         , "        <boxed meta=\"𝑛·1·9\">𝑛·1·7.φ</boxed>"
                         , "        <morph at=\"𝑛·1·9\">"
                         , "          <applied meta=\"𝑛·1·10\" by=\"morph\" at=\"𝑛·1·9\" of=\"Φ.bytes\"><datum attr=\"φ\">40-18-00-00-00-00-00-00</datum></applied>"
                         , "          <morphed meta=\"𝑛·1·11\">𝑛·1·10</morphed>"
                         , "        </morph>"
                         , "        <boxed meta=\"𝑛·1·12\">𝑛·1·10.φ</boxed>"
                         , "        <dataized meta=\"𝛿·1·2\" number=\"6.0\">40-18-00-00-00-00-00-00</dataized>"
                         , "      </dataize>"
                         , "      <bound meta=\"𝛿2·1\">𝛿·1·2</bound>"
                         , "      <minted symbol=\"𝜎1\">40-14-00-00-00-00-00-00 40-18-00-00-00-00-00-00</minted>"
                         , "      <applied meta=\"𝑛·1·13\" of=\"Φ.number\"><symbol attr=\"φ\">𝜎1</symbol></applied>"
                         , "      <built meta=\"𝑛·1·14\">𝑛·1·13</built>"
                         , "      <morph at=\"𝑛·1·14\">"
                         , "        <applied meta=\"𝑛·1·15\" by=\"morph\" at=\"𝑛·1·14\" of=\"Φ.number\"><symbol attr=\"φ\">𝜎1</symbol></applied>"
                         , "        <morphed meta=\"𝑛·1·16\">𝑛·1·15</morphed>"
                         , "      </morph>"
                         , "      <answered meta=\"𝑛·1·17\">𝑛·1·15</answered>"
                         , "    </evaluate>"
                         , "    <applied meta=\"𝑛·0·7\" of=\"Φ.bytes\"><datum attr=\"φ\">40-1C-00-00-00-00-00-00</datum></applied>"
                         , "    <applied meta=\"𝑛·0·8\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·7</meta></applied>"
                         , "    <applied meta=\"𝑛·0·9\" by=\"morph\" at=\"𝑛·0·1\" of=\"𝑛·1·15.plus\"><meta attr=\"x\">𝑛·0·8</meta></applied>"
                         , "    <morphed meta=\"𝑛·0·10\">𝑛·0·9</morphed>"
                         , "  </morph>"
                         , "  <evaluate λ=\"L_number_plus\" by=\"dataize\" at=\"𝑛·0·1\">"
                         , "    <dataize at=\"ξ.ρ\">"
                         , "      <morph at=\"ξ.ρ\">"
                         , "        <applied meta=\"𝑛·2·1\" by=\"morph\" at=\"ξ.ρ\" of=\"Φ.number\"><symbol attr=\"φ\">𝜎1</symbol></applied>"
                         , "        <morphed meta=\"𝑛·2·2\">𝑛·2·1</morphed>"
                         , "      </morph>"
                         , "      <boxed meta=\"𝑛·2·3\">𝑛·2·1.φ</boxed>"
                         , "      <manufactured meta=\"𝛿·2·1\">𝜎1</manufactured>"
                         , "    </dataize>"
                         , "    <bound meta=\"𝛿1·2\">𝛿·2·1</bound>"
                         , "    <dataize at=\"ξ.x\">"
                         , "      <morph at=\"ξ.x\">"
                         , "        <applied meta=\"𝑛·2·4\" by=\"morph\" at=\"ξ.x\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·7</meta></applied>"
                         , "        <morphed meta=\"𝑛·2·5\">𝑛·2·4</morphed>"
                         , "      </morph>"
                         , "      <boxed meta=\"𝑛·2·6\">𝑛·2·4.φ</boxed>"
                         , "      <morph at=\"𝑛·2·6\">"
                         , "        <applied meta=\"𝑛·2·7\" by=\"morph\" at=\"𝑛·2·6\" of=\"Φ.bytes\"><datum attr=\"φ\">40-1C-00-00-00-00-00-00</datum></applied>"
                         , "        <morphed meta=\"𝑛·2·8\">𝑛·2·7</morphed>"
                         , "      </morph>"
                         , "      <boxed meta=\"𝑛·2·9\">𝑛·2·7.φ</boxed>"
                         , "      <dataized meta=\"𝛿·2·2\" number=\"7.0\">40-1C-00-00-00-00-00-00</dataized>"
                         , "    </dataize>"
                         , "    <bound meta=\"𝛿2·2\">𝛿·2·2</bound>"
                         , "    <minted symbol=\"𝜎2\">𝜎1 40-1C-00-00-00-00-00-00</minted>"
                         , "    <applied meta=\"𝑛·2·10\" of=\"Φ.number\"><symbol attr=\"φ\">𝜎2</symbol></applied>"
                         , "    <built meta=\"𝑛·2·11\">𝑛·2·10</built>"
                         , "    <morph at=\"𝑛·2·11\">"
                         , "      <applied meta=\"𝑛·2·12\" by=\"morph\" at=\"𝑛·2·11\" of=\"Φ.number\"><symbol attr=\"φ\">𝜎2</symbol></applied>"
                         , "      <morphed meta=\"𝑛·2·13\">𝑛·2·12</morphed>"
                         , "    </morph>"
                         , "    <answered meta=\"𝑛·2·14\">𝑛·2·12</answered>"
                         , "  </evaluate>"
                         , "  <boxed meta=\"𝑛·0·11\">𝑛·2·12.φ</boxed>"
                         , "  <manufactured meta=\"𝛿·0·1\">𝜎2</manufactured>"
                         , "</dataize>"
                         ]

        it "writes a firing whose two sides do not join as diverged, naming both sides" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withLambdasOf (T.pack "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  join:\n    on: 𝛿1\n    left: $.a\n    right: $.b\n") $ \joins ->
              withStdin "⟦ y ↦ ⟦ c ↦ ⟦ λ ⤍ 𝜎1 ⟧, a ↦ ⟦ φ ↦ ⟦ Δ ⤍ 01- ⟧ ⟧, b ↦ ⟦ φ ↦ ⟦ Δ ⤍ 02- ⟧ ⟧, λ ⤍ L_fork ⟧.φ ⟧" $
                testCLIFailed ["morph", "--symbolic=" ++ joins, "--locator=Q.y", "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] ["No entry of --symbolic answers the λ function 'L_fork'"]
            records <- readProtocol path
            lines records
              `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                         , "<morph at=\"Φ.y\">"
                         , "  <evaluate λ=\"L_fork\" by=\"morph\" at=\"Φ.y\">"
                         , "    <dataize at=\"ξ.c\">"
                         , "      <manufactured meta=\"𝛿·1·1\">𝜎1</manufactured>"
                         , "    </dataize>"
                         , "    <bound meta=\"𝛿1·1\">𝛿·1·1</bound>"
                         , "    <morph at=\"ξ.a\">"
                         , "      <formed meta=\"𝑛·1·1\"><datum attr=\"φ\">01-</datum></formed>"
                         , "      <morphed meta=\"𝑛·1·2\">𝑛·1·1</morphed>"
                         , "    </morph>"
                         , "    <morph at=\"ξ.b\">"
                         , "      <formed meta=\"𝑛·1·3\"><datum attr=\"φ\">02-</datum></formed>"
                         , "      <morphed meta=\"𝑛·1·4\">𝑛·1·3</morphed>"
                         , "    </morph>"
                         , "    <diverged left=\"𝑛·1·1\" right=\"𝑛·1·3\" at=\".φ.Δ\" reason=\"the join names no otherwise\"/>"
                         , "  </evaluate>"
                         , "</morph>"
                         ]

        it "writes the symbol a join minted for two sides that differ only in their symbols as an element of its own" $
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            withLambdasOf (T.pack "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  join:\n    on: 𝛿1\n    left: $.a\n    right: $.b\n") $ \joins ->
              withStdin "⟦ y ↦ ⟦ c ↦ ⟦ λ ⤍ 𝜎1 ⟧, a ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ⟧, b ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎3 ⟧ ⟧, λ ⤍ L_fork ⟧.φ ⟧" $
                testCLISucceeded ["morph", "--symbolic=" ++ joins, "--locator=Q.y", "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
            records <- readProtocol path
            lines records
              `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                         , "<morph at=\"Φ.y\">"
                         , "  <evaluate λ=\"L_fork\" by=\"morph\" at=\"Φ.y\">"
                         , "    <dataize at=\"ξ.c\">"
                         , "      <manufactured meta=\"𝛿·1·1\">𝜎1</manufactured>"
                         , "    </dataize>"
                         , "    <bound meta=\"𝛿1·1\">𝛿·1·1</bound>"
                         , "    <morph at=\"ξ.a\">"
                         , "      <formed meta=\"𝑛·1·1\"><symbol attr=\"φ\">𝜎2</symbol></formed>"
                         , "      <morphed meta=\"𝑛·1·2\">𝑛·1·1</morphed>"
                         , "    </morph>"
                         , "    <morph at=\"ξ.b\">"
                         , "      <formed meta=\"𝑛·1·3\"><symbol attr=\"φ\">𝜎3</symbol></formed>"
                         , "      <morphed meta=\"𝑛·1·4\">𝑛·1·3</morphed>"
                         , "    </morph>"
                         , "    <joined symbol=\"𝜎4\" on=\"𝛿1·1\" left=\"𝜎2\" right=\"𝜎3\"/>"
                         , "    <formed meta=\"𝑛·1·5\"><symbol attr=\"φ\">𝜎4</symbol></formed>"
                         , "    <answered meta=\"𝑛·1·6\">𝑛·1·5</answered>"
                         , "  </evaluate>"
                         , "  <morphed meta=\"𝑛·0·1\">𝜎4</morphed>"
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
                         , "    <morph at=\"ξ.x\">"
                         , "      <morphed meta=\"𝑛·1·1\">01-</morphed>"
                         , "    </morph>"
                         , "    <bound meta=\"𝑛1·1\">01-</bound>"
                         , "    <minted symbol=\"𝜎1\"/>"
                         , "    <minted symbol=\"𝜎2\"/>"
                         , "    <formed meta=\"𝑛·1·2\"><symbol attr=\"left\">𝜎1</symbol><symbol attr=\"right\">𝜎2</symbol></formed>"
                         , "    <built meta=\"𝑛·1·3\">𝑛·1·2</built>"
                         , "    <morph at=\"𝑛·1·3\">"
                         , "      <morphed meta=\"𝑛·1·4\">𝑛·1·2</morphed>"
                         , "    </morph>"
                         , "    <answered meta=\"𝑛·1·5\">𝑛·1·2</answered>"
                         , "  </evaluate>"
                         , "  <morphed meta=\"𝑛·0·1\">𝜎1</morphed>"
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
                         , "    <morph at=\"ξ.x\">"
                         , "      <morphed meta=\"𝑛·1·1\">01-</morphed>"
                         , "    </morph>"
                         , "    <bound meta=\"𝑛1·1\">01-</bound>"
                         , "    <formed meta=\"𝑛·1·2\"><datum attr=\"z\">01-</datum></formed>"
                         , "    <built meta=\"𝑛·1·3\">𝑛·1·2</built>"
                         , "    <morph at=\"𝑛·1·3\">"
                         , "      <morphed meta=\"𝑛·1·4\">𝑛·1·2</morphed>"
                         , "    </morph>"
                         , "    <answered meta=\"𝑛·1·5\">𝑛·1·2</answered>"
                         , "  </evaluate>"
                         , "  <morphed meta=\"𝑛·0·1\">01-</morphed>"
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
                         , "  <boxed meta=\"𝑛·0·1\">Φ.φ</boxed>"
                         , "  <morph at=\"𝑛·0·1\">"
                         , "    <applied meta=\"𝑛·0·2\" of=\"Φ.bytes\"><datum attr=\"φ\">40-14-00-00-00-00-00-00</datum></applied>"
                         , "    <applied meta=\"𝑛·0·3\" by=\"morph\" at=\"𝑛·0·1\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·2</meta></applied>"
                         , "    <applied meta=\"𝑛·0·4\" of=\"Φ.bytes\"><datum attr=\"φ\">40-18-00-00-00-00-00-00</datum></applied>"
                         , "    <applied meta=\"𝑛·0·5\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·4</meta></applied>"
                         , "    <applied meta=\"𝑛·0·6\" of=\"Φ.bytes\"><datum attr=\"φ\">40-1C-00-00-00-00-00-00</datum></applied>"
                         , "    <applied meta=\"𝑛·0·7\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·6</meta></applied>"
                         , "    <applied meta=\"𝑛·0·8\" of=\"𝑛·0·5.plus\"><meta attr=\"α0\">𝑛·0·7</meta></applied>"
                         , "    <applied meta=\"𝑛·0·9\" by=\"morph\" at=\"𝑛·0·1\" of=\"𝑛·0·3.plus\"><meta attr=\"x\">𝑛·0·8</meta></applied>"
                         , "    <morphed meta=\"𝑛·0·10\">𝑛·0·9</morphed>"
                         , "  </morph>"
                         , "  <evaluate λ=\"L_number_plus\" by=\"dataize\" at=\"𝑛·0·1\">"
                         , "    <dataize at=\"ξ.ρ\">"
                         , "      <morph at=\"ξ.ρ\">"
                         , "        <applied meta=\"𝑛·1·1\" by=\"morph\" at=\"ξ.ρ\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·2</meta></applied>"
                         , "        <morphed meta=\"𝑛·1·2\">𝑛·1·1</morphed>"
                         , "      </morph>"
                         , "      <boxed meta=\"𝑛·1·3\">𝑛·1·1.φ</boxed>"
                         , "      <morph at=\"𝑛·1·3\">"
                         , "        <applied meta=\"𝑛·1·4\" by=\"morph\" at=\"𝑛·1·3\" of=\"Φ.bytes\"><datum attr=\"φ\">40-14-00-00-00-00-00-00</datum></applied>"
                         , "        <morphed meta=\"𝑛·1·5\">𝑛·1·4</morphed>"
                         , "      </morph>"
                         , "      <boxed meta=\"𝑛·1·6\">𝑛·1·4.φ</boxed>"
                         , "      <dataized meta=\"𝛿·1·1\" number=\"5.0\">40-14-00-00-00-00-00-00</dataized>"
                         , "    </dataize>"
                         , "    <bound meta=\"𝛿1·1\">𝛿·1·1</bound>"
                         , "    <dataize at=\"ξ.x\">"
                         , "      <morph at=\"ξ.x\">"
                         , "        <applied meta=\"𝑛·1·7\" by=\"morph\" at=\"ξ.x\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·4</meta></applied>"
                         , "        <applied meta=\"𝑛·1·8\" by=\"morph\" at=\"ξ.x\" of=\"𝑛·1·7.plus\"><meta attr=\"x\">𝑛·0·7</meta></applied>"
                         , "        <morphed meta=\"𝑛·1·9\">𝑛·1·8</morphed>"
                         , "      </morph>"
                         , "      <evaluate λ=\"L_number_plus\" by=\"dataize\" at=\"ξ.x\">"
                         , "        <dataize at=\"ξ.ρ\">"
                         , "          <morph at=\"ξ.ρ\">"
                         , "            <applied meta=\"𝑛·2·1\" by=\"morph\" at=\"ξ.ρ\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·4</meta></applied>"
                         , "            <morphed meta=\"𝑛·2·2\">𝑛·2·1</morphed>"
                         , "          </morph>"
                         , "          <boxed meta=\"𝑛·2·3\">𝑛·2·1.φ</boxed>"
                         , "          <morph at=\"𝑛·2·3\">"
                         , "            <applied meta=\"𝑛·2·4\" by=\"morph\" at=\"𝑛·2·3\" of=\"Φ.bytes\"><datum attr=\"φ\">40-18-00-00-00-00-00-00</datum></applied>"
                         , "            <morphed meta=\"𝑛·2·5\">𝑛·2·4</morphed>"
                         , "          </morph>"
                         , "          <boxed meta=\"𝑛·2·6\">𝑛·2·4.φ</boxed>"
                         , "          <dataized meta=\"𝛿·2·1\" number=\"6.0\">40-18-00-00-00-00-00-00</dataized>"
                         , "        </dataize>"
                         , "        <bound meta=\"𝛿1·2\">𝛿·2·1</bound>"
                         , "        <dataize at=\"ξ.x\">"
                         , "          <morph at=\"ξ.x\">"
                         , "            <applied meta=\"𝑛·2·7\" by=\"morph\" at=\"ξ.x\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·6</meta></applied>"
                         , "            <morphed meta=\"𝑛·2·8\">𝑛·2·7</morphed>"
                         , "          </morph>"
                         , "          <boxed meta=\"𝑛·2·9\">𝑛·2·7.φ</boxed>"
                         , "          <morph at=\"𝑛·2·9\">"
                         , "            <applied meta=\"𝑛·2·10\" by=\"morph\" at=\"𝑛·2·9\" of=\"Φ.bytes\"><datum attr=\"φ\">40-1C-00-00-00-00-00-00</datum></applied>"
                         , "            <morphed meta=\"𝑛·2·11\">𝑛·2·10</morphed>"
                         , "          </morph>"
                         , "          <boxed meta=\"𝑛·2·12\">𝑛·2·10.φ</boxed>"
                         , "          <dataized meta=\"𝛿·2·2\" number=\"7.0\">40-1C-00-00-00-00-00-00</dataized>"
                         , "        </dataize>"
                         , "        <bound meta=\"𝛿2·2\">𝛿·2·2</bound>"
                         , "        <minted symbol=\"𝜎1\">40-18-00-00-00-00-00-00 40-1C-00-00-00-00-00-00</minted>"
                         , "        <applied meta=\"𝑛·2·13\" of=\"Φ.number\"><symbol attr=\"φ\">𝜎1</symbol></applied>"
                         , "        <built meta=\"𝑛·2·14\">𝑛·2·13</built>"
                         , "        <morph at=\"𝑛·2·14\">"
                         , "          <applied meta=\"𝑛·2·15\" by=\"morph\" at=\"𝑛·2·14\" of=\"Φ.number\"><symbol attr=\"φ\">𝜎1</symbol></applied>"
                         , "          <morphed meta=\"𝑛·2·16\">𝑛·2·15</morphed>"
                         , "        </morph>"
                         , "        <answered meta=\"𝑛·2·17\">𝑛·2·15</answered>"
                         , "      </evaluate>"
                         , "      <boxed meta=\"𝑛·1·10\">𝑛·2·15.φ</boxed>"
                         , "      <manufactured meta=\"𝛿·1·2\">𝜎1</manufactured>"
                         , "    </dataize>"
                         , "    <bound meta=\"𝛿2·1\">𝛿·1·2</bound>"
                         , "    <minted symbol=\"𝜎2\">40-14-00-00-00-00-00-00 𝜎1</minted>"
                         , "    <applied meta=\"𝑛·1·11\" of=\"Φ.number\"><symbol attr=\"φ\">𝜎2</symbol></applied>"
                         , "    <built meta=\"𝑛·1·12\">𝑛·1·11</built>"
                         , "    <morph at=\"𝑛·1·12\">"
                         , "      <applied meta=\"𝑛·1·13\" by=\"morph\" at=\"𝑛·1·12\" of=\"Φ.number\"><symbol attr=\"φ\">𝜎2</symbol></applied>"
                         , "      <morphed meta=\"𝑛·1·14\">𝑛·1·13</morphed>"
                         , "    </morph>"
                         , "    <answered meta=\"𝑛·1·15\">𝑛·1·13</answered>"
                         , "  </evaluate>"
                         , "  <boxed meta=\"𝑛·0·11\">𝑛·1·13.φ</boxed>"
                         , "  <manufactured meta=\"𝛿·0·1\">𝜎2</manufactured>"
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
                         , "  <boxed meta=\"𝑛·0·1\">Φ.φ</boxed>"
                         , "  <morph at=\"𝑛·0·1\">"
                         , "    <applied meta=\"𝑛·0·2\" of=\"Φ.bytes\"><datum attr=\"φ\">40-00-00-00-00-00-00-00</datum></applied>"
                         , "    <applied meta=\"𝑛·0·3\" by=\"morph\" at=\"𝑛·0·1\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·2</meta></applied>"
                         , "    <applied meta=\"𝑛·0·4\" of=\"Φ.bytes\"><datum attr=\"φ\">40-08-00-00-00-00-00-00</datum></applied>"
                         , "    <applied meta=\"𝑛·0·5\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·4</meta></applied>"
                         , "    <applied meta=\"𝑛·0·6\" by=\"morph\" at=\"𝑛·0·1\" of=\"𝑛·0·3.times\"><meta attr=\"x\">𝑛·0·5</meta></applied>"
                         , "    <evaluate λ=\"L_number_times\" by=\"morph\" at=\"𝑛·0·1\">"
                         , "      <dataize at=\"ξ.ρ\">"
                         , "        <morph at=\"ξ.ρ\">"
                         , "          <applied meta=\"𝑛·1·1\" by=\"morph\" at=\"ξ.ρ\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·2</meta></applied>"
                         , "          <morphed meta=\"𝑛·1·2\">𝑛·1·1</morphed>"
                         , "        </morph>"
                         , "        <boxed meta=\"𝑛·1·3\">𝑛·1·1.φ</boxed>"
                         , "        <morph at=\"𝑛·1·3\">"
                         , "          <applied meta=\"𝑛·1·4\" by=\"morph\" at=\"𝑛·1·3\" of=\"Φ.bytes\"><datum attr=\"φ\">40-00-00-00-00-00-00-00</datum></applied>"
                         , "          <morphed meta=\"𝑛·1·5\">𝑛·1·4</morphed>"
                         , "        </morph>"
                         , "        <boxed meta=\"𝑛·1·6\">𝑛·1·4.φ</boxed>"
                         , "        <dataized meta=\"𝛿·1·1\" number=\"2.0\">40-00-00-00-00-00-00-00</dataized>"
                         , "      </dataize>"
                         , "      <bound meta=\"𝛿1·1\">𝛿·1·1</bound>"
                         , "      <dataize at=\"ξ.x\">"
                         , "        <morph at=\"ξ.x\">"
                         , "          <applied meta=\"𝑛·1·7\" by=\"morph\" at=\"ξ.x\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·4</meta></applied>"
                         , "          <morphed meta=\"𝑛·1·8\">𝑛·1·7</morphed>"
                         , "        </morph>"
                         , "        <boxed meta=\"𝑛·1·9\">𝑛·1·7.φ</boxed>"
                         , "        <morph at=\"𝑛·1·9\">"
                         , "          <applied meta=\"𝑛·1·10\" by=\"morph\" at=\"𝑛·1·9\" of=\"Φ.bytes\"><datum attr=\"φ\">40-08-00-00-00-00-00-00</datum></applied>"
                         , "          <morphed meta=\"𝑛·1·11\">𝑛·1·10</morphed>"
                         , "        </morph>"
                         , "        <boxed meta=\"𝑛·1·12\">𝑛·1·10.φ</boxed>"
                         , "        <dataized meta=\"𝛿·1·2\" number=\"3.0\">40-08-00-00-00-00-00-00</dataized>"
                         , "      </dataize>"
                         , "      <bound meta=\"𝛿2·1\">𝛿·1·2</bound>"
                         , "      <minted symbol=\"𝜎1\">40-00-00-00-00-00-00-00 40-08-00-00-00-00-00-00</minted>"
                         , "      <applied meta=\"𝑛·1·13\" of=\"Φ.number\"><symbol attr=\"φ\">𝜎1</symbol></applied>"
                         , "      <built meta=\"𝑛·1·14\">𝑛·1·13</built>"
                         , "      <morph at=\"𝑛·1·14\">"
                         , "        <applied meta=\"𝑛·1·15\" by=\"morph\" at=\"𝑛·1·14\" of=\"Φ.number\"><symbol attr=\"φ\">𝜎1</symbol></applied>"
                         , "        <morphed meta=\"𝑛·1·16\">𝑛·1·15</morphed>"
                         , "      </morph>"
                         , "      <answered meta=\"𝑛·1·17\">𝑛·1·15</answered>"
                         , "    </evaluate>"
                         , "    <morphed meta=\"𝑛·0·7\">𝑛·1·15.nope</morphed>"
                         , "  </morph>"
                         , "  <unanswered λ=\"L_number_nope\" by=\"dataize\">𝑛·1·15.nope</unanswered>"
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
                         , "  <formed meta=\"𝑛·0·1\" λ=\"L_number_nope\"/>"
                         , "  <unanswered λ=\"L_number_nope\" by=\"morph\">𝑛·0·1</unanswered>"
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
                         , "  <boxed meta=\"𝑛·0·1\">Φ.φ</boxed>"
                         , "  <morph at=\"𝑛·0·1\">"
                         , "    <applied meta=\"𝑛·0·2\" of=\"Φ.bytes\"><datum attr=\"φ\">40-00-00-00-00-00-00-00</datum></applied>"
                         , "    <applied meta=\"𝑛·0·3\" by=\"morph\" at=\"𝑛·0·1\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·2</meta></applied>"
                         , "    <applied meta=\"𝑛·0·4\" of=\"Φ.bytes\"><datum attr=\"φ\">40-08-00-00-00-00-00-00</datum></applied>"
                         , "    <applied meta=\"𝑛·0·5\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·4</meta></applied>"
                         , "    <applied meta=\"𝑛·0·6\" by=\"morph\" at=\"𝑛·0·1\" of=\"𝑛·0·3.times\"><meta attr=\"x\">𝑛·0·5</meta></applied>"
                         , "    <evaluate λ=\"L_number_times\" by=\"morph\" at=\"𝑛·0·1\">"
                         , "      <dataize at=\"ξ.ρ\">"
                         , "        <morph at=\"ξ.ρ\">"
                         , "          <applied meta=\"𝑛·1·1\" by=\"morph\" at=\"ξ.ρ\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·2</meta></applied>"
                         , "          <morphed meta=\"𝑛·1·2\">𝑛·1·1</morphed>"
                         , "        </morph>"
                         , "        <boxed meta=\"𝑛·1·3\">𝑛·1·1.φ</boxed>"
                         , "        <morph at=\"𝑛·1·3\">"
                         , "          <applied meta=\"𝑛·1·4\" by=\"morph\" at=\"𝑛·1·3\" of=\"Φ.bytes\"><datum attr=\"φ\">40-00-00-00-00-00-00-00</datum></applied>"
                         , "          <morphed meta=\"𝑛·1·5\">𝑛·1·4</morphed>"
                         , "        </morph>"
                         , "        <boxed meta=\"𝑛·1·6\">𝑛·1·4.φ</boxed>"
                         , "        <dataized meta=\"𝛿·1·1\" number=\"2.0\">40-00-00-00-00-00-00-00</dataized>"
                         , "      </dataize>"
                         , "      <bound meta=\"𝛿1·1\">𝛿·1·1</bound>"
                         , "      <dataize at=\"ξ.x\">"
                         , "        <morph at=\"ξ.x\">"
                         , "          <applied meta=\"𝑛·1·7\" by=\"morph\" at=\"ξ.x\" of=\"Φ.number\"><meta attr=\"φ\">𝑛·0·4</meta></applied>"
                         , "          <morphed meta=\"𝑛·1·8\">𝑛·1·7</morphed>"
                         , "        </morph>"
                         , "        <boxed meta=\"𝑛·1·9\">𝑛·1·7.φ</boxed>"
                         , "        <morph at=\"𝑛·1·9\">"
                         , "          <applied meta=\"𝑛·1·10\" by=\"morph\" at=\"𝑛·1·9\" of=\"Φ.bytes\"><datum attr=\"φ\">40-08-00-00-00-00-00-00</datum></applied>"
                         , "          <morphed meta=\"𝑛·1·11\">𝑛·1·10</morphed>"
                         , "        </morph>"
                         , "        <boxed meta=\"𝑛·1·12\">𝑛·1·10.φ</boxed>"
                         , "        <dataized meta=\"𝛿·1·2\" number=\"3.0\">40-08-00-00-00-00-00-00</dataized>"
                         , "      </dataize>"
                         , "      <bound meta=\"𝛿2·1\">𝛿·1·2</bound>"
                         , "      <minted symbol=\"𝜎1\">40-00-00-00-00-00-00-00 40-08-00-00-00-00-00-00</minted>"
                         , "      <applied meta=\"𝑛·1·13\" of=\"Φ.number\"><symbol attr=\"φ\">𝜎1</symbol></applied>"
                         , "      <built meta=\"𝑛·1·14\">𝑛·1·13</built>"
                         , "      <morph at=\"𝑛·1·14\">"
                         , "        <applied meta=\"𝑛·1·15\" by=\"morph\" at=\"𝑛·1·14\" of=\"Φ.number\"><symbol attr=\"φ\">𝜎1</symbol></applied>"
                         , "        <morphed meta=\"𝑛·1·16\">𝑛·1·15</morphed>"
                         , "      </morph>"
                         , "      <answered meta=\"𝑛·1·17\">𝑛·1·15</answered>"
                         , "    </evaluate>"
                         , "    <morphed meta=\"𝑛·0·7\">𝑛·1·15.nope</morphed>"
                         , "  </morph>"
                         , "  <unanswered λ=\"L_number_nope\" by=\"dataize\">𝑛·1·15.nope</unanswered>"
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
                         , "    <morph at=\"ξ.absent\">"
                         , "      <morphed meta=\"𝑛·1·1\">⊥</morphed>"
                         , "    </morph>"
                         , "    <bound meta=\"𝑛1·1\">⊥</bound>"
                         , "    <minted symbol=\"𝜎1\"/>"
                         , "    <built meta=\"𝑛·1·2\">𝜎1</built>"
                         , "    <morph at=\"𝑛·1·2\">"
                         , "      <morphed meta=\"𝑛·1·3\">𝜎1</morphed>"
                         , "    </morph>"
                         , "    <answered meta=\"𝑛·1·4\">𝜎1</answered>"
                         , "  </evaluate>"
                         , "  <unanswered λ=\"𝜎1\" by=\"morph\">𝜎1</unanswered>"
                         , "</morph>"
                         ]

        it "keeps writing text when the file is named anything else" $
          withTempFile "protocolXXXXXX.xmir" $ \(path, stream) -> do
            hClose stream
            withStdin sum' $
              testCLISucceeded ["dataize", symbolic, "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] []
            records <- readProtocol path
            take 1 (lines records) `shouldBe` ["𝔻(Φ):"]

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
            `shouldBe` [ "𝔻(Φ):"
                       , "  boxed 𝑛·0·1 := Φ.φ"
                       , "  𝕄(𝑛·0·1):"
                       , "    𝑛·0·2 := 2.0"
                       , "    𝑛·0·3 := 𝑛·0·2.times( x ↦ 3.0 )"
                       , "    𝔼(L_number_times):"
                       , "      𝔻(ξ.ρ):"
                       , "        𝕄(ξ.ρ):"
                       , "          𝑛·1·1 := 2.0"
                       , "        boxed 𝑛·1·2 := 𝑛·1·1.φ"
                       , "        𝕄(𝑛·1·2):"
                       , "          𝑛·1·3 := Φ.bytes( φ ↦ 40-00-00-00-00-00-00-00:Δ )"
                       , "        boxed 𝑛·1·4 := 𝑛·1·3.φ"
                       , "        𝛿·1·1 := 40-00-00-00-00-00-00-00  # 2.0"
                       , "      𝛿1·1 := 𝛿·1·1  # 𝔻(ξ.ρ)"
                       , "      𝔻(ξ.x):"
                       , "        𝕄(ξ.x):"
                       , "          𝑛·1·5 := 3.0"
                       , "        boxed 𝑛·1·6 := 𝑛·1·5.φ"
                       , "        𝕄(𝑛·1·6):"
                       , "          𝑛·1·7 := Φ.bytes( φ ↦ 40-08-00-00-00-00-00-00:Δ )"
                       , "        boxed 𝑛·1·8 := 𝑛·1·7.φ"
                       , "        𝛿·1·2 := 40-08-00-00-00-00-00-00  # 3.0"
                       , "      𝛿2·1 := 𝛿·1·2  # 𝔻(ξ.x)"
                       , "      𝑛·1·9 := Φ.number( φ ↦ 𝜎1:λ )  # 𝑛"
                       , "      𝕄(𝑛·1·9):"
                       , "        𝑛·1·10 := Φ.number( φ ↦ 𝜎1:λ )"
                       , "      𝑛·1·11 := 𝑛·1·10  # 𝕄(𝑛·1·9)"
                       , "    𝑛·0·4 := L_number_nope:λ"
                       , "  unanswered L_number_nope  # 𝔻(L_number_nope:λ)"
                       ]

      it "still prints bytes when nothing gets stuck" $
        withStdin "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(^, x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6) ]]" $
          testCLISucceeded ["dataize", symbolic, "--partial"] ["40-45-00-00-00-00-00-00"]

      it "prints the residual at --locator as XMIR, with ρ hidden" $
        withStdin wrapped $
          testCLISucceeded
            ["dataize", symbolic, "--partial", "--locator=Q.app", "--output=xmir", "--hide-rho"]
            ["<o name=\"λ\">L_number_nope</o>"]

      it "prints the residual at --locator, not the whole program" $
        withStdin wrapped $ do
          (out, _) <- withStdout (runCLI ["dataize", symbolic, "--partial", "--locator=Q.app", "--hide-rho", "--flat"])
          lines out `shouldBe` ["⟦ λ ⤍ L_number_nope ⟧"]

      it "prints a residual of several top bindings as XMIR" $
        withStdin dispatched $
          testCLISucceeded
            ["dataize", symbolic, "--partial", "--output=xmir"]
            ["<o base=\"Φ.foo\" name=\"ρ\"/>", "<o name=\"λ\">L_number_nope</o>"]

      it "prints the chain of steps ending in the residue with --sequence" $
        withStdin stuck $
          testCLISucceeded
            ["dataize", symbolic, "--partial", "--sequence", "--sweet", "--hide-rho", "--flat"]
            ["2.0.times( 3.0 ).nope", "L_number_nope:λ"]

      it "prints the terminator ⊥ the object reduces to as the residue" $
        withStdin "[[ ]]" $
          testCLISucceeded ["dataize", "--partial"] ["⊥"]

    describe "--symbolic" $ do
      let sum' = "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(^, x) -> [[ L> L_number_plus ]] ]], @ -> 5.plus(6) ]]"
      it "fires the λ function an entry of the file answers" $
        withStdin sum' $
          testCLISucceeded ["dataize", symbolic] ["40-45-00-00-00-00-00-00"]

      it "reports the progress of the run with --log-level=INFO" $
        withStdin sum' $
          testCLISucceeded ["dataize", symbolic, "--log-level=INFO", "--quiet"] ["[INFO]: Started "]

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

    describe "a join on a condition nobody decided" $ do
      let joining = T.pack "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  join:\n    on: 𝛿1\n    left: $.a\n    right: $.b\n"
          dataized :: String -> IO String
          dataized program =
            withLambdasOf joining $ \joins ->
              withStdin program $
                fst <$> withStdout (runCLI ["dataize", "--symbolic=" ++ joins, "--locator=Q.y"])
      it "gets stuck on two sides that do not join, instead of splitting the run" $
        withLambdasOf joining $ \joins ->
          withStdin "⟦ y ↦ ⟦ c ↦ ⟦ λ ⤍ 𝜎1 ⟧, a ↦ ⟦ Δ ⤍ 01- ⟧, b ↦ ⟦ Δ ⤍ 02- ⟧, λ ⤍ L_fork ⟧ ⟧" $
            testCLIFailed ["dataize", "--symbolic=" ++ joins, "--locator=Q.y"] ["No entry of --symbolic answers the λ function 'L_fork'"]

      it "gets stuck on a side whose morph got stuck" $
        withLambdasOf joining $ \joins ->
          withStdin "⟦ y ↦ ⟦ c ↦ ⟦ λ ⤍ 𝜎1 ⟧, a ↦ ⟦ Δ ⤍ 05- ⟧, b ↦ ⟦ λ ⤍ L_none ⟧.x, λ ⤍ L_fork ⟧ ⟧" $
            testCLIFailed ["dataize", "--symbolic=" ++ joins, "--locator=Q.y"] ["No entry of --symbolic answers the λ function 'L_none'"]

      it "dataizes two sides that come to the same term" $ do
        printed <- dataized "⟦ y ↦ ⟦ c ↦ ⟦ λ ⤍ 𝜎1 ⟧, a ↦ ⟦ Δ ⤍ 07- ⟧, b ↦ ⟦ Δ ⤍ 07- ⟧, λ ⤍ L_fork ⟧ ⟧"
        lines printed `shouldBe` ["07-"]

      it "dataizes two sides that differ only in their symbols" $ do
        printed <- dataized "⟦ y ↦ ⟦ c ↦ ⟦ λ ⤍ 𝜎1 ⟧, a ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎2 ⟧ ⟧, b ↦ ⟦ φ ↦ ⟦ λ ⤍ 𝜎3 ⟧ ⟧, λ ⤍ L_fork ⟧ ⟧"
        length (lines printed) `shouldBe` 1

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
          testCLISucceeded ["morph", symbolic, "--inside=5.plus( 6 )", "--sweet", "--hide-rho", "--flat"] ["⟦ x ↦ 6.0, λ ⤍ L_number_plus ⟧"]

      it "opens the text protocol before the objects it made while aiming" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin universe $
            testCLISucceeded ["morph", symbolic, "--inside=5.plus( 6 )", "--protocol=" ++ path, "--sweet", "--hide-rho", "--quiet"] []
          records <- readProtocol path
          takeWhile (/= '(') (concat (take 1 (lines records))) `shouldBe` "𝕄"

      it "opens the XML protocol before the objects it made while aiming" $
        withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
          hClose stream
          withStdin universe $
            testCLISucceeded ["dataize", symbolic, "--inside=5.plus( 6 )", "--protocol=" ++ path, "--sweet", "--hide-rho", "--quiet"] []
          records <- readProtocol path
          take 1 (lines records) `shouldBe` ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>"]

      it "sites what it made while aiming at the place it put the expression at" $
        withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
          hClose stream
          withStdin universe $
            testCLISucceeded ["dataize", "--inside=[[ x -> ? ]]( x -> [[ D> 2A- ]] ).x", "--protocol=" ++ path, "--sweet", "--quiet"] []
          records <- readProtocol path
          take 2 (lines records) `shouldBe` ["𝔻(Φ.a🌵0):", "  𝑛·0·1 := ∅:x( x ↦ 2A-:Δ )"]

      it "cannot be used together with --locator" $
        withStdin universe $
          testCLIFailed ["dataize", "--inside=Q.@", "--locator=Q.@"] ["--inside and --locator cannot be used together"]

      it "accepts --locator=Φ with --inside, since Φ and Q both name the root" $
        withStdin universe $
          testCLISucceeded ["dataize", "--inside=[[ x -> [[ D> 2A- ]] ]].x", "--locator=Φ"] ["2A-"]

      it "accepts --locator=Φ with morph --inside, since Φ and Q both name the root" $
        withStdin universe $
          testCLISucceeded ["morph", symbolic, "--inside=5.plus( 6 )", "--locator=Φ", "--sweet", "--hide-rho", "--flat"] ["⟦ x ↦ 6.0, λ ⤍ L_number_plus ⟧"]

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

      it "with wrong --locator option written through ξ, keeping ξ in the error" $
        withStdin "" $
          testCLIFailed
            ["dataize", "--locator=ξ.x"]
            ["[ERROR]:", "but given: ξ.x"]

    it "accepts --depth-sensitive" $
      withStdin "[[ D> 01- ]]" $
        testCLISucceeded ["dataize", "--depth-sensitive"] ["01-"]

    it "finishes exactly on --max-cycles without reporting the limit" $
      withStdin "⟦ φ ↦ ⟦ Δ ⤍ 01- ⟧ ⟧" $
        testCLISucceeded ["dataize", "--max-cycles=1"] ["01-"]

    it "reports --max-cycles exceeded when a rule still applies at the limit" $
      withStdin "[[ @ -> [[ x -> [[ D> 00- ]] ]].x ]]" $
        testCLIFailed
          ["dataize", "--max-cycles=1"]
          ["[ERROR]: Normalization did not finish before reaching the limit of cycles: --max-cycles=1"]

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
          ["⟦ x ↦ 7.0, λ ⤍ L_number_plus ⟧"]

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
          , "⟦ x ↦ 7.0, λ ⤍ L_number_plus ⟧"
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
          `shouldBe` [ "𝕄(Φ.φ):"
                     , "  𝑛·0·1 := 5.0"
                     , "  𝑛·0·2 := 𝑛·0·1.plus( x ↦ 6.0 )"
                     , "  𝔼(L_number_plus):"
                     , "    𝔻(ξ.ρ):"
                     , "      𝕄(ξ.ρ):"
                     , "        𝑛·1·1 := 5.0"
                     , "      boxed 𝑛·1·2 := 𝑛·1·1.φ"
                     , "      𝕄(𝑛·1·2):"
                     , "        𝑛·1·3 := Φ.bytes( φ ↦ 40-14-00-00-00-00-00-00:Δ )"
                     , "      boxed 𝑛·1·4 := 𝑛·1·3.φ"
                     , "      𝛿·1·1 := 40-14-00-00-00-00-00-00  # 5.0"
                     , "    𝛿1·1 := 𝛿·1·1  # 𝔻(ξ.ρ)"
                     , "    𝔻(ξ.x):"
                     , "      𝕄(ξ.x):"
                     , "        𝑛·1·5 := 6.0"
                     , "      boxed 𝑛·1·6 := 𝑛·1·5.φ"
                     , "      𝕄(𝑛·1·6):"
                     , "        𝑛·1·7 := Φ.bytes( φ ↦ 40-18-00-00-00-00-00-00:Δ )"
                     , "      boxed 𝑛·1·8 := 𝑛·1·7.φ"
                     , "      𝛿·1·2 := 40-18-00-00-00-00-00-00  # 6.0"
                     , "    𝛿2·1 := 𝛿·1·2  # 𝔻(ξ.x)"
                     , "    𝑛·1·9 := Φ.number( φ ↦ 𝜎1:λ )  # 𝑛"
                     , "    𝕄(𝑛·1·9):"
                     , "      𝑛·1·10 := Φ.number( φ ↦ 𝜎1:λ )"
                     , "    𝑛·1·11 := 𝑛·1·10  # 𝕄(𝑛·1·9)"
                     , "  𝑛·0·3 := 𝑛·1·10.plus( x ↦ 7.0 )"
                     ]

    it "saves morphing steps to dir with --steps-dir" $
      withTempDirectory "phino-steps-morph" $ \dir ->
        withStdin chained $ do
          testCLISucceeded
            ["morph", symbolic, "--locator=Q.@", "--steps-dir=" ++ dir, "--sweet", "--hide-rho", "--flat"]
            ["⟦ x ↦ 7.0, λ ⤍ L_number_plus ⟧"]
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
          ["[ERROR]: Morphing did not finish before reaching the limit of steps: --max-steps=3"]

    it "names morphing, not dataization, when the budget is spent before dataization starts" $
      withStdin "⟦ x ↦ Φ.x.y ⟧" $
        testCLIFailed
          ["morph", "--locator=Q.x", "--max-steps=5"]
          ["[ERROR]: Morphing did not finish before reaching the limit of steps: --max-steps=5"]

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
              ["L_split:λ.foo"]

      it "fires no more λ functions than --max-firings allows" $
        splitting $ \table ->
          withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
            hClose stream
            withStdin split $
              testCLISucceeded
                ["morph", "--symbolic=" ++ table, "--locator=Q.x", "--max-firings=64", "--partial", "--protocol=" ++ path, "--quiet"]
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

      it "fails with a --max-seconds value above maxBound Int, naming the limit" $
        withStdin rungs $
          testCLIFailed
            ["morph", "--max-seconds=9223372036854775808"]
            ["value must be between", "9223372036854775807"]

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

      it "fails once the --max-seconds budget is spent with --partial" $
        ladder $ \table ->
          bounded $
            withStdin rungs $
              testCLIFailed
                ["morph", "--symbolic=" ++ table, "--max-seconds=1", "--locator=Q.x", "--partial"]
                ["[ERROR]: Evaluation did not finish before reaching the limit of seconds: --max-seconds=1"]

      forM_ [["--locator=Q.x"], ["--locator=Q.x", "--partial"]] $ \opts ->
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
              dropWhile (== ' ') (last (lines records)) `shouldBe` "timeout 1"

      it "writes the timeout once to the XML protocol" $
        ladder $ \table ->
          withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
            hClose stream
            bounded $
              withStdin rungs $
                testCLIFailed
                  ["morph", "--symbolic=" ++ table, "--locator=Q.x", "--partial", "--max-seconds=1", "--protocol=" ++ path, "--quiet"]
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

    it "parks the spent budget as a residual with --partial" $
      withStdin "⟦ φ ↦ 5.gt(Φ.nan) ⟧" $
        testCLISucceeded
          ["morph", "--locator=Q.@", "--max-steps=10", "--partial", "--flat", "--hide-rho", "--sweet"]
          ["5.0.gt( Φ.nan )"]

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

    describe "a copy a 'defer' line of a join defers" $ do
      let joining = T.pack "- λ: L_fork\n  dataize:\n    𝛿1: $.c\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  defer:\n    𝑛3: 𝑛1\n    𝑛4: 𝑛2\n  join:\n    on: 𝛿1\n    left: 𝑛3\n    right: 𝑛4\n"
          counting = T.pack "- λ: L_inc\n  dataize:\n    𝛿1: $.x\n  𝑛: Φ.num( φ ↦ ⟦ λ ⤍ 𝜎 ⟧ )\n"
          recordedWith :: T.Text -> [String] -> String -> IO [String]
          recordedWith entries options program =
            withTempFile "protocolXXXXXX.xml" $ \(path, stream) -> do
              hClose stream
              withLambdasOf entries $ \joins ->
                withStdin program $
                  testCLIFailed (["dataize", "--symbolic=" ++ joins, "--locator=Q.y", "--protocol=" ++ path, "--quiet", "--sweet", "--hide-rho"] ++ options) ["No entry of --symbolic answers the λ function 'L_fork'"]
              lines <$> readProtocol path
          deferred :: [String] -> String -> IO [String]
          deferred = recordedWith joining
      it "writes the copy it deferred to the XML protocol as a call of the object it was made of" $ do
        records <- deferred [] "⟦ step(tup) ↦ ⟦ φ ↦ tup.next ⟧, items ↦ ⟦ tail ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧, y ↦ ⟦ c ↦ ⟦ λ ⤍ 𝜎2 ⟧, a ↦ ⟦ Δ ⤍ 01- ⟧, b ↦ Φ.step( tup ↦ Φ.items.tail ), λ ⤍ L_fork ⟧ ⟧"
        records
          `shouldBe` [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                     , "<dataize at=\"Φ.y\">"
                     , "  <evaluate λ=\"L_fork\" by=\"dataize\" at=\"Φ.y\">"
                     , "    <dataize at=\"ξ.c\">"
                     , "      <manufactured meta=\"𝛿·1·1\">𝜎2</manufactured>"
                     , "    </dataize>"
                     , "    <bound meta=\"𝛿1·1\">𝛿·1·1</bound>"
                     , "    <morph at=\"ξ.a\">"
                     , "      <morphed meta=\"𝑛·1·1\">01-</morphed>"
                     , "    </morph>"
                     , "    <bound meta=\"𝑛1·1\">01-</bound>"
                     , "    <morph at=\"ξ.b\">"
                     , "      <applied meta=\"𝑛·1·2\" by=\"morph\" at=\"ξ.b\" of=\"Φ.step\"><ref attr=\"tup\">Φ.items.tail</ref></applied>"
                     , "      <morphed meta=\"𝑛·1·3\">𝑛·1·2</morphed>"
                     , "    </morph>"
                     , "    <bound meta=\"𝑛2·1\">𝑛·1·2</bound>"
                     , "    <bound meta=\"𝑛3·1\">01-</bound>"
                     , "    <deferred symbol=\"𝜎3\" by=\"morph\" at=\"𝑛2.φ\" of=\"Φ.step\"><with><symbol attr=\"tup\">𝜎1</symbol></with></deferred>"
                     , "    <diverged left=\"𝑛3·1\" right=\"𝜎3\" at=\".Δ\" reason=\"the join names no otherwise\"/>"
                     , "  </evaluate>"
                     , "</dataize>"
                     ]

      it "names the object a deferred copy was made of through the formation its ρ holds" $ do
        records <- deferred [] "⟦ joined(items) ↦ ⟦ φ ↦ step( tup ↦ items ), step(ρ, tup) ↦ ⟦ φ ↦ tup.next ⟧ ⟧, items ↦ ⟦ tail ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧, y ↦ ⟦ c ↦ ⟦ λ ⤍ 𝜎2 ⟧, a ↦ ⟦ Δ ⤍ 01- ⟧, b ↦ Φ.joined( items ↦ Φ.items.tail ).φ, λ ⤍ L_fork ⟧ ⟧"
        records `shouldContain` ["    <deferred symbol=\"𝜎3\" by=\"morph\" at=\"𝑛2.φ\" of=\"Φ.joined.step\"><with><symbol attr=\"tup\">𝜎1</symbol></with></deferred>"]

      it "names an argument of a deferred copy that is no bare symbol by the element that formed it" $ do
        records <- deferred [] "⟦ box(x, w) ↦ ⟦ φ ↦ x.next ⟧, items ↦ ⟦ tail ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧, y ↦ ⟦ c ↦ ⟦ λ ⤍ 𝜎2 ⟧, a ↦ ⟦ Δ ⤍ 01- ⟧, b ↦ Φ.box( x ↦ Φ.items.tail, w ↦ ⟦ z ↦ Φ ⟧ ), λ ⤍ L_fork ⟧ ⟧"
        records `shouldContain` ["    <deferred symbol=\"𝜎3\" by=\"morph\" at=\"𝑛2.φ\" of=\"Φ.box\"><with><symbol attr=\"x\">𝜎1</symbol><meta attr=\"w\">𝑛·1·3</meta></with></deferred>"]

      it "names a deferred copy of a formation the world does not declare by the element that formed it" $ do
        records <- deferred [] "⟦ wrap(v) ↦ ⟦⟧, items ↦ ⟦ tail ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧, y ↦ ⟦ c ↦ ⟦ λ ⤍ 𝜎2 ⟧, a ↦ ⟦ Δ ⤍ 01- ⟧, b ↦ Φ.wrap( v ↦ ⟦ inner(x) ↦ ⟦ φ ↦ x.next ⟧ ⟧ ).v.inner( x ↦ Φ.items.tail ), λ ⤍ L_fork ⟧ ⟧"
        records `shouldContain` ["    <formed meta=\"𝑛·1·7\"><symbol attr=\"x\">𝜎1</symbol><ref attr=\"φ\">x.next</ref></formed>", "    <deferred symbol=\"𝜎3\" by=\"morph\" at=\"𝑛2.φ\" of=\"𝑛·1·7\"/>"]

      it "names an argument of a deferred copy by the object its morph answered" $ do
        records <- recordedWith (joining <> counting) [] "⟦ box(x, w) ↦ ⟦ φ ↦ x.next ⟧, num(φ) ↦ ⟦⟧, inc(x) ↦ ⟦ λ ⤍ L_inc ⟧, items ↦ ⟦ tail ↦ ⟦ λ ⤍ 𝜎1 ⟧ ⟧, y ↦ ⟦ c ↦ ⟦ λ ⤍ 𝜎2 ⟧, a ↦ ⟦ Δ ⤍ 01- ⟧, b ↦ Φ.box( x ↦ Φ.items.tail, w ↦ Φ.inc( ⟦ Δ ⤍ 2A- ⟧ ) ), λ ⤍ L_fork ⟧ ⟧"
        filter (isInfixOf "<deferred") records `shouldBe` ["    <deferred symbol=\"𝜎4\" by=\"morph\" at=\"𝑛2.φ\" of=\"Φ.box\"><with><symbol attr=\"x\">𝜎1</symbol><meta attr=\"w\">𝑛·2·3</meta></with></deferred>"]

    describe "--acyclic=plausible" $ do
      let twins = "[[ bytes ↦ ⟦ φ ↦ ∅ ⟧, number(φ) -> [[ plus(^, x) -> [[ L> L_number_plus ]] ]], pair(a, b) -> [[ L> L_pair ]], t -> Φ.pair( 7.plus( 5.plus( 6 ) ), 7.plus( 5.plus( 6 ) ) ).φ ]]"
          pairs = T.pack "- λ: L_number_plus\n  dataize:\n    𝛿1: $.ρ\n    𝛿2: $.x\n  𝑛: Φ.number( φ ↦ ⟦ λ ⤍ 𝜎 ⟧ )\n- λ: L_pair\n  morph:\n    𝑛1: $.a\n    𝑛2: $.b\n  evaluate:\n    𝑛3: 𝑛1\n    𝑛4: 𝑛2\n  𝑛: ⟦ φ ↦ ⟦ a ↦ 𝑛3, b ↦ 𝑛4 ⟧ ⟧\n"
          recorded :: String -> IO [String]
          recorded mode =
            withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
              hClose stream
              withLambdasOf pairs $ \file ->
                withStdin twins $
                  testCLISucceeded
                    ["morph", "--symbolic=" ++ file, "--locator=Q.t", "--acyclic=" ++ mode, "--protocol=" ++ path, "--quiet"]
                    []
              lines <$> readProtocol path
      it "charges a formation once per argument spelling it under proven" $
        withLambdasOf pairs $ \file ->
          withStdin twins $
            testCLIFailed
              ["morph", "--symbolic=" ++ file, "--locator=Q.t", "--acyclic=proven", "--max-firings=4"]
              ["[ERROR]: Evaluation did not finish before reaching the limit of firings: --max-firings=4"]

      it "charges a formation once for the run under plausible" $
        withLambdasOf pairs $ \file ->
          withStdin twins $
            testCLISucceeded
              ["morph", "--symbolic=" ++ file, "--locator=Q.t", "--acyclic=plausible", "--max-firings=3", "--flat", "--hide-rho", "--sweet"]
              ["b ↦ ⟦ φ ↦ 𝜎2:λ, plus(x) ↦ L_number_plus:λ ⟧"]

      it "reduces the operands of a formation once per argument spelling it under proven" $
        recorded "proven" >>= (`shouldSatisfy` ((== 4) . length . filter (isInfixOf "𝛿1·")))

      it "reduces the operands of a formation once for the run under plausible" $
        recorded "plausible" >>= (`shouldSatisfy` ((== 2) . length . filter (isInfixOf "𝛿1·")))

      it "writes a recalled firing at its own site under plausible" $
        recorded "plausible" >>= (`shouldSatisfy` ((== 3) . length . filter (isInfixOf "𝔼(L_number_plus)")))

      it "answers a recalled firing with the line of the first under plausible" $
        recorded "plausible" >>= (`shouldContain` ["      𝑛·4·1 := 𝑛·2·9  # 𝑛"])

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
              ["[ERROR]: Morphing did not finish before reaching the limit of steps: --max-steps=40"]

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
            ["⟦ x ↦ 7.0, λ ⤍ L_number_plus ⟧"]

    describe "--max-loops" $ do
      it "fires a looping λ function once more with --max-loops=1" $
        loopingLambdas $ \endless ->
          withTempFile "protocolXXXXXX.txt" $ \(path, stream) -> do
            hClose stream
            withStdin "⟦ x ↦ ⟦ λ ⤍ L_loop ⟧.foo ⟧" $
              testCLISucceeded
                ["morph", "--symbolic=" ++ endless, "--locator=Q.x", "--acyclic=proven", "--max-loops=1", "--protocol=" ++ path, "--quiet"]
                []
            records <- readProtocol path
            length (filter (isInfixOf "𝔼(L_loop)") (lines records)) `shouldBe` 2

      it "refuses the flag without --acyclic" $
        withStdin "⟦ x ↦ ⟦ λ ⤍ L_loop ⟧.foo ⟧" $
          testCLIFailed ["morph", "--locator=Q.x", "--max-loops=3"] ["The option --max-loops requires --acyclic, since only --acyclic cuts a loop"]

    it "prints a top formation of several bindings as XMIR" $
      withStdin "[[ x -> [[ D> 01- ]], y -> [[ D> 02- ]] ]]" $
        testCLISucceeded
          ["morph", "--output=xmir"]
          ["<o name=\"x\">01-</o>", "<o name=\"y\">02-</o>"]

    describe "fails" $ do
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

    it "shuffles the normalization rules away from their file order" $ do
      (plain, _) <- withStdout (runCLI ["explain", "--normalize"])
      (mixed, _) <- withStdout (runCLI ["explain", "--normalize", "--shuffle", "--seed=1"])
      mixed `shouldNotBe` plain

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

    it "keeps the type of an atom of XMIR under --canonize" $ do
      let xmir = "<object><o name=\"number\"><o name=\"plus\"><o base=\"∅\" name=\"b\"/><o atom=\"Φ.number\" name=\"λ\"/></o></o></object>"
      withTempFileContent "phino-canonized-atom.xmir" xmir $ \file ->
        testCLISucceeded
          ["rewrite", "--input=xmir", "--output=xmir", "--canonize", file]
          ["<o atom=\"Φ.number\" name=\"λ\">Fn1</o>"]

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
        writeFile (dir </> "phino.cabal") ""
        withCurrentDirectory dir (runCLI ["compile", "--target=Compiled.hs"])
        readFile (dir </> "cabal.project.local") `shouldReturn` "package phino\n  flags: +compiled\n"
    it "prints the lines an existing cabal.project.local lacks" $
      withTempDirectory "phino-compile" $ \dir -> do
        createDirectoryIfMissing True dir
        writeFile (dir </> "phino.cabal") ""
        writeFile (dir </> "cabal.project.local") "tests: True\n"
        withCurrentDirectory dir (testCLISucceeded ["compile", "--target=Compiled.hs"] ["package phino\n  flags: +compiled"])
    it "leaves an existing cabal.project.local as it is" $
      withTempDirectory "phino-compile" $ \dir -> do
        createDirectoryIfMissing True dir
        writeFile (dir </> "phino.cabal") ""
        writeFile (dir </> "cabal.project.local") "tests: True\n"
        withStdout (withCurrentDirectory dir (runCLI ["compile", "--target=Compiled.hs"]))
        readFile (dir </> "cabal.project.local") `shouldReturn` "tests: True\n"
    it "writes cabal.project.local next to the module's project, not the current directory" $
      withTempDirectory "phino-compile" $ \dir -> do
        createDirectoryIfMissing True (dir </> "project" </> "gen")
        writeFile (dir </> "project" </> "phino.cabal") ""
        createDirectoryIfMissing True (dir </> "elsewhere")
        withCurrentDirectory
          (dir </> "elsewhere")
          (runCLI ["compile", "--target=" ++ (dir </> "project" </> "gen" </> "Compiled.hs")])
        doesFileExist (dir </> "elsewhere" </> "cabal.project.local") `shouldReturn` False
        readFile (dir </> "project" </> "cabal.project.local") `shouldReturn` "package phino\n  flags: +compiled\n"
    it "only prints the lines when it cannot find the project of the module" $
      withTempDirectory "phino-compile" $ \dir -> do
        createDirectoryIfMissing True dir
        withCurrentDirectory dir (testCLISucceeded ["compile", "--target=gen/Compiled.hs"] ["package phino\n  flags: +compiled"])
        doesFileExist (dir </> "cabal.project.local") `shouldReturn` False
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

    it "describes --sweet and --flat as applying to the result only" $
      testCLISucceeded
        ["match", "--help"]
        ["Print result 𝜑-expression using syntax sugar", "Print result 𝜑-expression in one line"]

    it "rejects --sequence, which --help must not mention" $
      withStdin "[[ ]]" $
        testCLIFailed ["match", "--sequence", "--pattern=[[ !B ]]"] ["Invalid option"]

    it "takes from stdin" $
      withStdin "[[]]" $
        testCLISucceeded ["match", "--log-level=debug"] ["[DEBUG]"]

    it "takes from file" $
      testCLISucceeded ["match", resource "foo.phi", "--log-level=debug"] ["[DEBUG]"]

    it "does not print substitutions without pattern" $
      withStdin "[[]]" $
        testCLISucceeded ["match", "--log-level=debug"] ["[DEBUG]: The --pattern is not provided, no substitutions are built"]

    it "refuses --when without --pattern" $
      withStdin "[[]]" $
        testCLIFailed ["match", "--when=bogus"] ["The option --when requires --pattern"]

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

    it "numbers two anonymous captures of one kind, so they stay apart" $
      withStdin "[[ a -> [[ ]], b -> Q ]]" $
        testCLISucceeded ["match", "--pattern=[[ !t -> !e, !t -> !e ]]"] ["e#1 >> ⟦⟧\ne#2 >> Φ\nt#1 >> a\nt#2 >> b"]

    it "prints many substitutions" $
      withStdin "[[ x -> Q.x, y -> Q.y ]]" $
        testCLISucceeded ["match", "--pattern=Q.!t"] ["t >> x\n------\nt >> y"]

    it "does not match a length against a literal that wraps around Int" $
      withStdin "[[ a -> $, b -> Q ]]" $
        testCLIFailed ["match", "--pattern=[[ !B1 ]]", "--when=eq(length(!B1),18446744073709551618)"] ["no substitutions are built"]

    it "builds substitutions with conditions" $
      withStdin "[[ x -> Q.y ]].x" $
        testCLISucceeded
          ["match", "--pattern=[[ !t1 -> Q.y, !B1 ]].!t1", "--when=eq(length(!B1),0)"]
          ["B1 >> ⟦⟧\nt1 >> x"]

    it "builds with condition from file" $
      testCLISucceeded
        ["match", "--pattern=[[ !B1 ]]", "--when=eq(length(!B1),1)", resource "foo.phi"]
        ["B1 >> ⟦ foo ↦ Φ.org.eolang.x ⟧"]

    it "keeps an 'or' met when a raising item comes first" $
      withStdin "[[ s -> [[ q -> ? ]] ]]" $
        testCLISucceeded
          ["match", "--flat", "--pattern=[[ s -> !e1 ]]", "--when=or(matches(\"x\", !e1), eq(1,1))"]
          ["e1 >> ⟦ q ↦ ∅ ⟧"]

    it "keeps an 'or' met when a raising item comes last" $
      withStdin "[[ s -> [[ q -> ? ]] ]]" $
        testCLISucceeded
          ["match", "--flat", "--pattern=[[ s -> !e1 ]]", "--when=or(eq(1,1), matches(\"x\", !e1))"]
          ["e1 >> ⟦ q ↦ ∅ ⟧"]

    it "logs a raising 'or' item as not met" $
      withStdin "[[ s -> [[ q -> ? ]] ]]" $
        testCLISucceeded
          ["match", "--log-level=debug", "--pattern=[[ s -> !e1 ]]", "--when=or(matches(\"x\", !e1), eq(1,1))"]
          ["raised and was treated as not met"]

    it "rejects an anonymous meta in --when" $
      withStdin "[[ x -> Q.y ]]" $
        testCLIFailed
          ["match", "--pattern=[[ !B ]]", "--when=eq(length(!B),1)"]
          ["[ERROR]: Anonymous meta '!B' cannot be referenced in --when"]

    it "rejects a named meta in --when the pattern does not bind" $
      withStdin "[[ x -> Q.y ]]" $
        testCLIFailed
          ["match", "--pattern=[[ !B1 ]]", "--when=eq(length(!B2),1)"]
          ["[ERROR]: The --when condition reads the meta '!B2' it never binds, since neither --pattern binds it"]

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

  describe "--monoform" $ do
    forM_
      [ ("rewrite", ["rewrite", "--monoform"])
      , ("dataize", ["dataize", "--monoform"])
      , ("morph", ["morph", "--monoform"])
      , ("match", ["match", "--monoform", "--pattern=[[ !B ]]"])
      ]
      ( \(command, args) ->
          it (command ++ " rejects a program mixing ASCII and Unicode symbols") $
            withStdin "⟦ x -> ? ]]" $
              testCLIFailed args ["The ASCII symbol \"->\" at 1:5 mixes with the Unicode \"⟦\" at 1:1, while --monoform is set"]
      )
    it "merge rejects a program mixing ASCII and Unicode symbols" $
      withTempFileContent "monoformXXXXXX.phi" "⟦ x -> ? ]]" $ \path ->
        testCLIFailed ["merge", "--monoform", path] ["The ASCII symbol \"->\" at 1:5 mixes with the Unicode \"⟦\" at 1:1, while --monoform is set"]
    forM_
      [ ("rewrite accepts a program spelled in ASCII alone", "[[ x -> ? ]]", ["rewrite", "--monoform"], "⟦ x ↦ ∅ ⟧")
      , ("rewrite accepts a program spelled in Unicode alone", "⟦ x ↦ ∅ ⟧", ["rewrite", "--monoform"], "⟦ x ↦ ∅ ⟧")
      , ("rewrite accepts a program mixing ASCII and Unicode symbols without --monoform", "⟦ x -> ? ]]", ["rewrite"], "⟦ x ↦ ∅ ⟧")
      , ("dataize accepts a program spelled in ASCII alone", "[[ D> 01- ]]", ["dataize", "--monoform"], "01-")
      , ("morph accepts a program spelled in ASCII alone", "[[ x -> ? ]]", ["morph", "--monoform"], "⟦ x ↦ ∅ ⟧")
      , ("match accepts a program spelled in ASCII alone", "[[ x -> ? ]]", ["match", "--monoform", "--pattern=[[ !B ]]"], "B >> ⟦ x ↦ ∅ ⟧")
      ]
      (\(desc, input, args, expected) -> it desc (withStdin input (testCLISucceeded args [expected])))
    it "merge accepts programs spelled in ASCII alone" $
      withTempFileContent "monoformXXXXXX.phi" "[[ x -> ? ]]" $ \path ->
        testCLISucceeded ["merge", "--monoform", path] ["⟦ x ↦ ∅ ⟧"]
    it "rewrite names --monoform in its help" $
      testCLISucceeded ["rewrite", "--help"] ["--monoform"]
