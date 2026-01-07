{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLISpec (spec) where

import CLI (runCLI)
import Control.Exception
import Control.Monad (forM_, unless, when)
import Data.List (intercalate, isInfixOf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Version (showVersion)
import GHC.IO.Handle
import Paths_phino (version)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getTemporaryDirectory, listDirectory, removeDirectoryRecursive, removeFile)
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

  it "prints debug info with --log-level=DEBUG" $
    withStdin "{[[]]}" $
      testCLISucceeded ["rewrite", "--log-level=DEBUG"] ["[DEBUG]:"]

  describe "rewriting" $ do
    describe "fails" $ do
      it "with --input=latex" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--input=latex"]
            ["The value 'latex' can't be used for '--input' option"]

      it "with negative --log-lines" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--log-lines=-2"]
            ["--log-lines must be >= -1"]

      it "with negative --max-depth" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--max-depth=-1"]
            ["--max-depth must be positive"]

      it "with --normalize and --must=1" $
        withStdin "{[[ x -> [[ y -> 5 ]].y ]].x}" $
          testCLIFailed
            ["rewrite", "--max-cycles=2", "--max-depth=1", "--normalize", "--must=1"]
            ["it's expected rewriting cycles to be in range [1], but rewriting has already reached 2"]

      it "when --in-place is used without input file" $
        withStdin "Q -> [[ ]]" $
          testCLIFailed
            ["rewrite", "--in-place"]
            ["--in-place requires an input file"]

      it "when --in-place is used with --target" $
        withTempFile "inplaceXXXXXX.phi" $ \(path, h) -> do
          hPutStr h "Q -> [[ ]]"
          hClose h
          testCLIFailed
            ["rewrite", "--in-place", "--target=output.phi", path]
            ["--in-place and --target cannot be used together"]

      it "with --depth-sensitive" $
        withStdin "Q -> [[ x -> \"x\"]]" $
          testCLIFailed
            ["rewrite", "--depth-sensitive", "--max-depth=1", "--max-cycles=1", rule "infinite.yaml"]
            ["[ERROR]: With option --depth-sensitive it's expected rewriting iterations amount does not reach the limit: --max-depth=1"]

      it "with looping rules" $
        withStdin "Q -> [[ x -> \"0\" ]]" $
          testCLIFailed
            ["rewrite", rule "first.yaml", rule "second.yaml", "--max-depth=1", "--max-cycles=3"]
            ["it seems rewriting is looping"]

      it "with wrong attribute and valid error message" $
        testCLIFailed
          ["rewrite", resource "with-$this-attribute.phi"]
          [ "[ERROR]: Couldn't parse given phi program, cause: program:10:13:"
          , "10 |             $this ↦ ⟦⟧"
          , "   |             ^^"
          , "unexpected \"$t\""
          ]

      it "with --output != latex and --nonumber" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--nonumber", "--output=xmir"]
            ["The --nonumber option can stay together with --output=latex only"]

      it "with --omit-listing and --output != xmir" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--omit-listing", "--output=phi"]
            ["--omit-listing"]

      it "with --omit-comments and --output != xmir" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--omit-comments", "--output=phi"]
            ["--omit-comments"]

      it "with --expression and --output != latex" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--expression=foo", "--output=phi"]
            ["--expression option can stay together with --output=latex only"]

      it "with --label and --output != latex" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--label=foo", "--output=phi"]
            ["--label option can stay together with --output=latex only"]

      it "with --compress and --output != latex" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--compress", "--output=phi"]
            ["--compress option can stay together with --output=latex only"]

      it "with --meet-prefix and --output != latex" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--meet-prefix=foo", "--output=phi"]
            ["--meet-prefix option can stay together with --output=latex only"]

      it "with wrong --hide option" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--hide=Q.x(Q.y)"]
            ["[ERROR]: Invalid set of arguments: Only dispatch expression", "but given: Φ.x( Φ.y )"]

      it "with many --show options" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--show=Q.x.y", "--show=hello"]
            ["The option --show can be used only once"]

      it "with wrong --show option" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--show=Q.x(Q.y)"]
            ["[ERROR]:", "Only dispatch expression started with Φ (or Q) can be used in --show"]

      it "with --meet-popularity < 0" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--meet-popularity=-1"]
            ["[ERROR]:", "--meet-popularity must be positive"]

      it "with --meet-popularity > 100" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--meet-popularity=102"]
            ["[ERROR]:", "--meet-popularity must be <= 100"]

      it "with --meet-popularity and output != latex" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--meet-popularity=51", "--output=phi"]
            ["[ERROR]:", "--meet-popularity option can stay together with --output=latex only"]

      it "with --meet-length and output != latex" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--meet-length=4", "--output=phi"]
            ["[ERROR]:", "--meet-length option can stay together with --output=latex only"]

      it "with non-dispatch --focus" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--focus=Q.x(Q.y)"]
            ["[ERROR]"]

      it "with --focus!=Q and --output=XMIR" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--focus=Q.x", "--output=xmir"]
            ["[ERROR]"]

      it "with --margin < 0" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--margin=-1"]
            ["[ERROR]"]

    it "prints help" $
      testCLISucceeded
        ["rewrite", "--help"]
        ["Rewrite the 𝜑-program"]

    it "saves steps to dir with --steps-dir" $ do
      let dir = "test-steps-temp"
      dirExists <- doesDirectoryExist dir
      when dirExists (removeDirectoryRecursive dir)
      withStdin "Q -> [[ x -> \"hello\"]]" $ do
        testCLISucceeded
          ["rewrite", rule "infinite.yaml", "--max-cycles=2", "--max-depth=2", "--steps-dir=" ++ dir, "--sweet"]
          ["hello_hi_hi"]
        (`shouldBe` True) <$> doesDirectoryExist dir
        files <- listDirectory dir
        length files `shouldBe` 4
        (`shouldBe` True) <$> doesFileExist (dir ++ "/00001.phi")
        (`shouldBe` True) <$> doesFileExist (dir ++ "/00003.phi")
        removeDirectoryRecursive dir

    it "desugares without any rules flag from file" $
      testCLISucceeded
        ["rewrite", resource "desugar.phi"]
        ["Φ ↦ ⟦ foo ↦ ξ.x, ρ ↦ ∅ ⟧"]

    it "desugares with without any rules flag from stdin" $
      withStdin "{[[foo ↦ x]]}" $
        testCLISucceeded ["rewrite"] ["Φ ↦ ⟦ foo ↦ ξ.x, ρ ↦ ∅ ⟧"]

    it "rewrites with single rule" $
      withStdin "{T(x -> Q.y)}" $
        testCLISucceeded ["rewrite", "--rule=resources/dc.yaml"] ["Φ ↦ ⊥"]

    it "normalizes with --normalize flag" $
      testCLISucceeded
        ["rewrite", "--normalize", resource "normalize.phi", "--margin=25"]
        [ unlines
            [ "Φ ↦ ⟦"
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

    it "normalizes from stdin" $
      withStdin "Φ ↦ ⟦ a ↦ ⟦ b ↦ ∅ ⟧ (b ↦ [[ ]]) ⟧" $
        testCLISucceeded
          ["rewrite", "--normalize", "--margin=20"]
          [ unlines
              [ "Φ ↦ ⟦"
              , "  a ↦ ⟦"
              , "    b ↦ ⟦ ρ ↦ ∅ ⟧,"
              , "    ρ ↦ ∅"
              , "  ⟧,"
              , "  ρ ↦ ∅"
              , "⟧"
              ]
          ]

    it "rewrites with --sweet flag" $
      withStdin "Q -> [[ x -> 5]]" $
        testCLISucceeded
          ["rewrite", "--sweet"]
          ["{⟦ x ↦ 5 ⟧}"]

    it "rewrites as XMIR" $
      withStdin "Q -> [[ x -> Q.y ]]" $
        testCLISucceeded
          ["rewrite", "--output=xmir"]
          ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<object", "  <o base=\"Φ.y\" name=\"x\"/>"]

    it "rewrites as LaTeX" $
      withStdin "Q -> [[ x_o -> Q.z(y -> 5), q$ -> T, w -> $, ^ -> Q, @ -> 1, y -> \"H$@^M\", L> Fu_nc ]]" $
        testCLISucceeded
          ["rewrite", "--output=latex", "--sweet"]
          [ unlines
              [ "\\begin{phiquation}"
              , "\\Big\\{[["
              , "  |x\\char95{}o| -> Q.|z|( |y| -> 5 ),"
              , "  |q\\char36{}| -> T,"
              , "  |w| -> $,"
              , "  ^ -> Q,"
              , "  @ -> 1,"
              , "  |y| -> \"H$@^M\","
              , "  L> |Fu\\char95{}nc|"
              , "]]\\Big\\}{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "rewrites as LaTeX without numeration" $
      withStdin "Q -> [[ x -> 5 ]]" $
        testCLISucceeded
          ["rewrite", "--output=latex", "--sweet", "--nonumber", "--flat"]
          [ unlines
              [ "\\begin{phiquation*}"
              , "\\Big\\{[[ |x| -> 5 ]]\\Big\\}{.}"
              , "\\end{phiquation*}"
              ]
          ]

    it "rewrite as LaTeX with expression name" $
      withStdin "Q -> [[ x -> 5 ]]" $
        testCLISucceeded
          ["rewrite", "--output=latex", "--sweet", "--flat", "--expression=foo"]
          [ unlines
              [ "\\begin{phiquation}"
              , "\\phiExpression{foo} \\Big\\{[[ |x| -> 5 ]]\\Big\\}{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "rewrite as LaTeX with label name" $
      withStdin "Q -> [[ x -> 5 ]]" $
        testCLISucceeded
          ["rewrite", "--output=latex", "--sweet", "--flat", "--label=foo"]
          [ unlines
              [ "\\begin{phiquation}\n\\label{foo}"
              , "\\Big\\{[[ |x| -> 5 ]]\\Big\\}{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "rewrites with XMIR as input" $
      withStdin "<object><o name=\"app\"><o name=\"x\" base=\"Φ.number\"/></o></object>" $
        testCLISucceeded
          ["rewrite", "--input=xmir", "--sweet"]
          ["{⟦ app ↦ ⟦ x ↦ Φ.number ⟧ ⟧}"]

    it "rewrites as XMIR with omit-listing flag" $
      withStdin "Q -> [[ x -> Q.y ]]" $
        testCLISucceeded
          ["rewrite", "--output=xmir", "--omit-listing"]
          ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<object", "<listing>1 line(s)</listing>", "  <o base=\"Φ.y\" name=\"x\"/>"]

    it "does not fail on exactly 1 rewriting" $
      withStdin "{⟦ t ↦ ⟦ x ↦ \"foo\" ⟧ ⟧}" $
        testCLISucceeded
          ["rewrite", rule "simple.yaml", "--must=1", "--sweet"]
          ["x ↦ \"bar\""]

    it "prints many programs with --sequence" $
      withStdin "{[[ x -> \"foo\" ]]}" $
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
              [ "{⟦ x ↦ \"foo\" ⟧}"
              , "{Φ.x( y ↦ \"foo\" )}"
              , "{⟦ x ↦ \"foo\" ⟧}"
              ]
          ]

    it "prints only one latex preamble with --sequence" $
      withStdin "{[[ x -> \"foo\" ]]}" $
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
              , "\\Big\\{[[ |x| -> \"foo\" ]]\\Big\\} \\leadsto_{\\nameref{r:first}}"
              , "  \\leadsto \\Big\\{Q.|x|( |y| -> \"foo\" )\\Big\\} \\leadsto_{\\nameref{r:second}}"
              , "  \\leadsto \\Big\\{[[ |x| -> \"foo\" ]]\\Big\\}{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "prints meet prefix with --meet-prefix=foo in LaTeX" $
      withStdin "{[[ x -> ?, y -> $.x ]](x -> [[ D> 42- ]]).y}" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sweet", "--sequence", "--output=latex", "--flat", "--compress", "--meet-prefix=foo"]
          [ unlines
              [ "\\begin{phiquation}"
              , "\\Big\\{[[ |x| -> ?, |y| -> |x| ]]( |x| -> \\phiMeet{foo:1}{ [[ D> 42- ]] } ).|y|\\Big\\} \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto \\Big\\{\\phiMeet{foo:2}{ [[ |x| -> \\phiAgain{foo:1}, |y| -> |x| ]] }.|y|\\Big\\} \\leadsto_{\\nameref{r:dot}}"
              , "  \\leadsto \\Big\\{\\phiAgain{foo:2}.|x|( ^ -> \\phiAgain{foo:2} )\\Big\\} \\leadsto_{\\nameref{r:dot}}"
              , "  \\leadsto \\Big\\{\\phiAgain{foo:1}( ^ -> \\phiAgain{foo:2}, ^ -> \\phiAgain{foo:2} )\\Big\\} \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto \\Big\\{[[ D> 42-, ^ -> \\phiAgain{foo:2} ]]( ^ -> \\phiAgain{foo:2} )\\Big\\} \\leadsto_{\\nameref{r:stay}}"
              , "  \\leadsto \\Big\\{[[ D> 42-, ^ -> \\phiAgain{foo:2} ]]\\Big\\}{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "prints with compressed expressions in LaTeX" $
      withStdin "{[[ x -> ?, y -> $.x ]](x -> [[ D> 42- ]]).y}" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sweet", "--sequence", "--output=latex", "--flat", "--compress"]
          [ unlines
              [ "\\begin{phiquation}"
              , "\\Big\\{[[ |x| -> ?, |y| -> |x| ]]( |x| -> \\phiMeet{1}{ [[ D> 42- ]] } ).|y|\\Big\\} \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto \\Big\\{\\phiMeet{2}{ [[ |x| -> \\phiAgain{1}, |y| -> |x| ]] }.|y|\\Big\\} \\leadsto_{\\nameref{r:dot}}"
              , "  \\leadsto \\Big\\{\\phiAgain{2}.|x|( ^ -> \\phiAgain{2} )\\Big\\} \\leadsto_{\\nameref{r:dot}}"
              , "  \\leadsto \\Big\\{\\phiAgain{1}( ^ -> \\phiAgain{2}, ^ -> \\phiAgain{2} )\\Big\\} \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto \\Big\\{[[ D> 42-, ^ -> \\phiAgain{2} ]]( ^ -> \\phiAgain{2} )\\Big\\} \\leadsto_{\\nameref{r:stay}}"
              , "  \\leadsto \\Big\\{[[ D> 42-, ^ -> \\phiAgain{2} ]]\\Big\\}{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "should not print \\phiMeet{} twice" $
      withStdin "{[[ ex -> [[ x -> [[ y -> ?, k -> [[ t -> 42]]  ]]( y -> [[ t -> 42 ]]) ]].i ]]}" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sequence", "--flat", "--compress", "--output=latex", "--sweet"]
          [ unlines
              [ "\\begin{phiquation}"
              , "\\Big\\{[[ |ex| -> [[ |x| -> [[ |y| -> ?, |k| -> \\phiMeet{1}{ [[ |t| -> 42 ]] } ]]( |y| -> \\phiAgain{1} ) ]].|i| ]]\\Big\\} \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto \\Big\\{[[ |ex| -> [[ |x| -> [[ |y| -> \\phiAgain{1}, |k| -> \\phiAgain{1} ]] ]].|i| ]]\\Big\\} \\leadsto_{\\nameref{r:stop}}"
              , "  \\leadsto \\Big\\{[[ |ex| -> T ]]\\Big\\}{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "should not meet expression with high --meet-popularity" $
      withStdin "{[[ ex -> [[ x -> [[ y -> ?, k -> [[ t -> 42]]  ]]( y -> [[ t -> 42 ]]) ]].i ]]}" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sequence", "--flat", "--compress", "--output=latex", "--sweet", "--meet-popularity=70"]
          [ unlines
              [ "\\begin{phiquation}"
              , "\\Big\\{[[ |ex| -> [[ |x| -> [[ |y| -> ?, |k| -> [[ |t| -> 42 ]] ]]( |y| -> [[ |t| -> 42 ]] ) ]].|i| ]]\\Big\\} \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto \\Big\\{[[ |ex| -> [[ |x| -> [[ |y| -> [[ |t| -> 42 ]], |k| -> [[ |t| -> 42 ]] ]] ]].|i| ]]\\Big\\} \\leadsto_{\\nameref{r:stop}}"
              , "  \\leadsto \\Big\\{[[ |ex| -> T ]]\\Big\\}{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "meets with --meet-length=32" $
      withStdin "{[[ ex -> [[ x -> [[ y -> ?, k -> [[ t -> 42]]  ]]( y -> [[ t -> 42 ]]) ]].i ]]}" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sequence", "--flat", "--compress", "--output=latex", "--sweet", "--meet-length=32"]
          [ unlines
              [ "\\begin{phiquation}"
              , "\\Big\\{[[ |ex| -> [[ |x| -> [[ |y| -> ?, |k| -> [[ |t| -> 42 ]] ]]( |y| -> [[ |t| -> 42 ]] ) ]].|i| ]]\\Big\\} \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto \\Big\\{[[ |ex| -> [[ |x| -> [[ |y| -> [[ |t| -> 42 ]], |k| -> [[ |t| -> 42 ]] ]] ]].|i| ]]\\Big\\} \\leadsto_{\\nameref{r:stop}}"
              , "  \\leadsto \\Big\\{[[ |ex| -> T ]]\\Big\\}{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "focuses expression in latex with sequence" $
      withStdin "{[[ ex -> [[ x -> [[ y -> ?, k -> [[ t -> 42]]  ]]( y -> [[ t -> 42 ]]) ]].i ]]}" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sequence", "--flat", "--output=latex", "--sweet", "--focus=Q.ex"]
          [ unlines
              [ "\\begin{phiquation}"
              , "[[ |x| -> [[ |y| -> ?, |k| -> [[ |t| -> 42 ]] ]]( |y| -> [[ |t| -> 42 ]] ) ]].|i| \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto [[ |x| -> [[ |y| -> [[ |t| -> 42 ]], |k| -> [[ |t| -> 42 ]] ]] ]].|i| \\leadsto_{\\nameref{r:stop}}"
              , "  \\leadsto T{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "focuses expression in latex without sequence" $
      withStdin "{[[ ex -> [[ x -> [[ y -> ?, k -> [[ t -> 42]]  ]]( y -> [[ t -> 42 ]]) ]].i ]]}" $
        testCLISucceeded
          ["rewrite", "--normalize", "--flat", "--output=latex", "--sweet", "--focus=Q.ex"]
          [ unlines
              [ "\\begin{phiquation}"
              , "T{.}"
              , "\\end{phiquation}"
              ]
          ]

    it "focuses expression in phi without sequence" $
      withStdin "{[[ ex -> [[ x -> [[ y -> ?, k -> [[ t -> 42]]  ]]( y -> [[ t -> 42 ]]) ]].i ]]}" $
        testCLISucceeded
          ["rewrite", "--normalize", "--flat", "--output=phi", "--sweet", "--focus=Q.ex"]
          ["⊥"]

    it "focuses expression in phi with sequence" $
      withStdin "{[[ ex -> [[ x -> [[ y -> ?, k -> [[ t -> 42]]  ]]( y -> [[ t -> 42 ]]) ]].i ]]}" $
        testCLISucceeded
          ["rewrite", "--normalize", "--sequence", "--flat", "--output=phi", "--sweet", "--focus=Q.ex"]
          [ unlines
              [ "⟦ x ↦ ⟦ y ↦ ∅, k ↦ ⟦ t ↦ 42 ⟧ ⟧( y ↦ ⟦ t ↦ 42 ⟧ ) ⟧.i"
              , "⟦ x ↦ ⟦ y ↦ ⟦ t ↦ 42 ⟧, k ↦ ⟦ t ↦ 42 ⟧ ⟧ ⟧.i"
              , "⊥"
              ]
          ]

    it "prints input as listing in XMIR" $
      withStdin "{[[ app -> [[]] ]]}" $
        testCLISucceeded
          ["rewrite", "--output=xmir", "--omit-comments", "--sweet", "--flat"]
          ["  <listing>{[[ app -> [[]] ]]}</listing>"]

    it "print program in listing in XMIRs with --sequence" $
      withStdin "{[[ x -> \"foo\" ]]}" $
        testCLISucceeded
          ["rewrite", "--output=xmir", "--omit-comments", "--sweet", "--flat", "--sequence", rule "simple.yaml"]
          ["  <listing>{⟦ x ↦ \"foo\" ⟧}</listing>", "  <listing>{⟦ x ↦ \"bar\" ⟧}</listing>"]

    describe "must range tests" $ do
      describe "fails" $ do
        it "when cycles exceed range ..1" $
          withStdin "Q -> [[ x -> [[ y -> 5 ]].y ]].x" $
            testCLIFailed
              ["rewrite", "--max-depth=1", "--max-cycles=2", "--normalize", "--must=..1"]
              ["it's expected rewriting cycles to be in range [..1], but rewriting has already reached 2"]

        it "when cycles below range 2.." $
          withStdin "{⟦ t ↦ ⟦ x ↦ \"foo\" ⟧ ⟧}" $
            testCLIFailed
              ["rewrite", rule "simple.yaml", "--must=2.."]
              ["it's expected rewriting cycles to be in range [2..], but rewriting stopped after 1"]

        it "with invalid range 5..3" $
          withStdin "Q -> [[ ]]" $
            testCLIFailed
              ["rewrite", "--must=5..3"]
              ["cannot parse value `5..3'"]

        it "with negative in range -1..5" $
          withStdin "Q -> [[ ]]" $
            testCLIFailed
              ["rewrite", "--must=-1..5"]
              ["cannot parse value `-1..5'"]

        it "with malformed range syntax" $
          withStdin "Q -> [[ ]]" $
            testCLIFailed
              ["rewrite", "--must=3...5"]
              ["cannot parse value `3...5'"]

      it "accepts range ..5 (0 to 5 cycles)" $
        withStdin "Q -> [[ ]]" $
          testCLISucceeded ["rewrite", "--must=..5", "--sweet"] ["{⟦⟧}"]

      it "accepts range 0..0 (exactly 0 cycles)" $
        withStdin "Q -> [[ ]]" $
          testCLISucceeded ["rewrite", "--must=0..0", "--sweet"] ["{⟦⟧}"]

      it "accepts range 1..1 (exactly 1 cycle)" $
        withStdin "{⟦ t ↦ ⟦ x ↦ \"foo\" ⟧ ⟧}" $
          testCLISucceeded
            ["rewrite", rule "simple.yaml", "--must=1..1", "--sweet"]
            ["x ↦ \"bar\""]

      it "accepts range 1..3 when 1 cycle happens" $
        withStdin "{⟦ t ↦ ⟦ x ↦ \"foo\" ⟧ ⟧}" $
          testCLISucceeded
            ["rewrite", rule "simple.yaml", "--must=1..3", "--sweet"]
            ["x ↦ \"bar\""]

      it "accepts range 0.. (0 or more)" $
        withStdin "Q -> [[ ]]" $
          testCLISucceeded ["rewrite", "--must=0..", "--sweet"] ["{⟦⟧}"]

    it "prints to target file" $
      withStdin "Q -> [[ ]]" $
        withTempFile "targetXXXXXX.tmp" $ \(path, h) -> do
          hClose h
          testCLISucceeded ["rewrite", "--sweet", printf "--target=%s" path] []
          content <- readFile path
          content `shouldBe` "{⟦⟧}"

    it "modifies file in-place" $
      withTempFile "inplaceXXXXXX.phi" $ \(path, h) -> do
        hPutStr h "Q -> [[ x -> \"foo\" ]]"
        hClose h
        testCLISucceeded ["rewrite", rule "simple.yaml", "--in-place", "--sweet", path] []
        content <- readFile path
        content `shouldBe` "{⟦ x ↦ \"bar\" ⟧}"

    it "rewrites with cycles" $
      withStdin "Q -> [[ x -> \"x\" ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", rule "infinite.yaml", "--max-depth=1", "--max-cycles=2"]
          ["{⟦ x ↦ \"x_hi_hi\" ⟧}"]

    it "hides default package" $
      withStdin "{[[ org -> [[ eolang -> [[ number -> [[]] ]]]], x -> 42 ]]}" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--hide=Q.org"]
          ["{⟦ x ↦ 42 ⟧}"]

    it "hides several FQNs" $
      withStdin "{[[ org -> [[ eolang -> Q.x, yegor256 -> Q.y ]], x -> 42 ]]}" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--hide=Q.org.eolang", "--hide=Q.org.yegor256"]
          ["{⟦ org ↦ ⟦⟧, x ↦ 42 ⟧}"]

    it "shows and hides" $
      withStdin "{[[ org -> [[ eolang -> Q.x, yegor256 -> Q.y ]], x -> 42 ]]}" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--show=Q.org", "--hide=Q.org.eolang"]
          ["{⟦ org ↦ ⟦ yegor256 ↦ Φ.y ⟧ ⟧}"]

    it "prints in line with --flat" $
      withStdin "Q -> [[ x -> 5, y -> \"hey\", z -> [[ w -> [[ ]] ]] ]]" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat"]
          ["{⟦ x ↦ 5, y ↦ \"hey\", z ↦ ⟦ w ↦ ⟦⟧ ⟧ ⟧}"]

    it "removes unnecessary rho bindings in primitive applications" $
      withStdin
        ( unlines
            [ "{[["
            , "  z -> [[ x -> [[ t -> 42 ]].t ]].x,"
            , "  org -> [[ eolang -> [[ bytes -> [[ data -> ? ]], number -> [[ as-bytes -> ? ]] ]] ]]"
            , "]]}"
            ]
        )
        ( testCLISucceeded
            ["rewrite", "--sweet", "--normalize", "--flat"]
            ["{⟦ z ↦ 42, org ↦ ⟦ eolang ↦ ⟦ bytes(data) ↦ ⟦⟧, number(as-bytes) ↦ ⟦⟧ ⟧ ⟧ ⟧}"]
        )

    it "reduces log message" $
      withStdin "{[[ x -> [[ y -> ? ]](y -> 5) ]]}" $
        testCLISucceeded
          ["rewrite", "--log-level=debug", "--log-lines=1", "--normalize"]
          [ intercalate
              "\n"
              [ "[DEBUG]: Applied 'COPY' (44 nodes -> 39 nodes)"
              , "---| log is limited by --log-lines=1 option |---"
              ]
          ]

    it "canonizes program" $
      withStdin "{[[ x -> [[ y -> [[ L> Func ]].q, z -> Q.x(a -> [[ w -> [[ L> Atom ]], L> Hello ]]) ]], L> Package ]]}" $
        testCLISucceeded
          ["rewrite", "--canonize", "--sweet", "--flat"]
          ["{⟦ x ↦ ⟦ y ↦ ⟦ λ ⤍ F1 ⟧.q, z ↦ Φ.x( a ↦ ⟦ w ↦ ⟦ λ ⤍ F2 ⟧, λ ⤍ F3 ⟧ ) ⟧, λ ⤍ F4 ⟧}"]

    it "rewrites by locator" $
      withStdin "{[[ ex -> [[ x -> [[ y -> 5 ]].y ]], abc -> [[ x -> ? ]](x -> 5) ]]}" $
        testCLISucceeded
          ["rewrite", "--sweet", "--flat", "--locator=Q.ex", "--normalize"]
          ["{⟦ ex ↦ ⟦ x ↦ 5 ⟧, abc ↦ ⟦ x ↦ ∅ ⟧( x ↦ 5 ) ⟧}"]

  describe "dataize" $ do
    it "prints help" $
      testCLISucceeded ["dataize", "--help"] ["Dataize the 𝜑-program"]

    it "dataizes simple program" $
      withStdin "Q -> [[ D> 01- ]]" $
        testCLISucceeded ["dataize"] ["01-"]

    it "dataizes to dead" $
      withStdin "Q -> [[ ]]" $
        testCLISucceeded ["dataize"] ["⊥"]

    it "dataizes with --sequence" $
      withStdin "{[[ @ -> [[ x -> [[ D> 01-, y -> ? ]](y -> [[ ]]) ]].x ]]}" $
        testCLISucceeded
          ["dataize", "--sequence", "--output=latex", "--flat", "--sweet"]
          [ intercalate
              "\n"
              [ "\\begin{phiquation}"
              , "\\Big\\{[[ @ -> [[ |x| -> [[ D> 01-, |y| -> ? ]]( |y| -> [[]] ) ]].|x| ]]\\Big\\} \\leadsto_{\\nameref{r:contextualize}}"
              , "  \\leadsto \\Big\\{[[ |x| -> [[ D> 01-, |y| -> ? ]]( |y| -> [[]] ) ]].|x|\\Big\\} \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto \\Big\\{[[ |x| -> [[ D> 01-, |y| -> [[]] ]] ]].|x|\\Big\\} \\leadsto_{\\nameref{r:dot}}"
              , "  \\leadsto \\Big\\{[[ D> 01-, |y| -> [[]] ]]( ^ -> [[ |x| -> [[ D> 01-, |y| -> [[]] ]] ]] )\\Big\\} \\leadsto_{\\nameref{r:copy}}"
              , "  \\leadsto \\Big\\{[[ D> 01-, |y| -> [[]], ^ -> [[ |x| -> [[ D> 01-, |y| -> [[]] ]] ]] ]]\\Big\\} \\leadsto_{\\nameref{r:Mprim}}"
              , "  \\leadsto \\Big\\{[[ D> 01-, |y| -> [[]], ^ -> [[ |x| -> [[ D> 01-, |y| -> [[]] ]] ]] ]]\\Big\\}{.}"
              , "\\end{phiquation}"
              , "01-"
              ]
          ]

    it "dataizes with --locator" $
      withStdin "{[[ ex -> [[ @ -> Q.x ]], x -> [[ D> 42- ]] ]]}" $
        testCLISucceeded ["dataize", "--locator=Q.ex"] ["42-"]

    it "does not print bytes with --quiet" $
      withStdin "Q -> [[ D> 01- ]]" $
        testCLISucceeded ["dataize", "--quiet"] []

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

  describe "explain" $ do
    it "explains single rule" $
      testCLISucceeded
        ["explain", "--rule=resources/copy.yaml"]
        ["\\documentclass{article}", "\\usepackage{amsmath}", "\\begin{document}", "\\rule{COPY}", "\\end{document}"]

    it "explains multiple rules" $
      testCLISucceeded
        ["explain", "--rule=resources/copy.yaml", "--rule=resources/alpha.yaml"]
        ["\\documentclass{article}", "\\rule{COPY}", "\\rule{ALPHA}"]

    it "explains normalization rules" $
      testCLISucceeded
        ["explain", "--normalize"]
        ["\\documentclass{article}", "\\begin{document}", "\\end{document}"]

    it "fails with no rules specified" $
      testCLIFailed
        ["explain"]
        ["Either --rule or --normalize must be specified"]

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
            content `shouldContain` "\\documentclass{article}"
            content `shouldContain` "\\begin{document}"
        )

  describe "merge" $ do
    it "merges single program" $
      testCLISucceeded
        ["merge", resource "desugar.phi", "--sweet", "--flat"]
        ["{⟦ foo ↦ x ⟧}"]

    it "merges EO programs" $
      testCLISucceeded
        ["merge", "--sweet", resource "number.phi", resource "bytes.phi", resource "string.phi", "--margin=25"]
        [ unlines
            [ "{⟦"
            , "  org ↦ ⟦"
            , "    eolang ↦ ⟦"
            , "      number(φ) ↦ ⟦⟧,"
            , "      bytes(data) ↦ ⟦⟧,"
            , "      string(φ) ↦ ⟦⟧,"
            , "      λ ⤍ Package"
            , "    ⟧,"
            , "    λ ⤍ Package"
            , "  ⟧"
            , "⟧}"
            ]
        ]

    it "fails on merging non formations" $
      testCLIFailed
        ["merge", resource "dispatch.phi", resource "number.phi"]
        ["Invalid program format, only programs with top level formations are supported for 'merge' command"]

    it "fails on merging conflicted bindings" $
      testCLIFailed
        ["merge", resource "foo.phi", resource "desugar.phi"]
        ["Can't merge two bindings, conflict found"]

    it "fails on merging empty list of programs" $
      testCLIFailed
        ["merge"]
        ["At least one input file must be specified for 'merge' command"]

  describe "match" $ do
    it "takes from stdin" $
      withStdin "{[[]]}" $
        testCLISucceeded ["match", "--log-level=debug"] ["[DEBUG]"]

    it "takes from file" $
      testCLISucceeded ["match", "test-resources/cli/foo.phi", "--log-level=debug"] ["[DEBUG]"]

    it "does not print substitutions without pattern" $
      withStdin "{[[]]}" $
        testCLISucceeded ["match", "--log-level=debug"] ["[DEBUG]: The --pattern is not provided, no substitutions are built"]

    it "prints one substitution" $
      withStdin "{[[ x -> Q.x ]]}" $
        testCLISucceeded ["match", "--pattern=Q.!a"] ["a >> x"]

    it "prints many substitutions" $
      withStdin "{[[ x -> Q.x, y -> Q.y ]]}" $
        testCLISucceeded ["match", "--pattern=Q.!a"] ["a >> x\n------\na >> y"]

    it "builds substitutions with conditions" $
      withStdin "{[[ x -> Q.y ]].x}" $
        testCLISucceeded
          ["match", "--pattern=[[ !a -> Q.y, !B ]].!a", "--when=and(not(alpha(!a)),eq(length(!B),1))"]
          ["B >> ⟦ ρ ↦ ∅ ⟧\na >> x"]

    it "builds with condition from file" $
      testCLISucceeded
        ["match", "--pattern=[[ !B ]]", "--when=eq(length(!B),2)", "test-resources/cli/foo.phi"]
        ["B >> ⟦ foo ↦ Φ.org.eolang.x, ρ ↦ ∅ ⟧"]

    it "fails on parsing --when condition" $
      withStdin "{[[]]}" $
        testCLIFailed
          ["match", "--pattern=[[!B]]", "--when=hello"]
          ["[ERROR]: Couldn't parse given condition"]
