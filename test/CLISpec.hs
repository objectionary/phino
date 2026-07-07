{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLISpec (spec) where

import CLI (runCLI)
import Control.Exception
import Control.Monad (forM_, unless, when)
import Data.List (intercalate, isInfixOf)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Version (showVersion)
import GHC.IO.Handle
import Paths_phino (version)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getTemporaryDirectory, listDirectory, removeDirectoryRecursive, removeFile, setModificationTime)
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

  describe "--pin" $ do
    it "succeeds when --pin matches actual version" $
      withStdin "[[ ]]" $
        testCLISucceeded
          ["--pin=" ++ showVersion version, "rewrite", "--sweet"]
          ["⟦⟧"]

    it "fails when --pin doesn't match actual version" $
      withStdin "[[ ]]" $
        testCLIFailed
          ["--pin=9.9.9.9", "rewrite"]
          ["Version mismatch: --pin requires '9.9.9.9', but this is phino " ++ showVersion version]

    it "fails when --pin is empty" $
      withStdin "[[ ]]" $
        testCLIFailed
          ["--pin=", "rewrite"]
          ["Version mismatch: --pin requires ''"]

  it "prints debug info with --log-level=DEBUG" $
    withStdin "[[]]" $
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
        withStdin "[[ x -> [[ y -> 5 ]].y ]].x" $
          testCLIFailed
            ["rewrite", "--max-cycles=2", "--max-depth=1", "--normalize", "--must=1"]
            ["it's expected rewriting cycles to be in range [1], but rewriting has already reached 2"]

      it "when --in-place is used without input file" $
        withStdin "[[ ]]" $
          testCLIFailed
            ["rewrite", "--in-place"]
            ["--in-place requires an input file"]

      it "when --in-place is used with --target" $
        withTempFile "inplaceXXXXXX.phi" $ \(path, h) -> do
          hPutStr h "[[ ]]"
          hClose h
          testCLIFailed
            ["rewrite", "--in-place", "--target=output.phi", path]
            ["--in-place and --target cannot be used together"]

      it "when --update is used without --target" $
        withStdin "[[ ]]" $
          testCLIFailed
            ["rewrite", "--update"]
            ["--update requires --target"]

      it "when --update is used without an input file" $
        withStdin "[[ ]]" $
          testCLIFailed
            ["rewrite", "--update", "--target=output.phi"]
            ["--update requires an input file"]

      it "when --update is used with --in-place" $
        withStdin "[[ ]]" $
          testCLIFailed
            ["rewrite", "--update", "--in-place", "input.phi"]
            ["--update and --in-place cannot be used together"]

      it "with --depth-sensitive" $
        withStdin "[[ x -> \"x\"]]" $
          testCLIFailed
            ["rewrite", "--depth-sensitive", "--max-depth=1", "--max-cycles=1", rule "infinite.yaml"]
            ["[ERROR]: With option --depth-sensitive it's expected rewriting iterations amount does not reach the limit: --max-depth=1"]

      it "with looping rules" $
        withStdin "[[ x -> \"0\" ]]" $
          testCLIFailed
            ["rewrite", rule "first.yaml", rule "second.yaml", "--max-depth=1", "--max-cycles=3"]
            ["it seems rewriting is looping"]

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

      it "with --breakpoint which does not exist across the rules" $
        withStdin "" $
          testCLIFailed
            ["rewrite", "--breakpoint=hello", "--normalize"]
            ["[ERROR]"]

    it "prints help" $
      testCLISucceeded
        ["rewrite", "--help"]
        ["Rewrite the 𝜑-expression"]

    it "saves steps to dir with --steps-dir" $ do
      let dir = "test-steps-temp"
      dirExists <- doesDirectoryExist dir
      when dirExists (removeDirectoryRecursive dir)
      withStdin "[[ x -> \"hello\"]]" $ do
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
        ["⟦ foo ↦ ξ.x, ρ ↦ ∅ ⟧"]

    it "desugares with without any rules flag from stdin" $
      withStdin "[[foo ↦ x]]" $
        testCLISucceeded ["rewrite"] ["⟦ foo ↦ ξ.x, ρ ↦ ∅ ⟧"]

    it "rewrites with single rule" $
      withStdin "T(x -> Q.y)" $
        testCLISucceeded ["rewrite", "--rule=resources/normalize/dc.yaml"] ["⊥"]

    it "fails when a rewriting rule uses a dataization-only function" $
      withStdin "⟦⟧" $
        testCLIFailed
          ["rewrite", rule "evaluate-in-rewrite.yaml"]
          ["Function 'evaluate' in rule 'uses-evaluate' is available only for dataization and morphing, not for rewriting"]

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

    it "canonizes expression" $
      withStdin "[[ x -> [[ y -> [[ L> Func ]].q, z -> Q.x(a -> [[ w -> [[ L> Atom ]], L> Hello ]]) ]], L> Package ]]" $
        testCLISucceeded
          ["rewrite", "--canonize", "--sweet", "--flat"]
          ["⟦ x ↦ ⟦ y ↦ ⟦ λ ⤍ F1 ⟧.q, z ↦ Φ.x( a ↦ ⟦ w ↦ ⟦ λ ⤍ F2 ⟧, λ ⤍ F3 ⟧ ) ⟧, λ ⤍ F4 ⟧"]

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

    it "fails to dataize an empty object, which dataizes the terminator ⊥" $
      withStdin "[[ ]]" $
        testCLIFailed ["dataize"] ["terminator ⊥"]

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
              , "  \\leadsto [[ D> |01-|, |y| -> [[]], \\phiTerminal{\\rho} -> [[ |x| -> [[ D> |01-|, |y| -> [[]] ]] ]] ]]{.}"
              , "\\end{phiquation}"
              , "01-"
              ]
          ]

    it "focuses a compressed sequence whose meet replaces a step root" $
      withStdin "[[ @ -> [[ @ -> $.c.plus( 32.0 ), c -> 25.0 ]], bytes(data) -> [[ @ -> $.data ]], number(as-bytes) -> [[ @ -> $.as-bytes, plus -> [[ x -> ?, L> L_number_plus ]] ]] ]]" $
        testCLISucceeded
          ["dataize", "--output=latex", "--sweet", "--nonumber", "--compress", "--canonize", "--meet-prefix=dataization", "--sequence", "--flat", "--quiet", "--hide=Q.bytes", "--hide=Q.number", "--locator=Q.@", "--focus=Q.@", "--meet-length=5", "--meet-popularity=1"]
          ["\\phinoMeet{dataization:1}{ [[ @ -> |c| . |plus| ( 32 ), |c| -> 25 ]] } \\leadsto_{\\nameref{r:contextualize}}"]

    it "compresses a canonized whole-expression sequence into a meet" $
      withStdin "[[ @ -> [[ @ -> $.c.plus( 32.0 ), c -> 25.0 ]], bytes(data) -> [[ @ -> $.data ]], number(as-bytes) -> [[ @ -> $.as-bytes, plus -> [[ x -> ?, L> L_number_plus ]] ]] ]]" $
        testCLISucceeded
          ["dataize", "--output=latex", "--sweet", "--nonumber", "--compress", "--canonize", "--meet-prefix=dataization", "--sequence", "--flat", "--quiet", "--meet-length=5", "--meet-popularity=1"]
          ["\\phinoMeet{dataization:1}"]

    it "dataizes with --locator" $
      withStdin "[[ ex -> [[ @ -> Q.x ]], x -> [[ D> 42- ]] ]]" $
        testCLISucceeded ["dataize", "--locator=Q.ex"] ["42-"]

    it "does not print bytes with --quiet" $
      withStdin "[[ D> 01- ]]" $
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
        ["explain", "--rule=resources/normalize/copy.yaml"]
        [ unlines
            [ "\\phinoNormalizationRule{copy}"
            , "  { [[ B_1, \\tau -> ?, B_2 ]] ( \\tau -> k ) }"
            , "  { [[ B_1, \\tau -> k, B_2 ]] }"
            , "  { }"
            , "  { }"
            ]
        ]

    it "explains single rule with a label" $
      testCLISucceeded
        ["explain", "--rule=test-resources/cli/labeled.yaml"]
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

    it "explains normalization rules" $
      testCLISucceeded
        ["explain", "--normalize"]
        [ unlines
            [ "\\phinoNormalizationRule{alpha}"
            , "  { [[ B_1, \\tau -> ?, B_2 ]] ( \\phiTerminal{\\alpha_{i}} -> e ) }"
            , "  { [[ B_1, \\tau -> ?, B_2 ]] ( \\tau -> e ) }"
            , "  { i = \\vert \\overline{ B_1 } \\vert }"
            , "  { }"
            , "\\phinoNormalizationRule{amiss}"
            , "  { [[ B ]] ( \\phiTerminal{\\alpha_{i}} -> e ) }"
            , "  { T }"
            , "  { \\vert \\overline{ B } \\vert \\leq i }"
            , "  { }"
            , "\\phinoNormalizationRule{copy}"
            , "  { [[ B_1, \\tau -> ?, B_2 ]] ( \\tau -> k ) }"
            , "  { [[ B_1, \\tau -> k, B_2 ]] }"
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
            , "  { [[ B_1, \\tau -> n, B_2 ]] . \\tau }"
            , "  { e ( \\phiTerminal{\\rho} -> [[ B_1, \\tau -> n, B_2 ]] ) }"
            , "  { }"
            , "  { \\phinoContextualize{ n }{ [[ B_1, B_2 ]] }{ e } }"
            , "\\phinoNormalizationRule{miss}"
            , "  { [[ B ]] ( \\tau -> e ) }"
            , "  { T }"
            , "  { \\tau \\notin B }"
            , "  { }"
            , "\\phinoNormalizationRule{null}"
            , "  { [[ B_1, \\tau -> ?, B_2 ]] . \\tau }"
            , "  { T }"
            , "  { }"
            , "  { }"
            , "\\phinoNormalizationRule{over}"
            , "  { [[ B_1, \\tau -> e_1, B_2 ]] ( \\tau -> e_2 ) }"
            , "  { T }"
            , "  { \\tau \\not= \\phiTerminal{\\rho} }"
            , "  { }"
            , "\\phinoNormalizationRule{overa}"
            , "  { [[ B_1, \\tau -> e_1, B_2 ]] ( \\phiTerminal{\\alpha_{i}} -> e_2 ) }"
            , "  { T }"
            , "  { i = \\vert \\overline{ B_1 } \\vert \\;\\text{and}\\; \\tau \\not= \\phiTerminal{\\rho} }"
            , "  { }"
            , "\\phinoNormalizationRule{stay}"
            , "  { [[ B_1, \\phiTerminal{\\rho} -> e_1, B_2 ]] ( \\phiTerminal{\\rho} -> e_2 ) }"
            , "  { [[ B_1, \\phiTerminal{\\rho} -> e_1, B_2 ]] }"
            , "  { }"
            , "  { }"
            , "\\phinoNormalizationRule{stop}"
            , "  { [[ B ]] . \\tau }"
            , "  { T }"
            , "  { \\tau \\notin B \\;\\text{and}\\; @ \\notin B \\;\\text{and}\\; L \\notin B }"
            , "  { }"
            ]
        ]

    it "explains morphing rules" $
      testCLISucceeded
        ["explain", "--morph"]
        [ unlines
            [ "\\begin{phinoMorphingInference}"
            , "  \\phinoName{mf}"
            , "  \\phinoConclusion{ \\phinoMorph{ [[ B ]] }{ e }{ s }{ [[ B ]] }{ s } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{ml}"
            , "  \\phinoLabel{\\lambda}"
            , "  \\phinoPremise{ \\phinoEvaluate{ [[ B_1, L> F, B_2 ]] }{ e }{ s_1 }{ e_1 }{ s_2 } }"
            , "  \\phinoPremise{ \\phinoNormalize{ e_1 . \\tau }{ n_1 } }"
            , "  \\phinoPremise{ \\phinoMorph{ n_1 }{ e }{ s_2 }{ n_2 }{ s_3 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ [[ B_1, L> F, B_2 ]] . \\tau }{ e }{ s_1 }{ n_2 }{ s_3 } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{mphi}"
            , "  \\phinoLabel{\\varphi}"
            , "  \\phinoCondition{ @ \\in B \\;\\text{and}\\; \\tau \\notin B \\;\\text{and}\\; L \\notin B }"
            , "  \\phinoPremise{ \\phinoNormalize{ [[ B ]] . @ . \\tau }{ n } }"
            , "  \\phinoPremise{ \\phinoMorph{ n }{ e }{ s_1 }{ n_1 }{ s_2 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ [[ B ]] . \\tau }{ e }{ s_1 }{ n_1 }{ s_2 } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{md}"
            , "  \\phinoCondition{ \\phinoNotFormation{ n } }"
            , "  \\phinoPremise{ \\phinoMorph{ n }{ e }{ s_1 }{ n_1 }{ s_2 } }"
            , "  \\phinoPremise{ \\phinoNormalize{ n_1 . \\tau }{ n_2 } }"
            , "  \\phinoPremise{ \\phinoMorph{ n_2 }{ e }{ s_2 }{ n_3 }{ s_3 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ n . \\tau }{ e }{ s_1 }{ n_3 }{ s_3 } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{ma}"
            , "  \\phinoPremise{ \\phinoMorph{ n }{ e }{ s_1 }{ n_1 }{ s_2 } }"
            , "  \\phinoPremise{ \\phinoNormalize{ n_1 ( \\tau -> k_1 ) }{ n_2 } }"
            , "  \\phinoPremise{ \\phinoMorph{ n_2 }{ e }{ s_2 }{ n_3 }{ s_3 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ n ( \\tau -> k_1 ) }{ e }{ s_1 }{ n_3 }{ s_3 } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{maa}"
            , "  \\phinoPremise{ \\phinoMorph{ n }{ e }{ s_1 }{ n_1 }{ s_2 } }"
            , "  \\phinoPremise{ \\phinoNormalize{ n_1 ( \\phiTerminal{\\alpha_{i}} -> k_1 ) }{ n_2 } }"
            , "  \\phinoPremise{ \\phinoMorph{ n_2 }{ e }{ s_2 }{ n_3 }{ s_3 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ n ( \\phiTerminal{\\alpha_{i}} -> k_1 ) }{ e }{ s_1 }{ n_3 }{ s_3 } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{mad}"
            , "  \\phinoCondition{ \\phinoNotAbsolute{ n_1 } }"
            , "  \\phinoPremise{ \\phinoMorph{ T }{ e }{ s_1 }{ n_2 }{ s_2 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ n ( \\tau -> n_1 ) }{ e }{ s_1 }{ n_2 }{ s_2 } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{maad}"
            , "  \\phinoCondition{ \\phinoNotAbsolute{ n_1 } }"
            , "  \\phinoPremise{ \\phinoMorph{ T }{ e }{ s_1 }{ n_2 }{ s_2 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ n ( \\phiTerminal{\\alpha_{i}} -> n_1 ) }{ e }{ s_1 }{ n_2 }{ s_2 } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{universe}"
            , "  \\phinoLabel{\\Phi}"
            , "  \\phinoCondition{ e \\not= Q }"
            , "  \\phinoPremise{ \\phinoNormalize{ e }{ n } }"
            , "  \\phinoPremise{ \\phinoMorph{ n }{ e }{ s_1 }{ n_1 }{ s_2 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ Q }{ e }{ s_1 }{ n_1 }{ s_2 } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{dead}"
            , "  \\phinoConclusion{ \\phinoMorph{ T }{ e }{ s }{ T }{ s } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{xi}"
            , "  \\phinoPremise{ \\phinoMorph{ T }{ e }{ s_1 }{ n }{ s_2 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ \\phiTerminal{\\xi} }{ e }{ s_1 }{ n }{ s_2 } }"
            , "\\end{phinoMorphingInference}"
            , "\\begin{phinoMorphingInference}"
            , "  \\phinoName{mg}"
            , "  \\phinoPremise{ \\phinoMorph{ T }{ e }{ s_1 }{ n }{ s_2 } }"
            , "  \\phinoConclusion{ \\phinoMorph{ Q }{ Q }{ s_1 }{ n }{ s_2 } }"
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
            , "  \\phinoConclusion{ \\phinoDataize{ [[ B_1, D> \\delta, B_2 ]] }{ e }{ s }{ \\delta }{ s } }"
            , "\\end{phinoDataizationInference}"
            , "\\begin{phinoDataizationInference}"
            , "  \\phinoName{box}"
            , "  \\phinoCondition{ [ D \\char44{} L ] \\cap \\lparen B_1 \\cup B_2 \\rparen = \\emptyset }"
            , "  \\phinoPremise{ \\phinoContextualize{ e_1 }{ [[ B_1, @ -> e_1, B_2 ]] }{ e_2 } }"
            , "  \\phinoPremise{ \\phinoNormalize{ e_2 }{ n_1 } }"
            , "  \\phinoPremise{ \\phinoDataize{ n_1 }{ e }{ s_1 }{ \\delta }{ s_2 } }"
            , "  \\phinoConclusion{ \\phinoDataize{ [[ B_1, @ -> e_1, B_2 ]] }{ e }{ s_1 }{ \\delta }{ s_2 } }"
            , "\\end{phinoDataizationInference}"
            , "\\begin{phinoDataizationInference}"
            , "  \\phinoName{fire}"
            , "  \\phinoPremise{ \\phinoEvaluate{ [[ B_1, L> F, B_2 ]] }{ e }{ s_1 }{ e_1 }{ s_2 } }"
            , "  \\phinoPremise{ \\phinoNormalize{ e_1 }{ n_1 } }"
            , "  \\phinoPremise{ \\phinoDataize{ n_1 }{ e }{ s_2 }{ \\delta }{ s_3 } }"
            , "  \\phinoConclusion{ \\phinoDataize{ [[ B_1, L> F, B_2 ]] }{ e }{ s_1 }{ \\delta }{ s_3 } }"
            , "\\end{phinoDataizationInference}"
            , "\\begin{phinoDataizationInference}"
            , "  \\phinoName{none}"
            , "  \\phinoCondition{ [ D \\char44{} L \\char44{} @ ] \\cap B = \\emptyset }"
            , "  \\phinoPremise{ \\phinoDataize{ T }{ e }{ s_1 }{ \\delta }{ s_2 } }"
            , "  \\phinoConclusion{ \\phinoDataize{ [[ B ]] }{ e }{ s_1 }{ \\delta }{ s_2 } }"
            , "\\end{phinoDataizationInference}"
            , "\\begin{phinoDataizationInference}"
            , "  \\phinoName{norm}"
            , "  \\phinoCondition{ \\phinoNotFormation{ n } \\;\\text{and}\\; n \\not= T }"
            , "  \\phinoPremise{ \\phinoMorph{ n }{ e }{ s_1 }{ n_1 }{ s_2 } }"
            , "  \\phinoPremise{ \\phinoDataize{ n_1 }{ e }{ s_2 }{ \\delta }{ s_3 } }"
            , "  \\phinoConclusion{ \\phinoDataize{ n }{ e }{ s_1 }{ \\delta }{ s_3 } }"
            , "\\end{phinoDataizationInference}"
            ]
        ]

    it "explains contextualization rules" $
      testCLISucceeded
        ["explain", "--contextualize"]
        [ unlines
            [ "\\begin{phinoContextualizationInference}"
            , "  \\phinoName{cg}"
            , "  \\phinoConclusion{ \\phinoContextualize{ Q }{ k }{ Q } }"
            , "\\end{phinoContextualizationInference}"
            , "\\begin{phinoContextualizationInference}"
            , "  \\phinoName{cxi}"
            , "  \\phinoConclusion{ \\phinoContextualize{ \\phiTerminal{\\xi} }{ k }{ k } }"
            , "\\end{phinoContextualizationInference}"
            , "\\begin{phinoContextualizationInference}"
            , "  \\phinoName{ct}"
            , "  \\phinoConclusion{ \\phinoContextualize{ T }{ k }{ T } }"
            , "\\end{phinoContextualizationInference}"
            , "\\begin{phinoContextualizationInference}"
            , "  \\phinoName{cf}"
            , "  \\phinoConclusion{ \\phinoContextualize{ [[ B ]] }{ k }{ [[ B ]] } }"
            , "\\end{phinoContextualizationInference}"
            , "\\begin{phinoContextualizationInference}"
            , "  \\phinoName{cd}"
            , "  \\phinoPremise{ \\phinoContextualize{ n }{ k }{ n_1 } }"
            , "  \\phinoConclusion{ \\phinoContextualize{ n . \\tau }{ k }{ n_1 . \\tau } }"
            , "\\end{phinoContextualizationInference}"
            , "\\begin{phinoContextualizationInference}"
            , "  \\phinoName{ca}"
            , "  \\phinoPremise{ \\phinoContextualize{ n }{ k }{ n_1 } }"
            , "  \\phinoPremise{ \\phinoContextualize{ e_1 }{ k }{ n_2 } }"
            , "  \\phinoConclusion{ \\phinoContextualize{ n ( \\tau -> e_1 ) }{ k }{ n_1 ( \\tau -> n_2 ) } }"
            , "\\end{phinoContextualizationInference}"
            , "\\begin{phinoContextualizationInference}"
            , "  \\phinoName{caa}"
            , "  \\phinoPremise{ \\phinoContextualize{ n }{ k }{ n_1 } }"
            , "  \\phinoPremise{ \\phinoContextualize{ e_1 }{ k }{ n_2 } }"
            , "  \\phinoConclusion{ \\phinoContextualize{ n ( \\phiTerminal{\\alpha_{i}} -> e_1 ) }{ k }{ n_1 ( \\phiTerminal{\\alpha_{i}} -> n_2 ) } }"
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
        ["Only one of --rule, --normalize, --morph, --dataize or --contextualize can be specified"]

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

  describe "match" $ do
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

    it "prints many substitutions" $
      withStdin "[[ x -> Q.x, y -> Q.y ]]" $
        testCLISucceeded ["match", "--pattern=Q.!t"] ["t >> x\n------\nt >> y"]

    it "builds substitutions with conditions" $
      withStdin "[[ x -> Q.y ]].x" $
        testCLISucceeded
          ["match", "--pattern=[[ !t -> Q.y, !B ]].!t", "--when=eq(length(!B),1)"]
          ["B >> ⟦ ρ ↦ ∅ ⟧\nt >> x"]

    it "builds with condition from file" $
      testCLISucceeded
        ["match", "--pattern=[[ !B ]]", "--when=eq(length(!B),2)", "test-resources/cli/foo.phi"]
        ["B >> ⟦ foo ↦ Φ.org.eolang.x, ρ ↦ ∅ ⟧"]

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
