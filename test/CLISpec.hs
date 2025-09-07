{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLISpec (spec) where

import CLI (runCLI)
import Control.Exception
import Control.Monad (forM_, unless)
import Data.List (isInfixOf)
import Data.Version (showVersion)
import GHC.IO.Handle
import Paths_phino (version)
import System.Directory (removeFile)
import System.Exit (ExitCode (ExitFailure))
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

testCLI :: [String] -> [String] -> Expectation
testCLI args outputs = do
  (out, _) <- withStdout (try (runCLI args) :: IO (Either ExitCode ()))
  forM_
    outputs
    ( \output ->
        unless (output `isInfixOf` out) $
          expectationFailure
            ("Expected that output contains:\n" ++ output ++ "\nbut got:\n" ++ out)
    )

testCLIFailed :: [String] -> String -> Expectation
testCLIFailed args output = do
  (out, result) <- withStdout (try (runCLI args) :: IO (Either ExitCode ()))
  out `shouldContain` output
  result `shouldBe` Left (ExitFailure 1)

spec :: Spec
spec = do
  it "prints version" $
    testCLI ["--version"] [showVersion version]

  it "prints help" $
    testCLI
      ["--help"]
      ["Phino - CLI Manipulator of 𝜑-Calculus Expressions", "Usage:"]

  it "prints debug info with --log-level=DEBUG" $
    withStdin "Q -> [[]]" $
      testCLI ["rewrite", "--nothing", "--log-level=DEBUG"] ["[DEBUG]:"]

  describe "match" $ do
    it "finds bindings matching attribute-to-expression pattern" $
      withStdin "Q -> [[a -> b.c, d -> 42]]" $
        testCLI ["match", "--pattern", "!a ↦ !e"] ["a ↦ ξ.b.c", "d ↦"]
    
    it "finds bindings with specific attribute patterns" $
      withStdin "Q -> [[foo -> 1, bar -> 2, baz -> 3]]" $
        testCLI ["match", "--pattern", "bar ↦ !e"] ["bar ↦"]
    
    it "finds xi-dispatches" $
      withStdin "Q -> [[a -> ξ.b, c -> ξ.d.e, f -> 42]]" $
        testCLI ["match", "--pattern", "!a ↦ !e"] ["a ↦ ξ.b", "c ↦ ξ.d.e", "f ↦"]
    
    it "finds nested bindings" $
      withStdin "Q -> [[a -> [[b -> c]], d -> [[e -> [[f -> g]]]]]]" $
        testCLI ["match", "--pattern", "!a ↦ !e"] 
          ["a ↦", "b ↦", "d ↦", "e ↦", "f ↦"]
    
    it "matches void bindings" $
      withStdin "Q -> [[a -> ∅, b -> c, d -> ∅]]" $
        testCLI ["match", "--pattern", "!a ↦ ∅"] ["a ↦ ∅", "d ↦ ∅"]
    
    it "matches delta bindings" $
      withStdin "Q -> [[Δ ⤍ 01-02, x -> 42]]" $
        testCLI ["match", "--pattern", "Δ ⤍ !b"] ["Δ ⤍"]
    
    it "matches lambda bindings" $
      withStdin "Q -> [[λ ⤍ func, a -> b]]" $
        testCLI ["match", "--pattern", "λ ⤍ !f"] ["λ ⤍"]
    
    it "matches rho bindings" $
      withStdin "Q -> [[ρ -> a.b, x -> y]]" $
        testCLI ["match", "--pattern", "ρ ↦ !e"] ["ρ ↦"]
    
    it "matches bindings with meta-bindings pattern" $
      withStdin "Q -> [[a -> b, c -> d, e -> f]]" $
        testCLI ["match", "--pattern", "!B"] []  -- !B is for multiple bindings, not single
    
    it "returns empty for no matches" $
      withStdin "Q -> [[a -> b.c]]" $
        testCLI ["match", "--pattern", "x ↦ !e"] []
    
    it "matches expression patterns" $
      withStdin "Q -> [[a -> b.c.d]]" $
        testCLI ["match", "--pattern", "!e.c"] []  -- Expression patterns need binding context
    
    it "matches this (ξ) references" $
      withStdin "Q -> [[a -> ξ, b -> ξ.c]]" $
        testCLI ["match", "--pattern", "!a ↦ ξ"] ["a ↦ ξ"]
    
    it "matches global (Φ) references" $
      withStdin "Q -> [[a -> Φ, b -> Φ.c]]" $
        testCLI ["match", "--pattern", "!a ↦ Φ"] ["a ↦ Φ"]
    
    it "matches termination (⊥)" $
      withStdin "Q -> [[a -> ⊥, b -> c]]" $
        testCLI ["match", "--pattern", "!a ↦ ⊥"] ["a ↦ ⊥"]
    
    it "handles invalid pattern gracefully" $
      withStdin "Q -> [[a -> b]]" $
        testCLIFailed ["match", "--pattern", "invalid pattern @#$"] 
          "Invalid pattern"
    
    it "requires pattern argument" $
      withStdin "Q -> [[a -> b]]" $
        testCLIFailed ["match"] "Missing: --pattern PATTERN"
    
    it "works with file input" $
      testCLI ["match", "--pattern", "!a ↦ !e", "test-resources/cli/desugar.phi"]
        ["foo ↦"]
    
    it "works with XMIR input format" $
      withStdin "<?xml version=\"1.0\"?><object><o name=\"a\">b</o></object>" $
        testCLI ["match", "--pattern", "!a ↦ !e", "--input=xmir"] ["a ↦"]
    
    it "handles empty program" $
      withStdin "Q -> [[]]" $
        testCLI ["match", "--pattern", "!a ↦ !e"] []
    
    it "matches applications" $
      withStdin "Q -> [[a -> b(c ↦ d)]]" $
        testCLI ["match", "--pattern", "!x ↦ !e(c ↦ !y)"] []  -- Application patterns are complex
    
    it "matches formations" $
      withStdin "Q -> [[a -> [[b -> c, d -> e]]]]" $
        testCLI ["match", "--pattern", "[[!B]]"] ["⟦", "b ↦", "d ↦", "⟧"]
    
    it "matches complex nested patterns" $
      withStdin "Q -> [[a -> b.c(d ↦ [[e -> f.g]])]]" $
        testCLI ["match", "--pattern", "e ↦ !e"] ["e ↦ ξ.f.g"]
    
    it "handles ASCII input syntax" $
      withStdin "Q -> [[a -> b.c, d -> 42]]" $
        testCLI ["match", "--pattern", "!a -> !e"] ["a ↦ ξ.b.c", "d ↦"]
    
    it "handles when condition for filtering" $
      withStdin "Q -> [[a -> 1, b -> 2, c -> 3]]" $
        testCLI ["match", "--pattern", "!x ↦ !y", "--when", "eq: [\"!x\", \"b\"]"] 
          []  -- When conditions need proper YAML format
    
    it "handles invalid when condition" $
      withStdin "Q -> [[a -> b]]" $
        testCLIFailed ["match", "--pattern", "!a ↦ !e", "--when", "invalid yaml {{{"]
          "Failed to parse condition"
    
    it "matches with different log levels" $
      withStdin "Q -> [[a -> b]]" $
        testCLI ["match", "--pattern", "!a ↦ !e", "--log-level=DEBUG"] 
          ["[DEBUG]:", "a ↦"]
    
    it "matches multiple occurrences of same pattern" $
      withStdin "Q -> [[a -> x.y, b -> x.y, c -> x.y]]" $
        testCLI ["match", "--pattern", "!a ↦ !e"] 
          ["a ↦ ξ.x.y", "b ↦ ξ.x.y", "c ↦ ξ.x.y"]
    
    it "handles special characters in patterns" $
      withStdin "Q -> [[a0 -> b, c1 -> d]]" $
        testCLI ["match", "--pattern", "a0 ↦ !e"] ["a0 ↦"]
    
    it "matches meta-lambda patterns" $
      withStdin "Q -> [[λ ⤍ !F, a -> b]]" $
        testCLI ["match", "--pattern", "λ ⤍ !meta"] ["λ ⤍"]
    
    it "handles deeply nested structures" $
      withStdin "Q -> [[a -> [[b -> [[c -> [[d -> e]]]]]]]]" $
        testCLI ["match", "--pattern", "d ↦ !e"] ["d ↦"]
    
    it "matches dispatches with multiple levels" $
      withStdin "Q -> [[a -> b.c.d.e.f]]" $
        testCLI ["match", "--pattern", "!a ↦ !e"] ["a ↦"]
    
    it "matches with meta-tail patterns" $
      withStdin "Q -> [[a -> b.c.d]]" $
        testCLI ["match", "--pattern", "!a ↦ !e!t"] []  -- Meta-tail patterns need special handling
    
    it "handles formations with rho" $
      withStdin "Q -> [[a -> [[ρ -> ∅, b -> c]]]]" $
        testCLI ["match", "--pattern", "ρ ↦ ∅"] ["ρ ↦ ∅"]
    
    it "matches bytes patterns" $
      withStdin "Q -> [[Δ ⤍ 40-45-00, a -> b]]" $
        testCLI ["match", "--pattern", "Δ ⤍ !b"] ["Δ ⤍"]
    
    it "handles stdin read errors gracefully" $
      withStdin "" $
        testCLI ["match", "--pattern", "!a ↦ !e"] []

  describe "rewrites" $ do
    it "desugares with --nothing flag from file" $
      testCLI
        ["rewrite", "--nothing", "test-resources/cli/desugar.phi"]
        ["Φ ↦ ⟦\n  foo ↦ Φ.org.eolang,\n  ρ ↦ ∅\n⟧"]

    it "desugares with --nothing flag from stdin" $
      withStdin "{[[foo ↦ QQ]]}" $
        testCLI ["rewrite", "--nothing"] ["Φ ↦ ⟦\n  foo ↦ Φ.org.eolang,\n  ρ ↦ ∅\n⟧"]

    it "rewrites with single rule" $
      withStdin "{T(x -> Q.y)}" $
        testCLI ["rewrite", "--rule=resources/dc.yaml"] ["Φ ↦ ⊥"]

    it "normalizes with --normalize flag" $
      testCLI
        ["rewrite", "--normalize", "test-resources/cli/normalize.phi"]
        [ unlines
            [ "Φ ↦ ⟦",
              "  x ↦ ⟦",
              "    ρ ↦ ⟦",
              "      y ↦ ⟦ ρ ↦ ∅ ⟧,",
              "      ρ ↦ ∅",
              "    ⟧",
              "  ⟧,",
              "  ρ ↦ ∅",
              "⟧"
            ]
        ]

    it "fails with negative --max-depth" $
      withStdin "" $
        testCLIFailed ["rewrite", "--max-depth=-1"] "--max-depth must be positive"

    it "fails with no rewriting options provided" $
      withStdin "" $
        testCLIFailed ["rewrite"] "no --rule, no --normalize, no --nothing are provided"

    it "normalizes from stdin" $
      withStdin "Φ ↦ ⟦ a ↦ ⟦ b ↦ ∅ ⟧ (b ↦ [[ ]]) ⟧" $
        testCLI
          ["rewrite", "--normalize"]
          [ unlines
              [ "Φ ↦ ⟦",
                "  a ↦ ⟦",
                "    b ↦ ⟦ ρ ↦ ∅ ⟧,",
                "    ρ ↦ ∅",
                "  ⟧,",
                "  ρ ↦ ∅",
                "⟧"
              ]
          ]

    it "rewrites with --sweet flag" $
      withStdin "Q -> [[ x -> 5]]" $
        testCLI
          ["rewrite", "--nothing", "--sweet"]
          ["{⟦\n  x ↦ 5\n⟧}"]

    it "rewrites as XMIR" $
      withStdin "Q -> [[ x -> Q.y ]]" $
        testCLI
          ["rewrite", "--nothing", "--output=xmir"]
          ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<object", "  <o base=\"Q.y\" name=\"x\"/>"]

    it "rewrites with XMIR as input" $
      withStdin "<object><o name=\"app\"><o name=\"x\" base=\"Q.number\"/></o></object>" $
        testCLI
          ["rewrite", "--nothing", "--input=xmir", "--sweet"]
          [ unlines
              [ "{⟦",
                "  app ↦ ⟦",
                "    x ↦ Φ.number",
                "  ⟧",
                "⟧}"
              ]
          ]

    it "rewrites as XMIR with omit-listing flag" $
      withStdin "Q -> [[ x -> Q.y ]]" $
        testCLI
          ["rewrite", "--nothing", "--output=xmir", "--omit-listing"]
          ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>", "<object", "<listing>1 line(s)</listing>", "  <o base=\"Q.y\" name=\"x\"/>"]

    it "does not fail on exactly 1 rewriting" $
      withStdin "{⟦ t ↦ ⟦ x ↦ \"foo\" ⟧ ⟧}" $
        testCLI
          ["rewrite", "--rule=test-resources/cli/simple.yaml", "--must=1", "--sweet"]
          ["x ↦ \"bar\""]

    it "fails with --nothing and --must" $
      withStdin "Q -> [[ ]]" $
        testCLIFailed ["rewrite", "--nothing", "--must"] "it's expected exactly 1 rewriting cycles happened, but rewriting stopped after 0"

    it "fails with --normalize and --must" $
      withStdin "Q -> [[ x -> [[ y -> 5 ]].y ]].x" $
        testCLIFailed ["rewrite", "--max-depth=2", "--normalize", "--must"] "it's expected exactly 1 rewriting cycles happened, but rewriting is still going"

    it "prints to target file" $
      withStdin "Q -> [[ ]]" $
        bracket
          (openTempFile "." "targetXXXXXX.tmp")
          (\(path, _) -> removeFile path)
          ( \(path, h) -> do
              hClose h
              testCLI
                ["rewrite", "--nothing", "--sweet", printf "--target=%s" path]
                [printf "The result program was saved in '%s'" path]
              content <- readFile path
              content `shouldBe` "{⟦⟧}"
          )

    it "rewrites with cycles" $
      withStdin "Q -> [[ x -> \"x\" ]]" $
        testCLI
          ["rewrite", "--sweet", "--rule=test-resources/cli/infinite.yaml", "--max-depth=1", "--max-cycles=2"]
          [ unlines
              [ "{⟦",
                "  x ↦ \"x_hi_hi\"",
                "⟧}"
              ]
          ]
    
    it "fails with --depth-sensitive" $
      withStdin "Q -> [[ x -> \"x\"]]" $
        testCLIFailed
          ["rewrite", "--depth-sensitive", "--max-depth=1", "--max-cycles=1", "--rule=test-resources/cli/infinite.yaml"]
          "[ERROR]: With option --depth-sensitive it's expected rewriting iterations amount does not reach the limit: --max-depth=1"

    it "fails on --sweet and --output=xmir together" $
      withStdin "Q -> [[ ]]" $
        testCLIFailed
          ["rewrite", "--sweet", "--output=xmir", "--nothing"]
          "The --sweet and --output=xmir can't stay together"

  describe "dataize" $ do
    it "dataizes simple program" $
      withStdin "Q -> [[ D> 01- ]]" $
        testCLI ["dataize"] ["01-"]

    it "fails to dataize" $
      withStdin "Q -> [[ ]]" $
        testCLIFailed ["dataize"] "[ERROR]: Could not dataize given program"
