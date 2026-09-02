{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- These are direct unit tests of a few 'CLI.Helpers' branches that the CLI
-- itself makes unreachable by construction: the option parser never hands
-- '--input' a 'LATEX' value, and '--output=xmir' is only ever validated
-- together with '--focus=Q', which keeps 'printExpression' out of its own
-- XMIR branch. Both branches still exist as defensive, exhaustive pattern
-- matches, so they are exercised here by calling the functions directly.
module CLIHelpersSpec (spec) where

import AST (Expression (ExRoot))
import CLI.Helpers (parseInput, printExpression)
import CLI.Types (IOFormat (LATEX, PHI, XMIR), PrintContext (PrintCtx))
import Control.Exception (SomeException, try)
import Lining (LineFormat (MULTILINE))
import Sugar (SugarType (SWEET))
import Test.Hspec (Spec, describe, it, shouldSatisfy)
import XMIR (defaultXmirContext)

isLeft :: Either e a -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

{-# ANN testPrintContext ("HLint: ignore Eta reduce" :: String) #-}
testPrintContext :: IOFormat -> PrintContext
testPrintContext format =
  PrintCtx SWEET False MULTILINE 2 defaultXmirContext False False False False False 1 1 ExRoot Nothing Nothing Nothing format

spec :: Spec
spec = do
  describe "parseInput" $
    it "fails when asked to parse LaTeX as an input format" $ do
      result <- try (parseInput "whatever" LATEX) :: IO (Either SomeException Expression)
      result `shouldSatisfy` isLeft

  describe "printExpression" $ do
    it "fails when asked to print with --output=xmir (only --output=phi/latex are supported here)" $ do
      result <- try (printExpression (testPrintContext XMIR) ExRoot) :: IO (Either SomeException String)
      result `shouldSatisfy` isLeft

    it "succeeds when --output=phi is used" $ do
      result <- try (printExpression (testPrintContext PHI) ExRoot) :: IO (Either SomeException String)
      result `shouldSatisfy` not . isLeft
