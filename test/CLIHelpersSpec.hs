{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLIHelpersSpec (spec) where

import AST (Expression (ExRoot))
import CLI.Helpers (getRules, parseInput, printExpression, readInput)
import CLI.Types (IOFormat (LATEX, PHI, XMIR), PrintContext (PrintCtx))
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import Lining (LineFormat (MULTILINE))
import Sugar (SugarType (SWEET))
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)
import XMIR (defaultXmirContext)

isLeft :: Either e a -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

withScratchDir :: (FilePath -> IO a) -> IO a
withScratchDir =
  bracket
    ( do
        tmp <- getTemporaryDirectory
        stamp <- getPOSIXTime
        let dir = tmp </> ("phino-cli-helpers-spec-" ++ show (floor (stamp * 1000000) :: Integer))
        createDirectoryIfMissing True dir
        pure dir
    )
    removeDirectoryRecursive

{-# ANN testPrintContext ("HLint: ignore Eta reduce" :: String) #-}
testPrintContext :: IOFormat -> PrintContext
testPrintContext format =
  PrintCtx SWEET False Nothing False MULTILINE 2 defaultXmirContext False False False False False 1 1 ExRoot Nothing Nothing Nothing format

spec :: Spec
spec = do
  describe "parseInput" $
    it "fails when asked to parse LaTeX as an input format" $ do
      result <- try (parseInput "whatever" LATEX) :: IO (Either SomeException Expression)
      result `shouldSatisfy` isLeft

  describe "printExpression" $
    forM_
      [
        ( "fails when asked to print with --output=xmir (only --output=phi/latex are supported here)"
        , XMIR
        , isLeft
        )
      , ("succeeds when --output=phi is used", PHI, not . isLeft)
      ]
      ( \(desc, format, predicate) -> it desc $ do
          result <- try (printExpression (testPrintContext format) ExRoot) :: IO (Either SomeException String)
          result `shouldSatisfy` predicate
      )

  describe "getRules" $
    it "deduplicates the same --rule file listed twice" $ do
      rules <- getRules False False ["test-resources/cli/rules/simple.yaml", "test-resources/cli/rules/simple.yaml"]
      length rules `shouldBe` 1

  describe "readInput" $ do
    it "strips a leading UTF-8 byte order mark from a file" $ withScratchDir $ \dir -> do
      let path = dir </> "bom.phi"
      BS.writeFile path (TE.encodeUtf8 (T.pack "\xFEFF{⟦ a ↦ ⟦⟧ ⟧}"))
      readInput (Just path) `shouldReturn` "{⟦ a ↦ ⟦⟧ ⟧}"

    it "leaves a file without a byte order mark untouched" $ withScratchDir $ \dir -> do
      let path = dir </> "no-bom.phi"
      BS.writeFile path (TE.encodeUtf8 (T.pack "{⟦ a ↦ ⟦⟧ ⟧}"))
      readInput (Just path) `shouldReturn` "{⟦ a ↦ ⟦⟧ ⟧}"
