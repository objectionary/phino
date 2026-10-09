{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module BrokenPipeSpec (spec) where

#ifndef mingw32_HOST_OS
import CLI (runCLI)
import Control.Exception (bracket, evaluate, finally, try)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (removeFile)
import System.Exit (ExitCode (ExitFailure))
import System.IO
import System.Posix.IO (closeFd, createPipe, fdToHandle)
#endif
import Test.Hspec

#ifndef mingw32_HOST_OS
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

withBrokenStdout :: IO a -> IO (String, a)
withBrokenStdout action =
  bracket
    (openTempFile "." "stderrXXXXXX.tmp")
    (\(fp, _) -> removeFile fp)
    ( \(path, hErrTmp) -> do
        hSetEncoding hErrTmp utf8
        (readEnd, writeEnd) <- createPipe
        closeFd readEnd
        writePipe <- fdToHandle writeEnd
        oldOut <- hDuplicate stdout
        oldErr <- hDuplicate stderr
        oldOutBuffering <- hGetBuffering stdout
        hDuplicateTo writePipe stdout
        hDuplicateTo hErrTmp stderr
        hSetBuffering stdout NoBuffering

        result <-
          action `finally` do
            hDuplicateTo oldOut stdout >> hClose oldOut
            hSetBuffering stdout oldOutBuffering
            hDuplicateTo oldErr stderr >> hClose oldErr
            hClose writePipe
            hClose hErrTmp

        captured <- readFile path
        _ <- evaluate (length captured)
        return (captured, result)
    )
#endif

spec :: Spec
#ifndef mingw32_HOST_OS
spec =
  describe "CLI rewrite" $
    it "exits quietly, without an [ERROR] line, when stdout is a closed pipe" $
      withStdin "[[]]" $ do
        (err, result) <- withBrokenStdout (try (runCLI ["rewrite"]) :: IO (Either ExitCode ()))
        err `shouldNotContain` "[ERROR]"
        result `shouldBe` Left (ExitFailure 141)
#else
spec = pure ()
#endif
