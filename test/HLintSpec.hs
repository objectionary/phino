{-# LANGUAGE ScopedTypeVariables #-}
-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module HLintSpec (spec) where

import Control.Exception (try, IOException)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Test.Hspec

-- | Check if hlint is available on the system
isHLintAvailable :: IO Bool
isHLintAvailable = do
  result <- try (readProcessWithExitCode "hlint" ["--version"] "")
  case result of
    Left (_ :: IOException) -> return False
    Right (ExitSuccess, _, _) -> return True
    Right (ExitFailure _, _, _) -> return False

-- | Run hlint on a directory and check for success
runHLintCheck :: String -> IO ()
runHLintCheck dir = do
  available <- isHLintAvailable
  if not available 
    then pendingWith "hlint is not available on this system"
    else do
      (exitCode, stdout, stderr) <- readProcessWithExitCode "hlint" [dir] ""
      case exitCode of
        ExitSuccess -> return ()
        ExitFailure _ -> do
          putStrLn $ "HLint warnings/errors in " ++ dir ++ ":"
          putStrLn stdout
          putStrLn stderr
          exitCode `shouldBe` ExitSuccess

spec :: Spec
spec = do
  describe "HLint" $ do
    it "should pass hlint check for src/" $ do
      runHLintCheck "src/"

    it "should pass hlint check for app/" $ do
      runHLintCheck "app/"
          
    it "should pass hlint check for test/" $ do
      runHLintCheck "test/"