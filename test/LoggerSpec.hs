-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module LoggerSpec where

import Logger (LogLevel (..), logDebug, logError, setLogConfig)
import System.IO (stderr)
import System.IO.Silently (hCapture_)
import Test.Hspec (Spec, after_, describe, it, shouldBe)

-- setLogConfig mutates a global IORef, so every example resets it afterwards
-- to the module's own default; otherwise the last example to run here would
-- leak its log level/line-limit into whichever spec runs next.
spec :: Spec
spec = after_ (setLogConfig ERROR 25) $ do
  describe "logDebug" $ do
    it "prints when the level allows debug messages" $ do
      setLogConfig DEBUG 25
      captured <- hCapture_ [stderr] (logDebug "hello")
      captured `shouldBe` "[DEBUG]: hello\n"

    it "is suppressed when the configured level is above debug" $ do
      setLogConfig ERROR 25
      captured <- hCapture_ [stderr] (logDebug "hello")
      captured `shouldBe` ""

    it "is suppressed when the line limit is zero" $ do
      setLogConfig DEBUG 0
      captured <- hCapture_ [stderr] (logDebug "hello")
      captured `shouldBe` ""

    it "truncates a message with more lines than the configured limit" $ do
      setLogConfig DEBUG 2
      captured <- hCapture_ [stderr] (logDebug "line1\nline2\nline3\nline4")
      captured `shouldBe` "[DEBUG]: line1\nline2\n---| log is limited by --log-lines=2 option |---\n"

    it "does not truncate a message with no more lines than the configured limit" $ do
      setLogConfig DEBUG 3
      captured <- hCapture_ [stderr] (logDebug "line1\nline2")
      captured `shouldBe` "[DEBUG]: line1\nline2\n"

    it "prints the whole message unlimited when lines is -1, however many lines" $ do
      setLogConfig DEBUG (-1)
      captured <- hCapture_ [stderr] (logDebug "line1\nline2\nline3\nline4\nline5")
      captured `shouldBe` "[DEBUG]: line1\nline2\nline3\nline4\nline5\n"

  describe "logError" $ do
    it "prints when the level allows error messages" $ do
      setLogConfig ERROR 25
      captured <- hCapture_ [stderr] (logError "oops")
      captured `shouldBe` "[ERROR]: oops\n"

    it "prints at the debug level too, since error is more severe" $ do
      setLogConfig DEBUG 25
      captured <- hCapture_ [stderr] (logError "oops")
      captured `shouldBe` "[ERROR]: oops\n"

    it "is suppressed when the configured level is NONE" $ do
      setLogConfig NONE 25
      captured <- hCapture_ [stderr] (logError "oops")
      captured `shouldBe` ""

    it "is suppressed when the line limit is zero" $ do
      setLogConfig ERROR 0
      captured <- hCapture_ [stderr] (logError "oops")
      captured `shouldBe` ""
