-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module LoggerSpec where

import Control.Monad (forM_)
import Logger (LogLevel (..), logDebug, logError, setLogConfig)
import System.IO (stderr)
import System.IO.Silently (hCapture_)
import Test.Hspec (Spec, after_, describe, it, shouldBe)

-- setLogConfig mutates a global IORef, so every example resets it afterwards
-- to the module's own default; otherwise the last example to run here would
-- leak its log level/line-limit into whichever spec runs next.
spec :: Spec
spec = after_ (setLogConfig ERROR 25) $ do
  describe "logDebug" $
    forM_
      [ ("prints when the level allows debug messages", DEBUG, 25, "hello", "[DEBUG]: hello\n")
      , ("is suppressed when the configured level is above debug", ERROR, 25, "hello", "")
      , ("is suppressed when the line limit is zero", DEBUG, 0, "hello", "")
      ,
        ( "truncates a message with more lines than the configured limit"
        , DEBUG
        , 2
        , "line1\nline2\nline3\nline4"
        , "[DEBUG]: line1\nline2\n---| log is limited by --log-lines=2 option |---\n"
        )
      ,
        ( "does not truncate a message with no more lines than the configured limit"
        , DEBUG
        , 3
        , "line1\nline2"
        , "[DEBUG]: line1\nline2\n"
        )
      ,
        ( "prints the whole message unlimited when lines is -1, however many lines"
        , DEBUG
        , -1
        , "line1\nline2\nline3\nline4\nline5"
        , "[DEBUG]: line1\nline2\nline3\nline4\nline5\n"
        )
      ]
      ( \(desc, level, lineLimit, message, expected) -> it desc $ do
          setLogConfig level lineLimit
          captured <- hCapture_ [stderr] (logDebug message)
          captured `shouldBe` expected
      )

  describe "logError" $
    forM_
      [ ("prints when the level allows error messages", ERROR, 25, "[ERROR]: oops\n")
      , ("prints at the debug level too, since error is more severe", DEBUG, 25, "[ERROR]: oops\n")
      , ("is suppressed when the configured level is NONE", NONE, 25, "")
      , ("is suppressed when the line limit is zero", ERROR, 0, "")
      ]
      ( \(desc, level, lineLimit, expected) -> it desc $ do
          setLogConfig level lineLimit
          captured <- hCapture_ [stderr] (logError "oops")
          captured `shouldBe` expected
      )
