{-# LANGUAGE LambdaCase #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Logger
  ( logDebug,
    logInfo,
    logWarning,
    logError,
    setLogLevel,
    LogLevel(DEBUG, INFO, WARNING, ERROR, NONE),
  )
where

import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.IO (unsafePerformIO)
import System.IO
import Text.Printf (printf)
import Options.Applicative (ReadM)

data LogLevel = DEBUG | INFO | WARNING | ERROR | NONE
  deriving (Show, Ord, Eq, Bounded, Enum, Read)

newtype Logger = Logger {level :: LogLevel}

logger :: IORef Logger
{-# NOINLINE logger #-}
logger = unsafePerformIO (newIORef (Logger INFO))

setLogLevel :: LogLevel -> IO ()
setLogLevel lvl = writeIORef logger (Logger lvl)

logMessage :: LogLevel -> String -> IO ()
logMessage lvl message = do
  log <- readIORef logger
  when (lvl >= level log) $ hPutStrLn stderr ("[" ++ show lvl ++ "]: " ++ message)

logDebug, logInfo, logWarning, logError :: String -> IO ()
logDebug = logMessage DEBUG
logInfo = logMessage INFO
logWarning = logMessage WARNING
logError = logMessage ERROR
