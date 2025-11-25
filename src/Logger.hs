{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Logger
  ( logDebug,
    logInfo,
    logWarning,
    logError,
    setLogConfig,
    LogLevel (DEBUG, INFO, WARNING, ERROR, NONE),
  )
where

import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.List as DL
import GHC.IO (unsafePerformIO)
import System.IO

data LogLevel = DEBUG | INFO | WARNING | ERROR | NONE
  deriving (Show, Ord, Eq, Bounded, Enum, Read)

data Logger = Logger {level :: LogLevel, lines :: Int}

logger :: IORef Logger
{-# NOINLINE logger #-}
logger = unsafePerformIO (newIORef (Logger INFO 25))

setLogConfig :: LogLevel -> Integer -> IO ()
setLogConfig lvl lines = writeIORef logger (Logger lvl (fromIntegral lines))

logMessage :: LogLevel -> String -> IO ()
logMessage lvl message = do
  Logger {..} <- readIORef logger
  when
    (lvl >= level && lines /= 0)
    ( let lines' = DL.lines message
          toPrint = take lines lines'
          msg
            | lines == -1 = [message]
            | length lines' > lines = toPrint ++ ["---| log is limited by --log-lines=" ++ show lines ++ " option |---"]
            | otherwise = toPrint
       in hPutStrLn stderr ("[" ++ show lvl ++ "]: " ++ DL.intercalate "\n" msg)
    )

logDebug, logInfo, logWarning, logError :: String -> IO ()
logDebug = logMessage DEBUG
logInfo = logMessage INFO
logWarning = logMessage WARNING
logError = logMessage ERROR
