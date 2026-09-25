{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Logger
  ( logDebug
  , logInfo
  , logError
  , logging
  , setLogConfig
  , LogLevel (DEBUG, INFO, ERROR, NONE)
  )
where

import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.List as DL
import GHC.IO (unsafePerformIO)
import System.IO

data LogLevel = DEBUG | INFO | ERROR | NONE
  deriving (Show, Ord, Eq, Bounded, Enum, Read)

data Logger = Logger {level :: LogLevel, lns :: Int}

logger :: IORef Logger
{-# NOINLINE logger #-}
logger = unsafePerformIO (newIORef (Logger ERROR 25))

setLogConfig :: LogLevel -> Int -> IO ()
setLogConfig lvl cnt = writeIORef logger (Logger lvl cnt)

-- Whether a message of this level reaches the console at all, so a caller
-- whose message costs something to put together skips the work when it would
-- be thrown away.
logging :: LogLevel -> IO Bool
logging lvl = do
  Logger{..} <- readIORef logger
  pure (lvl >= level && lns /= 0)

logMessage :: LogLevel -> String -> IO ()
logMessage lvl message = do
  Logger{..} <- readIORef logger
  when
    (lvl >= level && lns /= 0)
    ( let split = DL.lines message
          toPrint = take lns split
          msg
            | lns == -1 = [message]
            | length split > lns = toPrint ++ ["---| log is limited by --log-lines=" ++ show lns ++ " option |---"]
            | otherwise = toPrint
       in hPutStrLn stderr ("[" ++ show lvl ++ "]: " ++ DL.intercalate "\n" msg)
    )

logDebug, logInfo, logError :: String -> IO ()
logDebug = logMessage DEBUG
logInfo = logMessage INFO
logError = logMessage ERROR
