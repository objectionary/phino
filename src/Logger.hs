{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Logger
  ( logDebug
  , logError
  , setLogConfig
  , LogLevel (DEBUG, ERROR, NONE)
  )
where

import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.List as DL
import GHC.IO (unsafePerformIO)
import System.IO

data LogLevel = DEBUG | ERROR | NONE
  deriving (Show, Ord, Eq, Bounded, Enum, Read)

data Logger = Logger {level :: LogLevel, lns :: Int}

logger :: IORef Logger
{-# NOINLINE logger #-}
logger = unsafePerformIO (newIORef (Logger ERROR 25))

setLogConfig :: LogLevel -> Int -> IO ()
setLogConfig lvl cnt = writeIORef logger (Logger lvl cnt)

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

logDebug, logError :: String -> IO ()
logDebug = logMessage DEBUG
logError = logMessage ERROR
