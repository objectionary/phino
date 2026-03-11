-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Main where

import CLI (runCLI)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Environment (getArgs)

main :: IO ()
main = setLocaleEncoding utf8 >> getArgs >>= runCLI
