-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Main where

import CLI (prepareIO, runCLI)
import System.Environment (getArgs)

main :: IO ()
main = prepareIO >> getArgs >>= runCLI
