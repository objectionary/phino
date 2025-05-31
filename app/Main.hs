-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Main where

import CLI (runCLI)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= runCLI
