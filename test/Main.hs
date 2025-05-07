-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Main where

import Spec qualified
import Test.Hspec.Runner

main :: IO ()
main = hspecWith defaultConfig Spec.spec
