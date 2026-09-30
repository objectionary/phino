-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The engine 'phino compile' writes, where it has written one: this module
-- stands in its place until it does, and a build without the flag 'compiled'
-- links it in, so phino interprets its rules of YAML (#1617).
module Compiled (compiled) where

import Engine (Engine)

-- No engine was compiled.
compiled :: Maybe Engine
compiled = Nothing
