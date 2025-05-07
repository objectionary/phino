-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Rewriter where

import Ast

rewriteProgram :: Program -> Expression -> Expression -> Program
rewriteProgram prog ptn repl = prog
