-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module provides commonly used helper functions for other modules
module Misc (withoutAt, isMetaBinding) where

import Ast

-- List without element by given index
withoutAt :: Int -> [a] -> [a]
withoutAt i xs = take i xs ++ drop (i + 1) xs

-- Returns True if given binding is BiMeta (!B)
isMetaBinding :: Binding -> Bool
isMetaBinding (BiMeta _) = True
isMetaBinding _ = False