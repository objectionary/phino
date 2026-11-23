{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-hpc #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Pointer (identical) where

import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)

identical :: a -> a -> Bool
identical left right = left `seq` right `seq` isTrue# (reallyUnsafePtrEquality# left right)
{-# NOINLINE identical #-}
