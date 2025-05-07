-- This pragma runs `hspec-discover` preprocessor at compile time.
-- This preprocessor scans for *Spec.hs modules and gathers them into
-- one big spec :: Spec module which is run by test/Main.hs
{-# OPTIONS_GHC -F -pgmF hspec-discover -optF --module-name=Spec #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT
