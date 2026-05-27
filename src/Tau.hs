{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- Uniqueness of generated 𝜏-labels is the engine's job, not the caller's.
-- Instead of asking every 'random-tau' call site to hand over the names it
-- must steer clear of, we scan the document once at the start of a run and
-- remember every attribute label it already uses. Each minted name is added
-- back to that set so later firings (the fixpoint loop re-applies rules across
-- cycles) never reuse one. A monotonic cursor advances past taken indices so
-- minting stays O(1) per call rather than rescanning. Names are sequential
-- rather than random, which makes the rewritten output deterministic.
module Tau (seedTaus, freshTau) where

import AST
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO (unsafePerformIO)

-- The set of attribute labels already taken in the document being rewritten,
-- paired with a monotonic cursor pointing at the next candidate index.
taus :: IORef (Set Text, Int)
{-# NOINLINE taus #-}
taus = unsafePerformIO (newIORef (Set.empty, 0))

-- Scan the program once and seed the avoid-set with every attribute label it
-- contains, resetting the cursor back to the start.
seedTaus :: Program -> IO ()
seedTaus (Program expr) = writeIORef taus (exprLabels expr, 0)

-- Mint a fresh, deterministic 𝜏-label that collides with no taken name,
-- advancing the cursor past taken indices and recording the new name so it is
-- never handed out again.
freshTau :: IO Text
freshTau = do
  (taken, cursor) <- readIORef taus
  let (minted, idx) = mint taken cursor
  writeIORef taus (Set.insert minted taken, idx + 1)
  pure minted

-- Find the first index at or after the cursor whose name is still free.
mint :: Set Text -> Int -> (Text, Int)
mint taken idx
  | name `Set.member` taken = mint taken (idx + 1)
  | otherwise = (name, idx)
  where
    name = "a🌵" <> T.pack (show idx)

exprLabels :: Expression -> Set Text
exprLabels (ExFormation bds) = Set.unions (map bindingLabels bds)
exprLabels (ExApplication expr bd) = exprLabels expr <> bindingLabels bd
exprLabels (ExDispatch expr attr) = exprLabels expr <> attrLabel attr
exprLabels (ExMetaTail expr _) = exprLabels expr
exprLabels (ExPhiMeet _ _ expr) = exprLabels expr
exprLabels (ExPhiAgain _ _ expr) = exprLabels expr
exprLabels _ = Set.empty

bindingLabels :: Binding -> Set Text
bindingLabels (BiTau attr expr) = attrLabel attr <> exprLabels expr
bindingLabels (BiVoid attr) = attrLabel attr
bindingLabels _ = Set.empty

attrLabel :: Attribute -> Set Text
attrLabel (AtLabel label) = Set.singleton label
attrLabel _ = Set.empty
