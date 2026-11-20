{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Tau (seedTaus, reserveTaus, freshTau) where

import AST
import Data.IORef (IORef, atomicModifyIORef', newIORef, writeIORef)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.IO (unsafePerformIO)

taus :: IORef (Set Text, Int)
{-# NOINLINE taus #-}
taus = unsafePerformIO (newIORef (Set.empty, 0))

seedTaus :: Expression -> IO ()
seedTaus expr = writeIORef taus (exprLabels expr, 0)

reserveTaus :: Expression -> IO ()
reserveTaus expr = atomicModifyIORef' taus (\(taken, cursor) -> ((taken <> exprLabels expr, cursor), ()))

freshTau :: IO Text
freshTau = atomicModifyIORef' taus advance
  where
    advance (taken, cursor) =
      let (minted, idx) = mint taken cursor
       in ((Set.insert minted taken, idx + 1), minted)

mint :: Set Text -> Int -> (Text, Int)
mint taken idx
  | name `Set.member` taken = mint taken (idx + 1)
  | otherwise = (name, idx)
  where
    name :: Text
    name = "a🌵" <> T.pack (show idx)

exprLabels :: Expression -> Set Text
exprLabels (ExFormation bds) = Set.unions (map bindingLabels bds)
exprLabels (ExApplication expr arg) = exprLabels expr <> argumentLabels arg
exprLabels (ExDispatch expr attr) = exprLabels expr <> attrLabel attr
exprLabels (ExPhiMeet _ _ expr) = exprLabels expr
exprLabels (ExPhiAgain _ _ expr) = exprLabels expr
exprLabels _ = Set.empty

bindingLabels :: Binding -> Set Text
bindingLabels (BiTau attr expr) = attrLabel attr <> exprLabels expr
bindingLabels (BiVoid attr) = attrLabel attr
bindingLabels _ = Set.empty

argumentLabels :: Argument -> Set Text
argumentLabels (ArTau attr expr) = attrLabel attr <> exprLabels expr
argumentLabels (ArAlpha _ expr) = exprLabels expr

attrLabel :: Attribute -> Set Text
attrLabel (AtLabel label) = Set.singleton label
attrLabel _ = Set.empty
