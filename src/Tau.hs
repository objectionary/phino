{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Tau (seedTaus, freshTau, tausOf) where

import AST
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
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

freshTau :: IO Text
freshTau = atomicModifyIORef' taus advance
  where
    advance (taken, cursor) =
      let (minted, idx) = mint taken cursor
       in ((Set.insert minted taken, idx + 1), minted)

tausOf :: Int -> IO (IO Text)
tausOf entry = do
  (taken, _) <- readIORef taus
  own <- newIORef (taken, 0)
  pure (atomicModifyIORef' own advance)
  where
    advance :: (Set Text, Int) -> ((Set Text, Int), Text)
    advance (taken, cursor) =
      let (minted, idx) = mint' (T.pack ("a🌵" <> show entry <> "-")) taken cursor
       in ((Set.insert minted taken, idx + 1), minted)

mint :: Set Text -> Int -> (Text, Int)
mint = mint' "a🌵"

mint' :: Text -> Set Text -> Int -> (Text, Int)
mint' stem taken idx
  | name `Set.member` taken = mint' stem taken (idx + 1)
  | otherwise = (name, idx)
  where
    name :: Text
    name = stem <> T.pack (show idx)

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
