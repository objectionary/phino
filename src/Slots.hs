-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to collect the anonymous meta-variable slots a
-- term was written with. A rule binds them where it matches and nowhere else,
-- so every other part of a rule is asked for its slots and rejected when it
-- has any.
module Slots (Slots (..), anonymous) where

import AST
import Data.Text (Text)

class Slots a where
  slots :: a -> [Slot]

-- The kind sigil of the first anonymous meta a term was written with, if any,
-- so a caller can word its own complaint about a term that must have none
anonymous :: (Slots a) => a -> Maybe Text
anonymous term = case slots term of
  [] -> Nothing
  Slot kind _ : _ -> Just kind

instance (Slots a) => Slots [a] where
  slots = concatMap slots

instance (Slots a) => Slots (Maybe a) where
  slots = maybe [] slots

instance Slots Expression where
  slots (ExAny slot) = [slot]
  slots (ExFormation bds) = slots bds
  slots (ExApplication expr arg) = slots expr ++ slots arg
  slots (ExDispatch expr attr) = slots expr ++ slots attr
  slots (ExPhiMeet _ _ expr) = slots expr
  slots (ExPhiAgain _ _ expr) = slots expr
  slots (ExBytes bts) = slots bts
  slots _ = []

instance Slots Argument where
  slots (ArTau attr expr) = slots attr ++ slots expr
  slots (ArAlpha alpha expr) = slots alpha ++ slots expr

instance Slots Binding where
  slots (BiAny slot) = [slot]
  slots (BiTau attr expr) = slots attr ++ slots expr
  slots (BiVoid attr) = slots attr
  slots (BiDelta bts) = slots bts
  slots (BiLambda func) = slots func
  slots (BiMeta _) = []

instance Slots Attribute where
  slots (AtAny slot) = [slot]
  slots _ = []

instance Slots Alpha where
  slots (AlAny slot) = [slot]
  slots _ = []

instance Slots Bytes where
  slots (BtAny slot) = [slot]
  slots _ = []

instance Slots Function where
  slots (FnAny slot) = [slot]
  slots _ = []
