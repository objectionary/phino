{-# LANGUAGE DeriveGeneric #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- This module represents Ast tree for parsed phi-calculus program
module Ast where
import GHC.Generics (Generic)

newtype Program = Program Expression      -- Q -> expr
  deriving (Eq, Show)

data Expression
  = ExFormation [Binding]                 -- [bindings]
  | ExThis                                -- $
  | ExGlobal                              -- Q
  | ExTermination                         -- T
  | ExMeta String                         -- !e
  | ExApplication Expression [Binding]    -- expr(attr -> expr)
  | ExDispatch Expression Attribute       -- expr.attr
  | ExMetaTail Expression String          -- expr * !t
  deriving (Eq, Show, Generic)

data Binding
  = BiTau Attribute Expression            -- attr -> expr
  | BiMeta String                         -- !B
  | BiDelta String                        -- D> 1F-2A
  | BiMetaDelta String                    -- D> !b
  | BiVoid Attribute                      -- attr -> ?
  | BiLambda String                       -- L> Function
  | BiMetaLambda String                   -- L> !F
  deriving (Eq, Show, Generic)

data Attribute
  = AtLabel String                        -- attr
  | AtAlpha Integer                       -- ~1
  | AtPhi                                 -- @
  | AtRho                                 -- ^
  | AtMeta String                         -- !a
  deriving (Eq, Show, Generic)
