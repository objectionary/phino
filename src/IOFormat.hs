-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- | Module for input/output format types used across the application.
module IOFormat (IOFormat(..)) where

-- | Supported input/output formats for 𝜑-programs.
data IOFormat 
  = XMIR  -- ^ XMIR (XML Intermediate Representation) format
  | PHI   -- ^ Native 𝜑-calculus format
  deriving (Eq)

instance Show IOFormat where
  show XMIR = "xmir"
  show PHI = "phi"