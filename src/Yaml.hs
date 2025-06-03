{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Yaml where

import Ast
import Data.Aeson
import Data.String (IsString (..))
import Data.Text (unpack)
import qualified Data.Yaml as Yaml
import GHC.Generics
import Parser

instance FromJSON Expression where
  parseJSON =
    withText
      "Expression"
      ( \txt -> case parseExpression (unpack txt) of
          Left err -> fail err
          Right expr -> pure expr
      )

data Rule = Rule
  { name :: Maybe String,
    pattern :: Expression,
    result :: Expression
  }
  deriving (Generic, FromJSON, Show)

yamlRule :: FilePath -> IO Rule
yamlRule = Yaml.decodeFileThrow
