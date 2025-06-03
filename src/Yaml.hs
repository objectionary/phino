{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Yaml where

import Ast
import Control.Applicative (asum)
import Data.Aeson
import Data.Aeson.KeyMap (keys)
import Data.String (IsString (..))
import Data.Text (unpack)
import Data.Yaml (Parser)
import qualified Data.Yaml as Yaml
import Debug.Trace
import GHC.Base
import GHC.Generics
import Parser

parseJSON' :: String -> (String -> Either String a) -> Value -> Parser a
parseJSON' name func =
  withText
    name
    ( \txt -> case func (unpack txt) of
        Left err -> fail err
        Right parsed -> pure parsed
    )

instance FromJSON Expression where
  parseJSON = parseJSON' "Expression" parseExpression

instance FromJSON Attribute where
  parseJSON = parseJSON' "Attribute" parseAttribute

instance FromJSON Binding where
  parseJSON = parseJSON' "Binding" parseBinding

instance FromJSON Condition where
  parseJSON =
    withObject
      "Condition"
      ( \v ->
          asum
            [ And <$> v .: "and",
              Or <$> v .: "or",
              do
                vals <- v .: "in"
                case vals of
                  [attrs_, bindings_] -> do
                    attrs' <- parseJSON attrs_
                    bds <- parseJSON bindings_
                    pure (In attrs' bds)
                  _ -> fail "'in' must contain exactly two elements"
            ]
      )

data Condition
  = And {and_ :: [Condition]}
  | Or {or_ :: [Condition]}
  | In {attrs :: [Attribute], bindings :: [Binding]}
  deriving (Generic, Show)

data Rule = Rule
  { name :: Maybe String,
    pattern :: Expression,
    result :: Expression,
    when :: Maybe Condition
  }
  deriving (Generic, FromJSON, Show)

yamlRule :: FilePath -> IO Rule
yamlRule = Yaml.decodeFileThrow
