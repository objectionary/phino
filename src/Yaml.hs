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
  parseJSON =
    withText
      "Attribute"
      ( \txt -> case unpack txt of
          "λ" -> pure AtLambda
          "Δ" -> pure AtDelta
          other -> case parseAttribute other of
            Left err -> fail err
            Right attr -> pure attr
      )
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
              Not <$> v .: "not",
              Alpha <$> v .: "alpha",
              do
                vals <- v .: "eq"
                case vals of
                  [left_, right_] -> do
                    left <- parseJSON left_
                    right <- parseJSON right_
                    pure (Eq left right)
                  _ -> fail "'eq' must contain exactly two elements",
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
  = And [Condition]
  | Or [Condition]
  | In [Attribute] [Binding]
  | Not Condition
  | Alpha Attribute
  | Eq Attribute Attribute
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
