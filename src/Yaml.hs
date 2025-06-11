{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
import Parser
import qualified Rule as R

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

instance FromJSON R.Number where
  parseJSON v = case v of
    Object o ->
      asum
        [ R.Ordinal <$> o .: "ordinal",
          R.Length <$> o .: "length",
          do
            vals <- o .: "add"
            case vals of
              [first_, second_] -> do
                first <- parseJSON first_
                second <- parseJSON second_
                pure (R.Add first second)
              _ -> fail "'add' requires exactly two elements"
        ]
    Number num -> pure (R.Literal (round num))
    _ ->
      fail "Expected a numerable expression (object or number)"

instance FromJSON R.Comparable where
  parseJSON v =
    asum
      [ R.CmpAttr <$> parseJSON v,
        R.CmpNum <$> parseJSON v
      ]

instance FromJSON R.Condition where
  parseJSON =
    withObject
      "Condition"
      ( \v ->
          asum
            [ R.And <$> v .: "and",
              R.Or <$> v .: "or",
              R.Not <$> v .: "not",
              R.Alpha <$> v .: "alpha",
              do
                vals <- v .: "eq"
                case vals of
                  [left_, right_] -> R.Eq <$> parseJSON left_ <*> parseJSON right_
                  _ -> fail "'eq' must contain exactly two elements",
              do
                vals <- v .: "in"
                case vals of
                  [attr_, binding_] -> do
                    attr <- parseJSON attr_
                    bd <- parseJSON binding_
                    pure (R.In attr bd)
                  _ -> fail "'in' must contain exactly two elements"
            ]
      )

instance FromJSON R.Extra where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON R.Rule where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \case
        "where_" -> "where"
        other -> other
    }

yamlRule :: FilePath -> IO R.Rule
yamlRule = Yaml.decodeFileThrow
