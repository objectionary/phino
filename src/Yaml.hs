{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Yaml where

import Ast
import Control.Applicative (asum)
import Data.Aeson
import qualified Data.ByteString as BS
import Data.FileEmbed (embedDir)
import Data.Text (unpack)
import Data.Yaml (Parser)
import qualified Data.Yaml as Yaml
import GHC.Generics
import Misc (allPathsIn)
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

instance FromJSON Number where
  parseJSON v = case v of
    Object o ->
      asum
        [ Ordinal <$> o .: "ordinal",
          Length <$> o .: "length",
          do
            vals <- o .: "add"
            case vals of
              [first_, second_] -> do
                first <- parseJSON first_
                second <- parseJSON second_
                pure (Add first second)
              _ -> fail "'add' requires exactly two elements"
        ]
    Number num -> pure (Literal (round num))
    _ ->
      fail "Expected a numerable expression (object or number)"

instance FromJSON Comparable where
  parseJSON v =
    asum
      [ CmpAttr <$> parseJSON v,
        CmpNum <$> parseJSON v
      ]

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
              NF <$> v .: "nf",
              FN <$> v .: "fn",
              do
                vals <- v .: "eq"
                case vals of
                  [left_, right_] -> Eq <$> parseJSON left_ <*> parseJSON right_
                  _ -> fail "'eq' must contain exactly two elements",
              do
                vals <- v .: "in"
                case vals of
                  [attr_, binding_] -> do
                    attr <- parseJSON attr_
                    bd <- parseJSON binding_
                    pure (In attr bd)
                  _ -> fail "'in' must contain exactly two elements"
            ]
      )

instance FromJSON Extra where
  parseJSON = genericParseJSON defaultOptions

instance FromJSON Rule where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = \case
            "where_" -> "where"
            other -> other
        }

data Number
  = Ordinal Attribute
  | Length Binding
  | Add Number Number
  | Literal Integer
  deriving (Generic, Show)

data Comparable
  = CmpAttr Attribute
  | CmpNum Number
  deriving (Generic, Show)

data Condition
  = And [Condition]
  | Or [Condition]
  | In Attribute Binding
  | Not Condition
  | Alpha Attribute
  | Eq Comparable Comparable
  | NF Expression
  | FN Expression
  deriving (Generic, Show)

data Extra = Extra
  { meta :: Expression,
    function :: String,
    args :: [Expression]
  }
  deriving (Generic, Show)

data Rule = Rule
  { name :: Maybe String,
    pattern :: Expression,
    result :: Expression,
    when :: Maybe Condition,
    where_ :: Maybe [Extra]
  }
  deriving (Generic, Show)

normalizationRules :: [Rule]
{-# NOINLINE normalizationRules #-}
normalizationRules = map decodeRule $(embedDir "resources")
  where
    decodeRule :: (FilePath, BS.ByteString) -> Rule
    decodeRule (path, bs) =
      case Yaml.decodeEither' bs of
        Right r -> r
        Left err -> error $ "YAML parse error in " ++ path ++ ": " ++ show err

yamlRule :: FilePath -> IO Rule
yamlRule = Yaml.decodeFileThrow
