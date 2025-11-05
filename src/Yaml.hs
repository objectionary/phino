{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Yaml where

import AST
import Control.Applicative (asum)
import Data.Aeson
import qualified Data.ByteString as BS
import Data.FileEmbed (embedDir)
import Data.Text (unpack)
import Data.Yaml (Parser)
import qualified Data.Yaml as Yaml
import GHC.Generics
import Misc (allPathsIn, validateYamlObject)
import Parser

parseJSON' :: String -> (String -> Either String a) -> Value -> Parser a
parseJSON' name func =
  withText
    name
    ( \txt -> case func (unpack txt) of
        Left err -> fail err
        Right parsed -> pure parsed
    )

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

instance FromJSON Bytes where
  parseJSON = parseJSON' "Bytes" parseBytes

instance FromJSON Expression where
  parseJSON = parseJSON' "Expression" parseExpression

instance FromJSON Binding where
  parseJSON = parseJSON' "Binding" parseBinding

instance FromJSON Number where
  parseJSON v = case v of
    Object o -> do
      validateYamlObject o ["ordinal", "length"]
      asum
        [ Ordinal <$> o .: "ordinal",
          Length <$> o .: "length"
        ]
    Number num -> pure (Literal (round num))
    _ ->
      fail "Expected a numerable expression (object or number)"

instance FromJSON Comparable where
  parseJSON v =
    asum
      [ CmpAttr <$> parseJSON v,
        CmpNum <$> parseJSON v,
        CmpExpr <$> parseJSON v
      ]

instance FromJSON Condition where
  parseJSON =
    withObject
      "Condition"
      ( \v -> do
          validateYamlObject v ["and", "or", "not", "alpha", "nf", "xi", "eq", "in", "matches", "part-of"]
          asum
            [ And <$> v .: "and",
              Or <$> v .: "or",
              Not <$> v .: "not",
              Alpha <$> v .: "alpha",
              NF <$> v .: "nf",
              XI <$> v .: "xi",
              do
                vals <- v .: "eq"
                case vals of
                  [left_, right_] -> Eq <$> parseJSON left_ <*> parseJSON right_
                  _ -> fail "'eq' expects exactly two arguments",
              do
                vals <- v .: "in"
                case vals of
                  [attr_, binding_] -> do
                    attr <- parseJSON attr_
                    bd <- parseJSON binding_
                    pure (In attr bd)
                  _ -> fail "'in' expects exactly two arguments",
              do
                vals <- v .: "matches"
                case vals of
                  [pat, exp] -> Matches <$> parseJSON pat <*> parseJSON exp
                  _ -> fail "'matches' expects exactly two arguments",
              do
                vals <- v .: "part-of"
                case vals of
                  [exp, bd] -> PartOf <$> parseJSON exp <*> parseJSON bd
                  _ -> fail "'part-of' expects exactly two arguments"
            ]
      )

instance FromJSON ExtraArgument where
  parseJSON v =
    asum
      [ ArgAttribute <$> parseJSON v,
        ArgBinding <$> parseJSON v,
        ArgExpression <$> parseJSON v,
        ArgBytes <$> parseJSON v
      ]

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
  | Literal Integer
  deriving (Generic, Show, Eq)

data Comparable
  = CmpAttr Attribute
  | CmpNum Number
  | CmpExpr Expression
  deriving (Generic, Show, Eq)

data Condition
  = And [Condition]
  | Or [Condition]
  | In Attribute Binding
  | Not Condition
  | Alpha Attribute
  | Eq Comparable Comparable
  | NF Expression
  | XI Expression
  | Matches String Expression
  | PartOf Expression Binding
  deriving (Generic, Show, Eq)

data ExtraArgument
  = ArgAttribute Attribute
  | ArgExpression Expression
  | ArgBinding Binding
  | ArgBytes Bytes
  deriving (Generic, Show, Eq)

data Extra = Extra
  { meta :: ExtraArgument,
    function :: String,
    args :: [ExtraArgument]
  }
  deriving (Generic, Show, Eq)

data Rule = Rule
  { name :: Maybe String,
    description :: Maybe String,
    pattern :: Expression,
    result :: Expression,
    when :: Maybe Condition,
    where_ :: Maybe [Extra],
    having :: Maybe Condition
  }
  deriving (Generic, Show, Eq)

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
