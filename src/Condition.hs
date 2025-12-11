{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Condition (parseCondition, parseConditionThrows) where

import Control.Exception (Exception)
import Control.Exception.Base (throwIO)
import Data.Void (Void)
import Parser (PhiParser (..), phiParser)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf (printf)
import qualified Yaml as Y

newtype ConditionException = CouldNotParseCondition {message :: String}
  deriving (Exception)

instance Show ConditionException where
  show CouldNotParseCondition{..} = printf "Couldn't parse given condition, cause: %s" message

type Parser = Parsec Void String

-- White space consumer
whiteSpace :: Parser ()
whiteSpace = L.space hspace1 empty empty

-- Lexeme that ignores white spaces after
lexeme :: Parser a -> Parser a
lexeme = L.lexeme whiteSpace

-- Strict symbol (or sequence of symbols) with ignored white spaces after
symbol :: String -> Parser String
symbol = L.symbol whiteSpace

lparen :: Parser String
lparen = symbol "("

rparen :: Parser String
rparen = symbol ")"

comma :: Parser String
comma = symbol ","

number :: Parser Y.Number
number =
  choice
    [ do
        _ <- symbol "ordinal" >> lparen
        attr <- _attribute phiParser
        _ <- rparen
        return (Y.Ordinal attr)
    , do
        _ <- symbol "length" >> lparen
        bd <- _binding phiParser
        _ <- rparen
        return (Y.Length bd)
    , do
        sign <- optional (choice [char '-', char '+'])
        unsigned <- lexeme L.decimal
        return
          ( Y.Literal
              ( case sign of
                  Just '-' -> negate unsigned
                  _ -> unsigned
              )
          )
    ]

comparable :: Parser Y.Comparable
comparable =
  choice
    [ try $ Y.CmpNum <$> number
    , try $ Y.CmpAttr <$> _attribute phiParser
    , Y.CmpExpr <$> _expression phiParser
    ]

condition :: Parser Y.Condition
condition =
  choice
    [ do
        _ <- symbol "and" >> lparen
        args <- condition `sepBy1` comma
        _ <- rparen
        return (Y.And args)
    , do
        _ <- symbol "or" >> lparen
        args <- condition `sepBy1` comma
        _ <- rparen
        return (Y.Or args)
    , do
        _ <- symbol "in" >> lparen
        attr <- _attribute phiParser
        _ <- comma
        bd <- _binding phiParser
        _ <- rparen
        return (Y.In attr bd)
    , do
        _ <- symbol "not" >> lparen
        cond <- condition
        _ <- rparen
        return (Y.Not cond)
    , do
        _ <- symbol "alpha" >> lparen
        attr <- _attribute phiParser
        _ <- rparen
        return (Y.Alpha attr)
    , do
        _ <- symbol "eq" >> lparen
        left <- comparable
        _ <- comma
        right <- comparable
        _ <- rparen
        return (Y.Eq left right)
    , do
        _ <- symbol "nf" >> lparen
        expr <- _expression phiParser
        _ <- rparen
        return (Y.NF expr)
    , do
        _ <- symbol "xi" >> lparen
        expr <- _expression phiParser
        _ <- rparen
        return (Y.XI expr)
    , do
        _ <- symbol "matches" >> lparen
        ptn <- _string phiParser
        _ <- comma
        expr <- _expression phiParser
        _ <- rparen
        return (Y.Matches ptn expr)
    , do
        _ <- symbol "part-of" >> lparen
        expr <- _expression phiParser
        _ <- comma
        bd <- _binding phiParser
        _ <- rparen
        return (Y.PartOf expr bd)
    ]

parseCondition :: String -> Either String Y.Condition
parseCondition input = do
  let parsed =
        runParser
          ( do
              _ <- whiteSpace
              p <- condition
              _ <- eof
              return p
          )
          "condition"
          input
  case parsed of
    Right parsed' -> Right parsed'
    Left err -> Left (errorBundlePretty err)

parseConditionThrows :: String -> IO Y.Condition
parseConditionThrows cnd = case parseCondition cnd of
  Right cond -> pure cond
  Left err -> throwIO (CouldNotParseCondition err)
