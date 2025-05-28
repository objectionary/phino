{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to parse given phi program to Ast
module Parser (parseProgram, parseProgramThrows, parseExpression, parseExpressionThrows) where

import Ast
import Control.Exception (Exception, throwIO)
import Control.Monad (guard)
import Data.Char (isDigit, isLower)
import Data.Scientific (toRealFloat)
import Data.Sequence (mapWithIndex)
import Data.Text.Internal.Fusion.Size (lowerBound)
import Data.Void
import Misc (numToHex, strToHex)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, hexDigitChar, letterChar, lowerChar, space1, string, upperChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf (printf)

type Parser = Parsec Void String

data ParserException
  = CouldNotParseProgram {message :: String}
  | CouldNotParseExpression {message :: String}
  deriving (Exception)

instance Show ParserException where
  show CouldNotParseProgram {..} = printf "Couldn't parse given phi program, cause: %s" message
  show CouldNotParseExpression {..} = printf "Couldn't parse given phi program, cause: %s" message

dataExpression :: String -> String -> Expression
dataExpression obj bts =
  ExApplication
    (ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel obj))
    [ BiTau
        (AtAlpha 0)
        ( ExApplication
            (ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel "bytes"))
            [ BiTau
                (AtAlpha 0)
                (ExFormation [BiDelta bts])
            ]
        )
    ]

-- White space consumer
whiteSpace :: Parser ()
whiteSpace = L.space space1 empty empty

-- Lexeme that ignores white spaces after
lexeme :: Parser a -> Parser a
lexeme = L.lexeme whiteSpace

-- Strict symbol (or sequence of symbols) with ignored white spaces after
symbol :: String -> Parser String
symbol = L.symbol whiteSpace

label' :: Parser String
label' = lexeme $ do
  first <- lowerChar
  rest <- many (satisfy (`notElem` " \r\n\t,.|':;!?][}{)(⟧⟦") <?> "allowed character")
  return (first : rest)

function :: Parser String
function = lexeme $ do
  first <- upperChar
  rest <-
    many
      ( satisfy
          (\ch -> isDigit ch || isLower ch || ch == '_' || ch == 'φ')
          <?> "allowed character in function name"
      )
  return (first : rest)

delta :: Parser String
delta =
  choice
    [ symbol "D>",
      symbol "Δ" >> dashedArrow
    ]

lambda :: Parser String
lambda =
  choice
    [ symbol "L>",
      symbol "λ" >> dashedArrow
    ]

dashedArrow :: Parser String
dashedArrow = symbol "⤍"

arrow :: Parser String
arrow = choice [symbol "->", symbol "↦"]

global :: Parser String
global = choice [symbol "Q", symbol "Φ"]

meta :: Char -> Parser String
meta ch = do
  _ <- char '!'
  c <- char ch
  ds <- lexeme (many digitChar)
  return (c : ds)

byte :: Parser String
byte = do
  f <- hexDigitChar >>= upperHex
  s <- hexDigitChar >>= upperHex
  return [f, s]
  where
    upperHex ch
      | isDigit ch || ('A' <= ch && ch <= 'F') = return ch
      | otherwise = fail ("expected 0-9 or A-F, got " ++ show ch)

-- bytes
-- 1. empty: --
-- 2. one byte: 01-
-- 3. many bytes: 01-02-...-FF
bytes :: Parser String
bytes = lexeme $ do
  choice
    [ symbol "--",
      try $ do
        first <- byte
        rest <- some $ do
          dash <- char '-'
          bte <- byte
          return (dash : bte)
        return (first ++ concat rest),
      do
        bte <- byte
        dash <- char '-'
        return (bte ++ [dash])
    ]
    <?> "bytes"

tauBinding :: Parser Attribute -> Parser Binding
tauBinding attr = do
  attr' <- attr
  choice
    [ try $ do
        _ <- arrow
        BiTau attr' <$> expression,
      do
        _ <- symbol "("
        voids <- map BiVoid <$> void' `sepBy` symbol ","
        _ <- symbol ")"
        _ <- arrow
        ExFormation bs <- formation
        return (BiTau attr' (ExFormation (voids ++ bs)))
    ]

-- binding
-- 1. tau
-- 2. void
-- 3. delta
-- 4. meta delta
-- 5. meta
-- 6. lambda
-- 7. meta lambda
binding :: Parser Binding
binding =
  choice
    [ try (tauBinding attribute),
      try $ do
        attr <- attribute
        _ <- arrow
        _ <- choice [symbol "?", symbol "∅"]
        return (BiVoid attr),
      try $ do
        _ <- delta
        BiDelta <$> bytes,
      try $ do
        _ <- delta
        BiMetaDelta <$> meta 'b',
      try (BiMeta <$> meta 'B'),
      try $ do
        _ <- lambda
        BiLambda <$> function,
      do
        _ <- lambda
        BiMetaLambda <$> meta 'F'
    ]
    <?> "binding"

-- inlined void attribute
-- 1. label
-- 2. rho
-- 3. phi
void' :: Parser Attribute
void' =
  choice
    [ AtLabel <$> label',
      do
        _ <- choice [symbol "^", symbol "ρ"]
        return AtRho,
      do
        _ <- choice [symbol "@", symbol "φ"]
        return AtPhi
    ]

-- attribute
-- 1. label
-- 2. meta
-- 3. rho
-- 4. phi
attribute :: Parser Attribute
attribute =
  choice
    [ void',
      AtMeta <$> meta 'a'
    ]
    <?> "attribute"

-- full attribute
-- 1. label
-- 2. meta
-- 3. rho
-- 4. phi
-- 5. alpha
fullAttribute :: Parser Attribute
fullAttribute =
  choice
    [ attribute,
      do
        _ <- choice [symbol "~", symbol "α"]
        AtAlpha <$> lexeme L.decimal
    ]
    <?> "full attribute"

-- formation
formation :: Parser Expression
formation = do
  _ <- choice [symbol "[[", symbol "⟦"]
  bs <- binding `sepBy` symbol ","
  _ <- choice [symbol "]]", symbol "⟧"]
  return (ExFormation bs)

-- head part of expression
-- 1. formation
-- 2. this
-- 3. global
-- 4. termination
-- 5. meta expression
-- 6. full attribute -> sugar for $.attr
exHead :: Parser Expression
exHead =
  choice
    [ formation,
      do
        _ <- choice [symbol "$", symbol "ξ"]
        return ExThis,
      try $ do
        _ <- choice [symbol "QQ", symbol "Φ̇"]
        return (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")),
      do
        _ <- global
        return ExGlobal,
      do
        _ <- choice [symbol "T", symbol "⊥"]
        return ExTermination,
      do
        sign <- optional (choice [char '-', char '+'])
        unsigned <- lexeme L.scientific
        let num =
              toRealFloat
                ( case sign of
                    Just '-' -> negate unsigned
                    _ -> unsigned
                )
        return (dataExpression "number" (numToHex num)),
      lexeme $ do
        _ <- char '"'
        str <- manyTill L.charLiteral (char '"')
        return (dataExpression "string" (strToHex str)),
      try (ExMeta <$> meta 'e'),
      ExDispatch ExThis <$> fullAttribute
    ]
    <?> "expression head"

-- tail optional part of application
-- 1. any head + dispatch
-- 2. any head except $ and Q + application
-- 3. any head except meta tail + meta tail
exTail :: Expression -> Parser Expression
exTail expr =
  choice
    [ do
        next <-
          choice
            [ do
                _ <- symbol "."
                ExDispatch expr <$> fullAttribute,
              do
                guard
                  ( case expr of
                      ExThis -> False
                      ExGlobal -> False
                      _ -> True
                  )
                _ <- symbol "("
                bds <-
                  choice
                    [ try $ tauBinding fullAttribute `sepBy1` symbol ",",
                      do
                        exprs <- expression `sepBy1` symbol ","
                        return (zipWith (BiTau . AtAlpha) [0 ..] exprs) -- \idx expr -> BiTau (AtAlpha idx) expr
                    ]
                _ <- symbol ")"
                return (ExApplication expr bds),
              do
                guard
                  ( case expr of
                      ExMetaTail _ _ -> False
                      _ -> True
                  )
                _ <- symbol "*"
                ExMetaTail expr <$> meta 't'
            ]
            <?> "dispatch or application"
        exTail next,
      return expr
    ]

expression :: Parser Expression
expression = do
  expr <- exHead
  exTail expr

program :: Parser Program
program =
  choice
    [ do
        _ <- symbol "{"
        prog <- Program <$> expression
        _ <- symbol "}"
        return prog,
      do
        _ <- global
        _ <- arrow
        Program <$> expression
    ]
    <?> "program"

-- Entry point
parse' :: String -> Parser a -> String -> Either String a
parse' name parser input = do
  let parsed =
        runParser
          ( do
              _ <- whiteSpace
              p <- parser
              _ <- eof
              return p
          )
          name
          input
  case parsed of
    Right parsed' -> Right parsed'
    Left err -> Left (errorBundlePretty err)

parseExpression :: String -> Either String Expression
parseExpression = parse' "expression" expression

parseExpressionThrows :: String -> IO Expression
parseExpressionThrows expression = case parseExpression expression of
  Right expr -> pure expr
  Left err -> throwIO (CouldNotParseExpression err)

parseProgram :: String -> Either String Program
parseProgram = parse' "program" program

parseProgramThrows :: String -> IO Program
parseProgramThrows program = case parseProgram program of
  Right prog -> pure prog
  Left err -> throwIO (CouldNotParseProgram err)
