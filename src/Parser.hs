{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to parse given phi program to Ast
module Parser
  ( parseProgram,
    parseProgramThrows,
    parseExpression,
    parseExpressionThrows,
    parseAttribute,
    parseBinding,
  )
where

import Ast
import Control.Exception (Exception, throwIO)
import Control.Monad (guard)
import Data.Char (isAsciiLower, isDigit, isLower)
import Data.Scientific (toRealFloat)
import Data.Sequence (mapWithIndex)
import Data.Text.Internal.Fusion.Size (lowerBound)
import Data.Void
import GHC.Char (chr)
import Misc
import Numeric (readHex)
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
  first <- oneOf ['a' .. 'z']
  rest <- many (satisfy (`notElem` " \r\n\t,.|':;!?][}{)(⟧⟦") <?> "allowed character")
  return (first : rest)

escapedChar :: Parser Char
escapedChar = do
  _ <- char '\\'
  c <- oneOf ['\\', '"', 'n', 'r', 't', 'b', 'f', 'u', 'x']
  case c of
    '\\' -> return '\\'
    '"' -> return '"'
    'n' -> return '\n'
    'r' -> return '\r'
    't' -> return '\t'
    'b' -> return '\b'
    'f' -> return '\f'
    'u' -> unicodeEscape
    'x' -> hexEscape
    _ -> fail $ "Unknown escape: \\" ++ [c]
  where
    unicodeEscape :: Parser Char
    unicodeEscape = do
      hexDigits <- count 4 hexDigitChar
      case readHex hexDigits of
        [(n, "")] ->
          if n >= 0xD800 && n <= 0xDBFF
            then -- High surrogate, look for low surrogate
              do
                _ <- string "\\u"
                lowHexDigits <- count 4 hexDigitChar
                case readHex lowHexDigits of
                  [(low, "")] ->
                    if low >= 0xDC00 && low <= 0xDFFF
                      then do
                        -- Valid surrogate pair, combine them
                        let codePoint = 0x10000 + ((n - 0xD800) * 0x400) + (low - 0xDC00)
                        return (chr codePoint)
                      else fail ("Invalid low surrogate: \\u" ++ lowHexDigits)
                  _ -> fail ("Invalid low surrogate hex: \\u" ++ lowHexDigits)
            else
              if n >= 0xDC00 && n <= 0xDFFF
                then fail ("Unexpected low surrogate: \\u" ++ hexDigits)
                else
                  if n >= 0 && n <= 0x10FFFF
                    then return (chr n)
                    else fail ("Invalid Unicode code point: \\u" ++ hexDigits)
    hexEscape :: Parser Char
    hexEscape = do
      digits <- count 2 hexDigitChar
      case readHex digits of
        [(n, "")] -> return (chr n)
        _ -> fail $ "Invalid hex escape: \\x" ++ digits

function :: Parser String
function = lexeme $ do
  first <- oneOf ['A' .. 'Z']
  rest <-
    many
      ( satisfy
          (\ch -> isDigit ch || isAsciiLower ch || ch == '_' || ch == 'φ')
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

meta' :: Char -> String -> Parser String
meta' ch uni =
  choice
    [ meta ch,
      do
        _ <- symbol uni
        ds <- lexeme (many digitChar)
        return (ch : ds)
    ]

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
bytes :: Parser Bytes
bytes =
  lexeme
    ( choice
        [ symbol "--" >> return BtEmpty,
          try $ do
            first <- byte
            rest <- some $ do
              _ <- char '-'
              byte
            return (BtMany (first : rest)),
          do
            bte <- byte
            _ <- char '-'
            return (BtOne bte)
        ]
        <?> "bytes"
    )

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
        return (BiTau attr' (ExFormation (withVoidRho (voids ++ bs))))
    ]

metaBinding :: Parser Binding
metaBinding = BiMeta <$> meta' 'B' "𝐵"

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
        BiDelta . BtMeta <$> meta 'b',
      try metaBinding,
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
      AtMeta <$> meta' 'a' "𝜏"
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
    [ do
        ExFormation bs <- formation
        return (ExFormation (withVoidRho bs)),
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
        return (DataObject "number" (numToBts num)),
      lexeme $ do
        _ <- char '"'
        str <- manyTill (choice [escapedChar, noneOf ['\\', '"']]) (char '"')
        return (DataObject "string" (strToBts str)),
      try (ExMeta <$> meta' 'e' "𝑒"),
      ExDispatch ExThis <$> fullAttribute
    ]
    <?> "expression head"

application :: Expression -> [Binding] -> Expression
application = foldl ExApplication

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
                return (application expr bds),
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

parseBinding :: String -> Either String Binding
parseBinding = parse' "binding" binding

parseAttribute :: String -> Either String Attribute
parseAttribute = parse' "attribute" fullAttribute

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
