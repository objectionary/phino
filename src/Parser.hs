{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to parse given phi program to AST
module Parser
  ( parseProgram
  , parseProgramThrows
  , parseExpression
  , parseExpressionThrows
  , parseAttribute
  , parseAttributeThrows
  , parseAlpha
  , parseIndex
  , parseNumber
  , parseNumberThrows
  , parseBinding
  , parseBytes
  , PhiParser (..)
  , phiParser
  )
where

import AST
import Control.Exception (Exception, throwIO)
import Control.Monad (guard)
import Data.Char (isAsciiLower, isDigit)
import Data.Scientific (toRealFloat)
import qualified Data.Text as T
import Data.Void
import GHC.Char
import Misc
import Numeric
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Printf (printf)

type Parser = Parsec Void String

data ParserException
  = CouldNotParseProgram {message :: String}
  | CouldNotParseExpression {message :: String}
  | CouldNotParseAttribute {message :: String}
  | CouldNotParseNumber {message :: String}
  deriving (Exception)

data PhiParser = PhiParser
  { _attribute :: Parser Attribute
  , _alpha :: Parser Alpha
  , _index :: Parser T.Text
  , _binding :: Parser Binding
  , _expression :: Parser Expression
  , _string :: Parser String
  }

phiParser :: PhiParser
phiParser = PhiParser attribute alpha index' binding expression quotedStr

instance Show ParserException where
  show CouldNotParseProgram{..} = printf "Couldn't parse given phi program, cause: %s" message
  show CouldNotParseExpression{..} = printf "Couldn't parse given phi expression, cause: %s" message
  show CouldNotParseAttribute{..} = printf "Couldn't parse given attribute, cause: %s" message
  show CouldNotParseNumber{..} = printf "Couldn't parse given number to 'Φ.number', cause: %s" message

-- White space consumer
whiteSpace :: Parser ()
whiteSpace = L.space space1 empty empty

-- Lexeme that ignores white spaces after
lexeme :: Parser a -> Parser a
lexeme = L.lexeme whiteSpace

-- Strict symbol (or sequence of symbols) with ignored white spaces after
symbol :: String -> Parser String
symbol = L.symbol whiteSpace

-- Parsed as String then packed to Text once; BiLambda keeps String so function stays String
label' :: Parser T.Text
label' = lexeme $ do
  first <- oneOf ['a' .. 'z']
  rest <- many (satisfy (`notElem` " \r\n\t,.|':;!?][}{)(⟧⟦") <?> "allowed character")
  return (T.pack (first : rest))

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
    [ symbol "D>"
    , symbol "Δ" >> dashedArrow
    ]

lambda :: Parser String
lambda =
  choice
    [ symbol "L>"
    , symbol "λ" >> dashedArrow
    ]

dashedArrow :: Parser String
dashedArrow = symbol "⤍"

arrow :: Parser String
arrow = choice [symbol "->", symbol "↦"]

global :: Parser String
global = choice [symbol "Q", symbol "Φ"]

metaSuffix :: Parser String
metaSuffix = lexeme (many (oneOf ('_' : '-' : ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']) <?> "meta suffix"))

-- Meta variable names are packed to Text once here; all AST meta fields are Text
meta :: Char -> Parser T.Text
meta ch = do
  _ <- char '!'
  c <- char ch
  suf <- metaSuffix
  return (T.pack (c : suf))

meta' :: Char -> String -> Parser T.Text
meta' ch uni =
  choice
    [ meta ch
    , do
        _ <- string uni
        suf <- metaSuffix
        return (T.pack (ch : suf))
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
-- 0. meta: !b
-- 1. empty: --
-- 2. one byte: 01-
-- 3. many bytes: 01-02-...-FF
bytes :: Parser Bytes
bytes =
  lexeme
    ( choice
        [ BtMeta <$> meta' 'd' "δ"
        , symbol "--" >> return BtEmpty
        , try $ do
            first <- byte
            rest <- some $ do
              _ <- char '-'
              byte
            return (BtMany (first : rest))
        , do
            bte <- byte
            _ <- char '-'
            return (BtOne bte)
        ]
        <?> "bytes"
    )

number :: Parser Expression
number = do
  sign <- optional (choice [char '-', char '+'])
  unsigned <- lexeme L.scientific
  return
    ( DataNumber
        ( numToBts
            ( case sign of
                -- Negate the Double rather than the Scientific so that a zero
                -- literal preserves its sign: Scientific has no negative zero,
                -- but negate on Double yields -0.0, a distinct IEEE-754 value.
                Just '-' -> negate (toRealFloat unsigned)
                _ -> toRealFloat unsigned
            )
        )
    )

quotedStr :: Parser String
quotedStr = char '"' >> manyTill (choice [escapedChar, noneOf ['\\', '"']]) (char '"')
  where
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
        _ -> fail ("Unknown escape: \\" ++ [c])
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
        _ -> fail ("Invalid Unicode escape: \\u" ++ hexDigits)
    hexEscape :: Parser Char
    hexEscape = do
      digits <- count 2 hexDigitChar
      case readHex digits of
        [(n, "")] -> return (chr n)
        _ -> fail ("Invalid hex escape: \\x" ++ digits)

tauValue :: Parser Expression
tauValue =
  choice
    [ try $ do
        _ <- arrow
        expression
    , do
        _ <- symbol "("
        voids <-
          choice
            [ rb >> return []
            , do
                voids' <- map BiVoid <$> void' `sepBy1` symbol ","
                rb >> return voids'
            ]
        _ <- arrow
        bs <- formationBindings
        bds <- validatedBindings (voids ++ bs)
        return (ExFormation (withVoidRho bds))
    ]
  where
    rb :: Parser String
    rb = symbol ")"

tauBinding :: Parser Attribute -> Parser Binding
tauBinding attr = BiTau <$> attr <*> tauValue

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
    [ try (tauBinding attribute)
    , try $ do
        attr <- attribute
        _ <- arrow
        _ <- choice [symbol "?", symbol "∅"]
        return (BiVoid attr)
    , try $ do
        _ <- delta
        BiDelta <$> bytes
    , try metaBinding
    , try $ do
        _ <- lambda
        BiLambda . Function . T.pack <$> function
    , do
        _ <- lambda
        BiLambda . FnMeta <$> meta' 'F' "𝐹"
    ]
    <?> "binding"

-- inlined void attribute
-- 1. label
-- 2. rho
-- 3. phi
void' :: Parser Attribute
void' =
  choice
    [ AtLabel <$> label'
    , do
        _ <- choice [symbol "^", symbol "ρ"]
        return AtRho
    , do
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
    [ void'
    , AtMeta <$> meta' 't' "𝜏"
    ]
    <?> "attribute"

-- index meta: !i, 𝑖
index' :: Parser T.Text
index' = meta' 'i' "𝑖"

-- alpha
-- 1. index: ~0, α0
-- 2. meta: α𝑖, ~!i
alpha :: Parser Alpha
alpha = do
  _ <- choice [symbol "~", symbol "α"]
  choice
    [ Alpha <$> lexeme L.decimal
    , AlMeta <$> index'
    ]
    <?> "alpha"

-- application argument
-- 1. tau: <attribute> ↦ <expression>
-- 2. alpha: <alpha> ↦ <expression>
argument :: Parser Argument
argument =
  choice
    [ try (ArAlpha <$> alpha <*> tauValue)
    , ArTau <$> attribute <*> tauValue
    ]
    <?> "argument"

validatedBindings :: [Binding] -> Parser [Binding]
validatedBindings bds = case uniqueBindings bds of
  Left msg -> fail msg
  Right bds' -> return bds'

-- formation
formationBindings :: Parser [Binding]
formationBindings = do
  _ <- choice [symbol "[[", symbol "⟦"]
  choice
    [ rsb >> return []
    , do
        bs <- binding `sepBy1` symbol ","
        rsb >> return bs
    ]
  where
    rsb :: Parser String
    rsb = choice [symbol "]]", symbol "⟧"]

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
        bs <- formationBindings >>= validatedBindings
        return (ExFormation (withVoidRho bs))
    , do
        _ <- choice [symbol "$", symbol "ξ"]
        return ExXi
    , do
        _ <- global
        return ExRoot
    , do
        _ <- choice [symbol "T", symbol "⊥"]
        return ExTermination
    , number
    , lexeme (DataString . strToBts <$> quotedStr)
    , try (ExMeta <$> meta' 'e' "𝑒")
    , try (ExMeta <$> meta' 'n' "𝑛")
    , try (ExMeta <$> meta' 'k' "𝑘")
    , ExDispatch ExXi <$> attribute
    ]
    <?> "expression head"

application :: Expression -> [Argument] -> Expression
application = foldl ExApplication

-- tail optional part of application
-- 1. any head + dispatch
-- 2. any head except $ and Q + application
exTail :: Expression -> Parser Expression
exTail expr =
  choice
    [ do
        next <-
          choice
            [ do
                _ <- symbol "."
                ExDispatch expr <$> attribute
            , do
                guard
                  ( case expr of
                      ExXi -> False
                      ExRoot -> False
                      _ -> True
                  )
                _ <- symbol "("
                bds <-
                  choice
                    [ try $ argument `sepBy1` symbol ","
                    , do
                        exprs <- expression `sepBy1` symbol ","
                        return (zipWith (ArAlpha . Alpha) [0 ..] exprs)
                    ]
                _ <- symbol ")"
                return (application expr bds)
            ]
            <?> "dispatch or application"
        exTail next
    , return expr
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
        return prog
    , do
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

parseBytes :: String -> Either String Bytes
parseBytes = parse' "bytes" bytes

parseBinding :: String -> Either String Binding
parseBinding = parse' "binding" binding

parseNumber :: String -> Either String Expression
parseNumber = parse' "number" number

parseNumberThrows :: String -> IO Expression
parseNumberThrows num = case parseNumber num of
  Right num' -> pure num'
  Left err -> throwIO (CouldNotParseNumber err)

parseAttribute :: String -> Either String Attribute
parseAttribute = parse' "attribute" attribute

parseAlpha :: String -> Either String Alpha
parseAlpha = parse' "alpha" alpha

parseIndex :: String -> Either String T.Text
parseIndex = parse' "index meta" index'

parseAttributeThrows :: String -> IO Attribute
parseAttributeThrows attr = case parseAttribute attr of
  Right attr' -> pure attr'
  Left err -> throwIO (CouldNotParseAttribute err)

parseExpression :: String -> Either String Expression
parseExpression = parse' "expression" expression

parseExpressionThrows :: String -> IO Expression
parseExpressionThrows ex = case parseExpression ex of
  Right expr -> pure expr
  Left err -> throwIO (CouldNotParseExpression err)

parseProgram :: String -> Either String Program
parseProgram = parse' "program" program

parseProgramThrows :: String -> IO Program
parseProgramThrows prg = case parseProgram prg of
  Right prog -> pure prog
  Left err -> throwIO (CouldNotParseProgram err)
