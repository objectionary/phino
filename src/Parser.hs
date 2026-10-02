{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Parser
  ( parseExpression
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
import Bytes (nonFiniteBts, nonFiniteOf, numToBts, strToBts)
import Control.Exception (Exception)
import Control.Monad (guard, when)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
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
import Text.Read (readMaybe)

type Parser = Parsec Void String

data ParserException
  = CouldNotParseExpression {message :: String}
  | CouldNotParseAttribute {message :: String}
  | CouldNotParseNumber {message :: String}
  deriving (Exception)

data PhiParser = PhiParser
  { _attribute :: Parser Attribute
  , _alpha :: Parser Alpha
  , _index :: Parser (Either Slot T.Text)
  , _binding :: Parser Binding
  , _expression :: Parser Expression
  , _string :: Parser String
  }

phiParser :: PhiParser
phiParser = PhiParser attribute alpha indexVar binding expression quotedStr

instance Show ParserException where
  show CouldNotParseExpression{..} = printf "Couldn't parse given phi expression, cause: %s" message
  show CouldNotParseAttribute{..} = printf "Couldn't parse given attribute, cause: %s" message
  show CouldNotParseNumber{..} = printf "Couldn't parse given number to 'Φ.number', cause: %s" message

whiteSpace :: Parser ()
whiteSpace = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whiteSpace

symbol :: String -> Parser String
symbol = L.symbol whiteSpace

label' :: Parser T.Text
label' = lexeme $ do
  first <- oneOf ['a' .. 'z']
  rest <- many (satisfy (`notElem` " \r\n\t,.|':;!?][}{)(⟧⟦") <?> "allowed character")
  return (T.pack (first : rest))

function :: Parser String
function =
  lexeme
    ( do
        first <- oneOf ['A' .. 'Z']
        rest <-
          many
            ( satisfy
                (\ch -> isDigit ch || isAsciiLower ch || ch == '_' || ch == 'φ')
                <?> "allowed character in function name"
            )
        return (first : rest)
    )
    <?> "function name"

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
global = choice [ascii 'Q', symbol "Φ"]

ascii :: Char -> Parser String
ascii letter = lexeme (try (pure <$> char letter <* notFollowedBy (satisfy named <|> '_' <$ lambdaOf)))
  where
    named :: Char -> Bool
    named ch = isDigit ch || isAsciiLower ch || ch == '_' || ch == 'φ'
    lambdaOf :: Parser Char
    lambdaOf = whiteSpace >> char ':' >> whiteSpace >> oneOf ['L', 'λ']

metaSuffix :: Parser String
metaSuffix = lexeme (many (oneOf ('_' : '-' : ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']) <?> "meta suffix"))

metaVar :: Char -> String -> Parser (Either Slot T.Text)
metaVar ch uni = do
  offset <- getOffset
  suf <-
    choice
      [ char '!' >> char ch >> metaSuffix
      , string uni >> metaSuffix
      ]
  when
    (suf == "0")
    (fail (printf "the meta variable '!%c0' is indexed with zero, while indexes start with one" ch))
  return
    ( if null suf
        then Left (Slot (T.singleton ch) offset)
        else Right (T.pack (ch : suf))
    )

sigma :: Parser Function
sigma = metaVar 'S' "𝜎" >>= either (pure . FnFresh) numbered
  where
    numbered :: T.Text -> Parser Function
    numbered named = case readMaybe (T.unpack (T.drop 1 named)) of
      Just idx -> pure (FnSymbol idx)
      Nothing -> fail (printf "the symbol '%s' is numbered by something that is not an integer" (T.unpack named))

byte :: Parser String
byte = do
  f <- hexDigitChar >>= upperHex
  s <- hexDigitChar >>= upperHex
  return [f, s]
  where
    upperHex :: Char -> Parser Char
    upperHex ch
      | isDigit ch || ('A' <= ch && ch <= 'F') = return ch
      | otherwise = fail ("expected 0-9 or A-F, got " ++ show ch)

bytes :: Parser Bytes
bytes =
  lexeme
    ( choice
        [ either BtAny BtMeta <$> metaVar 'd' "𝛿"
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
                Just '-' -> negate (toRealFloat unsigned)
                _ -> toRealFloat unsigned
            )
        )
    )

root :: Parser Expression
root = do
  _ <- global
  option ExRoot (try labelled)
  where
    labelled :: Parser Expression
    labelled = do
      _ <- symbol "."
      named <$> label'
    named :: T.Text -> Expression
    named name = maybe (ExDispatch ExRoot (AtLabel name)) (DataNumber . nonFiniteBts) (nonFiniteOf name)

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
            then do
              _ <- string "\\u"
              lowHexDigits <- count 4 hexDigitChar
              case readHex lowHexDigits of
                [(low, "")] ->
                  if low >= 0xDC00 && low <= 0xDFFF
                    then do
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
    [ do
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
        opened <- expression
        case opened of
          ExFormation bds -> ExFormation <$> validatedBindings (voids ++ bds)
          _ -> fail "Inline voids open a formation, so nothing but a formation may follow their arrow"
    ]
  where
    rb :: Parser String
    rb = symbol ")"

lambdaName :: Parser Function
lambdaName = choice [Function . T.pack <$> function, try (either FnAny FnMeta <$> metaVar 'F' "𝑓"), sigma]

colon :: Parser String
colon = symbol ":"

alone :: Parser Binding -> Parser Expression
alone bd = ExFormation . pure <$> bd

deltaHead :: Parser Expression
deltaHead =
  lookAhead (satisfy (\ch -> isDigit ch || ('A' <= ch && ch <= 'F') || ch `elem` ("-!𝛿" :: String)))
    >> alone (try (BiDelta <$> bytes <* colon <* choice [symbol "D", symbol "Δ"]))

lambdaHead :: Parser Expression
lambdaHead =
  lookAhead (satisfy (\ch -> isAsciiUpper ch || ch `elem` ("!𝑓𝜎" :: String)))
    >> alone (try (BiLambda <$> lambdaName <* colon <* choice [symbol "L", symbol "λ"]))

voidHead :: Parser Expression
voidHead = alone (choice [symbol "?", symbol "∅"] >> colon >> BiVoid <$> attribute)

metaBinding :: Parser Binding
metaBinding = either BiAny BiMeta <$> metaVar 'B' "𝐵"

binding :: Parser Binding
binding =
  choice
    [ do
        _ <- try delta
        BiDelta <$> bytes
    , try metaBinding
    , do
        _ <- try lambda
        BiLambda <$> lambdaName
    , do
        attr <- attribute
        choice
          [ try blank >> return (BiVoid attr)
          , BiTau attr <$> tauValue
          ]
    ]
    <?> "binding"
  where
    blank :: Parser String
    blank = arrow >> choice [symbol "?", symbol "∅"] <* notFollowedBy colon

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

attribute :: Parser Attribute
attribute =
  choice
    [ void'
    , either AtAny AtMeta <$> metaVar 't' "𝜏"
    ]
    <?> "attribute"

indexVar :: Parser (Either Slot T.Text)
indexVar = metaVar 'i' "𝑖"

alpha :: Parser Alpha
alpha = do
  _ <- choice [symbol "~", symbol "α"]
  choice
    [ lexeme L.decimal >>= ranged
    , either AlAny AlMeta <$> indexVar
    ]
    <?> "alpha"
  where
    ranged :: Integer -> Parser Alpha
    ranged idx
      | idx > toInteger (maxBound :: Int) = fail (printf "the index of 'α%d' is too big, while it must fit into %d" idx (maxBound :: Int))
      | otherwise = pure (Alpha (fromInteger idx))

argument :: Parser Argument
argument =
  choice
    [ ArAlpha <$> try alpha <*> tauValue
    , ArTau <$> attribute <*> tauValue
    ]
    <?> "argument"

validatedBindings :: [Binding] -> Parser [Binding]
validatedBindings bds = case uniqueBindings bds of
  Left msg -> fail msg
  Right bds' -> return bds'

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

exHead :: Parser Expression
exHead =
  choice
    [ do
        bs <- formationBindings >>= validatedBindings
        return (ExFormation bs)
    , do
        _ <- choice [symbol "$", symbol "ξ"]
        return ExXi
    , root
    , do
        _ <- choice [ascii 'T', symbol "⊥"]
        return ExTermination
    , lexeme (DataString . strToBts <$> quotedStr)
    , deltaHead
    , number
    , try (either ExAny ExMeta <$> metaVar 'e' "𝑒")
    , try (either ExAny ExMeta <$> metaVar 'n' "𝑛")
    , try (either ExAny ExMeta <$> metaVar 'k' "𝑘")
    , lambdaHead
    , ExDispatch ExXi <$> attribute
    , voidHead
    ]
    <?> "expression head"

application :: Expression -> [Argument] -> Expression
application = foldl ExApplication

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
            , do
                _ <- colon
                ExFormation . pure . (`BiTau` expr) <$> attribute
            ]
            <?> "dispatch or application"
        exTail next
    , return expr
    ]

expression :: Parser Expression
expression = do
  expr <- exHead
  exTail expr

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
parseNumberThrows num = orThrow CouldNotParseNumber (parseNumber num)

parseAttribute :: String -> Either String Attribute
parseAttribute = parse' "attribute" attribute

parseAlpha :: String -> Either String Alpha
parseAlpha = parse' "alpha" alpha

parseIndex :: String -> Either String (Either Slot T.Text)
parseIndex = parse' "index meta" indexVar

parseAttributeThrows :: String -> IO Attribute
parseAttributeThrows attr = orThrow CouldNotParseAttribute (parseAttribute attr)

parseExpression :: String -> Either String Expression
parseExpression = parse' "expression" expression

parseExpressionThrows :: String -> IO Expression
parseExpressionThrows ex = orThrow CouldNotParseExpression (parseExpression ex)
