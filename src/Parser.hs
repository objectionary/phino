{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The goal of the module is to parse given phi expression to AST
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

-- A one-letter ASCII token that a function name may start with, `Q` or `T`,
-- which is no such token where it is a function name itself or the start of
-- one, so `Q:λ` and `Qx:λ` stay the λ functions `Q` and `Qx` in the
-- one-binding sugar of #1385
ascii :: Char -> Parser String
ascii letter = lexeme (try (pure <$> char letter <* notFollowedBy (satisfy named <|> '_' <$ lambdaOf)))
  where
    named :: Char -> Bool
    named ch = isDigit ch || isAsciiLower ch || ch == '_' || ch == 'φ'
    lambdaOf :: Parser Char
    lambdaOf = whiteSpace >> char ':' >> whiteSpace >> oneOf ['L', 'λ']

metaSuffix :: Parser String
metaSuffix = lexeme (many (oneOf ('_' : '-' : ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']) <?> "meta suffix"))

-- A meta-variable, written either in ASCII ('!t') or in Unicode ('𝜏'). The
-- suffix tells the two kinds apart: with one the variable is named and a rule
-- may reference it from its result, without one it is an anonymous slot
-- pinned to the offset it starts at, unique within the parsed term. Named
-- variables are packed to Text once here; all AST meta fields are Text. A
-- suffix of '0' is no name but a first index written wrong: every index of the
-- calculus starts with one, so the whole term is refused where it stands.
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

-- A symbol standing where a λ name stands: 𝜎1, a name nothing answers, or a
-- bare 𝜎, which asks for a fresh one. It is spelled the way every meta of the
-- calculus is spelled, indexed or not, so 'metaVar' reads it, but what comes
-- back is a name and not a meta-variable: an index becomes the symbol it
-- numbers and a bare one the slot that tells it apart from its siblings.
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

-- bytes
-- 0. meta: !b
-- 1. empty: --
-- 2. one byte: 01-
-- 3. many bytes: 01-02-...-FF
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
                -- Negate the Double rather than the Scientific so that a zero
                -- literal preserves its sign: Scientific has no negative zero,
                -- but negate on Double yields -0.0, a distinct IEEE-754 value.
                Just '-' -> negate (toRealFloat unsigned)
                _ -> toRealFloat unsigned
            )
        )
    )

-- An expression head that starts with the root: either one of the three
-- non-finite doubles named off it — `Φ.nan`, `Φ.pinf` and `Φ.ninf`, read back
-- into the very 'DataNumber' the sweet printer collapsed, which keeps
-- print-then-parse idempotent (see #1065) — or the root itself. The label after
-- the root is parsed once, here, so an ordinary dispatch such as `Φ.number`
-- costs no more than it did before the three names existed; an attribute the
-- label parser rejects (ρ, φ, a meta) is left to 'exTail', as is any further
-- dispatch or application
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
        bs <- formationBindings
        bds <- validatedBindings (voids ++ bs)
        return (ExFormation (withVoidRho bds))
    ]
  where
    rb :: Parser String
    rb = symbol ")"

-- The name a λ binding carries: a function, a meta standing for one, or a symbol
lambdaName :: Parser Function
lambdaName = choice [Function . T.pack <$> function, try (either FnAny FnMeta <$> metaVar 'F' "𝑓"), sigma]

-- The colon that attaches an attribute to what stands before it, making a
-- formation of one binding out of the two (see #1385)
colon :: Parser String
colon = symbol ":"

-- A formation of one binding written as its asset followed by a colon and the
-- attribute it is bound to, the way the sugar of #1385 spells it:
-- `FF-AA:Δ` is `⟦ Δ ⤍ FF-AA ⟧`, `𝜎1:λ` is `⟦ λ ⤍ 𝜎1 ⟧` and `∅:a` is
-- `⟦ a ↦ ∅ ⟧`. A τ binding, `ξ.a:φ` for `⟦ φ ↦ ξ.a ⟧`, is no head but a tail,
-- since it attaches to a whole expression (see 'exTail'). Bytes and λ names
-- look like numbers and function-like heads, so their shapes are only
-- committed to once the attribute after the colon is read. Each of the three
-- is a head of its own in 'exHead', standing right before the first head it
-- could be taken for and opened by a look at a character it must start with,
-- so the heads a program is mostly made of never try it.
alone :: Parser Binding -> Parser Expression
alone bd = ExFormation . withVoidRho . pure <$> bd

-- `FF-AA:Δ`, `--:D` or `𝛿1:Δ`
deltaHead :: Parser Expression
deltaHead =
  lookAhead (satisfy (\ch -> isDigit ch || ('A' <= ch && ch <= 'F') || ch `elem` ("-!𝛿" :: String)))
    >> alone (try (BiDelta <$> bytes <* colon <* choice [symbol "D", symbol "Δ"]))

-- `Plus:λ`, `𝜎1:λ` or `!F1:L`
lambdaHead :: Parser Expression
lambdaHead =
  lookAhead (satisfy (\ch -> isAsciiUpper ch || ch `elem` ("!𝑓𝜎" :: String)))
    >> alone (try (BiLambda <$> lambdaName <* colon <* choice [symbol "L", symbol "λ"]))

-- `∅:a` or `?:a`
voidHead :: Parser Expression
voidHead = alone (choice [symbol "?", symbol "∅"] >> colon >> BiVoid <$> attribute)

metaBinding :: Parser Binding
metaBinding = either BiAny BiMeta <$> metaVar 'B' "𝐵"

-- binding
-- 1. delta
-- 2. meta delta
-- 3. meta
-- 4. lambda
-- 5. meta lambda
-- 6. void
-- 7. tau
--
-- Every alternative commits as soon as the token that tells it apart from its
-- siblings is consumed, so a failure deeper in the binding keeps its own
-- position instead of being rewound to the beginning of the binding.
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
    -- A void followed by a colon is no void of this binding but the head of
    -- a one-binding formation the binding is bound to, as in `x ↦ ∅:a`
    blank :: Parser String
    blank = arrow >> choice [symbol "?", symbol "∅"] <* notFollowedBy colon

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
    , either AtAny AtMeta <$> metaVar 't' "𝜏"
    ]
    <?> "attribute"

-- index meta: !i, 𝑖
indexVar :: Parser (Either Slot T.Text)
indexVar = metaVar 'i' "𝑖"

-- alpha
-- 1. index: ~0, α0
-- 2. meta: α𝑖, ~!i
alpha :: Parser Alpha
alpha = do
  _ <- choice [symbol "~", symbol "α"]
  choice
    [ Alpha <$> lexeme L.decimal
    , either AlAny AlMeta <$> indexVar
    ]
    <?> "alpha"

-- application argument
-- 1. tau: <attribute> ↦ <expression>
-- 2. alpha: <alpha> ↦ <expression>
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
-- 3. global, or an attribute or non-finite double named off it
-- 4. termination
-- 5. meta expression
-- 6. full attribute -> sugar for $.attr
-- 7. one-binding formation of a Δ, λ or void binding -> sugar for ⟦ Δ ⤍ FF- ⟧,
--    each standing before the first head it could be taken for
exHead :: Parser Expression
exHead =
  choice
    [ do
        bs <- formationBindings >>= validatedBindings
        return (ExFormation (withVoidRho bs))
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

-- tail optional part of application
-- 1. any head + dispatch
-- 2. any head except $ and Q + application
-- 3. any head + colon and attribute -> sugar for ⟦ attr ↦ head ⟧
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
                ExFormation . withVoidRho . pure . (`BiTau` expr) <$> attribute
            ]
            <?> "dispatch or application"
        exTail next
    , return expr
    ]

expression :: Parser Expression
expression = do
  expr <- exHead
  exTail expr

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
