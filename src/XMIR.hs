{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module XMIR (programToXMIR, printXMIR, toName, parseXMIR, parseXMIRThrows, xmirToPhi) where

import Ast
import Control.Exception (Exception (displayException), SomeException, throwIO)
import Control.Exception.Base (Exception)
import qualified Data.Bifunctor
import Data.Foldable (foldlM)
import Data.List (intercalate)
import qualified Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Version (showVersion)
import Debug.Trace (trace)
import Misc (foldlMi)
import Paths_phino (version)
import Pretty (PrintMode, prettyAttribute, prettyBinding, prettyExpression, prettyProgram')
import Text.Printf (printf)
import Text.Read (readMaybe)
import qualified Text.Read as TR
import Text.XML
import qualified Text.XML.Cursor as C

data XMIRException
  = UnsupportedExpression {expr :: Expression}
  | UnsupportedBinding {binding :: Binding}
  | CouldNotParseXMIR {message :: String}
  | InvalidXMIRFormat {message :: String, cursor :: C.Cursor}
  deriving (Exception)

instance Show XMIRException where
  show UnsupportedExpression {..} = printf "XMIR does not support such expression: %s" (prettyExpression expr)
  show UnsupportedBinding {..} = printf "XMIR does not support such bindings: %s" (prettyBinding binding)
  show CouldNotParseXMIR {..} = printf "Couldn't parse given XMIR, cause: %s" message
  show InvalidXMIRFormat {..} = printf "Couldn't traverse though given XMIR, cause: %s" message

toName :: String -> Name
toName str = Name (T.pack str) Nothing Nothing

element :: String -> [(String, String)] -> [Node] -> Element
element name attrs children = do
  let name' = toName name
      attrs' = M.fromList (map (Data.Bifunctor.bimap toName T.pack) attrs)
  Element name' attrs' children

object :: [(String, String)] -> [Node] -> Node
object attrs children = NodeElement (element "o" attrs children)

expression :: Expression -> IO (String, [Node])
expression ExThis = pure ("$", [])
expression ExGlobal = pure ("Q", [])
expression (ExFormation bds) = do
  nested <- nestedBindings bds
  pure ("", nested)
expression (ExDispatch expr attr) = do
  (base, children) <- expression expr
  let attr' = prettyAttribute attr
  if null base
    then pure ('.' : attr', [object [] children])
    else
      if head base == '.' || not (null children)
        then pure ('.' : attr', [object [("base", base)] children])
        else pure (base ++ ('.' : attr'), children)
expression (ExApplication expr (BiTau attr texpr)) = do
  (base, children) <- expression expr
  (base', children') <- expression texpr
  let as = prettyAttribute attr
      attrs =
        if null base'
          then [("as", as)]
          else [("as", as), ("base", base')]
  pure (base, children ++ [object attrs children'])
expression (ExApplication (ExFormation bds) tau) = throwIO (UnsupportedExpression (ExApplication (ExFormation bds) tau))
expression expr = throwIO (UnsupportedExpression expr)

nestedBindings :: [Binding] -> IO [Node]
nestedBindings bds = catMaybes <$> mapM formationBinding bds

formationBinding :: Binding -> IO (Maybe Node)
formationBinding (BiTau (AtLabel label) (ExFormation bds)) = do
  inners <- nestedBindings bds
  pure (Just (object [("name", label)] inners))
formationBinding (BiTau (AtLabel label) expr) = do
  (base, children) <- expression expr
  pure (Just (object [("name", label), ("base", base)] children))
formationBinding (BiTau AtRho _) = pure Nothing
formationBinding (BiDelta bytes) = pure (Just (NodeContent (T.pack bytes)))
formationBinding (BiLambda func) = pure (Just (object [("name", "λ")] []))
formationBinding (BiVoid AtRho) = pure Nothing
formationBinding (BiVoid AtPhi) = pure (Just (object [("name", "φ"), ("base", "∅")] []))
formationBinding (BiVoid (AtLabel label)) = pure (Just (object [("name", label), ("base", "∅")] []))
formationBinding binding = throwIO (UnsupportedBinding binding)

rootExpression :: Expression -> IO Node
rootExpression (ExFormation [bd, BiVoid AtRho]) = do
  [bd'] <- nestedBindings [bd]
  pure bd'
rootExpression expr = throwIO (UnsupportedExpression expr)

-- Extract package from given expression
-- The function returns tuple (X, Y), where
-- - X: list of package parts
-- - Y: root object expression
getPackage :: Expression -> IO ([String], Expression)
getPackage (ExFormation [BiTau (AtLabel label) (ExFormation [bd, BiLambda "Package", BiVoid AtRho]), BiVoid AtRho]) = do
  (pckg, expr') <- getPackage (ExFormation [bd, BiLambda "Package", BiVoid AtRho])
  pure (label : pckg, expr')
getPackage (ExFormation [BiTau (AtLabel label) (ExFormation [bd, BiLambda "Package", BiVoid AtRho]), BiLambda "Package", BiVoid AtRho]) = do
  (pckg, expr') <- getPackage (ExFormation [bd, BiLambda "Package", BiVoid AtRho])
  pure (label : pckg, expr')
getPackage (ExFormation [BiTau attr expr, BiLambda "Package", BiVoid AtRho]) = pure ([], ExFormation [BiTau attr expr, BiVoid AtRho])
getPackage (ExFormation [bd, BiVoid AtRho]) = pure ([], ExFormation [bd, BiVoid AtRho])
getPackage expr = throwIO (userError (printf "Can't extract package from given expression:\n %s" (prettyExpression expr)))

metasWithPackage :: String -> [Node]
metasWithPackage pckg =
  [ NodeElement
      ( element
          "metas"
          []
          [ NodeElement
              ( element
                  "meta"
                  []
                  [ NodeElement (element "head" [] [NodeContent (T.pack "package")]),
                    NodeElement (element "tail" [] [NodeContent (T.pack pckg)]),
                    NodeElement (element "part" [] [NodeContent (T.pack pckg)])
                  ]
              )
          ]
      )
    | not (null pckg)
  ]

time :: UTCTime -> String
time now = do
  let base = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now
      posix = utcTimeToPOSIXSeconds now
      fractional :: Double
      fractional = realToFrac posix - fromInteger (floor posix)
      nanos = floor (fractional * 1_000_000_000) :: Int
  base ++ "." ++ printf "%09d" nanos ++ "Z"

programToXMIR :: Program -> PrintMode -> IO Document
programToXMIR (Program expr) mode = do
  (pckg, expr') <- getPackage expr
  root <- rootExpression expr'
  now <- getCurrentTime
  pure
    ( Document
        (Prologue [] Nothing [])
        ( element
            "object"
            [ ("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance"),
              ("dob", formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now),
              ("ms", "0"),
              ("revision", "1234567"),
              ("time", time now),
              ("version", showVersion version),
              ("xsi:noNamespaceSchemaLocation", "https://raw.githubusercontent.com/objectionary/eo/refs/heads/gh-pages/XMIR.xsd")
            ]
            ( NodeElement (element "listing" [] [NodeContent (T.pack (prettyProgram' (Program expr) mode))])
                : root
                : metasWithPackage (intercalate "." pckg)
            )
        )
        []
    )

-- Add indentation (2 spaces per level).
indent :: Int -> TB.Builder
indent n = TB.fromText (T.replicate n (T.pack "  "))

newline :: TB.Builder
newline = TB.fromString "\n"

printElement :: Int -> Element -> TB.Builder
printElement indentLevel (Element name attrs nodes)
  | null nodes =
      indent indentLevel
        <> TB.fromString "<"
        <> TB.fromText (nameLocalName name)
        <> attrsText
        <> TB.fromString "/>"
        <> newline
  | all isTextNode nodes =
      indent indentLevel
        <> TB.fromString "<"
        <> TB.fromText (nameLocalName name)
        <> attrsText
        <> TB.fromString ">"
        <> mconcat (map printRawText nodes)
        <> TB.fromString "</"
        <> TB.fromText (nameLocalName name)
        <> TB.fromString ">"
        <> newline
  | otherwise =
      indent indentLevel
        <> TB.fromString "<"
        <> TB.fromText (nameLocalName name)
        <> attrsText
        <> TB.fromString ">"
        <> newline
        <> mconcat (map (printNode (indentLevel + 1)) nodes)
        <> indent indentLevel
        <> TB.fromString "</"
        <> TB.fromText (nameLocalName name)
        <> TB.fromString ">"
        <> newline
  where
    attrsText = do
      let attrs' = M.toList attrs
          first = if length attrs' > 4 then newline <> indent (indentLevel + 1) else TB.fromString " "
      mconcat
        [ first <> TB.fromText (nameLocalName k) <> TB.fromString "=\"" <> TB.fromText v <> TB.fromString "\""
          | (k, v) <- attrs'
        ]

    isTextNode (NodeContent _) = True
    isTextNode _ = False

    printRawText (NodeContent t) = TB.fromText t
    printRawText _ = mempty

printNode :: Int -> Node -> TB.Builder
printNode _ (NodeContent t) = TB.fromText t -- print text exactly as-is
printNode i (NodeElement e) = printElement i e -- pretty-print elements
printNode i (NodeComment t) = indent i <> TB.fromString "<!-- " <> TB.fromText t <> TB.fromString " -->" <> newline
printNode _ _ = mempty

printXMIR :: Document -> String
printXMIR (Document _ root _) =
  TL.unpack
    ( TB.toLazyText
        ( TB.fromString "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            <> newline
            <> printElement 0 root
        )
    )

parseXMIR :: String -> Either String Document
parseXMIR xmir = case parseText def (TL.pack xmir) of
  Right doc -> Right doc
  Left err -> Left (displayException err)

parseXMIRThrows :: String -> IO Document
parseXMIRThrows xmir = case parseXMIR xmir of
  Right doc -> pure doc
  Left err -> throwIO (CouldNotParseXMIR err)

xmirToPhi :: Document -> IO Program
xmirToPhi xmir = do
  let doc = C.fromDocument xmir
  case doc C.$/ C.element (toName "object") of
    [obj] -> do
      expr <- do
        case obj C.$/ C.element (toName "o") of
          [o] -> do
            bd <- xmirToFormationBinding o []
            pure (ExFormation [bd])
          _ -> throwIO (InvalidXMIRFormat "Expected single <o> element in <objet>" obj)
      pure (Program expr)
    _ -> throwIO (InvalidXMIRFormat "Expected single <object> element" doc)

-- xmirToExpression :: C.Cursor -> IO Expression
-- xmirToExpression cur
--   | hasAttr "name" cur && not (hasAttr "base" cur) = do
--       bds <- mapM xmirToBinding (cur C.$/ C.element (toName "o"))
--       pure (ExFormation bds)
--   | hasAttr "name" cur && hasAttr "base" cur = pure (ExFormation [])

xmirToFormationBinding :: C.Cursor -> [String] -> IO Binding
xmirToFormationBinding cur fqn
  | not (hasAttr "name" cur) = throwIO (InvalidXMIRFormat "Formation children must have @name attribute" cur)
  | not (hasAttr "base" cur) = do
      name <- getAttr "name" cur
      bds <- mapM (`xmirToFormationBinding` (name : fqn)) (cur C.$/ C.element (toName "o"))
      case name of
        "λ" -> pure (BiLambda (intercalate "_" ("L" : reverse fqn)))
        ('α' : _) -> throwIO (InvalidXMIRFormat "Formation child @name can't start with α" cur)
        "@" -> pure (BiTau AtPhi (ExFormation bds))
        _ -> pure (BiTau (AtLabel name) (ExFormation bds))
  | otherwise = do
      name <- getAttr "name" cur
      base <- getAttr "base" cur
      attr <- case name of
        "@" -> pure AtPhi
        ('α' : _) -> throwIO (InvalidXMIRFormat "Formation child @name can't start with α" cur)
        _ -> pure (AtLabel name)
      case base of
        "∅" -> pure (BiVoid attr)
        _ -> do
          expr <- xmirToExpression cur fqn
          pure (BiTau attr expr)

xmirToExpression :: C.Cursor -> [String] -> IO Expression
xmirToExpression cur fqn = do
  base <- getAttr "base" cur
  case base of
    '.' : rest ->
      if null rest
        then throwIO (InvalidXMIRFormat "The @base attribute can't be just '.'" cur)
        else do
          let args = cur C.$/ C.element (toName "o")
          if null args
            then throwIO (InvalidXMIRFormat (printf "Element with @base='%s' must have at least one child" base) cur)
            else do
              expr <- xmirToExpression (head args) fqn
              attr <- case rest of
                'α' : rest' -> do
                  case TR.readMaybe rest' :: Maybe Integer of
                    Just idx -> pure (AtAlpha idx)
                    Nothing -> throwIO (InvalidXMIRFormat "The @base started with '.α' must be followed by integer" cur)
                "@" -> pure AtPhi
                "^" -> pure AtRho
                _ -> if head rest `elem` ['a'..'z']
                  then throwIO (InvalidXMIRFormat "The @base attribute must start with ['a'..'z'] after dot" cur)
                  else pure (AtLabel rest)
              let disp = ExDispatch expr attr
              xmirToApplication disp (tail args) fqn
    "$" -> do
      if null (cur C.$/ C.element (toName "o"))
        then pure ExThis
        else throwIO (InvalidXMIRFormat "Application of '$' is illegal in XMIR" cur)
    "Q" -> do
      if null (cur C.$/ C.element (toName "o"))
        then pure ExGlobal
        else throwIO (InvalidXMIRFormat "Application of 'Q' is illegal in XMIR" cur)
    'Q' : '.' : rest -> if null rest
      then throwIO (InvalidXMIRFormat "The @base='Q.' is illegal in XMIR" cur)
      else pure ExGlobal -- todo
    '$' : '.' : rest -> if null rest
      then throwIO (InvalidXMIRFormat "The @base='$.' is illegal in XMIR" cur)
      else pure ExThis -- todo
    _ -> throwIO (InvalidXMIRFormat "The @base attribute must be either ['∅'|'Q'] or start with ['Q.'|'$.'|'.']" cur)

xmirToApplication :: Expression -> [C.Cursor] -> [String] -> IO Expression
xmirToApplication expr [] _ = pure expr
xmirToApplication expr args fqn = pure ExGlobal -- todo

getAttr :: String -> C.Cursor -> IO String
getAttr key cur = do
  let attrs = C.attribute (toName key) cur
  if null attrs
    then throwIO (InvalidXMIRFormat (printf "Couldn't find attribute '%s'" key) cur)
    else do
      let attr = (T.unpack . head) attrs
      if null attr
        then throwIO (InvalidXMIRFormat (printf "The attribute '%s' is not expected to be empty" attr) cur)
        else pure attr

hasAttr :: String -> C.Cursor -> Bool
hasAttr key cur = not (null (C.attribute (toName key) cur))
