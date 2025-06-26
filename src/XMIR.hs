{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module XMIR (programToXMIR, printXMIR, toName, element) where

import Ast
import Control.Exception (throwIO)
import Control.Exception.Base (Exception)
import qualified Data.Bifunctor
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
import Paths_phino (version)
import Pretty (prettyAttribute, prettyBinding, prettyExpression, prettyProgram', PrintMode)
import Text.Printf (printf)
import Text.XML

data XMIRException
  = UnsupportedExpression {expr :: Expression}
  | UnsupportedBinding {binding :: Binding}
  deriving (Exception)

instance Show XMIRException where
  show UnsupportedExpression {..} = printf "XMIR does not support such expression: %s" (prettyExpression expr)
  show UnsupportedBinding {..} = printf "XMIR does not support such bindings: %s" (prettyBinding binding)

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
  let attrs = case attr of
        AtAlpha _ -> -- For alpha attributes, omit the "as" attribute since position implies the index
          if null base'
            then []
            else [("base", base')]
        _ -> -- For non-alpha attributes, include the "as" attribute as before
          let as = prettyAttribute attr
          in if null base'
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
