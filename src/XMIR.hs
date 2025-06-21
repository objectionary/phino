{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module XMIR where

import Ast
import Control.Exception (throwIO)
import Control.Exception.Base (Exception)
import qualified Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, mapMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.IO as TL
import Prettyprinter (Pretty (pretty))
import Pretty (prettyAttribute, prettyBinding, prettyExpression, prettyProgram)
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
  let as = prettyAttribute attr
      attrs =
        if null base'
          then [("as", as)]
          else [("as", as), ("base", base')]
  pure (base, object attrs children' : children)
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
  pure (Just (object [("name", label), ("base", base)] (reverse children)))
formationBinding (BiDelta bytes) = pure (Just (NodeContent (T.pack bytes)))
formationBinding (BiLambda func) = pure (Just (object [("name", "λ")] []))
formationBinding (BiVoid AtRho) = pure Nothing
formationBinding (BiVoid AtPhi) = pure (Just (object [("name", "φ"), ("base", "∅")] []))
formationBinding (BiVoid (AtLabel label)) = pure (Just (object [("name", label), ("base", "∅")] []))
formationBinding binding = throwIO (UnsupportedBinding binding)

rootExpression :: Expression -> IO [Node]
rootExpression (ExFormation []) = pure []
rootExpression (ExFormation [bd, BiVoid AtRho]) = nestedBindings [bd]
rootExpression expr = throwIO (UnsupportedExpression expr)

programToXMIR :: Program -> IO Document
programToXMIR (Program expr) = do
  root <- rootExpression expr
  pure
    ( Document
        (Prologue [] Nothing [])
        ( element
            "object"
            [("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance")]
            ( NodeElement (element "listing" [] [NodeContent (T.pack (prettyProgram (Program expr)))])
                : root
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
    attrsText =
      mconcat
        [ TB.fromString " " <> TB.fromText (nameLocalName k) <> TB.fromString "=\"" <> TB.fromText v <> TB.fromString "\""
          | (k, v) <- M.toList attrs
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
