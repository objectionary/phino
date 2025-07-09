{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Misc
import Paths_phino (version)
import Pretty (PrintMode, prettyAttribute, prettyBinding, prettyExpression, prettyProgram')
import Text.Printf (printf)
import Text.Read (readMaybe)
import qualified Text.Read as TR
import Text.XML
import qualified Text.XML.Cursor as C

-- @todo #116:30min Refactor XMIR module. This module became so big and hard to read.
--  Now it's responsible for 3 different operations: 1) converting Phi AST to XML Document Ast,
--  2) printing XML Document, 3) parsing XMIR to Phi AST. I think we should separate the logic
--  in order to keep modules as little as possible.
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
  show InvalidXMIRFormat {..} =
    printf
      "Couldn't traverse though given XMIR, cause: %s\nXMIR:\n%s"
      message
      ( case C.node cursor of
          NodeElement el -> printXMIR (Document (Prologue [] Nothing []) el [])
          _ -> "Unknown"
      )

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
expression (DataObject "number" bytes) =
  pure
    ( "Q.org.eolang.number",
      [ NodeComment (T.pack (either show show (hexToNum bytes))),
        object
          [("base", "Q.org.eolang.bytes")]
          [object [] [NodeContent (T.pack bytes)]]
      ]
    )
expression (DataObject "string" bytes) =
  pure
    ( "Q.org.eolang.string",
      [ NodeComment (T.pack ('"' : hexToStr bytes ++ "\"")),
        object
          [("base", "Q.org.eolang.bytes")]
          [object [] [NodeContent (T.pack bytes)]]
      ]
    )
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

programToXMIR :: Program -> PrintMode -> Bool -> IO Document
programToXMIR (Program expr) mode omitListing = do
  (pckg, expr') <- getPackage expr
  root <- rootExpression expr'
  now <- getCurrentTime
  let phi = prettyProgram' (Program expr) mode
      listing =
        if omitListing
          then show (length (lines phi)) ++ " lines of phi"
          else phi
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
            ( NodeElement (element "listing" [] [NodeContent (T.pack listing)])
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
    attrsText =
      let attrs' = M.toList attrs
          first = if length attrs' > 4 then newline <> indent (indentLevel + 1) else TB.fromString " "
       in mconcat
            [ first <> TB.fromText (nameLocalName k) <> TB.fromString "=\"" <> TB.fromText v <> TB.fromString "\""
              | (k, v) <- attrs'
            ]

    isTextNode (NodeContent _) = True
    isTextNode _ = False

    printRawText (NodeContent t) = TB.fromText t
    printRawText _ = mempty

-- >>> printNode 0 (NodeComment (T.pack "--hello--"))
-- "<!-- &#45;&#45;hello&#45;&#45; -->\n"
printNode :: Int -> Node -> TB.Builder
printNode _ (NodeContent t) = TB.fromText t -- print text exactly as-is
printNode i (NodeElement e) = printElement i e -- pretty-print elements
printNode i (NodeComment t) =
  indent i
    <> TB.fromString "<!-- "
    <> TB.fromText (T.replace "--" "&#45;&#45;" t)
    <> TB.fromString " -->"
    <> newline
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
xmirToPhi xmir =
  let doc = C.fromDocument xmir
   in case C.node doc of
        NodeElement el
          | nameLocalName (elementName el) == "object" -> do
              obj <- case doc C.$/ C.element (toName "o") of
                [o] -> xmirToFormationBinding o []
                _ -> throwIO (InvalidXMIRFormat "Expected single <o> element in <object>" doc)
              let pckg =
                    [ T.unpack t
                      | meta <- doc C.$/ C.element (toName "metas") C.&/ C.element (toName "meta"),
                        let heads = meta C.$/ C.element (toName "head") C.&/ C.content,
                        heads == ["package"],
                        tail' <- meta C.$/ C.element (toName "tail") C.&/ C.content,
                        t <- T.splitOn "." tail'
                    ]
              if null pckg
                then pure (Program (ExFormation [obj, BiVoid AtRho]))
                else
                  let bd = foldr (\part acc -> BiTau (AtLabel part) (ExFormation [acc, BiLambda "Package", BiVoid AtRho])) obj pckg
                   in pure (Program (ExFormation [bd, BiVoid AtRho]))
          | otherwise -> throwIO (InvalidXMIRFormat "Expected single <object> element" doc)
        _ -> throwIO (InvalidXMIRFormat "NodeElement is expected as root element" doc)

xmirToFormationBinding :: C.Cursor -> [String] -> IO Binding
xmirToFormationBinding cur fqn
  | not (hasAttr "name" cur) = throwIO (InvalidXMIRFormat "Formation children must have @name attribute" cur)
  | not (hasAttr "base" cur) = do
      name <- getAttr "name" cur
      bds <- mapM (`xmirToFormationBinding` (name : fqn)) (cur C.$/ C.element (toName "o"))
      case name of
        "λ" -> pure (BiLambda (intercalate "_" ("L" : reverse fqn)))
        ('α' : _) -> throwIO (InvalidXMIRFormat "Formation child @name can't start with α" cur)
        "@" -> pure (BiTau AtPhi (ExFormation (withVoidRho bds)))
        _ -> pure (BiTau (AtLabel name) (ExFormation (withVoidRho bds)))
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
xmirToExpression cur fqn
  | hasAttr "base" cur = do
      base <- getAttr "base" cur
      case base of
        '.' : rest ->
          if null rest
            then throwIO (InvalidXMIRFormat "The @base attribute can't be just '.'" cur)
            else
              let args = cur C.$/ C.element (toName "o")
               in if null args
                    then throwIO (InvalidXMIRFormat (printf "Element with @base='%s' must have at least one child" base) cur)
                    else do
                      expr <- xmirToExpression (head args) fqn
                      attr <- toAttr rest cur
                      let disp = ExDispatch expr attr
                      xmirToApplication disp (tail args) fqn
        "$" ->
          if null (cur C.$/ C.element (toName "o"))
            then pure ExThis
            else throwIO (InvalidXMIRFormat "Application of '$' is illegal in XMIR" cur)
        "Q" ->
          if null (cur C.$/ C.element (toName "o"))
            then pure ExGlobal
            else throwIO (InvalidXMIRFormat "Application of 'Q' is illegal in XMIR" cur)
        'Q' : '.' : rest -> xmirToExpression' ExGlobal "Q" rest cur fqn
        '$' : '.' : rest -> xmirToExpression' ExThis "$" rest cur fqn
        _ -> throwIO (InvalidXMIRFormat "The @base attribute must be either ['∅'|'Q'] or start with ['Q.'|'$.'|'.']" cur)
  | otherwise = do
      bds <- mapM (`xmirToFormationBinding` fqn) (cur C.$/ C.element (toName "o"))
      pure (ExFormation (withVoidRho bds))
  where
    xmirToExpression' :: Expression -> String -> String -> C.Cursor -> [String] -> IO Expression
    xmirToExpression' start symbol rest cur fqn =
      if null rest
        then throwIO (InvalidXMIRFormat (printf "The @base='%s.' is illegal in XMIR" symbol) cur)
        else do
          head' <-
            foldlM
              (\acc part -> ExDispatch acc <$> toAttr (T.unpack part) cur)
              start
              (T.splitOn "." (T.pack rest))
          let args = cur C.$/ C.element (toName "o")
          xmirToApplication head' args fqn

xmirToApplication :: Expression -> [C.Cursor] -> [String] -> IO Expression
xmirToApplication = xmirToApplication' 0
  where
    xmirToApplication' :: Integer -> Expression -> [C.Cursor] -> [String] -> IO Expression
    xmirToApplication' _ expr [] _ = pure expr
    xmirToApplication' idx expr (arg : args) fqn = do
      let app
            | hasAttr "name" arg = throwIO (InvalidXMIRFormat "Application argument can't have @name attribute" arg)
            | hasAttr "base" arg && hasText arg = throwIO (InvalidXMIRFormat "It's illegal in XMIR to have @base and text() at the same time" arg)
            | not (hasAttr "base" arg) && not (hasText arg) = do
                bds <- mapM (`xmirToFormationBinding` fqn) (arg C.$/ C.element (toName "o"))
                as <- asToAttr arg idx
                pure (ExApplication expr (BiTau as (ExFormation (withVoidRho bds))))
            | not (hasAttr "base" arg) && hasText arg = do
                as <- asToAttr arg idx
                text <- getText arg
                pure (ExApplication expr (BiTau as (ExFormation [BiDelta text, BiVoid AtRho])))
            | otherwise = do
                as <- asToAttr arg idx
                arg' <- xmirToExpression arg fqn
                pure (ExApplication expr (BiTau as arg'))
      app' <- app
      xmirToApplication' (idx + 1) app' args fqn

    asToAttr :: C.Cursor -> Integer -> IO Attribute
    asToAttr cur idx
      | hasAttr "as" cur = do
          as <- getAttr "as" cur
          attr <- toAttr as cur
          case attr of
            AtRho -> throwIO (InvalidXMIRFormat "The '^' in @as attribute is illegal in XMIR" cur)
            other -> pure other
      | otherwise = pure (AtAlpha idx)

toAttr :: String -> C.Cursor -> IO Attribute
toAttr attr cur = case attr of
  'α' : rest' ->
    case TR.readMaybe rest' :: Maybe Integer of
      Just idx -> pure (AtAlpha idx)
      Nothing -> throwIO (InvalidXMIRFormat "The attribute started with 'α' must be followed by integer" cur)
  "@" -> pure AtPhi
  "^" -> pure AtRho
  _
    | head attr `notElem` ['a' .. 'z'] -> throwIO (InvalidXMIRFormat (printf "The attribute '%s' must start with ['a'..'z']" attr) cur)
    | '.' `elem` attr -> throwIO (InvalidXMIRFormat "Attribute can't contain dots" cur)
    | otherwise -> pure (AtLabel attr)

hasAttr :: String -> C.Cursor -> Bool
hasAttr key cur = not (null (C.attribute (toName key) cur))

getAttr :: String -> C.Cursor -> IO String
getAttr key cur =
  let attrs = C.attribute (toName key) cur
   in if null attrs
        then throwIO (InvalidXMIRFormat (printf "Couldn't find attribute '%s'" key) cur)
        else
          let attr = (T.unpack . head) attrs
           in if null attr
                then throwIO (InvalidXMIRFormat (printf "The attribute '%s' is not expected to be empty" attr) cur)
                else pure attr

hasText :: C.Cursor -> Bool
hasText cur = any isNonEmptyTextNode (C.child cur)
  where
    isNonEmptyTextNode cur' = case C.node cur' of
      NodeContent t -> not (T.null (T.strip t)) -- strip to ignore whitespace-only
      _ -> False

getText :: C.Cursor -> IO String
getText cur =
  case [t | c <- C.child cur, NodeContent t <- [C.node c]] of
    (t : _) -> pure (T.unpack t)
    [] -> throwIO (InvalidXMIRFormat "Text content inside <o> element can't be empty" cur)
