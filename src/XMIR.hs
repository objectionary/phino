{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module XMIR
  ( programToXMIR,
    printXMIR,
    toName,
    parseXMIR,
    parseXMIRThrows,
    xmirToPhi,
    defaultXmirContext,
    XmirContext (XmirContext),
  )
where

import AST
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
import Printer
import Text.Printf (printf)
import qualified Text.Read as TR
import Text.XML
import qualified Text.XML.Cursor as C

data XmirContext = XmirContext
  { omitListing :: Bool,
    omitComments :: Bool,
    listing :: String
  }

defaultXmirContext :: XmirContext
defaultXmirContext = XmirContext True True ""

data XMIRException
  = UnsupportedProgram {prog :: Program}
  | UnsupportedExpression {expr :: Expression}
  | UnsupportedBinding {binding :: Binding}
  | CouldNotParseXMIR {message :: String}
  | InvalidXMIRFormat {message :: String, cursor :: C.Cursor}
  deriving (Exception)

instance Show XMIRException where
  show UnsupportedProgram {..} = printf "XMIR does not support such program:\n%s" (printProgram prog)
  show UnsupportedExpression {..} = printf "XMIR does not support such expression:\n%s" (printExpression expr)
  show UnsupportedBinding {..} = printf "XMIR does not support such bindings: %s" (printBinding binding)
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

expression :: Expression -> XmirContext -> IO (String, [Node])
expression ExThis _ = pure (printExpression ExThis, [])
expression ExGlobal _ = pure (printExpression ExGlobal, [])
expression (ExFormation bds) ctx = do
  nested <- nestedBindings bds ctx
  pure ("", nested)
expression (ExDispatch expr attr) ctx = do
  (base, children) <- expression expr ctx
  let attr' = printAttribute attr
  if null base
    then pure ('.' : attr', [object [] children])
    else
      if head base == '.' || not (null children)
        then pure ('.' : attr', [object [("base", base)] children])
        else pure (base ++ ('.' : attr'), children)
expression (DataNumber bytes) XmirContext {..} =
  let bts =
        object
          [("as", printAttribute (AtAlpha 0)), ("base", "Φ.org.eolang.bytes")]
          [object [] [NodeContent (T.pack (printBytes bytes))]]
   in pure
        ( "Φ.org.eolang.number",
          if omitComments
            then [bts]
            else
              [ NodeComment (T.pack (either show show (btsToNum bytes))),
                bts
              ]
        )
expression (DataString bytes) XmirContext {..} =
  let bts =
        object
          [("as", printAttribute (AtAlpha 0)), ("base", "Φ.org.eolang.bytes")]
          [object [] [NodeContent (T.pack (printBytes bytes))]]
   in pure
        ( "Φ.org.eolang.string",
          if omitComments
            then [bts]
            else
              [ NodeComment (T.pack ('"' : btsToStr bytes ++ "\"")),
                bts
              ]
        )
expression (ExApplication expr (BiTau attr texpr)) ctx = do
  (base, children) <- expression expr ctx
  (base', children') <- expression texpr ctx
  let as = printAttribute attr
      attrs =
        if null base'
          then [("as", as)]
          else [("as", as), ("base", base')]
  pure (base, children ++ [object attrs children'])
expression (ExApplication (ExFormation bds) tau) _ = throwIO (UnsupportedExpression (ExApplication (ExFormation bds) tau))
expression expr _ = throwIO (UnsupportedExpression expr)

formationBinding :: Binding -> XmirContext -> IO (Maybe Node)
formationBinding (BiTau (AtLabel label) (ExFormation bds)) ctx = do
  inners <- nestedBindings bds ctx
  pure (Just (object [("name", label)] inners))
formationBinding (BiTau (AtLabel label) expr) ctx = do
  (base, children) <- expression expr ctx
  pure (Just (object [("name", label), ("base", base)] children))
formationBinding (BiTau AtPhi expr) ctx = do
  (base, children) <- expression expr ctx
  pure (Just (object [("name", show AtPhi), ("base", base)] children))
formationBinding (BiTau AtRho _) _ = pure Nothing
formationBinding (BiDelta bytes) _ = pure (Just (NodeContent (T.pack (printBytes bytes))))
formationBinding (BiLambda func) _ = pure (Just (object [("name", show AtLambda)] []))
formationBinding (BiVoid AtRho) _ = pure Nothing
formationBinding (BiVoid AtPhi) _ = pure (Just (object [("name", show AtPhi), ("base", "∅")] []))
formationBinding (BiVoid (AtLabel label)) _ = pure (Just (object [("name", label), ("base", "∅")] []))
formationBinding binding _ = throwIO (UnsupportedBinding binding)

nestedBindings :: [Binding] -> XmirContext -> IO [Node]
nestedBindings bds ctx = catMaybes <$> mapM (`formationBinding` ctx) bds

programToXMIR :: Program -> XmirContext -> IO Document
programToXMIR prog@(Program expr@(ExFormation [BiTau (AtLabel _) arg, BiVoid AtRho])) ctx@XmirContext {..} = case arg of
  ExFormation _ -> programToXMIR'
  ExApplication _ _ -> programToXMIR'
  ExDispatch _ _ -> programToXMIR'
  ExGlobal -> programToXMIR'
  _ -> throwIO (UnsupportedProgram prog)
  where
    programToXMIR' :: IO Document
    programToXMIR' = do
      (pckg, expr') <- getPackage expr
      root <- rootExpression expr' ctx
      now <- getCurrentTime
      let listingContent =
            if omitListing
              then show (length (lines listing)) ++ " line(s)"
              else listing
          listing' = NodeElement (element "listing" [] [NodeContent (T.pack listingContent)])
          metas = metasWithPackage (intercalate "." pckg)
      pure
        ( Document
            (Prologue [] Nothing [])
            ( element
                "object"
                [ ("dob", formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now),
                  ("ms", "0"),
                  ("revision", "1234567"),
                  ("time", time now),
                  ("version", showVersion version),
                  ("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance"),
                  ("xsi:noNamespaceSchemaLocation", "https://raw.githubusercontent.com/objectionary/eo/refs/heads/gh-pages/XMIR.xsd")
                ]
                ( if null pckg
                    then [listing', root]
                    else [listing', metas, root]
                )
            )
            []
        )
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
    getPackage expr = throwIO (userError (printf "Can't extract package from given expression:\n %s" (printExpression expr)))
    -- Convert root Expression to Node
    rootExpression :: Expression -> XmirContext -> IO Node
    rootExpression (ExFormation [bd, BiVoid AtRho]) ctx = do
      [bd'] <- nestedBindings [bd] ctx
      pure bd'
    rootExpression expr _ = throwIO (UnsupportedExpression expr)
    -- Returns metas Node with package:
    -- <metas>
    --   <meta>
    --     <head>package</head>
    --     <tail><!-- package here --></tail>
    --     <part><!-- package here --></part>
    --   </meta>
    -- </metas>
    metasWithPackage :: String -> Node
    metasWithPackage pckg =
      NodeElement
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
    time :: UTCTime -> String
    time now = do
      let base = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now
          posix = utcTimeToPOSIXSeconds now
          fractional :: Double
          fractional = realToFrac posix - fromInteger (floor posix)
          nanos = floor (fractional * 1_000_000_000) :: Int
      base ++ "." ++ printf "%09d" nanos ++ "Z"
programToXMIR prog _ = throwIO (UnsupportedProgram prog)

-- Add indentation (2 spaces per level).
indent :: Int -> TB.Builder
indent n = TB.fromText (T.replicate n (T.pack "  "))

newline :: TB.Builder
newline = TB.fromString "\n"

-- >>> printElement 0 (element "doc" [("a", ""), ("b", ""), ("c", ""), ("d", ""), ("e", "")] [])
-- "<doc a=\"\" b=\"\" c=\"\" d=\"\" e=\"\"/>\n"
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
      bds <- mapM (`xmirToFormationBinding` (name : fqn)) (cur C.$/ C.element (toName "o")) >>= uniqueBindings'
      case name of
        "λ" -> pure (BiLambda (intercalate "_" ("L" : reverse fqn)))
        ('α' : _) -> throwIO (InvalidXMIRFormat "Formation child @name can't start with α" cur)
        "φ" -> pure (BiTau AtPhi (ExFormation (withVoidRho bds)))
        _ -> pure (BiTau (AtLabel name) (ExFormation (withVoidRho bds)))
  | otherwise = do
      name <- getAttr "name" cur
      base <- getAttr "base" cur
      attr <- case name of
        "φ" -> pure AtPhi
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
        "ξ" ->
          if null (cur C.$/ C.element (toName "o"))
            then pure ExThis
            else throwIO (InvalidXMIRFormat "Application of 'ξ' is illegal in XMIR" cur)
        "Φ" ->
          if null (cur C.$/ C.element (toName "o"))
            then pure ExGlobal
            else throwIO (InvalidXMIRFormat "Application of 'Φ' is illegal in XMIR" cur)
        'Φ' : '.' : rest -> xmirToExpression' ExGlobal "Φ" rest cur fqn
        'ξ' : '.' : rest -> xmirToExpression' ExThis "ξ" rest cur fqn
        _ -> throwIO (InvalidXMIRFormat "The @base attribute must be either ['∅'|'Φ'] or start with ['Φ.'|'ξ.'|'.']" cur)
  | otherwise = do
      bds <- mapM (`xmirToFormationBinding` fqn) (cur C.$/ C.element (toName "o")) >>= uniqueBindings'
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
          xmirToApplication head' (cur C.$/ C.element (toName "o")) fqn

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
                bytes <- getText arg
                pure (ExApplication expr (BiTau as (ExFormation [BiDelta (bytesToBts bytes), BiVoid AtRho])))
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
            AtRho -> throwIO (InvalidXMIRFormat "The 'ρ' in @as attribute is illegal in XMIR" cur)
            other -> pure other
      | otherwise = pure (AtAlpha idx)

toAttr :: String -> C.Cursor -> IO Attribute
toAttr attr cur = case attr of
  'α' : rest' ->
    case TR.readMaybe rest' :: Maybe Integer of
      Just idx -> pure (AtAlpha idx)
      Nothing -> throwIO (InvalidXMIRFormat "The attribute started with 'α' must be followed by integer" cur)
  "φ" -> pure AtPhi
  "ρ" -> pure AtRho
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
