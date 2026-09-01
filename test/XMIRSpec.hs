{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module XMIRSpec where

import AST (Attribute (AtLabel, AtRho), Binding (BiMeta, BiTau, BiVoid), Expression (ExFormation))
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, unless)
import Data.Aeson
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import Files (allPathsIn)
import GHC.Generics (Generic)
import Parser (parseExpressionThrows)
import System.FilePath (makeRelative)
import Test.Hspec (Spec, anyException, describe, expectationFailure, it, runIO, shouldBe, shouldContain, shouldThrow)
import Text.XML (Document (..), Element (..), Node (NodeElement), Prologue (..))
import Text.XML.Cursor qualified as C
import XMIR (XmirContext (XmirContext), defaultXmirContext, escapeXML, expressionToXMIR, parseXMIRThrows, printXMIR, toName, xmirToPhi)

data ParsePack = ParsePack
  { failure :: Maybe Bool
  , xmir :: String
  , phi :: String
  }
  deriving (Generic, Show, FromJSON)

data PrintPack = PrintPack
  { phi :: String
  , xpaths :: [String]
  }
  deriving (Generic, Show, FromJSON)

parsePack :: FilePath -> IO ParsePack
parsePack = Yaml.decodeFileThrow

printPack :: FilePath -> IO PrintPack
printPack = Yaml.decodeFileThrow

-- | An XPath predicate that filters cursors.
data Predicate
  = AttrEquals String String
  | ChildText String String
  | ChildExists String [Predicate]
  | PositionIs Int
  | AndPred Predicate Predicate
  deriving (Show)

-- | An XPath step with element name and predicates.
data Step = Step String [Predicate]
  deriving (Show)

{- | Parse a simple XPath expression into steps.
Supports: /element/element[@attr="val" and child="val" and child[N][@attr="val"]]
-}
xpath :: String -> [Step]
xpath ('/' : rest) = steps rest
xpath _ = []

steps :: String -> [Step]
steps "" = []
steps str =
  let (step, rest) = span (\c -> c /= '/' && c /= '[') str
      (preds, remaining) = parsePredicate rest
   in Step step preds : steps (dropWhile (== '/') remaining)

parsePredicate :: String -> ([Predicate], String)
parsePredicate ('[' : rest) =
  let (inner, after) = splitBracket rest
      pred' = parsePredicateInner inner
      (more, final) = parsePredicate after
   in (pred' : more, final)
parsePredicate str = ([], str)

splitBracket :: String -> (String, String)
splitBracket = go (0 :: Int) ""
  where
    go :: Int -> String -> String -> (String, String)
    go _ acc "" = (reverse acc, "")
    go 0 acc (']' : rest) = (reverse acc, rest)
    go n acc ('[' : rest) = go (n + 1) ('[' : acc) rest
    go n acc (']' : rest) = go (n - 1) (']' : acc) rest
    go n acc (c : rest) = go n (c : acc) rest

parsePredicateInner :: String -> Predicate
parsePredicateInner str
  | " and " `isInfixOf'` str =
      let parts = splitAnd str
       in foldr1 AndPred (map parsePredicateInner parts)
  | all isDigit str = PositionIs (read str)
  | '@' : rest <- str = parseAttrPred rest
  | otherwise = parseChildPred str
  where
    isInfixOf' needle haystack = needle `elem` tails haystack
    tails [] = [[]]
    tails s@(_ : xs) = s : tails xs

splitAnd :: String -> [String]
splitAnd = go (0 :: Int) ""
  where
    go :: Int -> String -> String -> [String]
    go _ acc "" = [reverse acc | not (null acc)]
    go n acc ('[' : rest) = go (n + 1) ('[' : acc) rest
    go n acc (']' : rest) = go (n - 1) (']' : acc) rest
    go 0 acc (' ' : 'a' : 'n' : 'd' : ' ' : rest) = reverse acc : go 0 "" rest
    go n acc (c : rest) = go n (c : acc) rest

parseAttrPred :: String -> Predicate
parseAttrPred str =
  let (name, rest) = break (== '=') str
      val = extractQuoted (drop 1 rest)
   in AttrEquals name val

parseChildPred :: String -> Predicate
parseChildPred str
  | '[' `elem` str =
      let (name, rest) = break (== '[') str
          (preds, _) = parsePredicate rest
       in ChildExists name preds
  | '=' `elem` str =
      let (name, rest) = break (== '=') str
          val = extractQuoted (drop 1 rest)
       in ChildText name val
  | otherwise = ChildExists str []

extractQuoted :: String -> String
extractQuoted (q : rest)
  | q == '"' || q == '\'' = takeWhile (/= q) rest
extractQuoted s = s

{- | Evaluate an XPath expression on a document, returning matched cursors.
Note: fromDocument returns cursor at root element, so first step must match root.
-}
evaluate :: Document -> [Step] -> [C.Cursor]
evaluate doc [] = [C.fromDocument doc]
evaluate doc (Step name preds : rest) =
  let root = C.fromDocument doc
      rootName = elementName (documentRoot doc)
   in if rootName == toName name
        then foldl applyStep (applyPredicates [root] preds) rest
        else []

applyStep :: [C.Cursor] -> Step -> [C.Cursor]
applyStep curs (Step name preds) = do
  cur <- curs
  child <- cur C.$/ C.element (toName name)
  applyPredicates [child] preds

applyPredicates :: [C.Cursor] -> [Predicate] -> [C.Cursor]
applyPredicates = foldl applyPredicate

applyPredicate :: [C.Cursor] -> Predicate -> [C.Cursor]
applyPredicate curs pred' = case pred' of
  AttrEquals name val -> filter (hasAttrValue name val) curs
  ChildText name val -> filter (hasChildText name val) curs
  ChildExists name nested -> filter (hasChild name nested) curs
  PositionIs n -> take 1 (drop (n - 1) curs)
  AndPred p1 p2 -> applyPredicate (applyPredicate curs p1) p2

hasAttrValue :: String -> String -> C.Cursor -> Bool
hasAttrValue name val cur = C.attribute (toName name) cur == [T.pack val]

hasChildText :: String -> String -> C.Cursor -> Bool
hasChildText name val cur =
  let children = cur C.$/ C.element (toName name)
   in any (hasTextContent val) children

hasTextContent :: String -> C.Cursor -> Bool
hasTextContent val cur =
  let txt = concatMap T.unpack (cur C.$/ C.content)
   in txt == val

hasChild :: String -> [Predicate] -> C.Cursor -> Bool
hasChild name nested cur =
  let children = cur C.$/ C.element (toName name)
   in not (null (applyPredicates children nested))

-- | Check if an XPath expression matches anything in the document.
matches :: Document -> String -> Bool
matches doc path = not (null (evaluate doc (xpath path)))

spec :: Spec
spec = do
  describe "XMIR parsing packs" $ do
    let resources = "test-resources/xmir-parsing-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          pack <- parsePack pth
          let ParsePack{phi = phi'} = pack
              xmir' = do
                doc <- parseXMIRThrows (xmir pack)
                xmirToPhi doc
          case failure pack of
            Just True -> xmir' `shouldThrow` anyException
            _ -> do
              xmir'' <- xmir'
              phi'' <- parseExpressionThrows phi'
              xmir'' `shouldBe` phi''
      )

  describe "prohibit to convert to XMIR" $
    forM_
      [ "[[ ]]"
      , "T"
      , "[[ x -> ? ]]"
      , "[[ ^ -> 5 ]]"
      , "Q.x.y.z"
      , "\"Hello\""
      , "Q"
      , "$"
      , "[[ x -> T ]]"
      , "[[ top -> [[ x -> T ]] ]]"
      , "[[ x -> [[ !t1 -> 5 ]] ]]"
      , "[[ org -> [[ z -> ?, L> Package ]] ]]"
      ]
      ( \phi' -> it phi' $ do
          expr <- parseExpressionThrows phi'
          expressionToXMIR expr defaultXmirContext `shouldThrow` anyException
      )

  describe "XMIR printing packs" $ do
    let resources = "test-resources/xmir-printing-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth ->
          it (makeRelative resources pth) $ do
            pack <- printPack pth
            let PrintPack{phi = phi', xpaths = xpaths'} = pack
            expr <- parseExpressionThrows phi'
            xmir' <- expressionToXMIR expr defaultXmirContext
            let failed = filter (not . matches xmir') xpaths'
            unless
              (null failed)
              (expectationFailure ("Failed xpaths:\n - " ++ intercalate "\n - " failed ++ "\nXMIR is:\n" ++ printXMIR xmir'))
      )

  describe "XMIR round-trip" $ do
    it "keeps λ function name and bound ρ" $ do
      expr <- parseExpressionThrows "[[ k -> [[ x -> ?, L> Lorg_eolang_number_plus, ^ -> [[ y -> ? ]] ]] ]]"
      xmir' <- expressionToXMIR expr defaultXmirContext
      back <- xmirToPhi xmir'
      back `shouldBe` expr

    it "keeps Δ data bound to a named attribute" $ do
      expr <- parseExpressionThrows "[[ k -> [[ a -> [[ D> 01-02 ]], ^ -> [[ D> 03-04 ]] ]] ]]"
      xmir' <- expressionToXMIR expr defaultXmirContext
      back <- xmirToPhi xmir'
      back `shouldBe` expr

    it "keeps Δ data in a dispatched formation" $ do
      expr <- parseExpressionThrows "[[ k -> [[ D> 01-02 ]].plus ]]"
      xmir' <- expressionToXMIR expr defaultXmirContext
      back <- xmirToPhi xmir'
      back `shouldBe` expr

    it "keeps a bare 'Q' bound to a named attribute" $ do
      expr <- parseExpressionThrows "[[ x -> Q ]]"
      xmir' <- expressionToXMIR expr defaultXmirContext
      back <- xmirToPhi xmir'
      back `shouldBe` expr

  describe "XMIR exception messages" $ do
    it "explains an unsupported top-level expression" $ do
      expr <- parseExpressionThrows "[[ x -> $ ]]"
      result <- try (expressionToXMIR expr defaultXmirContext) :: IO (Either SomeException Document)
      case result of
        Left err -> displayException err `shouldContain` "XMIR does not support such top-level expression"
        Right _ -> expectationFailure "expected an exception"

    it "explains an unsupported nested expression" $ do
      expr <- parseExpressionThrows "[[ x -> [[ y -> T ]] ]]"
      result <- try (expressionToXMIR expr defaultXmirContext) :: IO (Either SomeException Document)
      case result of
        Left err -> displayException err `shouldContain` "XMIR does not support such expression"
        Right _ -> expectationFailure "expected an exception"

    it "explains an unsupported binding" $ do
      let expr = ExFormation [BiTau (AtLabel "x") (ExFormation [BiMeta "n", BiVoid AtRho]), BiVoid AtRho]
      result <- try (expressionToXMIR expr defaultXmirContext) :: IO (Either SomeException Document)
      case result of
        Left err -> displayException err `shouldContain` "XMIR does not support such bindings"
        Right _ -> expectationFailure "expected an exception"

    it "explains a parse failure" $ do
      result <- try (parseXMIRThrows "not-xml-at-all <<<") :: IO (Either SomeException Document)
      case result of
        Left err -> displayException err `shouldContain` "Couldn't parse given XMIR"
        Right _ -> expectationFailure "expected an exception"

    it "explains an invalid XMIR structure, including the offending element" $ do
      doc <- parseXMIRThrows "<object><o name=\"app\"><o/></o></object>"
      result <- try (xmirToPhi doc) :: IO (Either SomeException Expression)
      case result of
        Left err -> do
          displayException err `shouldContain` "Couldn't traverse though given XMIR"
          displayException err `shouldContain` "XMIR:"
        Right _ -> expectationFailure "expected an exception"

  describe "escapeXML" $
    it "escapes an apostrophe alongside the other reserved characters" $
      escapeXML "it's a & <b> \"quote\"" `shouldBe` "it&apos;s a &amp; &lt;b&gt; &quot;quote&quot;"

  describe "XMIR document structure" $ do
    it "produces an empty prologue and epilogue" $ do
      expr <- parseExpressionThrows "[[ x -> 5 ]]"
      Document prologue _ epilogue <- expressionToXMIR expr defaultXmirContext
      prologue `shouldBe` Prologue [] Nothing []
      epilogue `shouldBe` []

    it "renders package metas with empty attributes and a matching part" $ do
      expr <- parseExpressionThrows "[[ org -> [[ eolang -> [[ foo -> [[ x -> 5 ]], L> Package ]], L> Package ]] ]]"
      xmir' <- expressionToXMIR expr defaultXmirContext
      let root = C.fromDocument xmir'
      case root C.$/ C.element (toName "metas") of
        [metasCur] -> case C.node metasCur of
          NodeElement metasEl -> elementAttributes metasEl `shouldBe` M.empty
          _ -> expectationFailure "expected <metas> to be an element"
        _ -> expectationFailure "expected exactly one <metas> element"
      case root C.$/ C.element (toName "metas") C.&/ C.element (toName "meta") of
        [metaCur] -> case C.node metaCur of
          NodeElement metaEl -> elementAttributes metaEl `shouldBe` M.empty
          _ -> expectationFailure "expected <meta> to be an element"
        _ -> expectationFailure "expected exactly one <meta> element"
      let parts = root C.$/ C.element (toName "metas") C.&/ C.element (toName "meta") C.&/ C.element (toName "part") C.&/ C.content
      parts `shouldBe` ["org.eolang"]

  describe "XMIR comments" $ do
    let commentedContext :: XmirContext
        commentedContext = XmirContext True False (const "")

    it "includes a decimal comment for a number when comments aren't omitted" $ do
      expr <- parseExpressionThrows "[[ x -> 5 ]]"
      xmir' <- expressionToXMIR expr commentedContext
      printXMIR xmir' `shouldContain` "<!-- 5 -->"

    it "includes a quoted comment for a string when comments aren't omitted" $ do
      expr <- parseExpressionThrows "[[ x -> \"foo\" ]]"
      xmir' <- expressionToXMIR expr commentedContext
      printXMIR xmir' `shouldContain` "<!-- \"foo\" -->"

  describe "XMIR printing edge cases" $ do
    it "wraps a chained dispatch on a formation literal with a @base attribute" $ do
      expr <- parseExpressionThrows "[[ x -> [[ y -> 5 ]].plus.minus ]]"
      xmir' <- expressionToXMIR expr defaultXmirContext
      let root = C.fromDocument xmir'
          xCur = root C.$/ C.element (toName "o")
          outer = filter (\cur -> C.attribute (toName "base") cur == [".minus"]) xCur
          inner =
            concatMap
              (filter (\cur -> C.attribute (toName "base") cur == [".plus"]) . (C.$/ C.element (toName "o")))
              xCur
      length outer `shouldBe` 1
      length inner `shouldBe` 1

    it "renders a void φ binding as a nested formation" $ do
      expr <- parseExpressionThrows "[[ x -> [[ @ -> ? ]] ]]"
      xmir' <- expressionToXMIR expr defaultXmirContext
      let root = C.fromDocument xmir'
          nested = root C.$/ C.element (toName "o") C.&/ C.element (toName "o")
          phiVoid =
            filter
              (\cur -> C.attribute (toName "name") cur == ["φ"] && C.attribute (toName "base") cur == ["∅"])
              nested
      length phiVoid `shouldBe` 1

    it "renders a bare global reference as the top-level value" $ do
      expr <- parseExpressionThrows "[[ x -> Q ]]"
      xmir' <- expressionToXMIR expr defaultXmirContext
      let root = C.fromDocument xmir'
          xCur = filter (\cur -> C.attribute (toName "base") cur == ["Φ"]) (root C.$/ C.element (toName "o"))
      length xCur `shouldBe` 1

    it "omits @base when an application argument is a bare formation" $ do
      expr <- parseExpressionThrows "[[ foo -> Q.bar(x -> 5, α1 -> [[ z -> ? ]]) ]]"
      xmir' <- expressionToXMIR expr defaultXmirContext
      let root = C.fromDocument xmir'
          args = root C.$/ C.element (toName "o") C.&/ C.element (toName "o")
          namedArg = filter (\cur -> C.attribute (toName "as") cur == ["x"]) args
          formationArg = filter (\cur -> C.attribute (toName "as") cur == ["α1"]) args
      case namedArg of
        [argCur] -> C.attribute (toName "base") argCur `shouldBe` ["Φ.number"]
        _ -> expectationFailure "expected exactly one 'x' argument"
      case formationArg of
        [argCur] -> C.attribute (toName "base") argCur `shouldBe` []
        _ -> expectationFailure "expected exactly one α1 argument"

  describe "XMIR malformed input containing a processing instruction" $
    it "embeds a processing instruction verbatim when rendering the offending element" $ do
      doc <-
        parseXMIRThrows
          "<object><o name=\"x\" base=\"∅\"/><?a-pi some-data?><o name=\"y\" base=\"∅\"/></object>"
      result <- try (xmirToPhi doc) :: IO (Either SomeException Expression)
      case result of
        Left exc -> displayException exc `shouldContain` "Couldn't traverse though given XMIR"
        Right _ -> expectationFailure "expected an exception"
