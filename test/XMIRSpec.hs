{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module XMIRSpec where

import AST
import Control.Monad (forM_, unless)
import Data.Aeson
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import Misc (allPathsIn)
import Parser (parseExpressionThrows, parseProgramThrows)
import System.FilePath (makeRelative)
import Test.Hspec (Spec, anyException, describe, expectationFailure, it, runIO, shouldBe, shouldThrow)
import Text.XML (Document (..), Element (..))
import Text.XML.Cursor qualified as C
import XMIR (defaultXmirContext, parseXMIRThrows, printXMIR, programToXMIR, toName, xmirToPhi)

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
splitBracket = go 0 ""
  where
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
splitAnd = go 0 ""
  where
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
extractQuoted ('"' : rest) = takeWhile (/= '"') rest
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
              phi'' <- parseProgramThrows phi'
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
      ]
      ( \phi' -> it phi' $ do
          expr <- parseExpressionThrows phi'
          programToXMIR (Program expr) defaultXmirContext `shouldThrow` anyException
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
            prog <- parseProgramThrows phi'
            xmir' <- programToXMIR prog defaultXmirContext
            let failed = filter (not . matches xmir') xpaths'
            unless
              (null failed)
              (expectationFailure ("Failed xpaths:\n - " ++ intercalate "\n - " failed ++ "\nXMIR is:\n" ++ printXMIR xmir'))
      )
