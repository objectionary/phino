{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module AtomsSpec (spec) where

import AST (Expression)
import Atoms (Atom (..), atoms, printAtoms, printAtomsInJSON)
import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Aeson (FromJSON, eitherDecodeStrict)
import Data.List (nub, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Dataize (DataizeContext (..), Steps (..), dataize, implementedAtoms)
import Deps (dontSaveEval, dontSaveStep)
import Functions (buildTerm)
import GHC.Generics (Generic)
import Parser (parseExpressionThrows)
import Test.Hspec
import Text.Printf (printf)

-- The shape 'printAtomsInJSON' promises, parsed back so that the keys and the
-- values of the printed objects are checked against the catalogue itself
-- rather than against a copy of the expected text
data Entry = Entry
  { name :: Text
  , labels :: [Text]
  , rho :: Bool
  , forma :: Text
  , semantics :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Entry

entry :: Atom -> Entry
entry atom = Entry (_name atom) (_labels atom) (_rho atom) (_forma atom) (_semantics atom)

-- Ask the engine to fire the λ function of this name, the way a real λ binding
-- makes it fire: a formation carrying nothing but that λ, dataized so that 𝔻
-- drives 𝕄 into the LAMBDA rule and 𝔼 reaches the atom. Nothing is bound for
-- the atom to read, which is what makes one probe enough for every name: a
-- function phino implements fires, reads its unbound operands and answers ⊥,
-- while a function it does not implement never fires at all and reports itself
-- missing. The two outcomes are far apart, so neither test needs to know what
-- any particular atom computes.
fired :: Text -> IO String
fired func = do
  term <- parseExpressionThrows (printf "[[ L> %s ]]" (T.unpack func))
  locator <- parseExpressionThrows "Q"
  outcome <- try (dataize term (dataizing locator))
  pure (either (\err -> show (err :: SomeException)) (\(answer, _) -> printf "dataized to %s" (show answer)) outcome)
  where
    dataizing :: Expression -> DataizeContext
    dataizing locator =
      DataizeContext locator 25 25 (Steps 250 0) False False False buildTerm dontSaveStep dontSaveEval

-- What the engine says about a λ function it cannot fire
missing :: Text -> String
missing = printf "Atom '%s' does not exist" . T.unpack

-- What it says once an atom has fired against operands it cannot read
unreadable :: String
unreadable = "dataization reached the terminator"

-- Names the catalogue leaves out, which the engine must therefore refuse. No
-- list can be exhaustive, since a λ binding may name anything, so this one
-- holds the names that matter: the five EO's lowering declares precisely so
-- that '--partial' parks on them and renders the call to Java (#1108), the one
-- dropped once 'number.eq' turned out to be pure EO composition (#1074), the
-- one the README's own merge example spells, and — generated from the
-- catalogue, so that it grows with it — every near miss of a real name, which
-- no prefix or suffix confusion may let through.
unsupported :: [Text]
unsupported =
  [ "L_bool_if"
  , "L_string_slice"
  , "L_tuple_length"
  , "L_tuple_at"
  , "L_object_as_bytes"
  , "L_number_eq"
  , "L_number_minus"
  ]
    ++ concatMap (filter (`notElem` catalogued) . nearMisses) catalogued
  where
    catalogued :: [Text]
    catalogued = map _name atoms
    nearMisses :: Text -> [Text]
    nearMisses func = [T.init func, func <> "x"]

spec :: Spec
spec = do
  describe "catalogue" $ do
    it "names exactly the λ functions the dataizer implements" $
      map _name atoms `shouldBe` implementedAtoms

    it "keeps the names in alphabetical order, so that the output may be diffed" $
      map _name atoms `shouldBe` sort (map _name atoms)

    it "prefixes every name with 'L_', the way a λ binding spells it" $
      filter (not . T.isPrefixOf "L_") (map _name atoms) `shouldBe` []

    it "states the semantics of every function in a single line" $
      filter (\atom -> T.null (_semantics atom) || T.isInfixOf "\n" (_semantics atom)) atoms `shouldBe` []

    it "names the forma of what every function answers" $
      filter (T.null . _forma) atoms `shouldBe` []

    it "reads no label twice off the same formation" $
      filter (\atom -> _labels atom /= nub (_labels atom)) atoms `shouldBe` []

  describe "plain output" $ do
    it "prints one name per line and nothing else" $
      lines printAtoms `shouldBe` map (T.unpack . _name) atoms

    it "leaves the last line without an EOL, which the caller adds" $
      printAtoms `shouldEndWith` T.unpack (_name (last atoms))

  describe "JSON output" $ do
    it "parses back into the very catalogue it was printed from" $
      eitherDecodeStrict (TE.encodeUtf8 (T.pack printAtomsInJSON)) `shouldBe` Right (map entry atoms)

    it "keeps ρ and Φ readable instead of escaping them" $
      printAtomsInJSON `shouldContain` "Φ.number"

    it "spells a missing label list as an empty JSON array" $
      printAtomsInJSON `shouldContain` "\"labels\": []"

  -- The catalogue is checked against the engine twice over, because the two
  -- ways of being wrong fail differently. A name that is listed but dead is a
  -- false positive: a caller trusts it and its run dies on the λ. A name that
  -- fires but is unlisted is a false negative: a caller reads the list, decides
  -- the name is free for '--partial' to park on, and gets it computed instead.
  -- 'names exactly the λ functions the dataizer implements' above compares the
  -- catalogue with 'Dataize.implementations', which settles both directions for
  -- the dispatch table itself; these two run the engine, so they also hold when
  -- the table stops being the whole story.
  describe "no false positive: every catalogued λ function really fires" $
    forM_ atoms $ \atom ->
      it (printf "%s fires and reads its operands" (T.unpack (_name atom))) $ do
        outcome <- fired (_name atom)
        outcome `shouldContain` unreadable
        outcome `shouldNotContain` missing (_name atom)

  describe "no false negative: no λ function outside the catalogue fires" $
    forM_ unsupported $ \func ->
      it (printf "%s does not fire" (T.unpack func)) $ do
        outcome <- fired func
        outcome `shouldContain` missing func
