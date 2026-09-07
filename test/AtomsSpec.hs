{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module AtomsSpec (spec) where

import Atoms (Atom (..), atoms, printAtoms, printAtomsInJSON)
import Data.Aeson (FromJSON, eitherDecodeStrict)
import Data.List (nub, sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Dataize (implementedAtoms)
import GHC.Generics (Generic)
import Test.Hspec

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
