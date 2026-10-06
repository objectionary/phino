{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module LanguageSpec where

import Control.Monad (forM_)
import Data.Either (fromLeft)
import Data.List (isInfixOf)
import Data.Text (Text)
import Language (language, shared)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

common :: Text -> Text -> Either String (Maybe Text)
common first second = shared <$> language first <*> language second

spec :: Spec
spec = do
  describe "shared" $
    forM_
      [ ("L_[a-z]+_plus", "L_number_[a-z]+", Just "L_number_plus")
      , ("L_(foo|bar)", "L_foo", Just "L_foo")
      , ("L_number_(plus|times)", "L_number_gt", Nothing)
      , ("L_[a-z]+_plus", "L_number_[0-9]+", Nothing)
      , ("L_x{2,3}", "L_x{4,}", Nothing)
      , ("L_x{2,3}", "L_x+?", Just "L_xx")
      , ("L_\\d\\w*", "L_[^0-8]", Just "L_9")
      , ("L_.", "L_\\n", Nothing)
      , ("L_(?:ab)*", "L_(?<pair>ab){2}", Just "L_abab")
      , ("L_[]a-]", "L_-", Just "L_-")
      , ("L_[\\]]", "L_\\]", Just "L_]")
      , ("L_a{,2}", "L_a\\{,2}", Just "L_a{,2}")
      , ("", "x?", Just "")
      , ("L_é+", "L_[^a-z]", Just "L_é")
      , ("L_[\\d-z]", "L_A", Nothing)
      , ("L_[\\d-z]", "L_-", Just "L_-")
      ]
      ( \(first, second, answer) ->
          it ("tells what '" <> show first <> "' and '" <> show second <> "' both match") $
            common first second `shouldBe` Right answer
      )

  describe "language" $
    forM_
      [ ("L_(a)\\1", "cannot be compared")
      , ("L_(?=a)a", "cannot be compared")
      , ("^L_a", "cannot be compared")
      , ("L_a++", "cannot be compared")
      , ("L_[[:alpha:]]", "cannot be compared")
      , ("L_\\bfoo", "cannot be compared")
      , ("L_[z-a]", "runs backwards")
      , ("L_(a", "not closed")
      , ("L_a)", "not expected")
      , ("L_[a", "not closed")
      , ("*a", "quantifies nothing")
      ]
      ( \(key, message) ->
          it ("cannot read the key '" <> show key <> "'") $
            fromLeft "" (language key) `shouldSatisfy` (message `isInfixOf`)
      )
