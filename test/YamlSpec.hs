-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module YamlSpec where

import Control.Exception (Exception (displayException), SomeException)
import Control.Monad
import Data.List (isInfixOf)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml qualified as Yaml
import Misc
import System.FilePath
import Test.Hspec (Spec, describe, it, runIO, shouldReturn, shouldSatisfy, shouldThrow)
import Yaml (ContextualizeRule, DataizeRule, MorphRule, yamlRule)

spec :: Spec
spec = do
  describe "parses yaml rule" $ do
    let resources = "test-resources/yaml-packs"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth -> it (makeRelative resources pth) $ do
          _ <- yamlRule pth
          pure () `shouldReturn` ()
      )

  describe "fails on yaml typos" $ do
    let resources = "test-resources/yaml-typos"
    packs <- runIO (allPathsIn resources)
    forM_
      packs
      ( \pth ->
          it (makeRelative resources pth) $
            shouldThrow
              (yamlRule pth)
              ( \e ->
                  let msg = displayException (e :: SomeException)
                   in "Unknown" `isInfixOf` msg || "Exactly one" `isInfixOf` msg
              )
      )

  describe "rejects a label that equals the name" $ do
    let failsAsRedundant decoded = case decoded of
          Left err -> "redundant" `isInfixOf` Yaml.prettyPrintParseException err
          Right _ -> False
        decodeYaml :: (Yaml.FromJSON a) => String -> Either Yaml.ParseException a
        decodeYaml = Yaml.decodeEither' . encodeUtf8 . T.pack
    it "in a morphing rule" $
      (decodeYaml "name: prim\nlabel: prim\nmatch: ⟦𝐵⟧\ne-match: 𝑒\nn-result: ⟦𝐵⟧" :: Either Yaml.ParseException MorphRule)
        `shouldSatisfy` failsAsRedundant
    it "in a dataization rule" $
      (decodeYaml "name: bott\nlabel: bott\nmatch: ⊥\ne-match: 𝑒\nd-result: '--'" :: Either Yaml.ParseException DataizeRule)
        `shouldSatisfy` failsAsRedundant
    it "in a contextualization rule" $
      (decodeYaml "name: cxi\nlabel: cxi\nmatch: ξ\nc-match: 𝑘\nc-result: 𝑘" :: Either Yaml.ParseException ContextualizeRule)
        `shouldSatisfy` failsAsRedundant
