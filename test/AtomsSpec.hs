{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module AtomsSpec (spec) where

import AST
import Atoms (Atom (..), Runtime (RtNode), emptyRegistry, fireAtom, readRegistry, registeredAtom)
import Control.Exception (SomeException, bracket)
import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Fixtures (withNode)
import Parser (parseExpressionThrows)
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.IO (Handle, hClose, openBinaryTempFile)
import Test.Hspec

-- A registry file holding the given content, removed afterwards
withRegistry :: T.Text -> (FilePath -> IO a) -> IO a
withRegistry content action = do
  dir <- getTemporaryDirectory
  bracket (openBinaryTempFile dir "phino-registry-.json") discarded $ \(path, handle) -> do
    BS.hPut handle (encodeUtf8 content)
    hClose handle
    action path
  where
    discarded :: (FilePath, Handle) -> IO ()
    discarded (path, handle) = hClose handle >> removePathForcibly path

-- Fire the λ function 'L_answer' out of the given script, against a formation
-- binding 'x' inside a universe binding 'y'
fired :: T.Text -> IO Expression
fired script = do
  form <- parseExpressionThrows "⟦ x ↦ ⟦ Δ ⤍ 01- ⟧ ⟧"
  univ <- parseExpressionThrows "⟦ y ↦ ⟦ Δ ⤍ 02- ⟧ ⟧"
  fireAtom "L_answer" (Atom RtNode script) form univ

-- What the script wrote under 'n' has to come back parsed, so a case asserting
-- on it says which expression it expects in 𝜑 rather than in constructors
answers :: T.Text -> String -> Expectation
answers script expected = withNode $ do
  answer <- fired script
  wanted <- parseExpressionThrows expected
  answer `shouldBe` wanted

-- A firing that has to fail, with the reason naming the given fragments
fails :: T.Text -> [String] -> Expectation
fails script fragments =
  withNode $
    fired script
      `shouldThrow` (\failure -> all (`isInfixOf` show (failure :: SomeException)) fragments)

spec :: Spec
spec = do
  -- phino implements no λ function, so an empty registry is what a run without
  -- '--atoms' fires against: every name is unknown there
  describe "emptyRegistry" $
    it "registers no λ function at all" $
      registeredAtom emptyRegistry "L_bytes_eq" `shouldBe` Nothing

  describe "readRegistry" $ do
    it "reads a λ function together with its runtime and script" $
      withRegistry "{\"L_answer\": {\"rt\": \"node\", \"script\": \"say(1)\"}}" $ \path -> do
        registry <- readRegistry path
        registeredAtom registry "L_answer" `shouldBe` Just (Atom RtNode "say(1)")

    it "leaves a name the file does not carry unregistered" $
      withRegistry "{\"L_answer\": {\"rt\": \"node\", \"script\": \"say(1)\"}}" $ \path -> do
        registry <- readRegistry path
        registeredAtom registry "L_bytes_eq" `shouldBe` Nothing

    -- An unknown runtime is refused where the file is read, which is before any
    -- dataization starts, rather than at the moment an atom of it would fire
    forM_
      [
        ( "the runtime is not one phino can run"
        , "{\"L_answer\": {\"rt\": \"ruby\", \"script\": \"say(1)\"}}"
        , ["unknown runtime 'ruby'", "node"]
        )
      ,
        ( "an entry carries no script"
        , "{\"L_answer\": {\"rt\": \"node\"}}"
        , ["script"]
        )
      ,
        ( "an entry carries no runtime"
        , "{\"L_answer\": {\"script\": \"say(1)\"}}"
        , ["rt"]
        )
      ,
        ( "the file is not JSON at all"
        , "L_answer: js"
        , ["cannot be read"]
        )
      ]
      ( \(desc, content, fragments) ->
          it ("fails when " ++ desc) $
            withRegistry content $ \path ->
              readRegistry path
                `shouldThrow` (\failure -> all (`isInfixOf` show (failure :: SomeException)) fragments)
      )

    it "fails when the file is not there" $
      readRegistry "no-such-registry.json"
        `shouldThrow` (\failure -> "cannot be read" `isInfixOf` show (failure :: SomeException))

  describe "fireAtom" $ do
    it "hands back the 𝜑-expression the script wrote under 'n'" $
      answers "process.stdout.write(JSON.stringify({n: '⟦ Δ ⤍ 2A- ⟧'}))" "⟦ Δ ⤍ 2A- ⟧"

    -- One script may stand for several λ functions, so the name of the one
    -- being fired is its first command-line argument — where node puts it
    it "names the λ function being fired as the first command-line argument" $
      answers
        "process.stdout.write(JSON.stringify({n: process.argv[2] === 'L_answer' ? '⟦ Δ ⤍ FF- ⟧' : '⟦ Δ ⤍ 00- ⟧'}))"
        "⟦ Δ ⤍ FF- ⟧"

    -- The formation being evaluated arrives under 'b' and the universe Φ under
    -- 's', both as 𝜑 text on stdin
    it "feeds the formation and the universe to the script on stdin" $
      answers
        "const {b, s} = JSON.parse(require('fs').readFileSync(0, 'utf8'));\
        \process.stdout.write(JSON.stringify({n: b.includes('x ↦') && s.includes('y ↦') ? '⟦ Δ ⤍ FF- ⟧' : '⟦ Δ ⤍ 00- ⟧'}))"
        "⟦ Δ ⤍ FF- ⟧"

    -- Neither payload carries syntax sugar, whatever '--sweet' says about the
    -- output of the run, so a script finds every datum spelled as a Δ binding
    it "spells the payloads as canonical 𝜑-calculus" $
      answers
        "const {b} = JSON.parse(require('fs').readFileSync(0, 'utf8'));\
        \process.stdout.write(JSON.stringify({n: b.includes('Δ ⤍ 01-') ? '⟦ Δ ⤍ FF- ⟧' : '⟦ Δ ⤍ 00- ⟧'}))"
        "⟦ Δ ⤍ FF- ⟧"

    it "reads a script that says nothing to stdin without waiting for it" $
      answers "process.stdout.write(JSON.stringify({n: '⟦ Δ ⤍ 01- ⟧'}))" "⟦ Δ ⤍ 01- ⟧"

    it "fails with the script's own complaint when it exits non-zero" $
      fails
        "process.stderr.write('no idea what to do');process.exit(4)"
        ["L_answer", "exit code 4", "no idea what to do"]

    it "fails when the script writes something other than JSON" $
      fails "process.stdout.write('almost')" ["L_answer", "almost"]

    it "fails when the script writes JSON with no 'n' in it" $
      fails "process.stdout.write(JSON.stringify({m: '⟦ ⟧'}))" ["L_answer", "n"]

    it "fails when what the script put under 'n' is not a 𝜑-expression" $
      fails "process.stdout.write(JSON.stringify({n: '⟦ ⟧⟧'}))" ["L_answer"]
