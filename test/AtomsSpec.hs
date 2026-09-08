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
import System.Directory (getPermissions, getTemporaryDirectory, removePathForcibly, setOwnerExecutable, setPermissions)
import System.IO (Handle, hClose, openBinaryTempFile)
import System.Info (os)
import Test.Hspec
import Text.Printf (printf)

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

-- A file in the temporary directory holding the given POSIX shell script,
-- removed afterwards
withScript :: T.Text -> (FilePath -> IO a) -> IO a
withScript script action = do
  dir <- getTemporaryDirectory
  bracket (openBinaryTempFile dir "phino-exec-.sh") discarded $ \(path, handle) -> do
    BS.hPut handle (encodeUtf8 (T.unlines ["#!/bin/sh", script]))
    hClose handle
    action path
  where
    discarded :: (FilePath, Handle) -> IO ()
    discarded (path, handle) = hClose handle >> removePathForcibly path

-- The same file, executable, which is what an 'exec' atom names and phino
-- never stages itself
withExecutable :: T.Text -> (FilePath -> IO a) -> IO a
withExecutable script action = withScript script $ \path -> do
  permissions <- getPermissions path
  setPermissions path (setOwnerExecutable True permissions)
  action path

-- A POSIX shell script is executable nowhere on Windows, so a case that needs
-- one is pending there rather than red
withShell :: Expectation -> Expectation
withShell expectation
  | os == "mingw32" = pendingWith "no POSIX shell script is executable on Windows"
  | otherwise = expectation

-- The registry of one executable λ function, naming the given file
executing :: FilePath -> T.Text
executing file = T.pack (printf "{\"L_answer\": {\"rt\": \"exec\", \"path\": \"%s\"}}" file)

-- Fire the λ function 'L_answer' out of the given atom, against a formation
-- binding 'x' inside a universe binding 'y'
fired :: Atom -> IO Expression
fired atom = do
  form <- parseExpressionThrows "⟦ x ↦ ⟦ Δ ⤍ 01- ⟧ ⟧"
  univ <- parseExpressionThrows "⟦ y ↦ ⟦ Δ ⤍ 02- ⟧ ⟧"
  fireAtom "L_answer" atom form univ

-- What the script wrote under 'n' has to come back parsed, so a case asserting
-- on it says which expression it expects in 𝜑 rather than in constructors
answers :: T.Text -> String -> Expectation
answers script expected = withNode $ do
  answer <- fired (Scripted RtNode script)
  wanted <- parseExpressionThrows expected
  answer `shouldBe` wanted

-- The same, for an atom phino runs off its path instead of staging it
executes :: T.Text -> String -> Expectation
executes script expected = withShell $
  withExecutable script $ \file -> do
    answer <- fired (Executable file)
    wanted <- parseExpressionThrows expected
    answer `shouldBe` wanted

-- A firing that has to fail, with the reason naming the given fragments
fails :: T.Text -> [String] -> Expectation
fails script fragments =
  withNode $
    fired (Scripted RtNode script)
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
        registeredAtom registry "L_answer" `shouldBe` Just (Scripted RtNode "say(1)")

    -- An atom the object model brought as a binary of its own names no
    -- interpreter at all, only the file phino is to run
    it "reads an executable λ function as the file it runs" $
      withShell $
        withExecutable "" $ \file ->
          withRegistry (executing file) $ \path -> do
            registry <- readRegistry path
            registeredAtom registry "L_answer" `shouldBe` Just (Executable file)

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
        ( "an executable entry carries no path"
        , "{\"L_answer\": {\"rt\": \"exec\"}}"
        , ["path"]
        )
      ,
        ( "the executable file is not there"
        , "{\"L_answer\": {\"rt\": \"exec\", \"path\": \"no-such-atom\"}}"
        , ["L_answer", "no-such-atom", "there is no such file"]
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

    -- A file nobody may run is refused where the registry is read, not where
    -- the atom would fire
    it "fails when the file of an executable λ function cannot be run" $
      withScript "" $ \file ->
        withRegistry (executing file) $ \path ->
          readRegistry path
            `shouldThrow` (\failure -> "not executable" `isInfixOf` show (failure :: SomeException))

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

    -- An executable atom is spawned as it is, under no interpreter, so phino
    -- stages nothing of it and the file speaks the same protocol a script does
    it "runs an executable λ function straight off its path" $
      executes "echo '{\"n\": \"⟦ Δ ⤍ 2A- ⟧\"}'" "⟦ Δ ⤍ 2A- ⟧"

    it "names the λ function being fired as the first argument of an executable" $
      executes
        "if [ \"$1\" = L_answer ]; then echo '{\"n\": \"⟦ Δ ⤍ FF- ⟧\"}'; else echo '{\"n\": \"⟦ Δ ⤍ 00- ⟧\"}'; fi"
        "⟦ Δ ⤍ FF- ⟧"

    it "feeds the formation and the universe to an executable on stdin" $
      executes
        "case \"$(cat)\" in *'x ↦'*) echo '{\"n\": \"⟦ Δ ⤍ FF- ⟧\"}';; *) echo '{\"n\": \"⟦ Δ ⤍ 00- ⟧\"}';; esac"
        "⟦ Δ ⤍ FF- ⟧"
