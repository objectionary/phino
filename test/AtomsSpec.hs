{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module AtomsSpec (spec) where

import AST
import Atoms (Atom (..), Registry, Runtime (RtNode), Session (_program), closeRegistry, emptyRegistry, fireAtom, readRegistry, registeredAtom)
import Control.Exception (SomeException, bracket, finally)
import Control.Monad (forM_)
import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.List (isInfixOf)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Fixtures (resident, withExecutable, withNode, withRegistryOf, withScript, withShell)
import Parser (parseExpressionThrows)
import System.Directory (doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import System.IO (Handle, hClose, openBinaryTempFile)
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

-- The registry of one executable λ function, naming the given file. The path
-- goes through JSON encoding rather than into the text by hand, since a
-- Windows one spells its separators with the escape character of JSON
executing :: FilePath -> T.Text
executing file = decodeUtf8 (BSL.toStrict (encode (serving "exec" ["L_answer"] file)))

-- The registry of the given λ functions all served by, or executed as, the
-- given file
serving :: T.Text -> [T.Text] -> FilePath -> Value
serving runtime names file = object [Key.fromText name .= object ["rt" .= runtime, "path" .= file] | name <- names]

-- The λ functions of the registry read from a file naming a resident program
-- built of the given per-request snippet (see 'resident'), stopped afterwards,
-- so that no spec leaves a shell behind
withServed :: [T.Text] -> T.Text -> (Registry -> IO a) -> IO a
withServed names snippet action =
  withExecutable (resident snippet) $ \file ->
    withRegistryOf (serving "serve" names file) $ \path -> do
      registry <- readRegistry path
      action registry `finally` closeRegistry registry

-- Fire the given λ function out of the registry, against the same formation
-- and universe 'fired' uses, unless a different universe is given
firedFrom :: Registry -> T.Text -> String -> IO Expression
firedFrom registry func universe = do
  form <- parseExpressionThrows "⟦ x ↦ ⟦ Δ ⤍ 01- ⟧ ⟧"
  univ <- parseExpressionThrows universe
  maybe (fail (printf "'%s' is not registered" (T.unpack func))) (\atom -> fireAtom func atom form univ) (registeredAtom registry func)

-- The path a served λ function is served from, if it is one
servedFrom :: Maybe Atom -> Maybe FilePath
servedFrom (Just (Served session)) = Just (_program session)
servedFrom _ = Nothing

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

-- The same, for an atom served by a resident program built of the given
-- per-request snippet
serves :: T.Text -> String -> Expectation
serves snippet expected = withShell $
  withServed ["L_answer"] snippet $ \registry -> do
    answer <- firedFrom registry "L_answer" "⟦ y ↦ ⟦ Δ ⤍ 02- ⟧ ⟧"
    wanted <- parseExpressionThrows expected
    answer `shouldBe` wanted

-- A firing of a served atom that has to fail, with the reason naming the given
-- fragments
refuses :: T.Text -> [String] -> Expectation
refuses snippet fragments = withShell $
  withServed ["L_answer"] snippet $ \registry ->
    firedFrom registry "L_answer" "⟦ y ↦ ⟦ Δ ⤍ 02- ⟧ ⟧"
      `shouldThrow` (\failure -> all (`isInfixOf` show (failure :: SomeException)) fragments)

-- The reply of a resident program answering the request with the given bytes
replying :: T.Text -> T.Text
replying bytes = "printf '{\"id\": %s, \"𝑛\": \"⟦ Δ ⤍ %s ⟧\"}\\n' \"$id\" \"" <> bytes <> "\""

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

    it "reads a served λ function as the file it is served from" $
      withShell $
        withExecutable "" $ \file ->
          withRegistryOf (serving "serve" ["L_answer"] file) $ \path -> do
            registry <- readRegistry path
            servedFrom (registeredAtom registry "L_answer") `shouldBe` Just file

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
        ( "a served entry carries no path"
        , "{\"L_answer\": {\"rt\": \"serve\"}}"
        , ["path"]
        )
      ,
        ( "the served file is not there"
        , "{\"L_answer\": {\"rt\": \"serve\", \"path\": \"no-such-atom\"}}"
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

    it "fails when the file of a served λ function cannot be run" $
      withScript "" $ \file ->
        withRegistryOf (serving "serve" ["L_answer"] file) $ \path ->
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

    -- A served atom is asked over the streams of a program that stays for the
    -- run, one line per fire, so the program speaks another protocol than a
    -- one-shot one: the letters of the evaluation rule of the calculus
    it "hands back the 𝜑-expression the resident program wrote under '𝑛'" $
      serves (replying "2A-") "⟦ Δ ⤍ 2A- ⟧"

    it "keeps one resident program across the fires" $
      withShell $
        withServed ["L_answer"] (replying "0$n-") $ \registry -> do
          _ <- firedFrom registry "L_answer" "⟦ y ↦ ⟦ Δ ⤍ 02- ⟧ ⟧"
          second <- firedFrom registry "L_answer" "⟦ y ↦ ⟦ Δ ⤍ 02- ⟧ ⟧"
          wanted <- parseExpressionThrows "⟦ Δ ⤍ 02- ⟧"
          second `shouldBe` wanted

    -- One file may be registered under several λ names, and it is one program
    -- that serves them all, not one per name
    it "serves every λ name registered on the same file from one program" $
      withShell $
        withServed ["L_answer", "L_other"] (replying "0$n-") $ \registry -> do
          _ <- firedFrom registry "L_answer" "⟦ y ↦ ⟦ Δ ⤍ 02- ⟧ ⟧"
          second <- firedFrom registry "L_other" "⟦ y ↦ ⟦ Δ ⤍ 02- ⟧ ⟧"
          wanted <- parseExpressionThrows "⟦ Δ ⤍ 02- ⟧"
          second `shouldBe` wanted

    it "tells the resident program the universe under '𝑒' before the first request" $
      serves (replying "0$e-") "⟦ Δ ⤍ 01- ⟧"

    it "does not tell the resident program a universe it was told already" $
      withShell $
        withServed ["L_answer"] (replying "0$e-") $ \registry -> do
          _ <- firedFrom registry "L_answer" "⟦ y ↦ ⟦ Δ ⤍ 02- ⟧ ⟧"
          second <- firedFrom registry "L_answer" "⟦ y ↦ ⟦ Δ ⤍ 02- ⟧ ⟧"
          wanted <- parseExpressionThrows "⟦ Δ ⤍ 01- ⟧"
          second `shouldBe` wanted

    it "tells the resident program the universe again when it changes" $
      withShell $
        withServed ["L_answer"] (replying "0$e-") $ \registry -> do
          _ <- firedFrom registry "L_answer" "⟦ y ↦ ⟦ Δ ⤍ 02- ⟧ ⟧"
          second <- firedFrom registry "L_answer" "⟦ z ↦ ⟦ Δ ⤍ 03- ⟧ ⟧"
          wanted <- parseExpressionThrows "⟦ Δ ⤍ 02- ⟧"
          second `shouldBe` wanted

    it "names the λ function being fired under 'λ' in the request" $
      serves
        ("case \"$line\" in *'\"λ\":\"L_answer\"'*) " <> replying "FF-" <> ";; *) " <> replying "00-" <> ";; esac")
        "⟦ Δ ⤍ FF- ⟧"

    it "carries the formation under '𝑏' in the request" $
      serves
        ("case \"$line\" in *'x ↦'*) " <> replying "FF-" <> ";; *) " <> replying "00-" <> ";; esac")
        "⟦ Δ ⤍ FF- ⟧"

    it "fails when the resident program answers another request" $
      refuses "printf '{\"id\": 99, \"𝑛\": \"⟦ Δ ⤍ 2A- ⟧\"}\\n'" ["L_answer", "request 99"]

    it "fails with the resident program's own complaint when it quits non-zero" $
      refuses "echo 'no idea what to do' >&2; exit 4" ["L_answer", "exit code 4", "no idea what to do"]

    it "fails when the resident program quits without answering" $
      refuses "exit 0" ["L_answer", "without answering"]

    it "fails when the resident program writes something other than JSON" $
      refuses "echo almost" ["L_answer", "almost"]

    it "fails when the resident program writes JSON with no '𝑛' in it" $
      refuses "printf '{\"id\": %s, \"m\": \"⟦ ⟧\"}\\n' \"$id\"" ["L_answer", "𝑛"]

    it "fails when what the resident program put under '𝑛' is not a 𝜑-expression" $
      refuses "printf '{\"id\": %s, \"𝑛\": \"⟦ ⟧⟧\"}\\n' \"$id\"" ["L_answer"]

  describe "closeRegistry" $ do
    -- The program is told to quit by its stdin closing, which its read loop
    -- notices, so it gets to run whatever it does on exit
    it "stops the resident program the registry has started" $
      withShell $ do
        dir <- getTemporaryDirectory
        let mark = dir </> "phino-resident-quit"
        removePathForcibly mark
        withServed ["L_answer"] ("trap 'touch " <> T.pack mark <> "' EXIT; " <> replying "2A-") $ \registry -> do
          _ <- firedFrom registry "L_answer" "⟦ y ↦ ⟦ Δ ⤍ 02- ⟧ ⟧"
          closeRegistry registry
          doesFileExist mark `shouldReturn` True

    it "leaves a registry that started no program alone" $
      closeRegistry emptyRegistry `shouldReturn` ()
