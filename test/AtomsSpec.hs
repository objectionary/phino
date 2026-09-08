{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module AtomsSpec (spec) where

import AST
import Atoms (Atom (..), Program (..), Registry, Runtime (RtNode), Session (_program), closeRegistry, emptyRegistry, fireAtom, readRegistry, registeredAtom)
import Control.Exception (SomeException, finally)
import Control.Monad (forM_)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Pair)
import Data.ByteString qualified as BS
import Data.List (isInfixOf)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Fixtures (resident, withExecutable, withNode, withRegistryOf, withScript, withShell, withTemp)
import Parser (parseExpressionThrows)
import System.Directory (doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import Test.Hspec
import Text.Printf (printf)

-- The registry of the given λ functions, every one of them the same entry
registryOf :: [T.Text] -> [Pair] -> Value
registryOf names fields = object [Key.fromText name .= object fields | name <- names]

-- The entry of a λ function run as the given file, which goes through JSON
-- encoding rather than into text by hand, since a Windows path spells its
-- separators with the escape character of JSON
executing :: FilePath -> [Pair]
executing file = ["rt" .= ("exec" :: T.Text), "path" .= file]

-- The entry of a λ function run as the given script under node
scripted :: T.Text -> [Pair]
scripted script = ["rt" .= ("node" :: T.Text), "script" .= script]

-- The same entry, kept for the run
served :: [Pair] -> [Pair]
served fields = ("serve" .= True) : fields

-- The text of a registry of node scripts under the given keys, in exactly the
-- order given, which 'registryOf' cannot promise
ordered :: [(T.Text, T.Text)] -> BS.ByteString
ordered entries = encodeUtf8 ("{" <> T.intercalate ", " ["\"" <> key <> "\": {\"rt\": \"node\", \"script\": \"" <> script <> "\"}" | (key, script) <- entries] <> "}")

-- The λ functions of the given registry, read from a file, with every program
-- it has started stopped afterwards, so that no spec leaves a process behind
withRegistered :: Value -> (Registry -> IO a) -> IO a
withRegistered registry action =
  withRegistryOf registry $ \path -> do
    atoms <- readRegistry path
    action atoms `finally` closeRegistry atoms

-- The λ functions of the registry naming a resident program built of the given
-- per-request snippet (see 'resident') under every given name
withServed :: [T.Text] -> T.Text -> (Registry -> IO a) -> IO a
withServed names snippet action =
  withExecutable (resident snippet) $ \file ->
    withRegistered (registryOf names (served (executing file))) action

-- Fire the given λ function out of the registry, against the same formation
-- 'fired' uses, inside the given universe
firedFrom :: Registry -> T.Text -> String -> IO Expression
firedFrom registry func universe = do
  form <- parseExpressionThrows "⟦ x ↦ ⟦ Δ ⤍ 01- ⟧ ⟧"
  univ <- parseExpressionThrows universe
  maybe (fail (printf "'%s' is not registered" (T.unpack func))) (\atom -> fireAtom func atom form univ) (registeredAtom registry func)

-- Fire the λ function 'L_answer' out of the given atom, against a formation
-- binding 'x' inside a universe binding 'y'
fired :: Atom -> IO Expression
fired atom = do
  form <- parseExpressionThrows "⟦ x ↦ ⟦ Δ ⤍ 01- ⟧ ⟧"
  univ <- parseExpressionThrows "⟦ y ↦ ⟦ Δ ⤍ 02- ⟧ ⟧"
  fireAtom "L_answer" atom form univ

-- The program a λ function is kept for the run with, if it is kept at all
kept :: Maybe Atom -> Maybe Program
kept (Just (Resident session)) = Just (_program session)
kept _ = Nothing

-- A script run once per fire, answering the request with the given JavaScript
-- expression, in which 'lines' is every line phino said, 'universe' the one
-- carrying '𝑒' and 'request' the one carrying 'id', so a case asserts on what
-- phino says rather than on how a script reads it
scripting :: T.Text -> T.Text
scripting expr =
  T.unlines
    [ "const lines = require('fs').readFileSync(0, 'utf8').split('\\n').filter(Boolean).map((line) => JSON.parse(line));"
    , "const universe = lines.find((message) => '𝑒' in message);"
    , "const request = lines.find((message) => 'id' in message);"
    , "process.stdout.write(JSON.stringify({id: request.id, '𝑛': " <> expr <> "}));"
    ]

-- A script reading phino's lines one by one until its stdin closes and
-- answering every request with how many it has seen, so a case tells one
-- process kept across fires from one started afresh for each
counting :: T.Text
counting =
  T.unlines
    [ "let seen = 0;"
    , "require('readline').createInterface({input: process.stdin}).on('line', (line) => {"
    , "  const message = JSON.parse(line);"
    , "  if ('id' in message) {"
    , "    seen += 1;"
    , "    process.stdout.write(JSON.stringify({id: message.id, '𝑛': '⟦ Δ ⤍ 0' + seen + '- ⟧'}) + '\\n');"
    , "  }"
    , "});"
    ]

-- What the script wrote under '𝑛' has to come back parsed, so a case asserting
-- on it says which expression it expects in 𝜑 rather than in constructors
answers :: T.Text -> String -> Expectation
answers script expected = withNode $ do
  answer <- fired (Transient (Scripted RtNode script))
  wanted <- parseExpressionThrows expected
  answer `shouldBe` wanted

-- The same, for an atom phino runs off its path instead of staging it
executes :: T.Text -> String -> Expectation
executes script expected = withShell $
  withExecutable script $ \file -> do
    answer <- fired (Transient (Executable file))
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

-- A firing of a script that has to fail, with the reason naming the given
-- fragments
fails :: T.Text -> [String] -> Expectation
fails script fragments =
  withNode $
    fired (Transient (Scripted RtNode script))
      `shouldThrow` (\failure -> all (`isInfixOf` show (failure :: SomeException)) fragments)

-- The same, for a served atom
refuses :: T.Text -> [String] -> Expectation
refuses snippet fragments = withShell $
  withServed ["L_answer"] snippet $ \registry ->
    firedFrom registry "L_answer" "⟦ y ↦ ⟦ Δ ⤍ 02- ⟧ ⟧"
      `shouldThrow` (\failure -> all (`isInfixOf` show (failure :: SomeException)) fragments)

-- The reply of a resident program answering the request with the given bytes
replying :: T.Text -> T.Text
replying bytes = "printf '{\"id\": %s, \"𝑛\": \"⟦ Δ ⤍ %s ⟧\"}\\n' \"$id\" \"" <> bytes <> "\""

spec :: Spec
spec = do
  -- phino implements no λ function, so an empty registry is what a run without
  -- '--atoms' fires against: every name is unknown there
  describe "emptyRegistry" $
    it "registers no λ function at all" $
      registeredAtom emptyRegistry "L_bytes_eq" `shouldBe` Nothing

  describe "readRegistry" $ do
    it "reads a λ function together with its runtime and script" $
      withRegistryOf (registryOf ["L_answer"] (scripted "say(1)")) $ \path -> do
        registry <- readRegistry path
        registeredAtom registry "L_answer" `shouldBe` Just (Transient (Scripted RtNode "say(1)"))

    -- An atom the object model brought as a binary of its own names no
    -- interpreter at all, only the file phino is to run
    it "reads an executable λ function as the file it runs" $
      withShell $
        withExecutable "" $ \file ->
          withRegistryOf (registryOf ["L_answer"] (executing file)) $ \path -> do
            registry <- readRegistry path
            registeredAtom registry "L_answer" `shouldBe` Just (Transient (Executable file))

    -- Whether a program is kept for the run is its own flag, so any program
    -- may be kept, whatever runs it
    it "keeps an executable λ function for the run when its entry says serve" $
      withShell $
        withExecutable "" $ \file ->
          withRegistryOf (registryOf ["L_answer"] (served (executing file))) $ \path -> do
            registry <- readRegistry path
            kept (registeredAtom registry "L_answer") `shouldBe` Just (Executable file)

    it "keeps a script for the run when its entry says serve" $
      withRegistryOf (registryOf ["L_answer"] (served (scripted "say(1)"))) $ \path -> do
        registry <- readRegistry path
        kept (registeredAtom registry "L_answer") `shouldBe` Just (Scripted RtNode "say(1)")

    it "starts a program afresh for every fire when its entry says not to serve" $
      withRegistryOf (registryOf ["L_answer"] (("serve" .= False) : scripted "say(1)")) $ \path -> do
        registry <- readRegistry path
        registeredAtom registry "L_answer" `shouldBe` Just (Transient (Scripted RtNode "say(1)"))

    it "leaves a name the file does not carry unregistered" $
      withRegistryOf (registryOf ["L_answer"] (scripted "say(1)")) $ \path -> do
        registry <- readRegistry path
        registeredAtom registry "L_bytes_eq" `shouldBe` Nothing

    -- A key is a regular expression, so one entry may stand for a whole family
    -- of atoms and the same program need not be spelled once per name
    it "matches a λ name against the key as a regular expression" $
      withRegistryOf (registryOf ["L_number_.*"] (scripted "say(1)")) $ \path -> do
        registry <- readRegistry path
        registeredAtom registry "L_number_plus" `shouldBe` Just (Transient (Scripted RtNode "say(1)"))

    -- A plain name is a regular expression too, and it means that one atom,
    -- not every atom whose name it is a part of
    it "matches the key against the whole λ name" $
      withRegistryOf (registryOf ["L_number"] (scripted "say(1)")) $ \path -> do
        registry <- readRegistry path
        registeredAtom registry "L_number_plus" `shouldBe` Nothing

    -- The keys are tried in the order the file lists them, so a catch-all
    -- placed first hides everything below it, and the file is written by hand
    -- here because 'object' does not keep the order of its keys
    it "fires the first key top to bottom that matches" $
      withTemp "phino-atoms-.json" (ordered [(".*", "say(1)"), ("L_answer", "say(2)")]) $ \path -> do
        registry <- readRegistry path
        registeredAtom registry "L_answer" `shouldBe` Just (Transient (Scripted RtNode "say(1)"))

    it "reaches a later key when the earlier ones do not match" $
      withTemp "phino-atoms-.json" (ordered [("L_other", "say(1)"), (".*", "say(2)")]) $ \path -> do
        registry <- readRegistry path
        registeredAtom registry "L_answer" `shouldBe` Just (Transient (Scripted RtNode "say(2)"))

    -- A malformed entry is refused where the file is read, which is before any
    -- dataization starts, rather than at the moment an atom of it would fire
    forM_
      [
        ( "the runtime is not one phino can run"
        , registryOf ["L_answer"] ["rt" .= ("ruby" :: T.Text), "script" .= ("say(1)" :: T.Text)]
        , ["unknown runtime 'ruby'", "node"]
        )
      ,
        ( "an entry carries no script"
        , registryOf ["L_answer"] ["rt" .= ("node" :: T.Text)]
        , ["script"]
        )
      ,
        ( "an entry carries no runtime"
        , registryOf ["L_answer"] ["script" .= ("say(1)" :: T.Text)]
        , ["rt"]
        )
      ,
        ( "an executable entry carries no path"
        , registryOf ["L_answer"] ["rt" .= ("exec" :: T.Text)]
        , ["path"]
        )
      ,
        ( "the executable file is not there"
        , registryOf ["L_answer"] (executing "no-such-atom")
        , ["L_answer", "no-such-atom", "there is no such file"]
        )
      ,
        ( "the file to serve from is not there"
        , registryOf ["L_answer"] (served (executing "no-such-atom"))
        , ["L_answer", "no-such-atom", "there is no such file"]
        )
      ,
        ( "serve is not a boolean"
        , registryOf ["L_answer"] (("serve" .= ("yes" :: T.Text)) : scripted "say(1)")
        , ["serve", "Bool"]
        )
      ]
      ( \(desc, registry, fragments) ->
          it ("fails when " ++ desc) $
            withRegistryOf registry $ \path ->
              readRegistry path
                `shouldThrow` (\failure -> all (`isInfixOf` show (failure :: SomeException)) fragments)
      )

    it "fails when the file is not JSON at all" $
      withTemp "phino-atoms-.json" "L_answer: js" $ \path ->
        readRegistry path
          `shouldThrow` (\failure -> "cannot be read" `isInfixOf` show (failure :: SomeException))

    it "fails when a key is not a regular expression" $
      withRegistryOf (registryOf ["L_(answer"] (scripted "say(1)")) $ \path ->
        readRegistry path
          `shouldThrow` (\failure -> all (`isInfixOf` show (failure :: SomeException)) ["L_(answer", "regular expression"])

    it "fails when the file is a JSON array" $
      withTemp "phino-atoms-.json" "[]" $ \path ->
        readRegistry path
          `shouldThrow` (\failure -> all (`isInfixOf` show (failure :: SomeException)) ["cannot be read", "object"])

    it "fails when there is more in the file than the JSON object" $
      withTemp "phino-atoms-.json" "{} {}" $ \path ->
        readRegistry path
          `shouldThrow` (\failure -> all (`isInfixOf` show (failure :: SomeException)) ["cannot be read", "more in the file"])

    it "fails when the file is not there" $
      readRegistry "no-such-registry.json"
        `shouldThrow` (\failure -> "cannot be read" `isInfixOf` show (failure :: SomeException))

    -- A file nobody may run is refused where the registry is read, not where
    -- the atom would fire
    it "fails when the file of an executable λ function cannot be run" $
      withScript "" $ \file ->
        withRegistryOf (registryOf ["L_answer"] (executing file)) $ \path ->
          readRegistry path
            `shouldThrow` (\failure -> "not executable" `isInfixOf` show (failure :: SomeException))

    it "fails when the file to serve from cannot be run" $
      withScript "" $ \file ->
        withRegistryOf (registryOf ["L_answer"] (served (executing file))) $ \path ->
          readRegistry path
            `shouldThrow` (\failure -> "not executable" `isInfixOf` show (failure :: SomeException))

  describe "fireAtom" $ do
    -- Every program is spoken to in the letters of the evaluation rule of the
    -- calculus, 𝔼(𝑏, 𝑒, 𝑠) = 𝑛, one JSON object per line, whether it is
    -- started for the fire or kept for the run
    it "hands back the 𝜑-expression the script wrote under '𝑛'" $
      answers (scripting "'⟦ Δ ⤍ 2A- ⟧'") "⟦ Δ ⤍ 2A- ⟧"

    -- One script may stand for several λ functions, so every request names
    -- the one being fired
    it "names the λ function being fired under 'λ' in the request" $
      answers
        (scripting "request['λ'] === 'L_answer' ? '⟦ Δ ⤍ FF- ⟧' : '⟦ Δ ⤍ 00- ⟧'")
        "⟦ Δ ⤍ FF- ⟧"

    it "carries the formation under '𝑏' in the request and the universe under '𝑒'" $
      answers
        (scripting "request['𝑏'].includes('x ↦') && universe['𝑒'].includes('y ↦') ? '⟦ Δ ⤍ FF- ⟧' : '⟦ Δ ⤍ 00- ⟧'")
        "⟦ Δ ⤍ FF- ⟧"

    it "tells the script the universe before the request" $
      answers
        (scripting "'𝑒' in lines[0] && 'id' in lines[1] ? '⟦ Δ ⤍ FF- ⟧' : '⟦ Δ ⤍ 00- ⟧'")
        "⟦ Δ ⤍ FF- ⟧"

    -- Neither payload carries syntax sugar, whatever '--sweet' says about the
    -- output of the run, so a script finds every datum spelled as a Δ binding
    it "spells the payloads as canonical 𝜑-calculus" $
      answers
        (scripting "request['𝑏'].includes('Δ ⤍ 01-') ? '⟦ Δ ⤍ FF- ⟧' : '⟦ Δ ⤍ 00- ⟧'")
        "⟦ Δ ⤍ FF- ⟧"

    -- A script started for the fire is asked one request, the first, so it
    -- may answer without reading anything at all
    it "reads a script that says nothing to stdin without waiting for it" $
      answers "process.stdout.write(JSON.stringify({id: 1, '𝑛': '⟦ Δ ⤍ 01- ⟧'}))" "⟦ Δ ⤍ 01- ⟧"

    -- The stdin of a script started for the fire closes behind the request,
    -- so a script that reads line by line answers and quits on its own, the
    -- same as it would were it kept for the run
    it "lets a script that reads line by line answer and quit on its own" $
      answers counting "⟦ Δ ⤍ 01- ⟧"

    it "fails with the script's own complaint when it exits non-zero" $
      fails
        "process.stderr.write('no idea what to do');process.exit(4)"
        ["L_answer", "exit code 4", "no idea what to do"]

    -- A script is judged by its exit status even once it has answered, since
    -- an answer it did not stand behind is no answer
    it "fails when the script answers and then exits non-zero" $
      fails
        "process.stdout.write(JSON.stringify({id: 1, '𝑛': '⟦ Δ ⤍ 2A- ⟧'}) + '\\n');process.exit(2)"
        ["L_answer", "exit code 2"]

    it "fails when the script writes something other than JSON" $
      fails "process.stdout.write('almost')" ["L_answer", "almost"]

    it "fails when the script writes JSON with no '𝑛' in it" $
      fails "process.stdout.write(JSON.stringify({id: 1, m: '⟦ ⟧'}))" ["L_answer", "𝑛"]

    it "fails when the script answers another request" $
      fails "process.stdout.write(JSON.stringify({id: 7, '𝑛': '⟦ Δ ⤍ 2A- ⟧'}))" ["L_answer", "request 7"]

    it "fails when what the script put under '𝑛' is not a 𝜑-expression" $
      fails "process.stdout.write(JSON.stringify({id: 1, '𝑛': '⟦ ⟧⟧'}))" ["L_answer"]

    -- An executable atom is spawned as it is, under no interpreter, so phino
    -- stages nothing of it and the file speaks the same protocol a script does
    it "runs an executable λ function straight off its path" $
      executes "echo '{\"id\": 1, \"𝑛\": \"⟦ Δ ⤍ 2A- ⟧\"}'" "⟦ Δ ⤍ 2A- ⟧"

    it "speaks the same lines to an executable as to a script" $
      executes
        "case \"$(cat)\" in *'\"λ\":\"L_answer\"'*) echo '{\"id\": 1, \"𝑛\": \"⟦ Δ ⤍ FF- ⟧\"}';; *) echo '{\"id\": 1, \"𝑛\": \"⟦ Δ ⤍ 00- ⟧\"}';; esac"
        "⟦ Δ ⤍ FF- ⟧"

    -- A program kept for the run is asked over the streams of one process,
    -- whatever runs it, so a script that counts its requests sees them all
    it "keeps a script that serves across the fires" $
      withNode $
        withRegistered (registryOf ["L_answer"] (served (scripted counting))) $ \registry -> do
          _ <- firedFrom registry "L_answer" "⟦ y ↦ ⟦ Δ ⤍ 02- ⟧ ⟧"
          second <- firedFrom registry "L_answer" "⟦ y ↦ ⟦ Δ ⤍ 02- ⟧ ⟧"
          wanted <- parseExpressionThrows "⟦ Δ ⤍ 02- ⟧"
          second `shouldBe` wanted

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

    -- One key matching many names is the way to have one program serve them
    -- all without spelling it once per name
    it "serves every λ name one key matches from one program" $
      withShell $
        withServed [".*"] (replying "0$n-") $ \registry -> do
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

    it "fails when the resident program answers another request" $
      refuses "printf '{\"id\": 99, \"𝑛\": \"⟦ Δ ⤍ 2A- ⟧\"}\\n'" ["L_answer", "request 99"]

    it "fails with the resident program's own complaint when it quits non-zero" $
      refuses "echo 'no idea what to do' >&2; exit 4" ["L_answer", "exit code 4", "no idea what to do"]

    it "fails when the resident program quits without answering" $
      refuses "exit 0" ["L_answer", "without answering"]

    it "fails when the resident program writes something other than JSON" $
      refuses "echo almost" ["L_answer", "almost"]

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
