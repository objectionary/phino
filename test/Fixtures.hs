{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The λ functions the specs fire. phino implements none of them, so a spec that
-- needs an atom to answer brings its own: one JavaScript fixture,
-- 'test-resources/atoms/primitives.js', registered under every name in
-- 'fixtureAtoms' and branching on the one each request names under 'λ',
-- another, 'test-resources/atoms/asking.js', which reduces nothing itself and
-- asks phino for its operands, a third, 'test-resources/atoms/asking-loops.js',
-- which asks a question that never stops cycling, or a POSIX shell script
-- written for the occasion, either run once per fire or kept resident for the
-- run.
module Fixtures
  ( fixtureAtoms
  , fixtureRegistry
  , resident
  , withAskingRegistry
  , withExecutable
  , withFixtureRegistry
  , withLoopingAskRegistry
  , withNode
  , withRegistryOf
  , withScript
  , withServing
  , withShell
  , withTemp
  )
where

import Atoms (Registry, readRegistry)
import Control.Exception (bracket)
import Data.Aeson (Value, encode, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Maybe (isNothing)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Directory (findExecutable, getPermissions, getTemporaryDirectory, removePathForcibly, setOwnerExecutable, setPermissions)
import System.IO (Handle, hClose, openBinaryTempFile)
import System.Info (os)
import Test.Hspec (Expectation, pendingWith)

-- Every λ function the fixture answers for. A name outside this list is
-- unregistered, which is how a spec asks for an atom that cannot fire.
fixtureAtoms :: [T.Text]
fixtureAtoms =
  [ "L_number_plus"
  , "L_number_times"
  , "L_number_div"
  , "L_number_gt"
  , "L_bytes_eq"
  , "L_bytes_not"
  ]

-- One of the fixture scripts, read as UTF-8 rather than through the locale,
-- since they spell 𝜑 expressions.
fixtureScript :: FilePath -> IO T.Text
fixtureScript name = decodeUtf8 <$> BS.readFile ("test-resources/atoms/" ++ name)

-- The registry the specs that drive 'Dataize' directly run against: the same
-- file '--atoms' reads, read once and gone.
fixtureRegistry :: IO Registry
fixtureRegistry = withFixtureRegistry readRegistry

-- The same registry as the JSON file '--atoms' reads, in a temporary file
-- removed afterwards, for the specs that go through the command line.
withFixtureRegistry :: (FilePath -> IO a) -> IO a
withFixtureRegistry action = do
  script <- fixtureScript "primitives.js"
  withRegistryOf (object [Key.fromText name .= entry script | name <- fixtureAtoms]) action
  where
    entry :: T.Text -> Value
    entry script = object ["rt" .= ("node" :: T.Text), "script" .= script]

-- The registry of the one λ function the asking fixture answers,
-- 'L_number_plus', kept for the run, as the JSON file '--atoms' reads: a
-- program may ask phino to reduce an operand only while its stdin is open, and
-- phino closes the stdin of a program started for the fire behind its request.
withAskingRegistry :: (FilePath -> IO a) -> IO a
withAskingRegistry action = do
  script <- fixtureScript "asking.js"
  withRegistryOf (object ["L_number_plus" .= entry script]) action
  where
    entry :: T.Text -> Value
    entry script = object ["rt" .= ("node" :: T.Text), "script" .= script, "serve" .= True]

-- The registry of the resident program that asks about a term the universe
-- never finishes reducing: under '--partial' phino must park the looping
-- question and hand the residual back rather than fail the run (#1078)
withLoopingAskRegistry :: (FilePath -> IO a) -> IO a
withLoopingAskRegistry action = do
  script <- fixtureScript "asking-loops.js"
  withRegistryOf (object ["L_number_gt" .= entry script]) action
  where
    entry :: T.Text -> Value
    entry script = object ["rt" .= ("node" :: T.Text), "script" .= script, "serve" .= True]

-- The given JSON, as the registry file '--atoms' reads, in a temporary file
-- removed afterwards.
withRegistryOf :: Value -> (FilePath -> IO a) -> IO a
withRegistryOf registry = withTemp "phino-atoms-.json" (BSL.toStrict (encode registry))

-- Every atom the fixture provides runs under 'node', so a machine without it
-- cannot fire one at all: such an expectation is pending rather than red.
withNode :: Expectation -> Expectation
withNode expectation = do
  node <- findExecutable "node"
  if isNothing node
    then pendingWith "'node' is not installed, so no λ function can be fired"
    else expectation

-- A POSIX shell script is executable nowhere on Windows, so a case that needs
-- one is pending there rather than red.
withShell :: Expectation -> Expectation
withShell expectation
  | os == "mingw32" = pendingWith "no POSIX shell script is executable on Windows"
  | otherwise = expectation

-- A file in the temporary directory holding the given POSIX shell script,
-- removed afterwards.
withScript :: T.Text -> (FilePath -> IO a) -> IO a
withScript script = withTemp "phino-exec-.sh" (encodeUtf8 (T.unlines ["#!/bin/sh", script]))

-- The same file, executable, which is what an 'exec' or a 'serve' atom names
-- and phino never stages itself.
withExecutable :: T.Text -> (FilePath -> IO a) -> IO a
withExecutable script action = withScript script $ \path -> do
  permissions <- getPermissions path
  setPermissions path (setOwnerExecutable True permissions)
  action path

-- The registry of one λ function, 'L_answer', kept for the run, as the JSON
-- file '--atoms' reads, together with the resident program it names: a POSIX
-- shell script built of the given per-request snippet (see 'resident'). Both
-- files are removed afterwards.
withServing :: T.Text -> (FilePath -> IO a) -> IO a
withServing snippet action =
  withExecutable (resident snippet) $ \program ->
    withRegistryOf (object ["L_answer" .= object ["rt" .= ("exec" :: T.Text), "path" .= program, "serve" .= True]]) action

-- A program as a POSIX shell script that reads phino's lines until its stdin
-- closes, so it serves started once per fire and kept for the run alike: it
-- counts the universes it is told in 'e' and runs the given snippet for every
-- request, with the request in 'line', its number in 'id' and how many
-- requests it has seen so far in 'n'.
resident :: T.Text -> T.Text
resident snippet =
  T.unlines
    [ "e=0"
    , "n=0"
    , "while IFS= read -r line; do"
    , "  case \"$line\" in"
    , "    *'\"𝑒\"'*) e=$((e+1));;"
    , "    *) n=$((n+1)); id=$(printf '%s' \"$line\" | sed 's/.*\"id\":\\([0-9]*\\).*/\\1/'); " <> snippet <> ";;"
    , "  esac"
    , "done"
    ]

-- Write the content to a fresh temporary file, hand its path to the action and
-- delete the file afterwards.
withTemp :: String -> BS.ByteString -> (FilePath -> IO a) -> IO a
withTemp template content action = do
  dir <- getTemporaryDirectory
  bracket (openBinaryTempFile dir template) discarded $ \(path, handle) -> do
    BS.hPut handle content
    hClose handle
    action path
  where
    discarded :: (FilePath, Handle) -> IO ()
    discarded (path, handle) = hClose handle >> removePathForcibly path
