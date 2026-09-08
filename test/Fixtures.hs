{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The λ functions the specs fire. phino implements none of them, so a spec that
-- needs an atom to answer brings its own: one JavaScript fixture,
-- 'test-resources/atoms/primitives.js', registered under every name in
-- 'fixtureAtoms' and branching on the one it is handed as its first
-- command-line argument.
module Fixtures (fixtureAtoms, fixtureRegistry, withFixtureRegistry, withNode) where

import Atoms (Atom (..), Registry, Runtime (RtJs))
import Control.Exception (bracket)
import Data.Aeson (encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import System.Directory (findExecutable, getTemporaryDirectory, removePathForcibly)
import System.IO (Handle, hClose, openBinaryTempFile)
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

-- The fixture script itself, read as UTF-8 rather than through the locale,
-- since it spells 𝜑 expressions.
fixtureScript :: IO T.Text
fixtureScript = decodeUtf8 <$> BS.readFile "test-resources/atoms/primitives.js"

-- The registry the specs that drive 'Dataize' directly run against.
fixtureRegistry :: IO Registry
fixtureRegistry = do
  script <- fixtureScript
  pure (Map.fromList [(name, Atom RtJs script) | name <- fixtureAtoms])

-- The same registry as the JSON file '--atoms' reads, in a temporary file
-- removed afterwards, for the specs that go through the command line.
withFixtureRegistry :: (FilePath -> IO a) -> IO a
withFixtureRegistry action = do
  script <- fixtureScript
  dir <- getTemporaryDirectory
  bracket (openBinaryTempFile dir "phino-atoms-.json") discarded $ \(path, handle) -> do
    BSL.hPut handle (encode (object [Key.fromText name .= entry script | name <- fixtureAtoms]))
    hClose handle
    action path
  where
    entry script = object ["rt" .= ("js" :: T.Text), "script" .= script]
    discarded :: (FilePath, Handle) -> IO ()
    discarded (path, handle) = hClose handle >> removePathForcibly path

-- Every atom the fixture provides runs under 'node', so a machine without it
-- cannot fire one at all: such an expectation is pending rather than red.
withNode :: Expectation -> Expectation
withNode expectation = do
  node <- findExecutable "node"
  if isNothing node
    then pendingWith "'node' is not installed, so no λ function can be fired"
    else expectation
