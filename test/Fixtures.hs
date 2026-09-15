{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The λ functions the specs fire. phino implements none of them, so a spec that
-- needs one to answer brings its own: the fixture registry of
-- 'test-resources/functions/primitives.yaml', which spells them in the very
-- rule language '--functions' reads, or a registry of its own written for the
-- occasion.
module Fixtures
  ( defaultReduceContext
  , fixtureFunctions
  , functionsFile
  , loopingFunctions
  , primitives
  , recorded
  , withFunctions
  , withFunctionsOf
  , withTemp
  )
where

import AST (Expression)
import CLI.Helpers (withEvalFunc)
import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Dataize (reduction)
import Deps (SaveEvalFunc, dontSaveEval, dontSaveStep)
import Functions (buildTerm)
import Lambdas (Lambdas, emptyLambdas, readLambdas)
import Morph (ReduceContext (..), Steps (..))
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.IO (Handle, hClose, openBinaryTempFile)

-- The context every reduction of a spec starts from. Shuffle is enabled so the
-- suite exercises the order-independence of the morphing and dataization rules
-- (#909): a hidden overlap surfaces as a nondeterministic failure instead of
-- staying silently green. No λ function is registered, since phino implements
-- none of them: a case that needs one to answer brings the fixture registry in
-- through 'withFunctions'.
defaultReduceContext :: Expression -> ReduceContext
defaultReduceContext loc = ReduceContext loc 25 25 (Steps 250 0) False True False False emptyLambdas buildTerm reduction dontSaveStep dontSaveEval

-- The same context with the given λ functions registered
withFunctions :: Lambdas -> ReduceContext -> ReduceContext
withFunctions functions ctx = ctx{_functions = functions}

-- The file '--functions' reads in every case that fires one of the fixture λ
-- functions, for the specs that go through the command line.
functionsFile :: FilePath
functionsFile = "test-resources/functions/primitives.yaml"

-- The same λ functions, read once, for the specs that drive 𝕄 and 𝔻 directly.
fixtureFunctions :: IO Lambdas
fixtureFunctions = readLambdas functionsFile

-- The one λ function that answers with a firing of itself, so that a run
-- fires it until the step budget is gone. Recursion is nothing phino prevents
-- — that is the object model's business — so a program built on this one is
-- how the specs reach the '--max-steps' limit.
loopingFunctions :: (FilePath -> IO a) -> IO a
loopingFunctions = withFunctionsOf "- λ: L_loop\n  𝑛: ⟦ λ ⤍ L_loop ⟧\n"

-- The given λ functions, as the YAML file '--functions' reads, in a temporary
-- file removed afterwards.
withFunctionsOf :: T.Text -> (FilePath -> IO a) -> IO a
withFunctionsOf functions = withTemp "phino-functions-.yaml" (encodeUtf8 functions)

-- The EO objects the fixture λ functions answer for, declared the way
-- 'number.eo' and 'bytes.eo' declare them, so a case only has to spell the
-- expression under φ. 'number.eq' is the one operation with no λ function of
-- its own: EO spells it out of 'L_bytes_eq' (eq.eo), so the fixture composes it
-- the same way. Alongside them stand the objects the λ functions hand results
-- to: 'string' carries what a byte-array complaint would say, while 'true' and
-- 'false' fill in for the real bool objects, since the single byte an EO bool
-- dataizes to is all these cases assert. Those bytes are EO's own: 'true.eo'
-- asserts 'true.as-bytes.eq FF-' and 'bool.eo' branches 'if' over 'FF-' and
-- '00-', so a universe copied from here starts with a bool an EO program
-- recognizes. 'number.nope' is declared and left out of the file on
-- purpose: it is the λ function that cannot fire, the one '--partial' parks on.
primitives :: String -> String
primitives src =
  unlines
    [ "[["
    , "  bytes -> [["
    , "    φ -> ?,"
    , "    not -> [[ L> L_bytes_not ]],"
    , "    eq -> [[ b -> ?, L> L_bytes_eq ]]"
    , "  ]],"
    , "  number -> [["
    , "    φ -> ?,"
    , "    as-bytes -> $.φ,"
    , "    plus -> [[ x -> ?, L> L_number_plus ]],"
    , "    eq -> [[ x -> ?, @ -> $.^.as-bytes.eq( x.as-bytes ) ]],"
    , "    nope -> [[ L> L_number_nope ]]"
    , "  ]],"
    , "  string -> [[ φ -> ?, as-bytes -> $.φ ]],"
    , "  true -> [[ @ -> [[ D> FF- ]] ]],"
    , "  false -> [[ @ -> [[ D> 00- ]] ]],"
    , "  @ -> " ++ src
    , "]]"
    ]

-- Run the action with the function '--evaluations' records λ function firings
-- through, handing back what it wrote alongside the answer, one record per
-- line. The protocol goes through the very plumbing the option runs, so what a
-- case asserts is what a user of it reads back.
recorded :: (SaveEvalFunc -> IO a) -> IO (a, [String])
recorded action =
  withTemp "phino-protocol-.json" BS.empty $ \path -> do
    answer <- withEvalFunc (Just path) action
    written <- BS.readFile path
    pure (answer, lines (T.unpack (decodeUtf8 written)))

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
