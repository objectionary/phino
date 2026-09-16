{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- The λ functions the specs fire. phino implements none of them, so a spec that
-- needs one to answer brings its own: the fixture file
-- 'test-resources/atoms.yaml', which spells them in the very rule language
-- '--symbolic' reads, or a file of its own written for the occasion.
module Fixtures
  ( defaultReduceContext
  , fixtureLambdas
  , lambdasFile
  , loopingLambdas
  , primitives
  , recorded
  , withLambdas
  , withLambdasOf
  , withTemp
  )
where

import AST (Expression (ExRoot))
import CLI.Helpers (withEvalFunc)
import CLI.Types (IOFormat (PHI), PrintContext (PrintCtx))
import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Dataize (reduction)
import Deps (SaveEvalFunc, dontSaveEval, dontSaveStep)
import Functions (buildTerm)
import Lambdas (Lambdas, emptyLambdas, readLambdas)
import Lining (LineFormat (MULTILINE))
import Morph (ReduceContext (..), Steps (..))
import Sugar (SugarType (SWEET))
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.IO (Handle, hClose, openBinaryTempFile)
import XMIR (defaultXmirContext)

-- The context every reduction of a spec starts from. Shuffle is enabled so the
-- suite exercises the order-independence of the morphing and dataization rules
-- (#909): a hidden overlap surfaces as a nondeterministic failure instead of
-- staying silently green. No λ function is registered, since phino implements
-- none of them: a case that needs one to answer brings the fixture file in
-- through 'withLambdas'.
defaultReduceContext :: Expression -> ReduceContext
defaultReduceContext loc = ReduceContext loc 25 25 (Steps 250 0) 1 False True False False emptyLambdas buildTerm reduction dontSaveStep dontSaveEval

-- The same context with the given λ functions registered
withLambdas :: Lambdas -> ReduceContext -> ReduceContext
withLambdas lambdas ctx = ctx{_symbolic = lambdas}

-- The file '--symbolic' reads in every case that fires one of the fixture λ
-- functions, for the specs that go through the command line.
lambdasFile :: FilePath
lambdasFile = "test-resources/atoms.yaml"

-- The same λ functions, read once, for the specs that drive 𝕄 and 𝔻 directly.
fixtureLambdas :: IO Lambdas
fixtureLambdas = readLambdas lambdasFile

-- The one λ function that answers with a firing of itself, so that a run fires
-- it until the step budget is gone. Recursion is nothing phino prevents — that
-- is the object model's business — so a program built on this one is how the
-- specs reach the '--max-steps' limit.
loopingLambdas :: (FilePath -> IO a) -> IO a
loopingLambdas = withLambdasOf "- λ: L_loop\n  𝑛: ⟦ λ ⤍ L_loop ⟧\n"

-- The given λ functions, as the YAML file '--symbolic' reads, in a temporary
-- file removed afterwards.
withLambdasOf :: T.Text -> (FilePath -> IO a) -> IO a
withLambdasOf lambdas = withTemp "phino-symbolic-.yaml" (encodeUtf8 lambdas)

-- The EO objects the fixture λ functions answer for, declared the way
-- 'number.eo', 'bytes.eo' and 'bool.eo' declare them, so a case only has to
-- spell the expression under φ. 'number.eq' is the one operation with no λ
-- function of its own: EO spells it out of 'L_bytes_eq' (eq.eo), so the fixture
-- composes it the same way, and 'bool.if' is where a branch meets the symbol
-- its condition came down to. 'number.nope' is declared and left out of the
-- file on purpose: it is the λ function that cannot fire, the one '--partial'
-- parks on.
primitives :: String -> String
primitives src =
  unlines
    [ "[["
    , "  bytes -> [["
    , "    φ -> ?,"
    , "    not -> [[ L> L_bytes_not ]],"
    , "    eq -> [[ b -> ?, L> L_bytes_eq ]]"
    , "  ]],"
    , "  bool -> [["
    , "    φ -> ?,"
    , "    if -> [[ then -> ?, else -> ?, L> L_fork ]]"
    , "  ]],"
    , "  number -> [["
    , "    φ -> ?,"
    , "    as-bytes -> $.φ,"
    , "    plus -> [[ x -> ?, L> L_number_plus ]],"
    , "    times -> [[ x -> ?, L> L_number_times ]],"
    , "    div -> [[ x -> ?, L> L_number_div ]],"
    , "    gt -> [[ x -> ?, L> L_number_gt ]],"
    , "    eq -> [[ x -> ?, @ -> $.^.as-bytes.eq( x.as-bytes ) ]],"
    , "    nope -> [[ L> L_number_nope ]]"
    , "  ]],"
    , "  @ -> " ++ src
    , "]]"
    ]

-- Run the action with the function '--protocol' writes the run through, handing
-- back what it wrote alongside the answer, one line per record. The protocol
-- goes through the very plumbing the option runs, so what a case asserts is
-- what a user of it reads back.
recorded :: (SaveEvalFunc -> IO a) -> IO (a, [String])
recorded action =
  withTemp "phino-protocol-.txt" BS.empty $ \path -> do
    answer <- withEvalFunc (Just path) printing action
    written <- BS.readFile path
    pure (answer, lines (T.unpack (decodeUtf8 written)))
  where
    -- The protocol flattens every term itself, so the only thing this context
    -- decides is that the terms are 𝜑 and not XMIR.
    printing :: PrintContext
    printing =
      PrintCtx
        SWEET
        False
        MULTILINE
        2
        defaultXmirContext
        False
        False
        False
        False
        False
        1
        1
        ExRoot
        Nothing
        Nothing
        Nothing
        PHI

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
