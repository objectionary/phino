{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- Which λ functions exist is a property of the object model being dataized,
-- not of the calculus. phino therefore implements none of them: it reads a
-- registry of them from a JSON file given with '--atoms' and fires each one as
-- a POSIX process. The registry maps a λ name to the runtime that runs it and
-- the script it runs:
--
-- > {
-- >   "L_bytes_eq": {
-- >     "rt": "js",
-- >     "script": "const fs = require('fs'); ..."
-- >   }
-- > }
--
-- A name absent from the registry has no λ function at all: 𝔼 gets stuck on
-- it, exactly as it does for a name no one ever declared (see 'Stuck' in
-- 'Dataize').
module Atoms
  ( Atom (..)
  , AtomException (..)
  , Registry
  , Runtime (..)
  , emptyRegistry
  , fireAtom
  , readRegistry
  , registeredAtom
  , runtimeNames
  )
where

import AST
import Control.Exception (Exception, bracket, catch, throwIO)
import Control.Monad (unless)
import Data.Aeson (FromJSON (parseJSON), eitherDecodeStrict', object, withObject, withText, (.:), (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (find, intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import Encoding (Encoding (UNICODE))
import Lining (LineFormat (SINGLELINE))
import Logger (logDebug)
import Margin (defaultMargin)
import Parser (parseExpression)
import Printer (printExpression')
import Sugar (SugarType (SALTY))
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (Handle, IOMode (ReadMode, WriteMode), hClose, hSetBinaryMode, openBinaryTempFile, withBinaryFile)
import System.Process (CreateProcess (std_err, std_in, std_out), ProcessHandle, StdStream (CreatePipe, UseHandle), createProcess, proc, waitForProcess)
import Text.Printf (printf)

-- The interpreter a script is written for. Only JavaScript for now, meaning
-- 'node'. A registry naming any other runtime is rejected when it is read,
-- before dataization starts, so a run never gets half-way through a program to
-- discover that one of its atoms cannot be run at all.
data Runtime = RtJs
  deriving stock (Eq, Show)

-- One entry of the registry: the runtime and the source of the script.
data Atom = Atom
  { _runtime :: Runtime
  , _script :: T.Text
  }
  deriving stock (Eq, Show)

-- Every λ function phino may fire, keyed by name.
type Registry = Map T.Text Atom

data AtomException
  = -- The '--atoms' file is not a JSON registry of λ functions.
    BrokenRegistry FilePath String
  | -- The interpreter of a runtime is not installed, so no script of it can run.
    NoRuntime T.Text String String
  | -- The script exited with a non-zero status; the message carries its stderr.
    AtomBroke T.Text Int String
  | -- The script exited successfully but said nothing phino can use: its stdout
    -- is not a JSON object, carries no 'n' field, or the 𝜑-expression under it
    -- does not parse.
    AtomMute T.Text String String
  deriving anyclass (Exception)

instance Show AtomException where
  show (BrokenRegistry file failure) = printf "The registry of atoms '%s' cannot be read: %s" file failure
  show (NoRuntime func runner failure) =
    printf "Atom '%s' cannot be fired, '%s' is not runnable: %s" (T.unpack func) runner failure
  show (AtomBroke func status complaint) =
    printf "Atom '%s' failed with exit code %d: %s" (T.unpack func) status complaint
  show (AtomMute func answer failure) =
    printf "Atom '%s' returned '%s', which phino cannot use: %s" (T.unpack func) answer failure

-- The name a registry spells a runtime with.
runtimeName :: Runtime -> String
runtimeName RtJs = "js"

-- The POSIX executable the scripts of a runtime are run under.
interpreter :: Runtime -> String
interpreter RtJs = "node"

-- The extension the script of a runtime is written to disk with, so the
-- interpreter recognizes the file for what it is.
extension :: Runtime -> String
extension RtJs = "js"

-- Every runtime phino can run, in the order the '--atoms' help lists them.
runtimes :: [Runtime]
runtimes = [RtJs]

runtimeNames :: [String]
runtimeNames = map runtimeName runtimes

instance FromJSON Runtime where
  parseJSON = withText "runtime" $ \name -> case find ((== T.unpack name) . runtimeName) runtimes of
    Just runtime -> pure runtime
    Nothing -> fail (printf "unknown runtime '%s', expected one of: %s" (T.unpack name) (intercalate ", " runtimeNames))

instance FromJSON Atom where
  parseJSON = withObject "atom" $ \entry -> Atom <$> entry .: "rt" <*> entry .: "script"

-- What the script writes to stdout: one JSON object whose 'n' field is the
-- 𝜑-expression the atom answers with.
newtype Answer = Answer T.Text

instance FromJSON Answer where
  parseJSON = withObject "answer" $ \answer -> Answer <$> answer .: "n"

-- No λ function at all: every atom gets stuck. This is what a run without
-- '--atoms' fires against.
emptyRegistry :: Registry
emptyRegistry = Map.empty

-- The λ function registered under this name, if any.
registeredAtom :: Registry -> T.Text -> Maybe Atom
registeredAtom registry func = Map.lookup func registry

-- Read the registry of λ functions from a JSON file. An unknown runtime, a
-- missing 'script' or malformed JSON fails here, before any dataization
-- starts.
readRegistry :: FilePath -> IO Registry
readRegistry path = do
  content <- BS.readFile path `catch` unreadable
  case eitherDecodeStrict' content of
    Left failure -> throwIO (BrokenRegistry path failure)
    Right registry -> do
      logDebug (printf "Loaded %d atom(s) from '%s'" (Map.size registry) path)
      pure registry
  where
    unreadable :: IOError -> IO BS.ByteString
    unreadable failure = throwIO (BrokenRegistry path (show failure))

-- Fire the λ function 'func' by running its script as a POSIX process under
-- the interpreter of its runtime, with the λ name as the first command-line
-- argument — one script may be registered under several names and branch on
-- it. The script is fed a JSON object on stdin (see 'payload') and answers
-- with one on stdout; the 𝜑-expression under 'n' becomes the atom's raw
-- result, which 𝔼 normalizes exactly as it normalized the answer of a built-in
-- one. A non-zero exit, unparsable output or a missing 'n' fails the run.
fireAtom :: T.Text -> Atom -> Expression -> Expression -> IO Expression
fireAtom func Atom{..} form univ =
  withTemp (printf "phino-atom-.%s" (extension _runtime)) (encodeUtf8 _script) $ \script ->
    withTemp "phino-atom-.json" (payload form univ) $ \input ->
      withTemp "phino-atom-.err" "" $ \errors -> do
        logDebug (printf "Firing atom '%s' as '%s %s %s'" (T.unpack func) (interpreter _runtime) script (T.unpack func))
        (status, answer) <- executed script input errors
        complaint <- readErrors errors
        unless (null complaint) (logDebug (printf "Atom '%s' wrote to stderr: %s" (T.unpack func) complaint))
        case status of
          ExitFailure code -> throwIO (AtomBroke func code complaint)
          ExitSuccess -> answered answer
  where
    -- Run the interpreter with stdin and stderr wired to files, so only stdout
    -- is a pipe and neither side can ever block waiting for the other to drain
    -- one. Both streams are bytes: a 𝜑 expression carries characters no
    -- single-byte locale can spell, so nothing here is left to the locale.
    executed :: FilePath -> FilePath -> FilePath -> IO (ExitCode, BS.ByteString)
    executed script input errors =
      withBinaryFile input ReadMode $ \stdin' ->
        withBinaryFile errors WriteMode $ \stderr' -> do
          (stdout', process) <- spawned script stdin' stderr'
          hSetBinaryMode stdout' True
          answer <- BS.hGetContents stdout'
          status <- waitForProcess process
          pure (status, answer)
    spawned :: FilePath -> Handle -> Handle -> IO (Handle, ProcessHandle)
    spawned script stdin' stderr' = do
      spawn <- createProcess started `catch` missing
      case spawn of
        (_, Just stdout', _, process) -> pure (stdout', process)
        _ -> throwIO (AtomMute func "" "the interpreter gave phino no stdout to read")
      where
        started :: CreateProcess
        started =
          (proc (interpreter _runtime) [script, T.unpack func])
            { std_in = UseHandle stdin'
            , std_out = CreatePipe
            , std_err = UseHandle stderr'
            }
    missing :: IOError -> IO a
    missing failure = throwIO (NoRuntime func (interpreter _runtime) (show failure))
    -- Whatever the script complained about, decoded leniently: the stream is
    -- the script's, so it may hold anything at all.
    readErrors :: FilePath -> IO String
    readErrors errors = T.unpack . T.strip . decodeUtf8Lenient <$> BS.readFile errors
    -- Parse what the script said: a JSON object with the raw 𝜑-expression
    -- under 'n'.
    answered :: BS.ByteString -> IO Expression
    answered answer = case eitherDecodeStrict' answer of
      Left failure -> throwIO (AtomMute func spoken failure)
      Right (Answer raw) -> case parseExpression (T.unpack raw) of
        Left failure -> throwIO (AtomMute func (T.unpack raw) failure)
        Right expr -> pure expr
      where
        spoken :: String
        spoken = T.unpack (T.strip (decodeUtf8Lenient answer))

-- The JSON phino feeds a script on stdin: the formation being evaluated under
-- 'b', with its λ binding removed so the script may dispatch on it, and the
-- universe Φ under 's'. Both are rendered as canonical 𝜑-calculus on a single
-- line — no syntax sugar, whatever '--sweet' says about the output of the run —
-- so a script never has to know phino's sugar to find a datum: every byte array
-- it may need is spelled out as a Δ binding. The text is what phino's own parser
-- reads back, so a script may hand any part of it to another phino run (see the
-- '--inside' option).
payload :: Expression -> Expression -> BS.ByteString
payload form univ = BSL.toStrict (A.encode (object ["b" .= rendered form, "s" .= rendered univ]))
  where
    rendered :: Expression -> T.Text
    rendered expr = T.pack (printExpression' expr (SALTY, UNICODE, SINGLELINE, defaultMargin))

-- Write the content to a fresh temporary file, hand its path to the action and
-- delete the file afterwards, whatever the action does.
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
