{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- Which λ functions exist is a property of the object model being dataized,
-- not of the calculus. phino therefore implements none of them: it reads a
-- registry of them from a JSON file given with '--atoms' and fires each one as
-- a POSIX process. The registry maps a λ name either to the runtime that runs
-- its script or to 'exec' and the path of a file that runs on its own:
--
-- > {
-- >   "L_bytes_eq": {
-- >     "rt": "node",
-- >     "script": "const fs = require('fs'); ..."
-- >   },
-- >   "L_number_plus": {
-- >     "rt": "exec",
-- >     "path": "/opt/eo/atoms/number-plus"
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
import System.Directory (doesFileExist, executable, getPermissions, getTemporaryDirectory, removePathForcibly)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (Handle, IOMode (WriteMode), hClose, hSetBinaryMode, openBinaryTempFile, withBinaryFile)
import System.Process (CreateProcess (std_err, std_in, std_out), ProcessHandle, StdStream (CreatePipe, UseHandle), createProcess, proc, waitForProcess)
import Text.Printf (printf)

-- The interpreter a script is run under, named after the executable itself:
-- only 'node' for now. A registry naming any other runtime is rejected when it
-- is read, before dataization starts, so a run never gets half-way through a
-- program to discover that one of its atoms cannot be run at all.
data Runtime = RtNode
  deriving stock (Eq, Show)

-- One entry of the registry: the λ function phino runs as a POSIX process.
-- Either a script, which phino stages in a temporary file and hands to the
-- interpreter of its runtime, or an executable file, which phino runs as it
-- is, since the object model brought its own binary and there is nothing to
-- stage.
data Atom
  = Scripted Runtime T.Text
  | Executable FilePath
  deriving stock (Eq, Show)

-- Every λ function phino may fire, keyed by name.
type Registry = Map T.Text Atom

data AtomException
  = -- The '--atoms' file is not a JSON registry of λ functions.
    BrokenRegistry FilePath String
  | -- The program of an atom cannot be run: the interpreter of its runtime is
    -- not installed, or its executable file is missing or not executable.
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
runtimeName RtNode = "node"

-- The POSIX executable the scripts of a runtime are run under.
interpreter :: Runtime -> String
interpreter RtNode = "node"

-- The extension the script of a runtime is written to disk with, so the
-- interpreter recognizes the file for what it is. This is the language, not the
-- runtime: 'node' loads a file only if it is named '.js'.
extension :: Runtime -> String
extension RtNode = "js"

-- Every runtime phino can run, in the order the '--atoms' help lists them.
runtimes :: [Runtime]
runtimes = [RtNode]

-- The 'rt' of an atom that is a file rather than a script: it names no
-- interpreter, because the file runs on its own.
execName :: String
execName = "exec"

-- Every name the 'rt' field of a registry entry may take.
runtimeNames :: [String]
runtimeNames = map runtimeName runtimes ++ [execName]

instance FromJSON Runtime where
  parseJSON = withText "runtime" $ \name -> case find ((== T.unpack name) . runtimeName) runtimes of
    Just runtime -> pure runtime
    Nothing -> fail (printf "unknown runtime '%s', expected one of: %s" (T.unpack name) (intercalate ", " runtimeNames))

instance FromJSON Atom where
  parseJSON = withObject "atom" $ \entry -> do
    named <- entry .: "rt"
    if named == T.pack execName
      then Executable . T.unpack <$> entry .: "path"
      else Scripted <$> parseJSON (A.String named) <*> entry .: "script"

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
-- missing 'script', a 'path' that names no executable file or malformed JSON
-- fails here, before any dataization starts.
readRegistry :: FilePath -> IO Registry
readRegistry path = do
  content <- BS.readFile path `catch` unreadable
  case eitherDecodeStrict' content of
    Left failure -> throwIO (BrokenRegistry path failure)
    Right registry -> do
      mapM_ (uncurry runnable) (Map.toList registry)
      logDebug (printf "Loaded %d atom(s) from '%s'" (Map.size registry) path)
      pure registry
  where
    unreadable :: IOError -> IO BS.ByteString
    unreadable failure = throwIO (BrokenRegistry path (show failure))
    -- The file of an executable atom is the only thing phino knows about it,
    -- and it staged none of it, so the file is looked at here, while the
    -- registry is being read, rather than half-way through a program that
    -- turns out to name that atom.
    runnable :: T.Text -> Atom -> IO ()
    runnable _ (Scripted _ _) = pure ()
    runnable func (Executable file) = do
      there <- doesFileExist file
      unless there (throwIO (NoRuntime func file "there is no such file"))
      allowed <- executable <$> getPermissions file
      unless allowed (throwIO (NoRuntime func file "the file is not executable"))

-- Fire the λ function 'func' by running its program as a POSIX process, with
-- the λ name as its last command-line argument — one program may be registered
-- under several names and branch on it. A scripted atom is staged in a
-- temporary file and handed to the interpreter of its runtime; an executable
-- one is run straight off its path, under no interpreter at all. The program
-- is fed a JSON object on stdin (see 'payload') and answers with one on
-- stdout; the 𝜑-expression under 'n' becomes the atom's raw result, which 𝔼
-- normalizes exactly as it normalized the answer of a built-in one. A non-zero
-- exit, unparsable output or a missing 'n' fails the run.
fireAtom :: T.Text -> Atom -> Expression -> Expression -> IO Expression
fireAtom func atom form univ = commanded atom $ \program arguments ->
  withTemp "phino-atom-.err" "" $ \errors -> do
    logDebug (printf "Firing atom '%s' as '%s %s'" (T.unpack func) program (unwords arguments))
    (status, answer) <- executed program arguments errors
    complaint <- readErrors errors
    unless (null complaint) (logDebug (printf "Atom '%s' wrote to stderr: %s" (T.unpack func) complaint))
    case status of
      ExitFailure code -> throwIO (AtomBroke func code complaint)
      ExitSuccess -> answered answer
  where
    -- What to spawn and what to hand it: the interpreter of the runtime, with
    -- the script staged in a temporary file that outlives nothing but the
    -- action, or the executable file itself, which phino only points at.
    commanded :: Atom -> (String -> [String] -> IO a) -> IO a
    commanded (Scripted runtime script) action =
      withTemp (printf "phino-atom-.%s" (extension runtime)) (encodeUtf8 script) $ \staged ->
        action (interpreter runtime) [staged, T.unpack func]
    commanded (Executable file) action = action file [T.unpack func]
    -- Run the program with its input and its output on pipes and its
    -- complaints in a file. The input is written and closed before the output is
    -- read, so the parent never has two streams to drain at once — which would
    -- need threads to be safe — and the program's own stderr, which may be
    -- anything at all, cannot fill a pipe nobody is reading. Every stream is
    -- bytes: a 𝜑 expression carries characters no single-byte locale can spell,
    -- so nothing is left to the locale.
    executed :: String -> [String] -> FilePath -> IO (ExitCode, BS.ByteString)
    executed program arguments errors =
      withBinaryFile errors WriteMode $ \stderr' -> do
        (stdin', stdout', process) <- spawned program arguments stderr'
        hSetBinaryMode stdin' True
        hSetBinaryMode stdout' True
        -- A program that dies before reading its input leaves this write with
        -- nobody to drain it. The failure worth reporting is the one the program
        -- made, so a broken pipe is swallowed here and the exit status decides.
        BS.hPut stdin' (payload form univ) `catch` unheard
        hClose stdin' `catch` unheard
        answer <- BS.hGetContents stdout'
        status <- waitForProcess process
        pure (status, answer)
    spawned :: String -> [String] -> Handle -> IO (Handle, Handle, ProcessHandle)
    spawned program arguments stderr' = do
      spawn <- createProcess started `catch` missing program
      case spawn of
        (Just stdin', Just stdout', _, process) -> pure (stdin', stdout', process)
        _ -> throwIO (AtomMute func "" "the program gave phino no streams to talk over")
      where
        started :: CreateProcess
        started =
          (proc program arguments)
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = UseHandle stderr'
            }
    missing :: String -> IOError -> IO a
    missing program failure = throwIO (NoRuntime func program (show failure))
    unheard :: IOError -> IO ()
    unheard _ = pure ()
    -- Whatever the program complained about, decoded leniently: the stream is
    -- the program's, so it may hold anything at all.
    readErrors :: FilePath -> IO String
    readErrors errors = T.unpack . T.strip . decodeUtf8Lenient <$> BS.readFile errors
    -- Parse what the program said: a JSON object with the raw 𝜑-expression
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
