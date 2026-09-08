{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- Which λ functions exist is a property of the object model being dataized,
-- not of the calculus. phino therefore implements none of them: it reads a
-- registry of them from a JSON file given with '--atoms' and fires each one as
-- a POSIX process. The registry maps a λ name to the runtime that runs its
-- script, or to 'exec' and the path of a file that runs on its own, and says
-- with 'serve' whether the program is to be started once and kept for the run:
--
-- > {
-- >   "L_bytes_eq": {
-- >     "rt": "node",
-- >     "script": "const fs = require('fs'); ..."
-- >   },
-- >   "L_number_plus": {
-- >     "rt": "exec",
-- >     "path": "/opt/eo/atoms/number-plus"
-- >   },
-- >   "L_number_times": {
-- >     "rt": "exec",
-- >     "path": "/opt/eo/atoms/resident",
-- >     "serve": true
-- >   }
-- > }
--
-- Whichever way it is run, a program speaks one protocol, in the letters of the
-- evaluation rule of the calculus paper, 𝔼(𝑏, 𝑒, 𝑠) = 𝑛: one JSON object per
-- line, the universe under '𝑒', then a request with an 'id', the λ name under
-- 'λ' and the formation under '𝑏', answered by a line with the same 'id' and
-- the 𝜑-expression under '𝑛'.
--
-- A name absent from the registry has no λ function at all: 𝔼 gets stuck on
-- it, exactly as it does for a name no one ever declared (see 'Stuck' in
-- 'Dataize').
module Atoms
  ( Atom (..)
  , AtomException (..)
  , Program (..)
  , Registry
  , Runtime (..)
  , Session (_program)
  , closeRegistry
  , emptyRegistry
  , fireAtom
  , readRegistry
  , registeredAtom
  , runtimeNames
  )
where

import AST
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Exception (Exception, SomeException, catch, onException, throwIO, try)
import Control.Monad (foldM, unless)
import Data.Aeson (FromJSON (parseJSON), eitherDecodeStrict', object, withObject, withText, (.!=), (.:), (.:?), (.=))
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
import System.IO (Handle, hClose, hFlush, hSetBinaryMode, openBinaryTempFile)
import System.Process (CreateProcess (std_err, std_in, std_out), ProcessHandle, StdStream (CreatePipe, UseHandle), createProcess, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Text.Printf (printf)

-- The interpreter a script is run under, named after the executable itself:
-- only 'node' for now. A registry naming any other runtime is rejected when it
-- is read, before dataization starts, so a run never gets half-way through a
-- program to discover that one of its atoms cannot be run at all.
data Runtime = RtNode
  deriving stock (Eq, Ord, Show)

-- How the program of an atom is started: as a script under the interpreter of
-- its runtime, which phino stages in a temporary file, or as an executable
-- file, which phino runs as it is, since the object model brought its own
-- binary and there is nothing to stage.
data Program
  = Scripted Runtime T.Text
  | Executable FilePath
  deriving stock (Eq, Ord, Show)

-- One λ function phino may fire: its program, either started afresh for every
-- fire and gone once it has answered, or kept in a session for the run, so
-- that one process answers every fire — which is what an entry saying 'serve'
-- asks for, and what a program that is slow to start needs.
data Atom
  = Transient Program
  | Resident Session
  deriving stock (Eq, Show)

-- A program to be kept for the run, together with the process phino has
-- started of it, if it has: none until the first fire, since a run that never
-- reaches the atom should not pay for it. Every entry naming the same program
-- shares one session, so one process serves all the λ names it is registered
-- under.
data Session = Session
  { _program :: Program
  , _running :: MVar (Maybe Running)
  }

-- Two sessions are the same when they keep the same program, whatever their
-- processes are up to.
instance Eq Session where
  Session left _ == Session right _ = left == right

instance Show Session where
  show (Session program _) = show program

-- A program while it runs: its streams, the file its complaints go to, the
-- file its script is staged in, if it is a script, the universe it was told
-- last, so it is told again only when the universe changes, and how many
-- requests it has been asked, which numbers the next one.
data Running = Running
  { _input :: Handle
  , _output :: Handle
  , _process :: ProcessHandle
  , _complaints :: FilePath
  , _staged :: Maybe FilePath
  , _told :: Maybe Expression
  , _requests :: Int
  }

-- One entry of the registry, as the file spells it: the program and whether
-- it is to be kept for the run.
data Entry = Entry Program Bool

-- Every λ function phino may fire, keyed by name.
type Registry = Map T.Text Atom

data AtomException
  = -- The '--atoms' file is not a JSON registry of λ functions.
    BrokenRegistry FilePath String
  | -- The program of an atom cannot be run: the interpreter of its runtime is
    -- not installed, or its executable file is missing or not executable.
    NoRuntime T.Text String String
  | -- The program exited with a non-zero status; the message carries its stderr.
    AtomBroke T.Text Int String
  | -- The program said nothing phino can use: its reply is not a JSON object,
    -- carries no 𝜑-expression, answers another request, or the 𝜑-expression
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

instance FromJSON Program where
  parseJSON = withObject "atom" $ \entry -> do
    named <- entry .: "rt"
    if named == execName
      then Executable <$> entry .: "path"
      else Scripted <$> parseJSON (A.String (T.pack named)) <*> entry .: "script"

-- The 'serve' field is optional and off by default: a program is started for
-- every fire unless the entry says otherwise.
instance FromJSON Entry where
  parseJSON value = Entry <$> parseJSON value <*> withObject "atom" (\entry -> entry .:? "serve" .!= False) value

-- What a program writes back for one request: the 'id' of the request it
-- answers and, under '𝑛', the 𝜑-expression the atom answers with.
data Reply = Reply Int T.Text

instance FromJSON Reply where
  parseJSON = withObject "reply" $ \reply -> do
    number <- reply .: "id"
    raw <- reply .:? "𝑛"
    maybe (fail "there is no '𝑛' in it") (pure . Reply number) raw

-- No λ function at all: every atom gets stuck. This is what a run without
-- '--atoms' fires against.
emptyRegistry :: Registry
emptyRegistry = Map.empty

-- The λ function registered under this name, if any.
registeredAtom :: Registry -> T.Text -> Maybe Atom
registeredAtom registry func = Map.lookup func registry

-- Read the registry of λ functions from a JSON file. An unknown runtime, a
-- missing 'script', a 'path' that names no executable file or malformed JSON
-- fails here, before any dataization starts. The entries that are to keep the
-- same program are given one session between them, so that one resident
-- process answers for every λ name it is registered under.
readRegistry :: FilePath -> IO Registry
readRegistry path = do
  content <- BS.readFile path `catch` unreadable
  case eitherDecodeStrict' content of
    Left failure -> throwIO (BrokenRegistry path failure)
    Right entries -> do
      mapM_ (uncurry runnable) (Map.toList entries)
      (registry, _) <- foldM admitted (Map.empty, Map.empty) (Map.toList entries)
      logDebug (printf "Loaded %d atom(s) from '%s'" (Map.size registry) path)
      pure registry
  where
    unreadable :: IOError -> IO BS.ByteString
    unreadable failure = throwIO (BrokenRegistry path (show failure))
    -- The file of an executable atom is the only thing phino knows about it,
    -- and it staged none of it, so the file is looked at here, while the
    -- registry is being read, rather than half-way through a program that
    -- turns out to name that atom.
    runnable :: T.Text -> Entry -> IO ()
    runnable func (Entry (Executable file) _) = do
      there <- doesFileExist file
      unless there (throwIO (NoRuntime func file "there is no such file"))
      allowed <- executable <$> getPermissions file
      unless allowed (throwIO (NoRuntime func file "the file is not executable"))
    runnable _ _ = pure ()
    -- Turn an entry into the atom phino fires, sharing a session between the
    -- entries that are to keep the same program.
    admitted :: (Registry, Map Program Session) -> (T.Text, Entry) -> IO (Registry, Map Program Session)
    admitted (registry, sessions) (func, Entry program False) = pure (Map.insert func (Transient program) registry, sessions)
    admitted (registry, sessions) (func, Entry program True) = do
      session <- maybe (Session program <$> newMVar Nothing) pure (Map.lookup program sessions)
      pure (Map.insert func (Resident session) registry, Map.insert program session sessions)

-- Stop every resident program the registry has started: its stdin is closed,
-- which is its cue to quit, and a program that has not quit within a second is
-- terminated. The runners call this when the run is over, whatever it ended
-- with, so that no process outlives the phino that started it.
closeRegistry :: Registry -> IO ()
closeRegistry registry = mapM_ dismissed (Map.elems registry)
  where
    dismissed :: Atom -> IO ()
    dismissed (Resident Session{..}) = modifyMVar_ _running (maybe (pure Nothing) (\running -> Nothing <$ stopped briefly running))
    dismissed _ = pure ()

-- Fire the λ function 'func' by asking its program. A transient program is
-- started for the fire and waited for once it has answered, so that its exit
-- status has its say; a resident one is started on the first fire and stays
-- for the run, kept whatever the fire ended with, so that 'closeRegistry'
-- finds it. Whichever way, the 𝜑-expression the program answers with becomes
-- the atom's raw result, which 𝔼 normalizes exactly as it normalized the
-- answer of a built-in one.
fireAtom :: T.Text -> Atom -> Expression -> Expression -> IO Expression
fireAtom func (Transient program) form univ = do
  running <- started func program
  (_, answer) <- asked func running form univ hClose `onException` stopped patiently running
  (status, complaint) <- stopped patiently running
  unless (null complaint) (logDebug (printf "Atom '%s' wrote to stderr: %s" (T.unpack func) complaint))
  case status of
    ExitFailure code -> throwIO (AtomBroke func code complaint)
    ExitSuccess -> pure answer
fireAtom func (Resident Session{..}) form univ = do
  outcome <- modifyMVar _running $ \current -> do
    running <- maybe (started func _program) pure current
    attempt <- try (asked func running form univ hFlush) :: IO (Either SomeException (Running, Expression))
    pure (Just (either (const running) fst attempt), snd <$> attempt)
  either throwIO pure outcome

-- Start the program, with its input and its output on pipes and its complaints
-- in a file that lives as long as the process does: a script is staged in a
-- temporary file first and handed to the interpreter of its runtime, an
-- executable file is run as it is. Every stream is bytes: a 𝜑 expression
-- carries characters no single-byte locale can spell, so nothing is left to
-- the locale.
started :: T.Text -> Program -> IO Running
started func program = do
  dir <- getTemporaryDirectory
  (complaints, handle) <- openBinaryTempFile dir "phino-atom-.err"
  (executable, arguments, staged) <- commanded dir
  logDebug (printf "Starting atom '%s' as '%s'" (T.unpack func) (unwords (executable : arguments)))
  (input, output, process) <- spawned executable arguments handle `onException` discarded complaints staged
  pure (Running input output process complaints staged Nothing 0)
  where
    -- The command line the program is started with, and the file staged for
    -- it, if it is a script.
    commanded :: FilePath -> IO (String, [String], Maybe FilePath)
    commanded dir = case program of
      Executable file -> pure (file, [], Nothing)
      Scripted runtime script -> do
        (path, handle) <- openBinaryTempFile dir (printf "phino-atom-.%s" (extension runtime))
        BS.hPut handle (encodeUtf8 script)
        hClose handle
        pure (interpreter runtime, [path], Just path)
    spawned :: String -> [String] -> Handle -> IO (Handle, Handle, ProcessHandle)
    spawned executable arguments stderr' = do
      spawn <- createProcess (proc executable arguments){std_in = CreatePipe, std_out = CreatePipe, std_err = UseHandle stderr'} `catch` missing executable
      case spawn of
        (Just input, Just output, _, process) -> do
          hSetBinaryMode input True
          hSetBinaryMode output True
          pure (input, output, process)
        _ -> throwIO (AtomMute func "" "the program gave phino no streams to talk over")
    missing :: String -> IOError -> IO a
    missing executable failure = throwIO (NoRuntime func executable (show failure))

-- Ask the running program to fire the λ function: it is told the universe,
-- unless it was told already, then the request, and its reply is read back.
-- How the request is pushed through is the caller's: a transient program has
-- its stdin closed behind it, since it may read its input whole before it
-- answers, a resident one has it flushed, since it reads on. A reply that is
-- not JSON, carries no '𝑛', answers another request, or a program that hangs
-- up fails the fire, with the program's stderr in the message.
asked :: T.Text -> Running -> Expression -> Expression -> (Handle -> IO ()) -> IO (Running, Expression)
asked func running@Running{..} form univ pushed = do
  let number = _requests + 1
      universe = lined (object ["𝑒" .= rendered univ])
      request = lined (object ["id" .= number, "λ" .= func, "𝑏" .= rendered form])
  logDebug (printf "Asking atom '%s' as request %d" (T.unpack func) number)
  said (if _told == Just univ then request else universe <> request)
  reply <- BS.hGetLine _output `catch` hungUp
  answer <- replied number reply
  pure (running{_told = Just univ, _requests = number}, answer)
  where
    -- A program that has died leaves the write with nobody to drain it. The
    -- failure worth reporting is the one the program made, so a broken pipe is
    -- swallowed here and the read that follows finds out.
    said :: BS.ByteString -> IO ()
    said content = (BS.hPut _input content >> pushed _input) `catch` unheard
    -- The program closed its stdout instead of answering: if it has quit with
    -- a failure, that is the failure; otherwise it went mute.
    hungUp :: IOError -> IO BS.ByteString
    hungUp _ = do
      status <- timeout 1000000 (waitForProcess _process)
      complaint <- readErrors _complaints
      case status of
        Just (ExitFailure code) -> throwIO (AtomBroke func code complaint)
        Just ExitSuccess -> throwIO (AtomMute func "" (unwords ("the program quit without answering" : [complaint | not (null complaint)])))
        Nothing -> throwIO (AtomMute func "" (unwords ("the program closed its stdout without answering" : [complaint | not (null complaint)])))
    -- Parse what the program said back: a JSON object answering this very
    -- request, with the raw 𝜑-expression under '𝑛'.
    replied :: Int -> BS.ByteString -> IO Expression
    replied number reply = case eitherDecodeStrict' reply of
      Left failure -> throwIO (AtomMute func (spoken reply) failure)
      Right (Reply echoed raw)
        | echoed /= number -> throwIO (AtomMute func (spoken reply) (printf "it answers request %d, while phino asked request %d" echoed number))
        | otherwise -> case parseExpression (T.unpack raw) of
            Left failure -> throwIO (AtomMute func (T.unpack raw) failure)
            Right expr -> pure expr

-- Hang up on the program: close its stdin, which is its cue to quit, wait for
-- it the given way and remove the files it was given, its complaints read
-- first, since they are what a failure is reported with.
stopped :: (Running -> IO ExitCode) -> Running -> IO (ExitCode, String)
stopped waited running@Running{..} = do
  hClose _input `catch` unheard
  status <- waited running
  hClose _output `catch` unheard
  complaint <- readErrors _complaints
  discarded _complaints _staged
  pure (status, complaint)

-- Wait for the program to quit for as long as it takes, draining whatever else
-- it writes, so that a chatty one never blocks on a full pipe: a transient
-- program is on its way out once it has answered, and its exit status is the
-- verdict on its answer.
patiently :: Running -> IO ExitCode
patiently Running{..} = BS.hGetContents _output >> waitForProcess _process

-- Wait for the program to quit for a second, then terminate it: a resident one
-- was told to quit and gets no say in the matter.
briefly :: Running -> IO ExitCode
briefly Running{..} = timeout 1000000 (waitForProcess _process) >>= maybe (terminateProcess _process >> waitForProcess _process) pure

-- Remove the files a program was given: the one its complaints went to and the
-- one its script was staged in, if it was a script.
discarded :: FilePath -> Maybe FilePath -> IO ()
discarded complaints staged = removePathForcibly complaints >> mapM_ removePathForcibly staged

-- Whatever the program said, decoded leniently and trimmed: the stream is the
-- program's, so it may hold anything at all.
spoken :: BS.ByteString -> String
spoken = T.unpack . T.strip . decodeUtf8Lenient

-- Whatever the program complained about, read from the file its stderr goes to.
readErrors :: FilePath -> IO String
readErrors errors = spoken <$> BS.readFile errors

unheard :: IOError -> IO ()
unheard _ = pure ()

-- One JSON object as one line, for the programs that read by the line.
lined :: A.Value -> BS.ByteString
lined value = BSL.toStrict (A.encode value) <> "\n"

-- An expression as canonical 𝜑-calculus on a single line — no syntax sugar,
-- whatever '--sweet' says about the output of the run — so a program never has
-- to know phino's sugar to find a datum: every byte array it may need is
-- spelled out as a Δ binding. The text is what phino's own parser reads back,
-- so a program may hand any part of it to another phino run (see the
-- '--inside' option).
rendered :: Expression -> T.Text
rendered expr = T.pack (printExpression' expr (SALTY, UNICODE, SINGLELINE, defaultMargin))
