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
-- script, to 'exec' and the path of a file that runs on its own once per fire,
-- or to 'serve' and the path of a file that stays for the whole run and is
-- asked over its streams, one line per fire:
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
-- >     "rt": "serve",
-- >     "path": "/opt/eo/atoms/resident"
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
import Control.Exception (Exception, SomeException, bracket, catch, throwIO, try)
import Control.Monad (foldM, unless, void)
import Data.Aeson (FromJSON (parseJSON), eitherDecodeStrict', object, withObject, withText, (.:), (.:?), (.=))
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (find, intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
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
import System.IO (Handle, IOMode (WriteMode), hClose, hFlush, hSetBinaryMode, openBinaryTempFile, withBinaryFile)
import System.Process (CreateProcess (std_err, std_in, std_out), ProcessHandle, StdStream (CreatePipe, UseHandle), createProcess, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Text.Printf (printf)

-- The interpreter a script is run under, named after the executable itself:
-- only 'node' for now. A registry naming any other runtime is rejected when it
-- is read, before dataization starts, so a run never gets half-way through a
-- program to discover that one of its atoms cannot be run at all.
data Runtime = RtNode
  deriving stock (Eq, Show)

-- One entry of the registry, as the file spells it: a script under the runtime
-- that interprets it, a file that runs on its own once per fire, or a file
-- that is started once and serves every fire of the run. This is what the JSON
-- says; what phino fires is an 'Atom', which 'readRegistry' makes of it.
data Entry
  = EnScripted Runtime T.Text
  | EnExecutable FilePath
  | EnServed FilePath

-- One λ function phino may fire. Either a script, which phino stages in a
-- temporary file and hands to the interpreter of its runtime, or an executable
-- file, which phino runs as it is, since the object model brought its own
-- binary and there is nothing to stage, or a session with a program that stays
-- resident for the run and is asked over its streams, so that a run of many
-- fires spawns it once instead of once per fire.
data Atom
  = Scripted Runtime T.Text
  | Executable FilePath
  | Served Session
  deriving stock (Eq, Show)

-- The program a 'serve' entry names, together with the process phino has
-- started of it, if it has: none until the first fire, since a run that never
-- reaches the atom should not pay for it. Every entry naming the same file
-- shares one session, so one process serves all the λ names it is registered
-- under.
data Session = Session
  { _program :: FilePath
  , _resident :: MVar (Maybe Resident)
  }

-- Two sessions are the same when they serve from the same file, whatever
-- their processes are up to.
instance Eq Session where
  Session left _ == Session right _ = left == right

instance Show Session where
  show (Session program _) = show program

-- A resident program while it runs: its streams, the file its complaints go to,
-- the universe it was told last, so it is told again only when the universe
-- changes, and how many requests it has been asked, which numbers the next one.
data Resident = Resident
  { _input :: Handle
  , _output :: Handle
  , _process :: ProcessHandle
  , _complaints :: FilePath
  , _told :: Maybe Expression
  , _asked :: Int
  }

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
  | -- The program said nothing phino can use: its answer is not a JSON object,
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

-- The 'rt' of an atom that is a file started once and kept for the run.
serveName :: String
serveName = "serve"

-- Every name the 'rt' field of a registry entry may take.
runtimeNames :: [String]
runtimeNames = map runtimeName runtimes ++ [execName, serveName]

instance FromJSON Runtime where
  parseJSON = withText "runtime" $ \name -> case find ((== T.unpack name) . runtimeName) runtimes of
    Just runtime -> pure runtime
    Nothing -> fail (printf "unknown runtime '%s', expected one of: %s" (T.unpack name) (intercalate ", " runtimeNames))

instance FromJSON Entry where
  parseJSON = withObject "atom" $ \entry -> entry .: "rt" >>= shaped entry
    where
      shaped :: A.Object -> String -> Parser Entry
      shaped entry named
        | named == execName = EnExecutable <$> entry .: "path"
        | named == serveName = EnServed <$> entry .: "path"
        | otherwise = EnScripted <$> parseJSON (A.String (T.pack named)) <*> entry .: "script"

-- What a one-shot program writes to stdout: one JSON object whose 'n' field is
-- the 𝜑-expression the atom answers with.
newtype Answer = Answer T.Text

instance FromJSON Answer where
  parseJSON = withObject "answer" $ \answer -> Answer <$> answer .: "n"

-- What a resident program writes back for one request: the 'id' of the request
-- it answers and, under '𝑛', the 𝜑-expression the atom answers with.
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
-- fails here, before any dataization starts. The entries naming the same file
-- to serve from are given one session between them, so that one resident
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
    -- The file of an executable or a resident atom is the only thing phino
    -- knows about it, and it staged none of it, so the file is looked at here,
    -- while the registry is being read, rather than half-way through a program
    -- that turns out to name that atom.
    runnable :: T.Text -> Entry -> IO ()
    runnable _ (EnScripted _ _) = pure ()
    runnable func (EnExecutable file) = runs func file
    runnable func (EnServed file) = runs func file
    runs :: T.Text -> FilePath -> IO ()
    runs func file = do
      there <- doesFileExist file
      unless there (throwIO (NoRuntime func file "there is no such file"))
      allowed <- executable <$> getPermissions file
      unless allowed (throwIO (NoRuntime func file "the file is not executable"))
    -- Turn an entry into the atom phino fires, sharing a session between the
    -- entries that serve from the same file.
    admitted :: (Registry, Map FilePath Session) -> (T.Text, Entry) -> IO (Registry, Map FilePath Session)
    admitted (registry, sessions) (func, EnScripted runtime script) = pure (Map.insert func (Scripted runtime script) registry, sessions)
    admitted (registry, sessions) (func, EnExecutable file) = pure (Map.insert func (Executable file) registry, sessions)
    admitted (registry, sessions) (func, EnServed file) = do
      session <- maybe (Session file <$> newMVar Nothing) pure (Map.lookup file sessions)
      pure (Map.insert func (Served session) registry, Map.insert file session sessions)

-- Stop every resident program the registry has started: its stdin is closed,
-- which is its cue to quit, and a program that has not quit within a second is
-- terminated. The runners call this when the run is over, whatever it ended
-- with, so that no process outlives the phino that started it.
closeRegistry :: Registry -> IO ()
closeRegistry registry = mapM_ dismissed (Map.elems registry)
  where
    dismissed :: Atom -> IO ()
    dismissed (Served Session{..}) = modifyMVar_ _resident (maybe (pure Nothing) stopped)
    dismissed _ = pure ()
    stopped :: Resident -> IO (Maybe Resident)
    stopped Resident{..} = do
      hClose _input `catch` unheard
      quit <- timeout 1000000 (waitForProcess _process)
      unless (isJust quit) (terminateProcess _process >> void (waitForProcess _process))
      hClose _output `catch` unheard
      removePathForcibly _complaints
      pure Nothing

-- Fire the λ function 'func' by running its program. A scripted atom is staged
-- in a temporary file and handed to the interpreter of its runtime; an
-- executable one is run straight off its path, under no interpreter at all;
-- both get the λ name as their last command-line argument — one program may be
-- registered under several names and branch on it — and answer once, over
-- stdin and stdout, before they exit. A served atom is asked instead: its
-- program is started on the first fire and stays for the run, and every fire
-- is one line to it and one line back. Whichever way, the 𝜑-expression the
-- program answers with becomes the atom's raw result, which 𝔼 normalizes
-- exactly as it normalized the answer of a built-in one.
fireAtom :: T.Text -> Atom -> Expression -> Expression -> IO Expression
fireAtom func (Scripted runtime script) form univ =
  withTemp (printf "phino-atom-.%s" (extension runtime)) (encodeUtf8 script) $ \staged ->
    fireOnce func (interpreter runtime) [staged, T.unpack func] form univ
fireAtom func (Executable file) form univ = fireOnce func file [T.unpack func] form univ
fireAtom func (Served session) form univ = askResident func session form univ

-- Run the program as a POSIX process that answers once: it is fed a JSON
-- object on stdin (see 'payload') and answers with one on stdout, whose 'n'
-- field is the 𝜑-expression. A non-zero exit, unparsable output or a missing
-- 'n' fails the run.
fireOnce :: T.Text -> String -> [String] -> Expression -> Expression -> IO Expression
fireOnce func program arguments form univ =
  withTemp "phino-atom-.err" "" $ \errors -> do
    logDebug (printf "Firing atom '%s' as '%s %s'" (T.unpack func) program (unwords arguments))
    (status, answer) <- executed errors
    complaint <- readErrors errors
    unless (null complaint) (logDebug (printf "Atom '%s' wrote to stderr: %s" (T.unpack func) complaint))
    case status of
      ExitFailure code -> throwIO (AtomBroke func code complaint)
      ExitSuccess -> answered answer
  where
    -- Run the program with its input and its output on pipes and its
    -- complaints in a file. The input is written and closed before the output is
    -- read, so the parent never has two streams to drain at once — which would
    -- need threads to be safe — and the program's own stderr, which may be
    -- anything at all, cannot fill a pipe nobody is reading. Every stream is
    -- bytes: a 𝜑 expression carries characters no single-byte locale can spell,
    -- so nothing is left to the locale.
    executed :: FilePath -> IO (ExitCode, BS.ByteString)
    executed errors =
      withBinaryFile errors WriteMode $ \stderr' -> do
        (stdin', stdout', process) <- spawned func program arguments stderr'
        -- A program that dies before reading its input leaves this write with
        -- nobody to drain it. The failure worth reporting is the one the program
        -- made, so a broken pipe is swallowed here and the exit status decides.
        BS.hPut stdin' (payload form univ) `catch` unheard
        hClose stdin' `catch` unheard
        answer <- BS.hGetContents stdout'
        status <- waitForProcess process
        pure (status, answer)
    -- Parse what the program said: a JSON object with the raw 𝜑-expression
    -- under 'n'.
    answered :: BS.ByteString -> IO Expression
    answered answer = case eitherDecodeStrict' answer of
      Left failure -> throwIO (AtomMute func (spoken answer) failure)
      Right (Answer raw) -> parsedAnswer func raw

-- Ask the resident program of the session, starting it if this is the first
-- fire. The program is told the universe Φ first, as one line holding it under
-- '𝑒', and again only when a fire comes with a different universe; every fire
-- is then one line holding the λ name under 'λ', the formation under '𝑏' and
-- an 'id', and one line back holding the 𝜑-expression under '𝑛' and the same
-- 'id'. The letters are those of the evaluation rule of the calculus, 𝔼(𝑏, 𝑒,
-- 𝑠) = 𝑛. A reply that is not JSON, carries no '𝑛', answers another request,
-- or a program that hangs up fails the run, with the program's stderr in the
-- message. The process is kept whatever the fire ended with, so that
-- 'closeRegistry' finds it.
askResident :: T.Text -> Session -> Expression -> Expression -> IO Expression
askResident func Session{..} form univ = do
  outcome <- modifyMVar _resident $ \current -> do
    resident <- maybe started pure current
    attempt <- try (asked resident) :: IO (Either SomeException (Resident, Expression))
    pure (Just (either (const resident) fst attempt), snd <$> attempt)
  either throwIO pure outcome
  where
    -- Start the program with its input and its output on pipes and its
    -- complaints in a file that lives as long as the process does.
    started :: IO Resident
    started = do
      dir <- getTemporaryDirectory
      (complaints, handle) <- openBinaryTempFile dir "phino-atom-.err"
      logDebug (printf "Starting the resident program '%s' to serve atom '%s'" _program (T.unpack func))
      (input, output, process) <- spawned func _program [] handle
      pure (Resident input output process complaints Nothing 0)
    asked :: Resident -> IO (Resident, Expression)
    asked resident = do
      told <- informed resident
      let number = _asked told + 1
      logDebug (printf "Asking the resident program '%s' to fire atom '%s' as request %d" _program (T.unpack func) number)
      said (_input told) (request number)
      reply <- BS.hGetLine (_output told) `catch` hungUp told
      answer <- replied number reply
      pure (told{_asked = number}, answer)
    -- Tell the program the universe, unless it is the one it was told last.
    informed :: Resident -> IO Resident
    informed resident
      | _told resident == Just univ = pure resident
      | otherwise = do
          said (_input resident) (lined (object ["𝑒" .= rendered univ]))
          pure resident{_told = Just univ}
    request :: Int -> BS.ByteString
    request number = lined (object ["id" .= number, "λ" .= func, "𝑏" .= rendered form])
    -- One line to the program, pushed through at once, since a pipe is
    -- buffered and the program answers nothing it has not seen. A program that
    -- has died leaves the write with nobody to drain it; the failure worth
    -- reporting is the one the program made, so a broken pipe is swallowed
    -- here and the read that follows finds out.
    said :: Handle -> BS.ByteString -> IO ()
    said handle line = (BS.hPut handle line >> hFlush handle) `catch` unheard
    -- The program closed its stdout instead of answering: if it has exited
    -- with a failure, that is the failure; otherwise it went mute.
    hungUp :: Resident -> IOError -> IO BS.ByteString
    hungUp Resident{..} _ = do
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
        | otherwise -> parsedAnswer func raw

-- Spawn the program with its input and its output on pipes and its stderr on
-- the given handle, as bytes on every stream: a 𝜑 expression carries
-- characters no single-byte locale can spell, so nothing is left to the locale.
spawned :: T.Text -> String -> [String] -> Handle -> IO (Handle, Handle, ProcessHandle)
spawned func program arguments stderr' = do
  spawn <- createProcess started `catch` missing
  case spawn of
    (Just stdin', Just stdout', _, process) -> do
      hSetBinaryMode stdin' True
      hSetBinaryMode stdout' True
      pure (stdin', stdout', process)
    _ -> throwIO (AtomMute func "" "the program gave phino no streams to talk over")
  where
    started :: CreateProcess
    started =
      (proc program arguments)
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = UseHandle stderr'
        }
    missing :: IOError -> IO a
    missing failure = throwIO (NoRuntime func program (show failure))

-- The 𝜑-expression a program answered with, parsed, or the failure to.
parsedAnswer :: T.Text -> T.Text -> IO Expression
parsedAnswer func raw = case parseExpression (T.unpack raw) of
  Left failure -> throwIO (AtomMute func (T.unpack raw) failure)
  Right expr -> pure expr

-- Whatever the program said, decoded leniently and trimmed: the stream is the
-- program's, so it may hold anything at all.
spoken :: BS.ByteString -> String
spoken = T.unpack . T.strip . decodeUtf8Lenient

-- Whatever the program complained about, read from the file its stderr goes to.
readErrors :: FilePath -> IO String
readErrors errors = spoken <$> BS.readFile errors

unheard :: IOError -> IO ()
unheard _ = pure ()

-- The JSON phino feeds a one-shot program on stdin: the formation being
-- evaluated under 'b', with its λ binding removed so the program may dispatch
-- on it, and the universe Φ under 's'. The text is what phino's own parser
-- reads back, so a program may hand any part of it to another phino run (see
-- the '--inside' option).
--
-- @todo #1121:35min Rename the keys of the one-shot payload and its answer to
--  the letters of the calculus paper, '𝑏', '𝑒' and '𝑛', the way the resident
--  protocol already spells them: here the universe goes under 's', which in
--  the paper stands for the state, not for the universe. The fixture script,
--  the README and every registered script must change together with it.
payload :: Expression -> Expression -> BS.ByteString
payload form univ = BSL.toStrict (A.encode (object ["b" .= rendered form, "s" .= rendered univ]))

-- One JSON object as one line, for the programs that read by the line.
lined :: A.Value -> BS.ByteString
lined value = BSL.toStrict (A.encode value) <> "\n"

-- An expression as canonical 𝜑-calculus on a single line — no syntax sugar,
-- whatever '--sweet' says about the output of the run — so a program never has
-- to know phino's sugar to find a datum: every byte array it may need is
-- spelled out as a Δ binding.
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
