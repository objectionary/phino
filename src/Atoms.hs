{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

-- Which λ functions exist is a property of the object model being dataized,
-- not of the calculus. phino therefore implements none of them: it reads a
-- registry of them from a JSON file given with '--atoms' and fires each one as
-- a POSIX process. Each key of the registry is a regular expression over λ
-- names, tried top to bottom, and the first one matching the whole name of the
-- atom being fired wins; its entry names the runtime that runs a script, or
-- 'exec' and the path of a file that runs on its own, and says with 'serve'
-- whether the program is to be started once and kept for the run:
--
-- > {
-- >   "L_bytes_eq": {
-- >     "rt": "node",
-- >     "script": "const readline = require('readline'); ..."
-- >   },
-- >   "L_number_plus": {
-- >     "rt": "exec",
-- >     "path": "/opt/eo/atoms/number-plus"
-- >   },
-- >   ".*": {
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
-- The channel carries questions as well as answers. An operand reaches a
-- program unreduced, since reducing it may take the very atom being fired, so
-- instead of running a phino of its own on the universe with the operand
-- spliced into its text, a program writes a line of its own: an 'id' it minted
-- and, under 'ask', the 𝜑-expression it wants reduced. phino reduces it by
-- re-entering its own evaluator and answers with that 'id' and the reduced
-- expression under '𝑛'. Only a program kept for the run may ask: the stdin of
-- one started for the fire is closed behind its request, so there is nothing
-- left to answer it over.
--
-- A kept program may also ask by reference, naming an operand instead of
-- quoting it: 'of' carries the 'id' of a request still in flight, 'attr' the
-- canonical name of an attribute of the receiver that request was made of, and
-- the optional 'reduce' says whether to hand the node over as it is (false,
-- by default) or to dataize it the way 'ask' does. phino serves such a
-- question from the formation it already holds for that request, so neither
-- side ever re-prints a receiver the other side has in hand (#1165).
--
-- The whole 𝜑-text on the channel is the currency of programs started for one
-- fire: a kept one, able to ask for whatever the text left out, is served a
-- lean one — '𝑏' and every answer carry no ρ chain, since that chain climbs
-- to Φ and, through questions quoting earlier questions, compounds the
-- message by the depth of the ask (#1165).
--
-- A name no key matches has no λ function at all: 𝔼 gets stuck on it, exactly
-- as it does for a name no one ever declared (see 'Stuck' in 'Dataize').
module Atoms
  ( Atom (..)
  , AtomException (..)
  , Program (..)
  , ReduceFunc
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
import Control.Exception (Exception, catch, onException, throwIO)
import Control.Monad (foldM, unless)
import Data.Aeson (FromJSON (parseJSON), eitherDecodeStrict', object, withObject, withText, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as A
import Data.Aeson.Decoding (toEitherValue)
import Data.Aeson.Decoding.ByteString (bsToTokens)
import Data.Aeson.Decoding.Tokens (TkRecord (TkPair, TkRecordEnd, TkRecordErr), Tokens (TkErr, TkRecordOpen))
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (JSONPathElement (Key), parseEither, (<?>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
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
import Printer (printAttribute, printExpression', printExpressionHidingRho')
import Sugar (SugarType (SALTY))
import System.Directory (doesFileExist, executable, getPermissions, getTemporaryDirectory, removePathForcibly)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO (Handle, hClose, hFlush, hSetBinaryMode, openBinaryTempFile)
import System.Process (CreateProcess (std_err, std_in, std_out), ProcessHandle, StdStream (CreatePipe, UseHandle), createProcess, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Text.Printf (printf)
import Text.Regex.PCRE (matchTest)
import Text.Regex.PCRE.ByteString (Regex, compUTF8, compile, execBlank)

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
-- last, so it is told again only when the universe changes, how many
-- requests it has been asked, which numbers the next one, and the receivers
-- of the requests still in flight, which by-reference questions name instead
-- of quoting (#1165). The last three are mutable, since a fire may nest:
-- serving a question of the program takes an evaluator that fires atoms of
-- its own, and the one it reaches may be this very program, asked again over
-- these very handles while its question is still open.
data Running = Running
  { _input :: Handle
  , _output :: Handle
  , _process :: ProcessHandle
  , _complaints :: FilePath
  , _staged :: Maybe FilePath
  , _told :: IORef (Maybe Expression)
  , _requests :: IORef Int
  , _forms :: IORef (IM.IntMap Expression)
  }

-- What is left of the channel to a program once its request is pushed through:
-- the stdin of a program started for the fire is closed behind the request,
-- since the program may read its input whole before it answers, so nothing
-- more can be said to it; the stdin of one kept for the run is flushed and
-- stays open, so its questions can be answered.
data Channel = Closed | Open

-- How phino reduces a 𝜑-expression a program asks about. Only the caller of
-- 'fireAtom' can do it, since it alone holds the universe to reduce inside and
-- the context to reduce under, so it hands the way down (see 'reduction' in
-- 'Dataize').
type ReduceFunc = Expression -> IO Expression

-- One entry of the registry, as the file spells it: the program and whether
-- it is to be kept for the run.
data Entry = Entry Program Bool

-- Every λ function phino may fire, in the order the registry file lists them:
-- each key of the file, a regular expression over λ names, paired with the
-- atom its entry describes. A lookup tries them top to bottom and the first
-- key matching the whole name wins, so one entry may stand for many atoms,
-- while a plain name, being a regular expression matching itself, keeps
-- meaning that one atom.
newtype Registry = Registry [(Regex, Atom)]

data AtomException
  = -- The '--atoms' file is not a JSON registry of λ functions.
    BrokenRegistry FilePath String
  | -- The program of an atom cannot be run: the interpreter of its runtime is
    -- not installed, or its executable file is missing or not executable.
    NoRuntime T.Text String String
  | -- The program exited with a non-zero status; the message carries its stderr.
    AtomBroke T.Text Int String
  | -- The program said nothing phino can use: its reply is not a JSON object,
    -- carries no 𝜑-expression, answers another request, asks a question phino
    -- has no channel left to answer, or the 𝜑-expression does not parse.
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

-- What a program writes back: the answer to the request it was asked, the
-- 𝜑-expression under '𝑛', a question of its own, the 𝜑-expression under
-- 'ask' that it needs reduced before it can answer, or a question by
-- reference, naming an in-flight request under 'of' and one of the receiver's
-- attributes under 'attr', with 'reduce' deciding whether the answer is the
-- node as it is held or its dataization (#1165). An answer echoes the 'id'
-- of the request it answers, a question mints an 'id' of its own, which phino
-- echoes back.
data Said
  = Answer Int T.Text
  | Question Int T.Text
  | Reference Int Int T.Text Bool

instance FromJSON Said where
  parseJSON = withObject "reply" $ \said -> do
    number <- said .: "id"
    answer <- said .:? "𝑛"
    question <- said .:? "ask"
    case (answer, question) of
      (Just raw, _) -> pure (Answer number raw)
      (Nothing, Just raw) -> pure (Question number raw)
      _ -> do
        request <- said .:? "of"
        attr <- said .:? "attr"
        reduced <- said .:? "reduce" .!= False
        case (request, attr) of
          (Just req, Just name) -> pure (Reference number req name reduced)
          _ -> fail "there is neither '𝑛', nor 'ask', nor 'of' with 'attr' in it"

-- No λ function at all: every atom gets stuck. This is what a run without
-- '--atoms' fires against.
emptyRegistry :: Registry
emptyRegistry = Registry []

-- The λ function of the first key that matches the whole name, if any.
registeredAtom :: Registry -> T.Text -> Maybe Atom
registeredAtom (Registry rules) func = snd <$> find (\(pattern, _) -> matchTest pattern (encodeUtf8 func)) rules

-- Read the registry of λ functions from a JSON file. A key that is no regular
-- expression, an unknown runtime, a missing 'script', a 'path' that names no
-- executable file or malformed JSON fails here, before any dataization starts.
-- The entries that are to keep the same program are given one session between
-- them, so that one resident process answers for every key it is registered
-- under.
readRegistry :: FilePath -> IO Registry
readRegistry path = do
  content <- BS.readFile path `catch` unreadable
  entries <- either (throwIO . BrokenRegistry path) pure (listed content)
  mapM_ (uncurry runnable) entries
  (rules, _) <- foldM admitted ([], Map.empty) entries
  logDebug (printf "Loaded %d atom(s) from '%s'" (length rules) path)
  pure (Registry (reverse rules))
  where
    unreadable :: IOError -> IO BS.ByteString
    unreadable failure = throwIO (BrokenRegistry path (show failure))
    -- The entries of the file in the order it lists them, which is the order
    -- the keys are tried in and which the object aeson would decode the file
    -- to forgets, so the file is walked token by token instead.
    listed :: BS.ByteString -> Either String [(T.Text, Entry)]
    listed content = case bsToTokens content of
      TkRecordOpen record -> paired record
      TkErr failure -> Left failure
      _ -> Left "the file is not a JSON object"
    paired :: TkRecord BS.ByteString String -> Either String [(T.Text, Entry)]
    paired (TkPair key tokens) = do
      (value, rest) <- toEitherValue tokens
      entry <- parseEither (\raw -> parseJSON raw <?> Key key) value
      ((Key.toText key, entry) :) <$> paired rest
    paired (TkRecordEnd rest)
      | BS.all (`BS.elem` " \t\r\n") rest = Right []
      | otherwise = Left "there is more in the file than the JSON object"
    paired (TkRecordErr failure) = Left failure
    -- The key as the regular expression it is, made to match the whole name,
    -- so that a plain name means that one atom and not every name it is a
    -- part of.
    compiled :: T.Text -> IO Regex
    compiled key = compile compUTF8 execBlank (encodeUtf8 ("^(?:" <> key <> ")$")) >>= either broken pure
      where
        broken :: (a, String) -> IO Regex
        broken (_, failure) = throwIO (BrokenRegistry path (printf "the key '%s' is not a regular expression: %s" (T.unpack key) failure))
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
    -- Turn an entry into the atom phino fires, keyed by its pattern and, when
    -- it is to keep its program, sharing a session with the entries keeping
    -- the same one; the rules come out newest first.
    admitted :: ([(Regex, Atom)], Map Program Session) -> (T.Text, Entry) -> IO ([(Regex, Atom)], Map Program Session)
    admitted (rules, sessions) (key, Entry program serve) = do
      pattern <- compiled key
      (atom, kept) <- if serve then resident program sessions else pure (Transient program, sessions)
      pure ((pattern, atom) : rules, kept)
    resident :: Program -> Map Program Session -> IO (Atom, Map Program Session)
    resident program sessions = do
      session <- maybe (Session program <$> newMVar Nothing) pure (Map.lookup program sessions)
      pure (Resident session, Map.insert program session sessions)

-- Stop every resident program the registry has started: its stdin is closed,
-- which is its cue to quit, and a program that has not quit within a second is
-- terminated. The runners call this when the run is over, whatever it ended
-- with, so that no process outlives the phino that started it.
closeRegistry :: Registry -> IO ()
closeRegistry (Registry rules) = mapM_ (dismissed . snd) rules
  where
    dismissed :: Atom -> IO ()
    dismissed (Resident Session{..}) = modifyMVar_ _running (maybe (pure Nothing) (\running -> Nothing <$ stopped briefly running))
    dismissed _ = pure ()

-- Fire the λ function 'func' by asking its program, reducing with 'reduce'
-- whatever the program asks about on the way. A transient program is started
-- for the fire and waited for once it has answered, so that its exit status
-- has its say; a resident one is started on the first fire and stays for the
-- run, whatever the fire ended with, so that 'closeRegistry' finds it. The
-- session is let go of before the program is spoken to, since serving a
-- question may fire the same atom again and a fire waiting for the session it
-- is already inside would wait forever. Whichever way, the 𝜑-expression the
-- program answers with becomes the atom's raw result, which 𝔼 normalizes
-- exactly as it normalized the answer of a built-in one.
fireAtom :: T.Text -> Atom -> Expression -> Expression -> ReduceFunc -> IO Expression
fireAtom func (Transient program) form univ reduce = do
  running <- started func program
  answer <- asked func running form univ Closed reduce `onException` stopped patiently running
  (status, complaint) <- stopped patiently running
  unless (null complaint) (logDebug (printf "Atom '%s' wrote to stderr: %s" (T.unpack func) complaint))
  case status of
    ExitFailure code -> throwIO (AtomBroke func code complaint)
    ExitSuccess -> pure answer
fireAtom func (Resident Session{..}) form univ reduce = do
  running <- modifyMVar _running (\current -> (\kept -> (Just kept, kept)) <$> maybe (started func _program) pure current)
  asked func running form univ Open reduce

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
  Running input output process complaints staged <$> newIORef Nothing <*> newIORef 0 <*> newIORef IM.empty
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
-- unless it was told already, then the request, and its lines are read back
-- until it answers. A line carrying '𝑛' with the 'id' of the request is the
-- answer; a line carrying 'ask' is a question of the program's own, which
-- phino reduces and replies to before it goes on reading. What is left of the
-- channel is the caller's: a transient program has its stdin closed behind the
-- request, since it may read its input whole before it answers, a resident one
-- has it flushed, since it reads on. A reply that is not JSON, carries neither
-- '𝑛' nor 'ask', answers another request, or a program that hangs up fails the
-- fire, with the program's stderr in the message.
asked :: T.Text -> Running -> Expression -> Expression -> Channel -> ReduceFunc -> IO Expression
asked func Running{..} form univ channel reduce = do
  number <- atomicModifyIORef' _requests (\spent -> (spent + 1, spent + 1))
  modifyIORef' _forms (IM.insert number form)
  told <- readIORef _told
  logDebug (printf "Asking atom '%s' as request %d" (T.unpack func) number)
  said (if told == Just univ then request number else universe <> request number)
  writeIORef _told (Just univ)
  heard number `onException` forget number
  where
    universe :: BS.ByteString
    universe = lined (object ["𝑒" .= spelled univ])
    request :: Int -> BS.ByteString
    request number = lined (object ["id" .= number, "λ" .= func, "𝑏" .= spelled form])
    -- Everything phino says to a program kept for the run is spelled without
    -- the ρ chain: such a program can ask for what the chain holds, by value
    -- with 'ask' or by reference with 'of' and 'attr', so quoting it into
    -- every message only makes the next question bigger (#1165). A program
    -- started for the fire has no channel to ask over and keeps getting the
    -- whole receiver, ρ and all.
    spelled :: Expression -> T.Text
    spelled = case channel of
      Open -> lean
      Closed -> rendered
    lean :: Expression -> T.Text
    lean expr = T.pack (printExpressionHidingRho' expr (SALTY, UNICODE, SINGLELINE, defaultMargin))
    -- The receiver of a request is of no use to the channel once the request
    -- has been answered.
    forget :: Int -> IO ()
    forget = modifyIORef' _forms . IM.delete
    -- Read the program's lines until it answers the request phino asked,
    -- serving every question it asks on the way.
    heard :: Int -> IO Expression
    heard number = do
      reply <- BC.hGetLine _output `catch` hungUp
      case eitherDecodeStrict' reply of
        Left failure -> throwIO (AtomMute func (spoken reply) failure)
        Right (Answer echoed raw)
          | echoed /= number -> throwIO (AtomMute func (spoken reply) (printf "it answers request %d, while phino asked request %d" echoed number))
          | otherwise -> forget number >> either (throwIO . AtomMute func (T.unpack raw)) pure (parseExpression (T.unpack raw))
        Right (Question minted raw) -> served minted raw >> heard number
        Right (Reference minted req name doReduce) -> referenced minted req name doReduce >> heard number
    -- The by-reference sibling of 'served': the question names an in-flight
    -- request and one attribute of its receiver, and phino answers from the
    -- formation it still holds for that request, without either side
    -- re-printing or re-parsing a receiver. 'reduce' says whether to dataize
    -- what the attribute carries, as 'ask' does, or to hand the node over as
    -- it is (#1165).
    referenced :: Int -> Int -> T.Text -> Bool -> IO ()
    referenced minted req attrName doReduce = case channel of
      Closed -> throwIO (AtomMute func described "it asks phino for an attribute of a previous request, while its stdin is closed, since its entry does not say 'serve'")
      Open -> do
        spoken' <- describe
        case spoken' of
          Left failure -> throwIO (AtomMute func described failure)
          Right value -> do
            logDebug (printf "Atom '%s' asks phino for '%s' of request %d%s as question %d" (T.unpack func) (T.unpack attrName) req (if doReduce then ", reduced," else ", as it is," :: String) minted)
            answer <- if doReduce then reduce value else pure value
            said (lined (object ["id" .= minted, "𝑛" .= spelled answer]))
      where
        described :: String
        described = printf "{'of':%d,'attr':'%s'}" req (T.unpack attrName)
        describe :: IO (Either String Expression)
        describe = do
          forms <- readIORef _forms
          pure $ case IM.lookup req forms of
            Nothing -> Left (printf "there is no in-flight request %d to take '%s' from" req (T.unpack attrName))
            Just form' -> case attributeValue attrName form' of
              Nothing -> Left (printf "the receiver of request %d carries no attribute '%s'" req (T.unpack attrName))
              Just value -> Right value
        attributeValue :: T.Text -> Expression -> Maybe Expression
        attributeValue name (ExFormation bds) = go bds
          where
            go :: [Binding] -> Maybe Expression
            go [] = Nothing
            go (BiTau attr value : rest)
              | T.pack (printAttribute attr) == name = Just value
              | otherwise = go rest
            go (_ : rest) = go rest
        attributeValue _ _ = Nothing
    -- Reduce the 𝜑-expression the program asks about and say it back under
    -- '𝑛', with the 'id' the question minted. A program started for the fire
    -- has nothing to be answered over, since phino closed its stdin behind the
    -- request, so its question fails the fire instead of hanging it.
    served :: Int -> T.Text -> IO ()
    served minted raw = case channel of
      Closed -> throwIO (AtomMute func (T.unpack raw) "it asks phino to reduce an expression, while its stdin is closed, since its entry does not say 'serve'")
      Open -> do
        logDebug (printf "Atom '%s' asks phino to reduce '%s' as question %d" (T.unpack func) (T.unpack raw) minted)
        target <- either (unreadable raw) pure (parseExpression (T.unpack raw))
        answer <- reduce target
        said (lined (object ["id" .= minted, "𝑛" .= spelled answer]))
    unreadable :: T.Text -> String -> IO a
    unreadable raw failure = throwIO (AtomMute func (T.unpack raw) (printf "it asks phino to reduce an expression that does not parse: %s" failure))
    -- A program that has died leaves the write with nobody to drain it. The
    -- failure worth reporting is the one the program made, so a broken pipe is
    -- swallowed here and the read that follows finds out.
    said :: BS.ByteString -> IO ()
    said content = (BS.hPut _input content >> pushed channel _input) `catch` unheard
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

-- Push the request through the channel: closing the stdin of a program started
-- for the fire is the cue a program reading its input whole waits for, while a
-- program kept for the run reads on and needs no more than a flush.
pushed :: Channel -> Handle -> IO ()
pushed Closed = hClose
pushed Open = hFlush

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
