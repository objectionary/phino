{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Fixtures
  ( defaultReduceContext
  , explainPack
  , fixtureLambdas
  , lambdasFile
  , linked
  , loopingLambdas
  , overdue
  , primitives
  , readProtocol
  , readUtf8
  , recorded
  , recorded'
  , withLambdas
  , withLambdasOf
  , withTemp
  )
where

import AST (Expression (ExRoot))
import CLI.Helpers (withEvalFunc)
import CLI.Types (IOFormat (PHI), PrintContext (PrintCtx))
import Compiled (compiled)
import Control.Exception (bracket, evaluate)
import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.IORef (newIORef)
import Data.List (isPrefixOf, stripPrefix)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml qualified as Yaml
import Dataize (reduction)
import Deps (Judgment (..), SaveEvalFunc, dontSaveEval, dontSaveStep)
import Engine (Engine, building, yaml)
import Evaluate (evaluation, fired)
import GHC.Clock (getMonotonicTime)
import Lambdas (Lambdas, emptyLambdas, readLambdas)
import Lining (LineFormat (MULTILINE))
import Morph (Deadline (..), ReduceContext (..), Steps (..))
import Sugar (SugarType (SWEET))
import System.Directory (getTemporaryDirectory, removePathForcibly)
import System.FilePath (takeExtension)
import System.IO (Handle, IOMode (ReadMode), hClose, hGetContents, hSetEncoding, openBinaryTempFile, utf8, withFile)
import XMIR (defaultXmirContext)

defaultReduceContext :: Expression -> IO ReduceContext
defaultReduceContext loc = do
  minted <- newIORef 0
  pure (ReduceContext loc loc Nothing 25 25 (Steps 250 0) Nothing minted Nothing Nothing 1 False True False False 1 Nothing Morphing [] Map.empty emptyLambdas (building linked) reduction evaluation fired dontSaveStep dontSaveEval linked)

linked :: Engine
linked = fromMaybe yaml compiled

withLambdas :: Lambdas -> ReduceContext -> ReduceContext
withLambdas lambdas ctx = ctx{_symbolic = lambdas}

lambdasFile :: FilePath
lambdasFile = "test-resources/atoms.yaml"

fixtureLambdas :: IO Lambdas
fixtureLambdas = readLambdas lambdasFile

loopingLambdas :: (FilePath -> IO a) -> IO a
loopingLambdas = withLambdasOf "- λ: L_loop\n  𝑛: ⟦ λ ⤍ L_loop ⟧\n"

overdue :: Int -> IO Deadline
overdue cap = Deadline cap . subtract 1 <$> getMonotonicTime

withLambdasOf :: T.Text -> (FilePath -> IO a) -> IO a
withLambdasOf lambdas = withTemp "phino-symbolic-.yaml" (encodeUtf8 lambdas)

primitives :: String -> String
primitives src =
  unlines
    [ "[["
    , "  bytes -> [["
    , "    φ -> ?,"
    , "    not -> [[ ^ -> ?, L> L_bytes_not ]],"
    , "    eq -> [[ ^ -> ?, b -> ?, L> L_bytes_eq ]]"
    , "  ]],"
    , "  bool -> [["
    , "    φ -> ?,"
    , "    if -> [[ ^ -> ?, then -> ?, else -> ?, L> L_fork ]]"
    , "  ]],"
    , "  number -> [["
    , "    φ -> ?,"
    , "    as-bytes -> $.φ,"
    , "    plus -> [[ ^ -> ?, x -> ?, L> L_number_plus ]],"
    , "    times -> [[ ^ -> ?, x -> ?, L> L_number_times ]],"
    , "    div -> [[ ^ -> ?, x -> ?, L> L_number_div ]],"
    , "    gt -> [[ ^ -> ?, x -> ?, L> L_number_gt ]],"
    , "    eq -> [[ ^ -> ?, x -> ?, @ -> $.^.as-bytes.eq( x.as-bytes ) ]],"
    , "    nope -> [[ ^ -> ?, L> L_number_nope ]]"
    , "  ]],"
    , "  @ -> " ++ src
    , "]]"
    ]

recorded :: (SaveEvalFunc -> IO a) -> IO (a, String)
recorded = recorded' False

recorded' :: Bool -> (SaveEvalFunc -> IO a) -> IO (a, String)
recorded' hidden action =
  withTemp "phino-protocol-.txt" BS.empty $ \path -> do
    answer <- withEvalFunc (Just path) printing action
    written <- withoutTotals <$> readUtf8 path
    pure (answer, written)
  where
    printing :: PrintContext
    printing =
      PrintCtx
        SWEET
        hidden
        Nothing
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

newtype ExplainPack = ExplainPack String

instance FromJSON ExplainPack where
  parseJSON = withObject "ExplainPack" (\pack -> ExplainPack <$> pack .: "latex")

explainPack :: FilePath -> IO String
explainPack path = do
  ExplainPack latex <- Yaml.decodeFileThrow path
  pure latex

readUtf8 :: FilePath -> IO String
readUtf8 path =
  withFile path ReadMode $ \stream -> do
    hSetEncoding stream utf8
    content <- hGetContents stream
    _ <- evaluate (length content)
    pure content

readProtocol :: FilePath -> IO String
readProtocol path = sansTotals <$> readUtf8 path
  where
    sansTotals :: String -> String
    sansTotals
      | map toLower (takeExtension path) == ".xml" = withoutWrapper
      | otherwise = withoutTotals

withoutTotals :: String -> String
withoutTotals text
  | [msec, firings, fps] <- drop (length ls - 3) ls
  , "msec(" `isPrefixOf` msec
  , "firings(" `isPrefixOf` firings
  , "fps(" `isPrefixOf` fps =
      unlines (take (length ls - 3) ls)
  | otherwise = text
  where
    ls = lines text

withoutWrapper :: String -> String
withoutWrapper text = case lines text of
  (decl : "<protocol>" : rest)
    | Just kept <- withoutRunTotals rest -> unlines (decl : map dedented kept)
  _ -> text
  where
    withoutRunTotals :: [String] -> Maybe [String]
    withoutRunTotals rest = case reverse rest of
      (closing : fps : firings : msec : kept)
        | closing == "</protocol>"
        , "<fps>" `isPrefixOf` dropWhile (== ' ') fps
        , "<firings>" `isPrefixOf` dropWhile (== ' ') firings
        , "<msec>" `isPrefixOf` dropWhile (== ' ') msec ->
            Just (reverse kept)
      _ -> Nothing
    dedented :: String -> String
    dedented line = fromMaybe line (stripPrefix "  " line)

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
