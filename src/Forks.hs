{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Forks (Decision, Forks, decide, explored, joined, straight) where

import AST (Argument (..), Binding (..), Bytes, Expression (ExApplication, ExDispatch, ExFormation, ExPhiAgain, ExPhiMeet), Function (FnSymbol))
import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Deps (RewindEvalFunc, Side (..), dontRewindEval)
import GHC.Exts (PromptTag#, control0#, newPromptTag#, prompt#)
import GHC.IO (IO (..))

type Decision = (Either Int Bytes, Side)

type Joining = (Int, Map (Int, Int) Int, [(Int, (Int, Int))])

data Tag = Tag (PromptTag# ())

data Forks = Forks
  { _tag :: Tag
  , _decided :: IORef (Map (Either Int Bytes) Side)
  , _pending :: IORef [IO ()]
  , _room :: IORef (Maybe Int)
  , _rewind :: RewindEvalFunc
  }

straight :: IO Forks
straight = opened (Just 1) dontRewindEval

decide :: Forks -> IO (IO ()) -> Either Int Bytes -> IO Side
decide forks marked condition = do
  decided <- readIORef forks._decided
  maybe (undecided decided) pure (Map.lookup condition decided)
  where
    undecided :: Map (Either Int Bytes) Side -> IO Side
    undecided decided = do
      room <- readIORef forks._room
      if room == Just 0
        then taken decided LeftSide
        else do
          writeIORef forks._room (pred <$> room)
          captured forks._tag $ \resumed -> do
            restored <- marked
            rewound <- forks._rewind
            modifyIORef' forks._pending (resumed (restored >> rewound >> taken decided RightSide) :)
            prompted forks._tag (resumed (taken decided LeftSide))
    taken :: Map (Either Int Bytes) Side -> Side -> IO Side
    taken decided side = side <$ writeIORef forks._decided (Map.insert condition side decided)

explored :: forall answer. Maybe Int -> RewindEvalFunc -> (Forks -> IO answer) -> IO [answer]
explored cap rewind action = do
  forks <- opened cap rewind
  answers <- newIORef []
  prompted forks._tag (action forks >>= \answer -> modifyIORef' answers (answer :))
  go forks
  reverse <$> readIORef answers
  where
    go :: Forks -> IO ()
    go forks = do
      queued <- atomicModifyIORef' forks._pending ([],)
      unless (null queued) (mapM_ (prompted forks._tag) (reverse queued) >> go forks)

opened :: Maybe Int -> RewindEvalFunc -> IO Forks
opened cap rewind = Forks <$> tagged <*> newIORef Map.empty <*> newIORef [] <*> newIORef (pred <$> cap) <*> pure rewind
  where
    tagged :: IO Tag
    tagged = IO (\world -> case newPromptTag# world of (# world', tag #) -> (# world', Tag tag #))

prompted :: Tag -> IO () -> IO ()
prompted (Tag tag) (IO body) = IO (prompt# tag body)

captured :: Tag -> ((IO Side -> IO ()) -> IO ()) -> IO Side
captured (Tag tag) handler = IO (control0# tag (\resume -> case handler (\(IO side) -> IO (resume side)) of IO body -> body))

joined :: Expression -> Expression -> Int -> Maybe (Expression, [(Int, (Int, Int))], Int)
joined left right spent = (\(term, (spent', _, made)) -> (term, reverse made, spent')) <$> goExpr left right (spent, Map.empty, [])
  where
    goExpr :: Expression -> Expression -> Joining -> Maybe (Expression, Joining)
    goExpr (ExFormation one) (ExFormation two) joining = first ExFormation <$> goList goBinding one two joining
    goExpr (ExApplication one arg) (ExApplication two arg') joining = do
      (target, joining') <- goExpr one two joining
      first (ExApplication target) <$> goArgument arg arg' joining'
    goExpr (ExDispatch one attr) (ExDispatch two attr') joining
      | attr == attr' = first (`ExDispatch` attr) <$> goExpr one two joining
    goExpr (ExPhiMeet prefix idx one) (ExPhiMeet prefix' idx' two) joining
      | prefix == prefix' && idx == idx' = first (ExPhiMeet prefix idx) <$> goExpr one two joining
    goExpr (ExPhiAgain prefix idx one) (ExPhiAgain prefix' idx' two) joining
      | prefix == prefix' && idx == idx' = first (ExPhiAgain prefix idx) <$> goExpr one two joining
    goExpr one two joining = same one two joining
    goBinding :: Binding -> Binding -> Joining -> Maybe (Binding, Joining)
    goBinding (BiTau attr one) (BiTau attr' two) joining
      | attr == attr' = first (BiTau attr) <$> goExpr one two joining
    goBinding (BiLambda (FnSymbol one)) (BiLambda (FnSymbol two)) joining
      | one /= two = Just (first (BiLambda . FnSymbol) (picked (one, two) joining))
    goBinding one two joining = same one two joining
    goArgument :: Argument -> Argument -> Joining -> Maybe (Argument, Joining)
    goArgument (ArTau attr one) (ArTau attr' two) joining
      | attr == attr' = first (ArTau attr) <$> goExpr one two joining
    goArgument (ArAlpha alpha one) (ArAlpha alpha' two) joining
      | alpha == alpha' = first (ArAlpha alpha) <$> goExpr one two joining
    goArgument one two joining = same one two joining
    goList :: (item -> item -> Joining -> Maybe (item, Joining)) -> [item] -> [item] -> Joining -> Maybe ([item], Joining)
    goList _ [] [] joining = Just ([], joining)
    goList walk (one : ones) (two : twos) joining = do
      (item, joining') <- walk one two joining
      first (item :) <$> goList walk ones twos joining'
    goList _ _ _ _ = Nothing
    same :: (Eq item) => item -> item -> Joining -> Maybe (item, Joining)
    same one two joining
      | one == two = Just (one, joining)
      | otherwise = Nothing
    picked :: (Int, Int) -> Joining -> (Int, Joining)
    picked pair joining@(spent', names, made) = case Map.lookup pair names of
      Just name -> (name, joining)
      Nothing -> (succ spent', (succ spent', Map.insert pair (succ spent') names, (succ spent', pair) : made))
