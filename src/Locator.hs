{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Locator (locatedExpression, withLocatedExpression) where

import AST
import Control.Exception (Exception, throwIO)
import Data.Functor ((<&>))
import Misc
import Printer (printExpression)
import Text.Printf (printf)

data LocatorException
  = InvalidLocatorProvided {fqn :: Expression}
  | CanNotFindObjectByLocator {fqn :: Expression}
  deriving (Exception)

instance Show LocatorException where
  show InvalidLocatorProvided{..} =
    printf
      "Invalid locator is provided. 'Q' or dispatch started with 'Q' expected, but got: '%s'"
      (printExpression fqn)
  show CanNotFindObjectByLocator{..} = printf "Can't find object by locator: '%s'" (printExpression fqn)

locatedExpression :: Expression -> Program -> IO Expression
locatedExpression ExGlobal (Program expr) = pure expr
locatedExpression locator (Program expr) = case fqnToAttrs locator of
  Just attrs -> locatedExpression' expr attrs
  _ -> invalidLocator locator
  where
    locatedExpression' :: Expression -> [Attribute] -> IO Expression
    locatedExpression' expr' [] = pure expr'
    locatedExpression' (ExFormation bds) [attr] = case locatedInBindings attr bds of
      Just expr' -> pure expr'
      _ -> cantFindBy locator
    locatedExpression' (ExFormation bds) (attr : rest) = case locatedInBindings attr bds of
      Just expr' -> locatedExpression' expr' rest
      _ -> cantFindBy locator
    locatedExpression' _ _ = cantFindBy locator

withLocatedExpression :: Expression -> Expression -> Program -> IO Program
withLocatedExpression ExGlobal tgt _ = pure (Program tgt)
withLocatedExpression locator target (Program expr) = case fqnToAttrs locator of
  Just attrs -> Program <$> withLocatedExpression' expr attrs
  _ -> invalidLocator locator
  where
    withLocatedExpression' :: Expression -> [Attribute] -> IO Expression
    withLocatedExpression' (ExFormation bds) [attr] = case locatedInBindings attr bds of
      Just _ -> pure (ExFormation (withReplacedExpression bds))
      _ -> cantFindBy locator
      where
        withReplacedExpression :: [Binding] -> [Binding]
        withReplacedExpression [] = []
        withReplacedExpression (bd@(BiTau at _) : bds')
          | at == attr = BiTau at target : bds'
          | otherwise = bd : withReplacedExpression bds'
        withReplacedExpression (bd@(BiVoid at) : bds')
          | at == attr = BiTau at target : bds'
          | otherwise = bd : withReplacedExpression bds'
        withReplacedExpression (bd : bds') = bd : withReplacedExpression bds'
    withLocatedExpression' (ExFormation bds) attrs = ExFormation <$> withLocatedBindings bds attrs
      where
        withLocatedBindings :: [Binding] -> [Attribute] -> IO [Binding]
        withLocatedBindings (bd@(BiTau at expr') : rbds) attrs'@(at' : rattrs)
          | at == at' = withLocatedExpression' expr' rattrs <&> (: rbds) . BiTau at
          | otherwise = withLocatedBindings rbds attrs' <&> (:) bd
        withLocatedBindings (bd : rbds) attrs' = (:) bd <$> withLocatedBindings rbds attrs'
        withLocatedBindings [] _ = cantFindBy locator
    withLocatedExpression' _ _ = cantFindBy locator

locatedInBindings :: Attribute -> [Binding] -> Maybe Expression
locatedInBindings _ [] = Nothing
locatedInBindings attr (BiTau attr' expr' : rest)
  | attr == attr' = Just expr'
  | otherwise = locatedInBindings attr rest
locatedInBindings attr (_ : rest) = locatedInBindings attr rest

invalidLocator :: Expression -> IO a
invalidLocator = throwIO <$> InvalidLocatorProvided

cantFindBy :: Expression -> IO a
cantFindBy = throwIO <$> CanNotFindObjectByLocator
