-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLI.Validators where

import AST
import CLI.Types
import Control.Exception
import Control.Monad (forM_, unless, when, (>=>))
import Data.Char (isAlphaNum)
import Data.Foldable (for_)
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Misc (fqnToAttrs)
import Must
import Parser (parseExpressionThrows)
import Printer
import Text.Printf (printf)

invalidCLIArguments :: String -> IO a
invalidCLIArguments msg = throwIO (InvalidCLIArguments msg)

validatedDispatches :: String -> [String] -> IO [Expression]
validatedDispatches opt = traverse (parseExpressionThrows >=> asDispatch)
  where
    asDispatch :: Expression -> IO Expression
    asDispatch expr = go expr
      where
        go :: Expression -> IO Expression
        go ex@ExRoot = pure ex
        go disp@(ExDispatch ex _) = go ex >> pure disp
        go _ =
          invalidCLIArguments
            ( printf
                "Only dispatch expression started with Φ (or Q) can be used in --%s, but given: %s"
                opt
                (printExpression' expr saltyLogPrintConfig)
            )

validateNoOverlap :: String -> [Expression] -> String -> [Expression] -> IO ()
validateNoOverlap showOpt shown hideOpt hidden =
  for_ shown $ \shown' ->
    for_ hidden $ \hidden' -> do
      when (printExpression shown' == printExpression hidden') $
        invalidCLIArguments
          ( printf
              "The --%s locator '%s' is also listed in --%s, which would hide it from the result"
              showOpt
              (printExpression shown')
              hideOpt
          )
      when (inside (fqnToAttrs shown') (fqnToAttrs hidden')) $
        invalidCLIArguments
          ( printf
              "The --%s locator '%s' lies inside the --%s locator '%s', which would hide it from the result"
              showOpt
              (printExpression shown')
              hideOpt
              (printExpression hidden')
          )
  where
    inside :: Maybe [Attribute] -> Maybe [Attribute] -> Bool
    inside (Just inner) (Just outer) = length outer < length inner && outer `isPrefixOf` inner
    inside _ _ = False

validateLatexOptions :: IOFormat -> [(Bool, String)] -> [(Maybe String, String)] -> [(Maybe Int, String)] -> IO ()
validateLatexOptions LATEX _ strings _ = forM_ strings validateLatexString
  where
    validateLatexString (Just label, "label") =
      unless (all safeLabelCharacter label) $
        invalidCLIArguments
          "The --label option must contain only letters, numbers, colons, periods, underscores, and hyphens"
    validateLatexString _ = pure ()
    safeLabelCharacter character = isAlphaNum character || character `elem` ":._-"
validateLatexOptions _ bools strings ints = do
  let (bools', opts) = unzip bools
      msg = "The --%s option can stay together with --output=latex only"
      callback :: (Maybe a, String) -> IO ()
      callback (maybe', opt) = when (isJust maybe') (invalidCLIArguments (printf msg opt))
  validateBoolOpts (zip bools' (map (printf msg) opts))
  forM_ strings callback
  forM_ ints callback

validateMust' :: Must -> IO ()
validateMust' must = for_ (validateMust must) invalidCLIArguments

validateXmirOptions :: IOFormat -> [(Bool, String)] -> String -> IO ()
validateXmirOptions XMIR _ focus = when (focus /= "Q") (invalidCLIArguments "Only --focus=Q is allowed to be used with --output=xmir")
validateXmirOptions _ bools _ =
  let (bools', opts) = unzip bools
   in validateBoolOpts (zip bools' (map (printf "The --%s can be used only with --output=xmir") opts))

validateXmirTopLevel :: IOFormat -> Expression -> IO ()
validateXmirTopLevel XMIR (ExFormation [_]) = pure ()
validateXmirTopLevel XMIR (ExFormation [_, BiVoid AtRho]) = pure ()
validateXmirTopLevel XMIR (ExFormation [BiVoid AtRho, _]) = pure ()
validateXmirTopLevel XMIR expr =
  invalidCLIArguments
    ( printf
        "Expression cannot be printed with --output=xmir: its top level must be a single binding, but got: %s"
        (printExpression expr)
    )
validateXmirTopLevel _ _ = pure ()

validateBoolOpts :: [(Bool, String)] -> IO ()
validateBoolOpts bools = forM_ bools (\(bool, msg) -> when bool (invalidCLIArguments msg))
