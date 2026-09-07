-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module CLI.Validators where

import AST
import CLI.Types
import Control.Exception
import Control.Monad (forM_, when, (>=>))
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Must
import Parser (parseExpressionThrows)
import Printer
import Text.Printf (printf)

invalidCLIArguments :: String -> IO a
invalidCLIArguments msg = throwIO (InvalidCLIArguments msg)

-- Validate given expressions as valid dispatches
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
                (printExpression' expr logPrintConfig)
            )

-- Reject a --show locator that is also hidden via --hide: 'exclude' runs over
-- the result of 'include', so an overlap would silently wipe the very subtree
-- --show was meant to keep.
validateNoOverlap :: String -> [Expression] -> String -> [Expression] -> IO ()
validateNoOverlap showOpt shown hideOpt hidden =
  for_ shown $ \shown' ->
    for_ hidden $ \hidden' ->
      when (printExpression shown' == printExpression hidden') $
        invalidCLIArguments
          ( printf
              "The --%s locator '%s' is also listed in --%s, which would hide it from the result"
              showOpt
              (printExpression shown')
              hideOpt
          )

-- Validate LaTeX options
validateLatexOptions :: IOFormat -> [(Bool, String)] -> [(Maybe String, String)] -> [(Maybe Int, String)] -> IO ()
validateLatexOptions LATEX _ _ _ = pure ()
validateLatexOptions _ bools strings ints = do
  let (bools', opts) = unzip bools
      msg = "The --%s option can stay together with --output=latex only"
      callback :: (Maybe a, String) -> IO ()
      callback (maybe', opt) = when (isJust maybe') (invalidCLIArguments (printf msg opt))
  validateBoolOpts (zip bools' (map (printf msg) opts))
  forM_ strings callback
  forM_ ints callback

-- Validate 'must' option
validateMust' :: Must -> IO ()
validateMust' must = for_ (validateMust must) invalidCLIArguments

-- Validate options for output to XMIR
validateXmirOptions :: IOFormat -> [(Bool, String)] -> String -> IO ()
validateXmirOptions XMIR _ focus = when (focus /= "Q") (invalidCLIArguments "Only --focus=Q is allowed to be used with --output=xmir")
validateXmirOptions _ bools _ =
  let (bools', opts) = unzip bools
   in validateBoolOpts (zip bools' (map (printf "The --%s can be used only with --output=xmir") opts))

-- Check that an expression is printable as XMIR: its top level must be a
-- single binding followed by ρ ↦ ∅ (the shape 'expressionToXMIR' accepts).
-- Called right after parsing, so a bad shape fails before any rewriting or
-- dataization work instead of at print time (issue #1082).
validateXmirTopLevel :: IOFormat -> Expression -> IO ()
validateXmirTopLevel XMIR (ExFormation [_, BiVoid AtRho]) = pure ()
validateXmirTopLevel XMIR expr =
  invalidCLIArguments
    ( printf
        "Expression cannot be printed with --output=xmir: its top level must be a single binding followed by ρ ↦ ∅, but got: %s"
        (printExpression expr)
    )
validateXmirTopLevel _ _ = pure ()

validateBoolOpts :: [(Bool, String)] -> IO ()
validateBoolOpts bools = forM_ bools (\(bool, msg) -> when bool (invalidCLIArguments msg))
