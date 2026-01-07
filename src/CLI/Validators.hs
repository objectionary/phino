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
    asDispatch expr = asDispatch' expr
      where
        asDispatch' :: Expression -> IO Expression
        asDispatch' ex@ExGlobal = pure ex
        asDispatch' disp@(ExDispatch ex _) = asDispatch' ex >> pure disp
        asDispatch' _ =
          invalidCLIArguments
            ( printf
                "Only dispatch expression started with Φ (or Q) can be used in --%s, but given: %s"
                opt
                (printExpression' expr logPrintConfig)
            )

-- Validate LaTeX options
validateLatexOptions :: IOFormat -> [(Bool, String)] -> [(Maybe String, String)] -> [(Maybe Int, String)] -> IO ()
validateLatexOptions LATEX _ _ _ = pure ()
validateLatexOptions _ bools strings ints = do
  let (bools', opts) = unzip bools
      msg = "The --%s option can stay together with --output=latex only"
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

validateBoolOpts :: [(Bool, String)] -> IO ()
validateBoolOpts bools = forM_ bools (\(bool, msg) -> when bool (invalidCLIArguments msg))
