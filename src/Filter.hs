{-# HLINT ignore "Avoid restricted module" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Filter (hide, show) where

import AST
import Logger (logDebug)
import Misc
import Printer (logPrintConfig, printExpression')
import Rewriter
import Text.Printf (printf)
import Prelude hiding (show)
import Merge (merge)

fqnToAttrs :: Expression -> [Attribute]
fqnToAttrs = reverse . fqnToAttrs'
  where
    fqnToAttrs' :: Expression -> [Attribute]
    fqnToAttrs' (ExDispatch ExGlobal attr) = [attr]
    fqnToAttrs' (ExDispatch expr attr) = attr : fqnToAttrs' expr
    fqnToAttrs' expr = []

filter' :: (Maybe Attribute -> Maybe Attribute -> Bool) -> Program -> [Expression] -> Program
filter' _ prog [] = prog
filter' cmp (Program expr@(ExFormation _)) (fqn : rest) = filter' cmp (Program (filteredFormation expr (fqnToAttrs fqn))) rest
  where
    filteredFormation :: Expression -> [Attribute] -> Expression
    filteredFormation (ExFormation bds) [attr] =
      let bds' = [bd | bd <- bds, cmp (attributeFromBinding bd) (Just attr)]
       in ExFormation bds'
    filteredFormation (ExFormation bds) attrs = ExFormation (filteredBindings bds attrs)
      where
        filteredBindings :: [Binding] -> [Attribute] -> [Binding]
        filteredBindings [] _ = []
        filteredBindings (bd@(BiTau attr form@(ExFormation _)) : bds) attrs@(attr' : rest)
          | attr == attr' = BiTau attr (filteredFormation form rest) : bds
          | otherwise = bd : filteredBindings bds attrs
        filteredBindings (bd : bds) attrs = bd : filteredBindings bds attrs
    filteredFormation expr _ = expr
filter' _ prog _ = prog

hide :: [Rewritten] -> [Expression] -> [Rewritten]
hide [] _ = []
hide ((program, maybeRule) : rest) exprs = (filter' (/=) program exprs, maybeRule) : hide rest exprs

show :: [Rewritten] -> [Expression] -> IO [Rewritten]
show [] _ = pure []
-- show rewrs exprs =
--   let x =
--         map
--           ( \(prog, mr) -> do
--               let progs = map (\expr -> filter' (==) prog [expr]) exprs
--               merge
--           )
--           rewrs
--    in (filter' (==) program exprs, maybeRule) : show rest exprs
