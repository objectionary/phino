{-# LANGUAGE FlexibleInstances #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Printer (printExpression, printProgram) where

import Ast
import Prettyprinter
import Prettyprinter.Render.String (renderString)

prettyMeta :: String -> Doc ann
prettyMeta meta = pretty "!" <> pretty meta

instance Pretty Attribute where
  pretty (AtLabel name) = pretty name
  pretty (AtAlpha index) = pretty "~" <> pretty index
  pretty AtRho = pretty "^"
  pretty AtPhi = pretty "@"
  pretty (AtMeta meta) = prettyMeta meta

instance Pretty Binding where
  pretty (BiTau attr expr) = pretty attr <+> pretty "->" <+> pretty expr
  pretty (BiMeta meta) = prettyMeta meta
  pretty (BiDelta bytes) = pretty "D>" <+> pretty bytes
  pretty (BiMetaDelta meta) = pretty "D>" <+> prettyMeta meta
  pretty (BiVoid attr) = pretty attr <+> pretty "-> ?"
  pretty (BiLambda func) = pretty "L>" <+> pretty func
  pretty (BiMetaLambda meta) = pretty "L>" <+> prettyMeta meta

instance {-# OVERLAPPING #-} Pretty [Binding] where
  pretty bindings = vsep (punctuate comma (map pretty bindings))

instance Pretty Expression where
  pretty (ExFormation []) = pretty "[[]]"
  pretty (ExFormation bindings) = vsep [pretty "[[", indent 2 (pretty bindings), pretty "]]"]
  pretty ExThis = pretty "$"
  pretty ExGlobal = pretty "Q"
  pretty ExTermination = pretty "T"
  pretty (ExMeta meta) = prettyMeta meta
  pretty (ExApplication expr []) = pretty expr <> pretty "()"
  pretty (ExApplication expr taus) = pretty expr <> vsep [lparen, indent 2 (pretty taus), rparen]
  pretty (ExDispatch expr attr) = pretty expr <> pretty "." <> pretty attr
  pretty (ExMetaTail expr meta) = pretty expr <+> pretty "*" <+> prettyMeta meta

instance Pretty Program where
  pretty (Program expr) = pretty "Q ->" <+> pretty expr

prettyPrint :: Pretty a => a -> String
prettyPrint printable = renderString (layoutPretty defaultLayoutOptions (pretty printable))

printExpression :: Expression -> String
printExpression = prettyPrint

printProgram :: Program -> String
printProgram = prettyPrint
