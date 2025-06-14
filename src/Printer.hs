{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Printer (printExpression, printProgram, printSubstitutions, printCondition) where

import Ast
import qualified Data.Map.Strict as Map
import Matcher
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Text.Printf (vFmt)
import Yaml (Condition)
import qualified Yaml as Y

prettyMeta :: String -> Doc ann
prettyMeta meta = pretty "!" <> pretty meta

prettyArrow :: Doc ann
prettyArrow = pretty "↦"

prettyDashedArrow :: Doc ann
prettyDashedArrow = pretty "⤍"

instance Pretty Attribute where
  pretty (AtLabel name) = pretty name
  pretty (AtAlpha index) = pretty "α" <> pretty index
  pretty AtRho = pretty "ρ"
  pretty AtPhi = pretty "φ"
  pretty (AtMeta meta) = prettyMeta meta

instance Pretty Binding where
  pretty (BiTau attr expr) = pretty attr <+> prettyArrow <+> pretty expr
  pretty (BiMeta meta) = prettyMeta meta
  pretty (BiDelta bytes) = pretty "Δ" <+> prettyDashedArrow <+> pretty bytes
  pretty (BiMetaDelta meta) = pretty "Δ" <+> prettyDashedArrow <+> prettyMeta meta
  pretty (BiVoid attr) = pretty attr <+> prettyArrow <+> pretty "∅"
  pretty (BiLambda func) = pretty "λ" <+> prettyDashedArrow <+> pretty func
  pretty (BiMetaLambda meta) = pretty "λ" <+> prettyDashedArrow <+> prettyMeta meta

instance {-# OVERLAPPING #-} Pretty [Binding] where
  pretty bindings = vsep (punctuate comma (map pretty bindings))

instance Pretty Expression where
  pretty (ExFormation []) = pretty "⟦⟧"
  pretty (ExFormation [binding]) = case binding of
    BiTau _ _ -> vsep [pretty "⟦", indent 2 (pretty binding), pretty "⟧"]
    _ -> pretty "⟦" <+> pretty binding <+> pretty "⟧"
  pretty (ExFormation bindings) = vsep [pretty "⟦", indent 2 (pretty bindings), pretty "⟧"]
  pretty ExThis = pretty "ξ"
  pretty ExGlobal = pretty "Φ"
  pretty ExTermination = pretty "⊥"
  pretty (ExMeta meta) = prettyMeta meta
  pretty (ExApplication expr tau) = pretty expr <> vsep [lparen, indent 2 (pretty tau), rparen]
  pretty (ExDispatch expr attr) = pretty expr <> pretty "." <> pretty attr
  pretty (ExMetaTail expr meta) = pretty expr <+> pretty "*" <+> prettyMeta meta

instance Pretty Program where
  pretty (Program expr) = pretty "Φ" <+> prettyArrow <+> pretty expr

instance Pretty Tail where
  pretty (TaApplication tau) = vsep [lparen, indent 2 (pretty tau), rparen]
  pretty (TaDispatch attr) = pretty "." <> pretty attr

instance Pretty MetaValue where
  pretty (MvAttribute attr) = pretty attr
  pretty (MvBytes bytes) = pretty bytes
  pretty (MvBindings []) = pretty "[]"
  pretty (MvBindings bindings) = vsep [pretty "[", indent 2 (pretty bindings), pretty "]"]
  pretty (MvFunction func) = pretty func
  pretty (MvExpression expr) = pretty expr
  pretty (MvTail tails) = vsep (punctuate comma (map pretty tails))

instance Pretty Subst where
  pretty (Subst mp) =
    vsep
      [ lparen,
        indent
          2
          ( vsep
              ( punctuate
                  comma
                  ( map
                      (\(key, value) -> prettyMeta key <+> pretty ">>" <+> pretty value)
                      (Map.toList mp)
                  )
              )
          ),
        rparen
      ]

instance Pretty Condition where
  pretty (Y.NF expr) = pretty "NF(" <+> pretty expr <+> rparen

instance {-# OVERLAPPING #-} Pretty [Subst] where
  pretty :: [Subst] -> Doc ann
  pretty [] = pretty "[]"
  pretty substs = vsep [pretty "[", indent 2 (vsep (punctuate comma (map pretty substs))), pretty "]"]

prettyPrint :: (Pretty a) => a -> String
prettyPrint printable = renderString (layoutPretty defaultLayoutOptions (pretty printable))

printCondition :: Condition -> String
printCondition = prettyPrint

printSubstitutions :: [Subst] -> String
printSubstitutions = prettyPrint

printExpression :: Expression -> String
printExpression = prettyPrint

printProgram :: Program -> String
printProgram = prettyPrint
