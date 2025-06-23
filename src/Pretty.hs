{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Pretty
  ( prettyExpression,
    prettyProgram,
    prettyProgram',
    prettyAttribute,
    prettySubsts,
    prettyBinding,
    PrintMode (SWEET, SALTY),
  )
where

import Ast
import qualified Data.Map.Strict as Map
import Matcher
import Prettyprinter
import Prettyprinter.Render.String (renderString)

data PrintMode = SWEET | SALTY
  deriving (Eq)

instance Show PrintMode where
  show SWEET = "sweet"
  show SALTY = "salty"

newtype Formatted a = Formatted {unFormatted :: (PrintMode, a)}

-- Minimal matcher function (required for view pattern)
matchDataoObject :: Expression -> Maybe (String, String)
matchDataoObject
  ( ExApplication
      (ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel label))
      ( BiTau
          (AtAlpha 0)
          ( ExApplication
              (ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel "bytes"))
              ( BiTau
                  (AtAlpha 0)
                  (ExFormation [BiDelta bts, BiVoid AtRho])
                )
            )
        )
    ) = Just (label, bts)
matchDataoObject _ = Nothing

pattern DataObject :: String -> String -> Expression
pattern DataObject label bts <- (matchDataoObject -> Just (label, bts))
  where
    DataObject label bts =
      ExApplication
        (ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel label))
        ( BiTau
            (AtAlpha 0)
            ( ExApplication
                (ExDispatch (ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang")) (AtLabel "bytes"))
                ( BiTau
                    (AtAlpha 0)
                    (ExFormation [BiDelta bts, BiVoid AtRho])
                )
            )
        )

prettyMeta :: String -> Doc ann
prettyMeta meta = pretty "!" <> pretty meta

prettyArrow :: Doc ann
prettyArrow = pretty "↦"

prettyLsb :: Doc ann
prettyLsb = pretty "⟦"

prettyRsb :: Doc ann
prettyRsb = pretty "⟧"

prettyDashedArrow :: Doc ann
prettyDashedArrow = pretty "⤍"

instance Pretty Attribute where
  pretty (AtLabel name) = pretty name
  pretty (AtAlpha index) = pretty "α" <> pretty index
  pretty AtRho = pretty "ρ"
  pretty AtPhi = pretty "φ"
  pretty (AtMeta meta) = prettyMeta meta

instance Pretty (Formatted Binding) where
  pretty (Formatted (SWEET, BiTau attr expr)) = pretty ""

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

instance Pretty (Formatted Expression) where
  pretty (Formatted (SWEET, ExFormation [])) = pretty "⟦⟧"
  pretty (Formatted (SWEET, ExFormation [binding])) = case binding of
    BiTau _ _ -> vsep [pretty "⟦", indent 2 (pretty binding), pretty "⟧"]
    _ -> pretty "⟦" <+> pretty binding <+> pretty "⟧"
  pretty (Formatted (SWEET, ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang"))) = pretty "Φ̇"
  pretty (Formatted (SWEET, DataObject "string" bytes)) = pretty "\"" <> pretty "\""
  pretty (Formatted (SWEET, DataObject "number" bytes)) = pretty ""
  pretty (Formatted (SWEET, DataObject other bytes)) = pretty (DataObject other bytes)
  pretty (Formatted (_, expr)) = pretty expr

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

instance Pretty (Formatted Program) where
  pretty (Formatted (SALTY, Program expr)) = pretty "Φ" <+> prettyArrow <+> pretty expr
  pretty (Formatted (SWEET, Program expr)) = vsep [pretty "{", indent 2 (pretty (Formatted (SWEET, expr))), pretty "}"]

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

instance {-# OVERLAPPING #-} Pretty [Subst] where
  pretty [] = pretty "[]"
  pretty substs = vsep [pretty "[", indent 2 (vsep (punctuate comma (map pretty substs))), pretty "]"]

render :: (Pretty a) => a -> String
render printable = renderString (layoutPretty defaultLayoutOptions (pretty printable))

prettyBinding :: Binding -> String
prettyBinding = render

prettyAttribute :: Attribute -> String
prettyAttribute = render

prettySubsts :: [Subst] -> String
prettySubsts = render

prettyExpression :: Expression -> String
prettyExpression = render

prettyProgram :: Program -> String
prettyProgram prog = render (Formatted (SALTY, prog))

prettyProgram' :: Program -> PrintMode -> String
prettyProgram' prog mode = render (Formatted (mode, prog))