{-# LANGUAGE FlexibleInstances #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Pretty
  ( prettyExpression,
    prettyExpression',
    prettyProgram,
    prettyProgram',
    prettyAttribute,
    prettySubsts,
    prettySubsts',
    prettyBinding,
    prettyBytes,
    PrintMode (SWEET, SALTY),
  )
where

import Ast
import qualified Data.Map.Strict as Map
import Matcher
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Misc
import Data.List (intercalate)

data PrintMode = SWEET | SALTY
  deriving (Eq)

instance Show PrintMode where
  show SWEET = "sweet"
  show SALTY = "salty"

newtype Formatted a = Formatted {unFormatted :: (PrintMode, a)}

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

instance Pretty Bytes where
  pretty BtEmpty = pretty "--"
  pretty (BtOne bt) = pretty bt <> pretty "-"
  pretty (BtMany bts) = pretty (intercalate "-" bts)

instance Pretty Attribute where
  pretty (AtLabel name) = pretty name
  pretty (AtAlpha index) = pretty "α" <> pretty index
  pretty AtRho = pretty "ρ"
  pretty AtPhi = pretty "φ"
  pretty AtDelta = pretty "Δ"
  pretty AtLambda = pretty "λ"
  pretty (AtMeta meta) = prettyMeta meta

instance Pretty (Formatted Binding) where
  pretty (Formatted (SWEET, BiTau attr (ExFormation bindings))) =
    let voids' = voids bindings
     in if null voids'
          then pretty attr <+> prettyArrow <+> pretty (Formatted (SWEET, ExFormation bindings))
          else
            if length voids' == length bindings && last voids' == AtRho
              then inlineVoids (init voids') <+> prettyLsb <> prettyRsb
              else inlineVoids voids' <+> pretty (Formatted (SWEET, ExFormation (drop (length voids') bindings)))
    where
      voids :: [Binding] -> [Attribute]
      voids [] = []
      voids (bd : bds) = case bd of
        BiVoid attr -> attr : voids bds
        _ -> []
      inlineVoids :: [Attribute] -> Doc ann
      inlineVoids [] = pretty attr <+> prettyArrow
      inlineVoids voids' = pretty attr <> lparen <> hsep (punctuate comma (map pretty voids')) <> rparen <+> prettyArrow
  pretty (Formatted (mode, BiTau attr expr)) = pretty attr <+> prettyArrow <+> pretty (Formatted (mode, expr))
  pretty (Formatted (_, BiMeta meta)) = prettyMeta meta
  pretty (Formatted (_, BiDelta bytes)) = pretty "Δ" <+> prettyDashedArrow <+> pretty bytes
  pretty (Formatted (_, BiMetaLambda meta)) = pretty "λ" <+> prettyDashedArrow <+> prettyMeta meta
  pretty (Formatted (_, BiMetaDelta meta)) = pretty "Δ" <+> prettyDashedArrow <+> prettyMeta meta
  pretty (Formatted (_, BiVoid attr)) = pretty attr <+> prettyArrow <+> pretty "∅"
  pretty (Formatted (_, BiLambda func)) = pretty "λ" <+> prettyDashedArrow <+> pretty func

instance {-# OVERLAPPING #-} Pretty (Formatted [Binding]) where
  pretty (Formatted (SWEET, bds)) = vsep (punctuate comma (excludeVoidRho (\bd -> pretty (Formatted (SWEET, bd))) [] bds))
    where
      excludeVoidRho :: (Binding -> Doc ann) -> [Doc ann] -> [Binding] -> [Doc ann]
      excludeVoidRho func acc [bd] = case bd of
        BiVoid AtRho -> reverse acc
        _ -> reverse (func bd : acc)
      excludeVoidRho func acc (x : xs) = excludeVoidRho func (func x : acc) xs
      excludeVoidRho func acc [] = reverse acc
  pretty (Formatted (SALTY, bds)) = vsep (punctuate comma (map (\bd -> pretty (Formatted (SALTY, bd))) bds))

complexApplication :: Expression -> (Expression, [Binding], [Expression])
complexApplication (ExApplication (ExApplication expr tau) tau') =
  let (before, taus, exprs) = complexApplication (ExApplication expr tau)
      taus' = tau' : taus
   in if null exprs
        then (before, taus', [])
        else case tau' of
          BiTau (AtAlpha idx) expr' ->
            if idx == fromIntegral (length exprs)
              then (before, taus', expr' : exprs)
              else (before, taus', [])
          _ -> (before, taus', [])
complexApplication (ExApplication expr (BiTau (AtAlpha 0) expr')) = (expr, [BiTau (AtAlpha 0) expr'], [expr'])
complexApplication (ExApplication expr tau) = (expr, [tau], [])

instance Pretty (Formatted Expression) where
  pretty (Formatted (SWEET, ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang"))) = pretty "Φ̇"
  pretty (Formatted (SWEET, DataObject "string" bytes)) = pretty "\"" <> pretty (btsToStr bytes) <> pretty "\""
  pretty (Formatted (SWEET, DataObject "number" bytes)) = either pretty pretty (btsToNum bytes)
  pretty (Formatted (SWEET, DataObject other bytes)) = pretty (Formatted (SALTY, DataObject other bytes))
  pretty (Formatted (mode, ExFormation [binding])) = case binding of
    BiTau _ _ -> vsep [pretty "⟦", indent 2 (pretty (Formatted (mode, binding))), pretty "⟧"]
    _ -> pretty "⟦" <+> pretty (Formatted (mode, binding)) <+> pretty "⟧"
  pretty (Formatted (_, ExFormation [])) = pretty "⟦⟧"
  pretty (Formatted (mode, ExFormation bindings)) = vsep [pretty "⟦", indent 2 (pretty (Formatted (mode, bindings))), pretty "⟧"]
  pretty (Formatted (_, ExThis)) = pretty "ξ"
  pretty (Formatted (_, ExGlobal)) = pretty "Φ"
  pretty (Formatted (_, ExTermination)) = pretty "⊥"
  pretty (Formatted (_, ExMeta meta)) = prettyMeta meta
  pretty (Formatted (SWEET, ExApplication (ExApplication expr tau) tau')) =
    let (expr', taus, exprs) = complexApplication (ExApplication (ExApplication expr tau) tau')
        args =
          if null exprs
            then pretty (Formatted (SWEET, reverse taus))
            else vsep (punctuate comma (map (\exp -> pretty (Formatted (SWEET, exp))) (reverse exprs)))
     in pretty (Formatted (SWEET, expr')) <> vsep [lparen, indent 2 args, rparen]
  pretty (Formatted (SWEET, ExApplication expr tau)) =
    let arg = case tau of
          BiTau (AtAlpha 0) expr' -> pretty (Formatted (SWEET, expr'))
          _ -> pretty (Formatted (SWEET, tau))
     in pretty (Formatted (SWEET, expr)) <> vsep [lparen, indent 2 arg, rparen]
  pretty (Formatted (mode, ExApplication expr tau)) = pretty (Formatted (mode, expr)) <> vsep [lparen, indent 2 (pretty (Formatted (mode, tau))), rparen]
  pretty (Formatted (mode, ExDispatch expr attr)) = pretty (Formatted (mode, expr)) <> pretty "." <> pretty attr
  pretty (Formatted (mode, ExMetaTail expr meta)) = pretty (Formatted (mode, expr)) <+> pretty "*" <+> prettyMeta meta

instance Pretty (Formatted Program) where
  pretty (Formatted (SALTY, Program expr)) = pretty "Φ" <+> prettyArrow <+> pretty (Formatted (SALTY, expr))
  pretty (Formatted (SWEET, Program expr)) = pretty "{" <> pretty (Formatted (SWEET, expr)) <> pretty "}"

instance Pretty Tail where
  pretty (TaApplication tau) = vsep [lparen, indent 2 (pretty (Formatted (SALTY, tau))), rparen]
  pretty (TaDispatch attr) = pretty "." <> pretty attr

instance Pretty MetaValue where
  pretty (MvAttribute attr) = pretty attr
  pretty (MvBytes bytes) = pretty bytes
  pretty (MvBindings []) = pretty "[]"
  pretty (MvBindings bindings) = vsep [pretty "[", indent 2 (pretty (Formatted (SALTY, bindings))), pretty "]"]
  pretty (MvFunction func) = pretty func
  pretty (MvExpression expr _) = pretty (Formatted (SALTY, expr))
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

instance {-# OVERLAPPING #-} Pretty (Formatted [Subst]) where
  pretty (Formatted (_, [])) = pretty "[]"
  pretty (Formatted (mode, substs)) = vsep [pretty "[", indent 2 (vsep (punctuate comma (map pretty substs))), pretty "]"]

render :: (Pretty a) => a -> String
render printable = renderString (layoutPretty defaultLayoutOptions (pretty printable))

prettyBinding :: Binding -> String
prettyBinding binding = render (Formatted (SALTY, binding))

prettyBytes :: Bytes -> String
prettyBytes = render

prettyAttribute :: Attribute -> String
prettyAttribute = render

prettySubsts :: [Subst] -> String
prettySubsts = render

prettySubsts' :: [Subst] -> PrintMode -> String
prettySubsts' substs mode = render (Formatted (mode, substs))

prettyExpression :: Expression -> String
prettyExpression expr = render (Formatted (SALTY, expr))

prettyExpression' :: Expression -> String
prettyExpression' expr = render (Formatted (SWEET, expr))

prettyProgram :: Program -> String
prettyProgram prog = render (Formatted (SALTY, prog))

prettyProgram' :: Program -> PrintMode -> String
prettyProgram' prog mode = render (Formatted (mode, prog))