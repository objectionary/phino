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
    prettySubst,
    prettyBinding,
    prettyBytes,
    prettyExtraArg,
    PrintMode (SWEET, SALTY),
    Encoding (ASCII, UNICODE),
  )
where

import Ast
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Matcher
import Misc
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Yaml (ExtraArgument (..))

data PrintMode = SWEET | SALTY
  deriving (Eq)

instance Show PrintMode where
  show SWEET = "sweet"
  show SALTY = "salty"

data Encoding = ASCII | UNICODE

newtype Formatted a = Formatted {unFormatted :: (PrintMode, Encoding, a)}

formatted :: a -> Formatted a
formatted x = Formatted (SALTY, UNICODE, x)

prettyMeta :: String -> Doc ann
prettyMeta meta = pretty "!" <> pretty meta

prettyArrow :: Encoding -> Doc ann
prettyArrow UNICODE = pretty "↦"
prettyArrow ASCII = pretty "->"

prettyLsb :: Encoding -> Doc ann
prettyLsb UNICODE = pretty "⟦"
prettyLsb ASCII = pretty "[["

prettyRsb :: Encoding -> Doc ann
prettyRsb UNICODE = pretty "⟧"
prettyRsb ASCII = pretty "]]"

prettyDashedArrow :: Doc ann
prettyDashedArrow = pretty "⤍"

prettyLambda :: Encoding -> Doc ann
prettyLambda UNICODE = pretty "λ" <+> prettyDashedArrow
prettyLambda ASCII = pretty "L>"

prettyDelta :: Encoding -> Doc ann
prettyDelta UNICODE = pretty "Δ" <+> prettyDashedArrow
prettyDelta ASCII = pretty "D>"

instance Pretty ExtraArgument where
  pretty (ArgExpression expr) = pretty (Formatted (SWEET, UNICODE, expr))
  pretty (ArgBinding bd) = pretty (Formatted (SWEET, UNICODE, bd))
  pretty (ArgAttribute attr) = pretty (formatted attr)
  pretty (ArgBytes bytes) = pretty bytes

instance Pretty Bytes where
  pretty BtEmpty = pretty "--"
  pretty (BtOne bt) = pretty bt <> pretty "-"
  pretty (BtMany bts) = pretty (intercalate "-" bts)
  pretty (BtMeta meta) = prettyMeta meta

instance Pretty (Formatted Attribute) where
  pretty (Formatted (_, _, AtMeta meta)) = prettyMeta meta
  pretty (Formatted (_, ASCII, AtAlpha idx)) = pretty "~" <> pretty idx
  pretty (Formatted (_, ASCII, AtPhi)) = pretty "@"
  pretty (Formatted (_, ASCII, AtRho)) = pretty "^"
  pretty (Formatted (_, _, attr)) = pretty (show attr)

instance Pretty (Formatted Binding) where
  pretty (Formatted (SWEET, encoding, BiTau attr (ExFormation bindings))) =
    let voids' = voids bindings
     in if null voids'
          then pretty (Formatted (SWEET, encoding, attr)) <+> prettyArrow encoding <+> pretty (Formatted (SWEET, encoding, ExFormation bindings))
          else
            if length voids' == length bindings && last voids' == AtRho
              then inlineVoids (init voids') <+> prettyLsb encoding <> prettyRsb encoding
              else inlineVoids voids' <+> pretty (Formatted (SWEET, encoding, ExFormation (drop (length voids') bindings)))
    where
      voids :: [Binding] -> [Attribute]
      voids [] = []
      voids (bd : bds) = case bd of
        BiVoid attr -> attr : voids bds
        _ -> []
      inlineVoids :: [Attribute] -> Doc ann
      inlineVoids [] = pretty (Formatted (SWEET, encoding, attr)) <+> prettyArrow encoding
      inlineVoids voids' =
        pretty (Formatted (SWEET, encoding, attr))
          <> lparen
          <> hsep (punctuate comma (map (\attr -> pretty (Formatted (SWEET, encoding, attr))) voids'))
          <> rparen
          <+> prettyArrow encoding
  pretty (Formatted (mode, encoding, BiTau attr expr)) = pretty (Formatted (mode, encoding, attr)) <+> prettyArrow encoding <+> pretty (Formatted (mode, encoding, expr))
  pretty (Formatted (_, _, BiMeta meta)) = prettyMeta meta
  pretty (Formatted (_, encoding, BiDelta bytes)) = prettyDelta encoding <+> pretty bytes
  pretty (Formatted (_, encoding, BiLambda func)) = prettyLambda encoding <+> pretty func
  pretty (Formatted (_, encoding, BiMetaLambda meta)) = prettyLambda encoding <+> prettyMeta meta
  pretty (Formatted (mode, UNICODE, BiVoid attr)) = pretty (Formatted (mode, UNICODE, attr)) <+> prettyArrow UNICODE <+> pretty "∅"
  pretty (Formatted (mode, ASCII, BiVoid attr)) = pretty (Formatted (mode, ASCII, attr)) <+> prettyArrow ASCII <+> pretty "?"

-- >>> render (Formatted (SWEET, UNICODE, [BiVoid AtRho]))
-- ""
-- >>> render (Formatted (SWEET, UNICODE, [BiTau (AtLabel "x") ExGlobal, BiVoid AtPhi]))
-- "x \8614 \934,\n\966 \8614 \8709"
instance {-# OVERLAPPING #-} Pretty (Formatted [Binding]) where
  pretty (Formatted (SWEET, encoding, bds)) = vsep (punctuate comma (excludeVoidRho (\bd -> pretty (Formatted (SWEET, encoding, bd))) [] bds))
    where
      excludeVoidRho :: (Binding -> Doc ann) -> [Doc ann] -> [Binding] -> [Doc ann]
      excludeVoidRho func acc [bd] = case bd of
        BiVoid AtRho -> reverse acc
        _ -> reverse (func bd : acc)
      excludeVoidRho func acc (x : xs) = excludeVoidRho func (func x : acc) xs
      excludeVoidRho func acc [] = reverse acc
  pretty (Formatted (SALTY, encoding, bds)) = vsep (punctuate comma (map (\bd -> pretty (Formatted (SALTY, encoding, bd))) bds))

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

-- >>> render (Formatted (SWEET, ExFormation [BiVoid AtRho]))
-- "\10214\10215"
instance Pretty (Formatted Expression) where
  pretty (Formatted (SWEET, UNICODE, ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang"))) = pretty "Φ̇"
  pretty (Formatted (SWEET, ASCII, ExDispatch (ExDispatch ExGlobal (AtLabel "org")) (AtLabel "eolang"))) = pretty "QQ"
  pretty (Formatted (SWEET, _, DataString bytes)) = pretty "\"" <> pretty (btsToStr bytes) <> pretty "\""
  pretty (Formatted (SWEET, _, DataNumber bytes)) = either pretty pretty (btsToNum bytes)
  pretty (Formatted (SWEET, encoding, DataObject other bytes)) = pretty (Formatted (SALTY, encoding, DataObject other bytes))
  pretty (Formatted (SWEET, UNICODE, ExFormation [BiVoid AtRho])) = pretty "⟦⟧"
  pretty (Formatted (SWEET, ASCII, ExFormation [BiVoid AtRho])) = pretty "[[]]"
  pretty (Formatted (mode, encoding, ExFormation [binding])) = case binding of
    BiTau _ _ -> vsep [prettyLsb encoding, indent 2 (pretty (Formatted (mode, encoding, binding))), prettyRsb encoding]
    _ -> prettyLsb encoding <+> pretty (Formatted (mode, encoding, binding)) <+> prettyRsb encoding
  pretty (Formatted (_, UNICODE, ExFormation [])) = pretty "⟦⟧"
  pretty (Formatted (_, ASCII, ExFormation [])) = pretty "[[]]"
  pretty (Formatted (mode, encoding, ExFormation bindings)) = vsep [prettyLsb encoding, indent 2 (pretty (Formatted (mode, encoding, bindings))), prettyRsb encoding]
  pretty (Formatted (_, UNICODE, ExThis)) = pretty "ξ"
  pretty (Formatted (_, ASCII, ExThis)) = pretty "$"
  pretty (Formatted (_, UNICODE, ExGlobal)) = pretty "Φ"
  pretty (Formatted (_, ASCII, ExGlobal)) = pretty "Q"
  pretty (Formatted (_, UNICODE, ExTermination)) = pretty "⊥"
  pretty (Formatted (_, ASCII, ExTermination)) = pretty "T"
  pretty (Formatted (_, _, ExMeta meta)) = prettyMeta meta
  pretty (Formatted (SWEET, encoding, ExApplication (ExApplication expr tau) tau')) =
    let (expr', taus, exprs) = complexApplication (ExApplication (ExApplication expr tau) tau')
        args =
          if null exprs
            then pretty (Formatted (SWEET, encoding, reverse taus))
            else vsep (punctuate comma (map (\exp -> pretty (Formatted (SWEET, encoding, exp))) (reverse exprs)))
     in pretty (Formatted (SWEET, encoding, expr')) <> vsep [lparen, indent 2 args, rparen]
  pretty (Formatted (SWEET, encoding, ExApplication expr tau)) =
    let arg = case tau of
          BiTau (AtAlpha 0) expr' -> pretty (Formatted (SWEET, encoding, expr'))
          _ -> pretty (Formatted (SWEET, encoding, tau))
     in pretty (Formatted (SWEET, encoding, expr)) <> vsep [lparen, indent 2 arg, rparen]
  pretty (Formatted (mode, encoding, ExApplication expr tau)) = pretty (Formatted (mode, encoding, expr)) <> vsep [lparen, indent 2 (pretty (Formatted (mode, encoding, tau))), rparen]
  pretty (Formatted (mode, encoding, ExDispatch expr attr)) = pretty (Formatted (mode, encoding, expr)) <> pretty "." <> pretty (Formatted (mode, encoding, attr))
  pretty (Formatted (mode, encoding, ExMetaTail expr meta)) = pretty (Formatted (mode, encoding, expr)) <+> pretty "*" <+> prettyMeta meta

instance Pretty (Formatted Program) where
  pretty (Formatted (SALTY, encoding, Program expr)) = pretty "Φ" <+> prettyArrow encoding <+> pretty (Formatted (SALTY, encoding, expr))
  pretty (Formatted (SWEET, encoding, Program expr)) = pretty "{" <> pretty (Formatted (SWEET, encoding, expr)) <> pretty "}"

instance Pretty Tail where
  pretty (TaApplication tau) = vsep [lparen, indent 2 (pretty (formatted tau)), rparen]
  pretty (TaDispatch attr) = pretty "." <> pretty (formatted attr)

instance Pretty MetaValue where
  pretty (MvAttribute attr) = pretty (formatted attr)
  pretty (MvBytes bytes) = pretty bytes
  pretty (MvBindings []) = pretty "[]"
  pretty (MvBindings bindings) = vsep [pretty "[", indent 2 (pretty (formatted bindings)), pretty "]"]
  pretty (MvFunction func) = pretty func
  pretty (MvExpression expr _) = pretty (formatted expr)
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
  pretty (Formatted (_, _, [])) = pretty "[]"
  pretty (Formatted (mode, _, substs)) = vsep [pretty "[", indent 2 (vsep (punctuate comma (map pretty substs))), pretty "]"]

render :: (Pretty a) => a -> String
render printable = renderString (layoutPretty defaultLayoutOptions (pretty printable))

prettyBinding :: Binding -> String
prettyBinding binding = render (formatted binding)

prettyExtraArg :: ExtraArgument -> String
prettyExtraArg = render

prettyBytes :: Bytes -> String
prettyBytes = render

prettyAttribute :: Attribute -> String
prettyAttribute attr = render (formatted attr)

prettySubst :: Subst -> String
prettySubst = render

prettySubsts :: [Subst] -> String
prettySubsts = render

prettySubsts' :: [Subst] -> PrintMode -> Encoding -> String
prettySubsts' substs mode encoding = render (Formatted (mode, encoding, substs))

prettyExpression :: Expression -> String
prettyExpression expr = render (formatted expr)

prettyExpression' :: Expression -> String
prettyExpression' expr = render (Formatted (SWEET, UNICODE, expr))

prettyProgram :: Program -> String
prettyProgram prog = render (formatted prog)

prettyProgram' :: Program -> PrintMode -> Encoding -> String
prettyProgram' prog mode encoding = render (Formatted (mode, encoding, prog))
