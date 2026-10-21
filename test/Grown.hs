{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module Grown (Grown (..)) where

import AST
import Bytes (numToBts, strToBts)
import Data.List (nub)
import Data.Text qualified as T
import Test.QuickCheck (Arbitrary (..), Gen, chooseInt, elements, frequency, shrinkList, shuffle, sized, suchThat, vectorOf)

newtype Grown = Grown Expression
  deriving (Show)

instance Arbitrary Grown where
  arbitrary = Grown <$> sized (expression . (`div` 20))
    where
      expression :: Int -> Gen Expression
      expression depth =
        frequency
          ( [ (2, pure ExXi)
            , (2, pure ExRoot)
            , (1, pure ExTermination)
            , (1, ExMeta <$> meta "enk")
            , (1, DataNumber . numToBts <$> arbitrary)
            , (1, DataString . strToBts <$> arbitrary)
            , (2, ExFormation <$> bindings 0)
            ]
              ++ [ (weight, grown)
                 | depth > 0
                 , (weight, grown) <-
                     [ (4, ExFormation <$> bindings depth)
                     , (3, ExDispatch <$> expression (depth - 1) <*> attribute)
                     , (3, (ExApplication <$> expression (depth - 1) <*> argument depth) `suchThat` spelled)
                     ]
                 ]
          )
      spelled :: Expression -> Bool
      spelled (ExApplication ExXi _) = False
      spelled (ExApplication ExRoot _) = False
      spelled (ExApplication (DataNumber _) (ArTau AtRho _)) = False
      spelled (ExApplication (DataString _) (ArTau AtRho _)) = False
      spelled _ = True
      argument :: Int -> Gen Argument
      argument depth = frequency [(3, ArTau <$> attribute <*> expression (depth - 1)), (2, ArAlpha <$> alpha <*> expression (depth - 1))]
      alpha :: Gen Alpha
      alpha = frequency [(4, Alpha <$> chooseInt (0, 3)), (1, AlMeta <$> meta "i")]
      bindings :: Int -> Gen [Binding]
      bindings depth = do
        attrs <- nub <$> (chooseInt (0, 4) >>= (`vectorOf` attribute))
        named <- mapM (\attr -> frequency [(1, pure (BiVoid attr)), (3, BiTau attr <$> expression (depth - 1))]) attrs
        wildcard <- frequency [(3, pure []), (1, pure . BiMeta <$> meta "B")]
        delta <- frequency [(2, pure []), (1, pure . BiDelta <$> bytes)]
        lambda <- frequency [(2, pure []), (1, pure . BiLambda <$> function)]
        (++ delta ++ lambda) <$> shuffle (named ++ wildcard)
      attribute :: Gen Attribute
      attribute = frequency [(6, AtLabel <$> label), (1, pure AtPhi), (1, pure AtRho), (1, AtMeta <$> meta "t")]
      label :: Gen T.Text
      label = T.pack <$> spelt ['a' .. 'z'] (['a' .. 'z'] ++ ['0' .. '9'] ++ "-_")
      function :: Gen Function
      function = frequency [(4, Function . T.pack <$> spelt ['A' .. 'Z'] (['a' .. 'z'] ++ ['0' .. '9'] ++ "_")), (1, FnMeta <$> meta "F"), (2, FnSymbol <$> chooseInt (1, 99))]
      bytes :: Gen Bytes
      bytes = frequency [(1, pure BtEmpty), (2, BtOne <$> byte), (3, BtMany <$> (chooseInt (2, 9) >>= (`vectorOf` byte))), (1, BtMeta <$> meta "d")]
      byte :: Gen String
      byte = vectorOf 2 (elements (['0' .. '9'] ++ ['A' .. 'F']))
      meta :: String -> Gen T.Text
      meta heads = T.pack <$> ((:) <$> elements heads <*> (chooseInt (1, 3) >>= (`vectorOf` elements (['1' .. '9'] ++ ['a' .. 'z']))))
      spelt :: String -> String -> Gen String
      spelt heads rest = (:) <$> elements heads <*> (chooseInt (0, 5) >>= (`vectorOf` elements rest))
  shrink (Grown expr) = Grown <$> smaller expr
    where
      smaller :: Expression -> [Expression]
      smaller = \case
        ExFormation bds -> [inner | BiTau _ inner <- bds] ++ (ExFormation <$> shrinkList binding bds)
        ExDispatch inner attr -> inner : (ExDispatch <$> smaller inner <*> pure attr)
        ExApplication inner arg -> inner : held arg : [ExApplication less arg | less <- smaller inner, less `notElem` [ExXi, ExRoot]] ++ (ExApplication inner <$> argued arg)
        _ -> []
      binding :: Binding -> [Binding]
      binding (BiTau attr inner) = BiTau attr <$> smaller inner
      binding _ = []
      held :: Argument -> Expression
      held (ArTau _ inner) = inner
      held (ArAlpha _ inner) = inner
      argued :: Argument -> [Argument]
      argued (ArTau attr inner) = ArTau attr <$> smaller inner
      argued (ArAlpha idx inner) = ArAlpha idx <$> smaller inner
