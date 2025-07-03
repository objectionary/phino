-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module DataizeSpec (spec) where

import Ast (Binding (BiDelta), Expression (ExFormation, ExTermination, ExThis, ExGlobal, ExDispatch), Attribute (AtLabel))
import Control.Monad
import Dataize (morph)
import Test.Hspec

testMorph :: [(String, Expression, Maybe Expression)] -> SpecWith (Arg Expectation)
testMorph useCases =
  forM_ useCases $ \(desc, input, output) ->
    it desc $ do
      res <- morph input
      res `shouldBe` output

spec :: Spec
spec =
  describe "morph" $
    testMorph
      [ ("[[ D> 00- ]] => [[ D> 00- ]]", ExFormation [BiDelta "00-"], Just (ExFormation [BiDelta "00-"])),
        ("T => T", ExTermination, Just ExTermination),
        ("$ => X", ExThis, Nothing),
        ("Q => X", ExGlobal, Nothing),
        ("Q.x", ExDispatch ExGlobal (AtLabel "x"), Nothing)
      ]
