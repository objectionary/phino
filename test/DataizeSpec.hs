-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module DataizeSpec (spec) where

import Ast (Attribute (AtLabel), Binding (BiDelta, BiTau), Expression (ExDispatch, ExFormation, ExGlobal, ExTermination, ExThis), Program (Program))
import Control.Monad
import Dataize (morph)
import Test.Hspec

testMorph :: [(String, Expression, Expression, Maybe Expression)] -> SpecWith (Arg Expectation)
testMorph useCases =
  forM_ useCases $ \(desc, input, prog, output) ->
    it desc $ do
      res <- morph input (Program prog)
      res `shouldBe` output

spec :: Spec
spec =
  describe "morph" $
    testMorph
      [ ("[[ D> 00- ]] => [[ D> 00- ]]", ExFormation [BiDelta "00-"], ExGlobal, Just (ExFormation [BiDelta "00-"])),
        ("T => T", ExTermination, ExGlobal, Just ExTermination),
        ("$ => X", ExThis, ExGlobal, Nothing),
        ("Q => X", ExGlobal, ExGlobal, Nothing),
        ( "Q.x (Q -> [[ x -> [[]] ]]) => [[]]",
          ExDispatch ExGlobal (AtLabel "x"),
          ExFormation [BiTau (AtLabel "x") (ExFormation [])],
          Just (ExFormation [])
        )
      ]
