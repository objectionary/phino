{-# LANGUAGE OverloadedStrings #-}

-- SPDX-FileCopyrightText: Copyright (c) 2025 Objectionary.com
-- SPDX-License-Identifier: MIT

module ForksSpec (spec) where

import AST
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Deps (Side (..), dontRewindEval)
import Forks (decide, explored, joined, straight)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

spec :: Spec
spec = do
  describe "explored" $ do
    it "runs what comes before a fork once for both of its paths" $ do
      count <- newIORef (0 :: Int)
      _ <- explored Nothing dontRewindEval (\forks -> modifyIORef' count (+ 1) >> decide forks (pure (pure ())) (Left 7))
      readIORef count `shouldReturn` 1
    it "takes the left side on the first path and the right side on the second" $
      explored Nothing dontRewindEval (\forks -> decide forks (pure (pure ())) (Right (BtOne "1F"))) `shouldReturn` [LeftSide, RightSide]
    it "opens no second path once --max-paths is spent" $
      explored (Just 1) dontRewindEval (\forks -> decide forks (pure (pure ())) (Left 3)) `shouldReturn` [LeftSide]
    it "takes the side a path took for a condition it meets again" $
      explored Nothing dontRewindEval (\forks -> (,) <$> decide forks (pure (pure ())) (Left 5) <*> decide forks (pure (pure ())) (Left 5)) `shouldReturn` [(LeftSide, LeftSide), (RightSide, RightSide)]
    it "takes the paths in the order their forks opened them" $
      explored Nothing dontRewindEval (\forks -> (,) <$> decide forks (pure (pure ())) (Left 2) <*> decide forks (pure (pure ())) (Left 9))
        `shouldReturn` [(LeftSide, LeftSide), (RightSide, LeftSide), (LeftSide, RightSide), (RightSide, RightSide)]
    it "goes on with the second path from what the fork marked" $ do
      cell <- newIORef (11 :: Int)
      explored Nothing dontRewindEval (\forks -> decide forks (writeIORef cell <$> readIORef cell) (Left 4) >> readIORef cell <* writeIORef cell 23)
        `shouldReturn` [11, 11]
    it "rewinds the protocol to the fork before the second path goes on" $ do
      trail <- newIORef []
      _ <- explored Nothing (pure (modifyIORef' trail ("rewound" :))) (\forks -> decide forks (pure (pure ())) (Left 6) >>= \side -> modifyIORef' trail (show side :))
      reverse <$> readIORef trail `shouldReturn` ["LeftSide", "rewound", "RightSide"]

  describe "straight" $
    it "takes the left side of a fork and opens no other path" $ do
      forks <- straight
      decide forks (pure (pure ())) (Left 8) `shouldReturn` LeftSide

  describe "joined" $ do
    let application :: [Int] -> Expression
        application idxs = ExApplication (ExDispatch ExRoot (AtLabel "f")) (ArTau (AtLabel "x") (ExFormation (map (\idx -> BiTau (AtLabel "k") (ExFormation [BiLambda (FnSymbol idx)])) idxs)))
    it "joins two terms that differ in one symbol under a fresh symbol past the count" $
      joined (application [3]) (application [8]) 11 `shouldBe` Just (application [12], [(12, (3, 8))], 12)
    it "names a pair of symbols met twice by one fresh symbol" $
      joined (application [4, 4]) (application [9, 9]) 17 `shouldBe` Just (application [18, 18], [(18, (4, 9))], 18)
    it "names two pairs sharing a symbol by two fresh symbols" $
      joined (application [5, 5]) (application [6, 7]) 20 `shouldBe` Just (application [21, 22], [(21, (5, 6)), (22, (5, 7))], 22)
    it "mints nothing for two terms that are the same" $
      joined (application [2, 6]) (application [2, 6]) 13 `shouldBe` Just (application [2, 6], [], 13)
    it "does not join two terms that differ in a datum" $
      joined (ExFormation [BiDelta (BtOne "01"), BiLambda (FnSymbol 4)]) (ExFormation [BiDelta (BtOne "02"), BiLambda (FnSymbol 9)]) 10 `shouldBe` Nothing
    it "does not join two terms that differ in a λ name" $
      joined (ExFormation [BiLambda (Function "L_one")]) (ExFormation [BiLambda (Function "L_two")]) 7 `shouldBe` Nothing
    it "does not join a symbol with a λ name" $
      joined (ExFormation [BiLambda (FnSymbol 3)]) (ExFormation [BiLambda (Function "L_three")]) 5 `shouldBe` Nothing
    it "does not join two terms that differ in an attribute" $
      joined (ExDispatch (application [3]) (AtLabel "a")) (ExDispatch (application [8]) (AtLabel "b")) 9 `shouldBe` Nothing
    it "does not join two formations of different sizes" $
      joined (application [3]) (application [3, 4]) 6 `shouldBe` Nothing
