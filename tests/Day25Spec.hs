{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day25Spec where

import Day25
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 3
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 2900
