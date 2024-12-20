{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day20Spec where

import Day20
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day 20 ex `shouldBe` 5
    it "of second star" $ do
      day' 74 ex `shouldBe` 7
  describe "works" $ do
    it "on first star" $ do
      day 100 fileContent `shouldBe` 1404
    it "on second star" $ do
      day' 100 fileContent `shouldBe` 1010981
