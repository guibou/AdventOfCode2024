{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day05Spec where

import Day05
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 143
    it "of second star" $ do
      day' ex `shouldBe` 123
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 6051
    it "on second star" $ do
      day' fileContent `shouldBe` 5093
