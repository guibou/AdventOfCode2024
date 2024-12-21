{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day16Spec where

import Day16
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 7036
    it "of second star" $ do
      day' ex `shouldBe` 45
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 99488
    it "on second star" $ do
      day' fileContent `shouldBe` 516
