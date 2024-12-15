{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day15Spec where

import Day15
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 10092
    it "of second star" $ do
      day' ex `shouldBe` 9021
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 1514333
    it "on second star" $ do
      day' fileContent `shouldBe` 1528453
