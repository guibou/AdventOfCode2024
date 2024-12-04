{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day04Spec where

import Day04
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 18
    it "of second star" $ do
      day' ex `shouldBe` 9
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 2434
    it "on second star" $ do
      day' fileContent `shouldBe` 1835
