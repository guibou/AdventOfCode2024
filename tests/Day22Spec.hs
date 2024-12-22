{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day22Spec where

import Day22
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 37327623
    it "of second star" $ do
      day' ex' `shouldBe` 23
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 19847565303
    it "on second star" $ do
      day' fileContent `shouldBe` 2250
