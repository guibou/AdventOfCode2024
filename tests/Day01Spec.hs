{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day01Spec where

import Day01
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 11
    it "of second star" $ do
      day' ex `shouldBe` 31
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 1197984
    it "on second star" $ do
      day' fileContent `shouldBe` 23387399
