{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day19Spec where

import Day19
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 6
    it "of second star" $ do
      day' ex `shouldBe` 16
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 238
    it "on second star" $ do
      day' fileContent `shouldBe` 635018909726691
