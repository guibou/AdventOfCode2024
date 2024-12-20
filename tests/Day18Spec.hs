{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day18Spec where

import Day18
import Test.Syd
import Linear (V2(..))

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day 12 ex `shouldBe` Just 22
    it "of second star" $ do
      day' ex `shouldBe` V2 6 1
  describe "works" $ do
    it "on first star" $ do
      day 1024 fileContent `shouldBe` Just 272
    it "on second star" $ do
      day' fileContent `shouldBe` V2 16 44
