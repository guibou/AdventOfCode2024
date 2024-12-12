{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day11Spec where

import Day11
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 55312
    it "of second star" $ do
      -- This value is not provided in the example, but I've computed it, so
      -- let's keep if for testing purpose.
      day' ex `shouldBe` 65601038650482
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 197157
    it "on second star" $ do
      day' fileContent `shouldBe` 234430066982597
