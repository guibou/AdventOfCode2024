{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day13Spec where

import Day13
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 480
    it "of second star" $ do
      -- We don't have this value in AoC reference, but we computed it, so let's
      -- keep it for test
      day' ex `shouldBe` 875318608908
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 29436
    it "on second star" $ do
      day' fileContent `shouldBe` 103729094227877
