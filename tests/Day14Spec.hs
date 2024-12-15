{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day14Spec where

import Day14
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 12
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 225521010
    it "on second star" $ do
      day' fileContent `shouldBe` Just 7774
