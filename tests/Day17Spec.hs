{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day17Spec where

import Day17
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` "4,6,3,5,6,3,5,2,1,0"
    it "of second star" $ do
      day' ex' `shouldReturn` Just 117440
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` "7,1,3,7,5,1,0,3,4"
    it "on second star" $ do
      day' fileContent `shouldReturn` Just 190384113204239
