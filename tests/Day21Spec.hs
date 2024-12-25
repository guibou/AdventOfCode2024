{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day21Spec where

import Day21
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 126384
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 109758
    it "on second star" $ do
      day' fileContent `shouldBe` 134341709499296
