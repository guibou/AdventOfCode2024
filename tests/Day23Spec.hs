{-# OPTIONS_GHC -Wno-type-defaults #-}
module Day23Spec where

import Day23
import Test.Syd

-- We default to Int for performance, if Integer is required, it will be force typed on the different Day
default (Int)

spec :: Spec
spec = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex `shouldBe` 7
    it "of second star" $ do
      day' ex `shouldBe` "co,de,ka,ta"
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 1308
    it "on second star" $ do
      day' fileContent `shouldBe` "bu,fq,fz,pn,rr,st,sv,tr,un,uy,zf,zi,zy"
