module Tema_3_Spec (main, spec) where

import Tema_3
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "suma" $
    it "e1" $
      suma (2,3) `shouldBe` 5
  describe "deCeroA" $
    it "e1" $
      deCeroA 5 `shouldBe` [0,1,2,3,4,5]
  describe "suma'" $ do
    it "e1" $
      suma' 2 3   `shouldBe` 5
    it "e2" $
      (suma' 2) 3 `shouldBe` 5
  describe "mult" $ do
    it "e1" $
      mult 2 5 7     `shouldBe` 70
    it "e2" $
      (mult 2) 5 7   `shouldBe` 70
    it "e3" $
      ((mult 2) 5) 7 `shouldBe` 70
  describe "suc" $
    it "e1" $
      suc 5 `shouldBe` 6
