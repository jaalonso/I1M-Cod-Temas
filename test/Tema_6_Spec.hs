module Tema_6_Spec (main, spec) where

import Tema_6
import Test.Hspec

import Prelude hiding (product, reverse, length, (++), zip, drop, init)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "factorial" $
    it "e1" $
      factorial 3  `shouldBe`  6
  describe "por" $
    it "e1" $
      3 `por` 2  `shouldBe`  6
  describe "product" $
    it "e1" $
      product [7,5,2] `shouldBe` 70
  describe "length" $
    it "e1" $
      length [2,4,5] `shouldBe` 3
  describe "reverse" $
    it "e1" $
      reverse [2,5,3]  `shouldBe`  [3,5,2]
  describe "++" $
    it "e1" $
      [2,5] ++ [3,5,6]  `shouldBe`  [2,5,3,5,6]
  describe "inserta" $
    it "e1" $
      inserta 5 [2,4,7,3,6,8,10] `shouldBe` [2,4,5,7,3,6,8,10]
  describe "ordena_por_insercion" $
    it "e1" $
      ordena_por_insercion [2,4,3,6,3] `shouldBe` [2,3,3,4,6]
  describe "zip" $
    it "e1" $
      zip [1,3,5] [2,4,6,8]  `shouldBe`  [(1,2),(3,4),(5,6)]
  describe "drop" $ do
    it "e1" $
      drop 2 [5,7,9,4] `shouldBe` [9,4]
    it "e2" $
      drop 5 [1,4]     `shouldBe`  []
  describe "fibonacci" $
    it "e1" $
      fibonacci 8  `shouldBe`  21
  describe "ordena" $
    it "e1" $
      ordena [2,5,4,7]  `shouldBe`  [2,4,5,7]
  describe "par" $
    it "e1" $
      par 3    `shouldBe`  False
  describe "impar 3" $
    it "e1" $
      impar 3  `shouldBe`  True
  describe "pares" $
    it "e1" $
      pares [1,3,5,7]  `shouldBe`  [1,5]
  describe "init" $
    it "e1" $
      init [3,2,5]  `shouldBe`  [3,2]
