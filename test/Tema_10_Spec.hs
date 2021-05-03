module Tema_10_Spec (main, spec) where

import Tema_10
import Test.Hspec
import Data.List (foldl')

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "inf" $
    it "e1" $
      fst (0,inf)  `shouldBe`  0
  describe "cuadrado" $
    it "e1" $
      cuadrado 3  `shouldBe`  9
  describe "unos" $ do
    it "e1" $
      head unos    `shouldBe`  1
    it "e2" $
      take 3 unos  `shouldBe`  [1,1,1]
  describe "primos" $
    it "e1" $
      take 15 primos  `shouldBe`  [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47]
  describe "sumaAcu'" $ do
    it "e1" $
      sumaAcu' 3 [4,7,2]       `shouldBe`  16
    it "e2" $
      sumaAcu' 0 [1..1000000]  `shouldBe`  500000500000
  describe "sumaAcu''" $ do
    it "e1" $
      sumaAcu'' 3 [4,7,2]     `shouldBe`  16
    it "e2" $
      sumaAcu'' 0 [1..10000]  `shouldBe`  50005000
