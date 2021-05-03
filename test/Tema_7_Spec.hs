module Tema_7_Spec (main, spec) where

import Tema_7
import Test.Hspec
import Prelude hiding (id, foldr, foldl, (.))
import Test.QuickCheck
import Data.Char

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "dosVeces" $ do
    it "e1" $
      dosVeces (*3) 2           `shouldBe`  18
    it "e2" $
      dosVeces reverse [2,5,7]  `shouldBe`  [2,5,7]
  describe "map" $ do
    it "e1" $
      map (*2) [3,4,7]     `shouldBe` [6,8,14]
    it "e2" $
      map (1/) [1,2,4]     `shouldBe` [1.0,0.5,0.25]
    it "e3" $
      map even [1..5]      `shouldBe` [False,True,False,True,False]
  describe "prop_sum_map" $
    it "e1" $
      property prop_sum_map
  describe "filter" $ do
    it "e1" $
      filter even [1,3,5,4,2,6,1] `shouldBe` [4,2,6]
    it "e2" $
      filter (>3) [1,3,5,4,2,6,1] `shouldBe` [5,4,6]
  describe "sumaCuadradosPares" $
    it "e1" $
      sumaCuadradosPares [1..5]  `shouldBe`  20
  describe "longitud" $
    it "e1" $
      longitud [4,2,5]  `shouldBe`  3
  describe "inversa" $
    it "e1" $
      inversa [4,2,5]  `shouldBe`  [5,2,4]
  describe "conc" $
    it "e1" $
      conc [4,2,5] [7,9]  `shouldBe`  [4,2,5,7,9]
  describe "suma" $
    it "e1" $
      suma [2,3,7]  `shouldBe`  12
  describe "composicionLista" $ do
    it "e1" $
      composicionLista [(*2),(^2)] 3       `shouldBe`  18
    it "e2" $
      composicionLista [(^2),(*2)] 3       `shouldBe`  36
    it "e3" $
      composicionLista [(/9),(^2),(*2)] 3  `shouldBe`  4.0
  describe "bin2int" $
    it "e1" $
      bin2int [1,0,1,1]  `shouldBe`  13
  describe "bin2intR" $
    it "e1" $
      bin2intR [1,0,1,1]  `shouldBe`  13
  describe "bin2intC" $
    it "e1" $
      bin2intC [1,0,1,1]  `shouldBe`  13
  describe "int2bin" $
    it "e1" $
      int2bin 13  `shouldBe`  [1,0,1,1]
  describe "prop_int_bin" $
    it "e1" $
      property prop_int_bin
  describe "creaOcteto" $ do
    it "e1" $
      creaOcteto [1,0,1,1,0,0,1,1,1,0,0,0]  `shouldBe`  [1,0,1,1,0,0,1,1]
    it "e2" $
      creaOcteto [1,0,1,1]                  `shouldBe`  [1,0,1,1,0,0,0,0]
  describe "creaOcteto'" $ do
    it "e1" $
      creaOcteto' [1,0,1,1,0,0,1,1,1,0,0,0]  `shouldBe`  [1,0,1,1,0,0,1,1]
    it "e2" $
      creaOcteto' [1,0,1,1]                  `shouldBe`  [1,0,1,1,0,0,0,0]
  describe "prop_transmite" $
    it "e1" $
      property prop_transmite
