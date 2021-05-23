module Tema_22c_AlgoritmoDeKruskal_Spec (main, spec) where

import Tema_22.AlgoritmoDeKruskal
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "kruskal" $ do
    it "e1" $
      kruskal g1  `shouldBe`  [(55,2,4),(34,1,3),(32,2,5),(12,1,2)]
    it "e2" $
      kruskal g2  `shouldBe`  [(32,2,5),(13,1,2),(12,2,4),(11,1,3)]
    it "e3" $
      kruskal g3  `shouldBe`  [(9,5,7),(7,2,3),(6,1,6),(5,4,5),(5,1,2),(3,5,6)]
    it "e4" $
      kruskal g4  `shouldBe`  [(9,5,7),(6,1,6),(5,4,5),(5,1,2),(3,5,6),(1,3,5)]

  describe "prim" $ do
    it "e1" $
      prim g1  `shouldBe` [(55,2,4),(34,1,3),(32,2,5),(12,1,2)]
    it "e2" $
      prim g2  `shouldBe` [(32,2,5),(12,2,4),(13,1,2),(11,1,3)]
    it "e3" $
      prim g3  `shouldBe` [(9,5,7),(7,2,3),(5,5,4),(3,6,5),(6,1,6),(5,1,2)]

  describe "propiedad" $ do
    it "p1" $
      property prop_AE
