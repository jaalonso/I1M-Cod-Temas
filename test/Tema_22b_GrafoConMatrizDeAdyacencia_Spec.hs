module Tema_22b_GrafoConMatrizDeAdyacencia_Spec (main, spec) where

import Tema_22.GrafoConMatrizDeAdyacencia
-- import I1M.Grafo
import Test.Hspec

main :: IO ()
main = hspec spec

-- Ejemplos de grafos
ejGrafoND :: Grafo Int Int
ejGrafoND = creaGrafo ND (1,5) [(1,2,12),(1,3,34),(1,5,78),
                                (2,4,55),(2,5,32),
                                (3,4,61),(3,5,44),
                                (4,5,93)]

ejGrafoD :: Grafo Int Int
ejGrafoD = creaGrafo D (1,5) [(1,2,12),(1,3,34),(1,5,78),
                              (2,4,55),(2,5,32),
                              (3,4,61),(3,5,44),
                              (4,5,93)]

spec :: Spec
spec = do
  describe "dirigido" $ do
    it "e1" $
      dirigido ejGrafoD   `shouldBe`  True
    it "e2" $
      dirigido ejGrafoND  `shouldBe`  False

  describe "nodos" $ do
    it "e1" $
      nodos ejGrafoND  `shouldBe`  [1,2,3,4,5]
    it "e2" $
      nodos ejGrafoD   `shouldBe`  [1,2,3,4,5]

  describe "adyacentes" $ do
    it "e1" $
      adyacentes ejGrafoND 4  `shouldBe`  [2,3,5]
    it "e2" $
      adyacentes ejGrafoD  4  `shouldBe`  [5]

  describe "aristaEn" $ do
    it "e1" $
      aristaEn ejGrafoND (5,1)  `shouldBe`  True
    it "e2" $
      aristaEn ejGrafoND (4,1)  `shouldBe`  False
    it "e3" $
      aristaEn ejGrafoD (5,1)   `shouldBe`  False
    it "e4" $
      aristaEn ejGrafoD (1,5)   `shouldBe`  True

  describe "peso" $ do
    it "e1" $
      peso 1 5 ejGrafoND  `shouldBe`  78
    it "e2" $
      peso 1 5 ejGrafoD   `shouldBe`  78

  describe "aristas" $ do
    it "e1" $
      aristas ejGrafoD `shouldBe`
      [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),
       (3,5,44),(4,5,93)]
    it "e2" $
      aristas ejGrafoND `shouldBe`
      [(1,2,12),(1,3,34),(1,5,78),(2,1,12),(2,4,55),(2,5,32),
       (3,1,34),(3,4,61),(3,5,44),(4,2,55),(4,3,61),(4,5,93),
       (5,1,78),(5,2,32),(5,3,44),(5,4,93)]
