module Tema_5_Spec (main, spec) where

import Tema_5
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "concat'" $
    it "e1" $
      concat' [[1,3],[2,5,6],[4,7]]  `shouldBe`  [1,3,2,5,6,4,7]
  describe "primeros" $
    it "e1" $
      primeros [(1,3),(2,5),(6,3)]  `shouldBe`  [1,2,6]
  describe "length'" $
    it "e1" $
      length' [4,2,5]  `shouldBe`  3
  describe "factores" $
    it "e1" $
      factores 30  `shouldBe`  [1,2,3,5,6,10,15,30]
  describe "primo" $ do
    it "e1" $
      primo 30  `shouldBe` False
    it "e2" $
      primo 31  `shouldBe` True
  describe "primos" $
    it "e1" $
      primos 31  `shouldBe` [2,3,5,7,11,13,17,19,23,29,31]
  describe "busca" $
    it "e1" $
      busca 'b' [('a',1),('b',3),('c',5),('b',2)]  `shouldBe`  [3,2]
  describe "adyacentes" $
    it "e1" $
      adyacentes [2,5,3,7]  `shouldBe`  [(2,5),(5,3),(3,7)]
  describe "ordenada" $ do
    it "e1" $
      ordenada [1,3,5,6,7]  `shouldBe`  True
    it "e2" $
      ordenada [1,3,6,5,7]  `shouldBe`  False
  describe "posiciones" $
    it "e1" $
      posiciones 5 [1,5,3,5,5,7]  `shouldBe`  [1,3,4]
  describe "minusculas" $
    it "e1" $
      minusculas "EstoEsUnaPrueba"  `shouldBe`  "stosnarueba"
  describe "ocurrencias" $
    it "e1" $
      ocurrencias 'a' "Salamanca"  `shouldBe`  4
  describe "let2int" $ do
    it "e1" $
      let2int 'a'  `shouldBe`  0
    it "e2" $
      let2int 'd'  `shouldBe`  3
    it "e3" $
      let2int 'z'  `shouldBe`  25
  describe "int2let" $ do
    it "e1" $
      int2let 0   `shouldBe`  'a'
    it "e2" $
      int2let 3   `shouldBe`  'd'
    it "e3" $
      int2let 25  `shouldBe`  'z'
  describe "desplaza" $ do
    it "e1" $
      desplaza   3  'a'  `shouldBe`  'd'
    it "e2" $
      desplaza   3  'y'  `shouldBe`  'b'
    it "e3" $
      desplaza (-3) 'd'  `shouldBe`  'a'
    it "e4" $
      desplaza (-3) 'b'  `shouldBe`  'y'
  describe "codifica" $ do
    it "e1" $
      codifica   3  "En todo la medida"   `shouldBe`  "Eq wrgr od phglgd"
    it "e2" $
      codifica (-3) "Eq wrgr od phglgd"   `shouldBe`  "En todo la medida"
  describe "prop_desplaza" $
    it "e1" $
      property prop_desplaza
  describe "prop_codifica" $
    it "e1" $
      property prop_codifica
  describe "porcentaje" $
    it "e1" $
      porcentaje 2 5  `shouldBe`  40.0
  describe "chiCuadrado" $ do
    it "e1" $
      chiCuadrado [3,5,6] [3,5,6]  `shouldBe`  0.0
    it "e2" $
      chiCuadrado [3,5,6] [5,6,3]  `shouldBe`  3.9666667
  describe "rota" $
    it "e1" $
      rota 2 "ramo"  `shouldBe`  "mora"
  describe "descifra" $
    it "e1" $
      descifra "Lt htruqnhfit ij qf anif jx ijxhzgwnw qt xnruqj vzj jx"
      `shouldBe` "Lo complicado de la vida es descubrir lo simple que es"
