module Tema_21e_PolOperaciones_Spec (main, spec) where

import Tema_21.PolOperaciones
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "creaTermino" $
    it "e1" $
      show (creaTermino 2 5)  `shouldBe`  "5*x^2"

  describe "termLider" $
    it "e1" $
      show (termLider ejPol2)  `shouldBe`  "x^5"

  describe "sumaPol" $ do
    it "e1" $
      show (sumaPol ejPol1 ejPol2)  `shouldBe`  "x^5 + 3*x^4 + 4*x + 3"
    it "p1" $
      property prop_neutroSumaPol
    it "p2" $
      property prop_conmutativaSuma

  describe "multPorTerm" $
    it "e1" $
      show (multPorTerm ejTerm ejPol2)  `shouldBe`  "4*x^6 + 20*x^3 + 16*x^2"

  describe "multPol" $ do
    it "e1" $
      show (multPol ejPol1 ejPol2) `shouldBe`
        "3*x^9 + -5*x^7 + 15*x^6 + 15*x^5 + -25*x^4 + -20*x^3 + 15*x^2 + 12*x"
    it "p1" $
      property prop_conmutativaProducto
    it "p2" $
      property prop_distributivaProductoSuma

  describe "polUnidad" $ do
    it "e1" $
      show polUnidad `shouldBe` "1"
    it "p1" $
      property prop_polUnidad

  describe "valor" $ do
    it "e1" $
      valor ejPol1 0     `shouldBe`  3
    it "e2" $
      valor ejPol1 1     `shouldBe`  1
    it "e3" $
      valor ejPol1 (-2)  `shouldBe`  31

  describe "esRaiz" $ do
    it "e1" $
      esRaiz 1 ejPol3  `shouldBe`  False
    it "e1¡2" $
      esRaiz 0 ejPol3  `shouldBe`  True

  describe "derivada" $ do
    it "e1" $
      show (derivada ejPol2)  `shouldBe`  "5*x^4 + 10*x + 4"
    it "p1" $
      property prop_derivada

  describe "restaPol" $
    it "e1" $
      show (restaPol ejPol1 ejPol2)  `shouldBe`  "-1*x^5 + 3*x^4 + -10*x^2 + -4*x + 3"
