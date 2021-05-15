module Tema_21c_PolRepDispersa_Spec (main, spec) where

import Tema_21.PolRepDispersa
import Test.Hspec

main :: IO ()
main = hspec spec

-- Ejemplos de polinomios con coeficientes enteros:
ejPol1, ejPol2, ejPol3 :: Polinomio Int
ejPol1 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
ejPol2 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
ejPol3 = consPol 4 6 (consPol 1 2 polCero)

spec :: Spec
spec = do
  describe "show" $ do
    it "e1" $
      show ejPol1 `shouldBe` "3*x^4 + -5*x^2 + 3"
    it "e2" $
      show ejPol2 `shouldBe` "x^5 + 5*x^2 + 4*x"
    it "e3" $
      show ejPol3 `shouldBe` "6*x^4 + 2*x"

  describe "polCero" $
    it "e1" $
      show polCero `shouldBe` "0"

  describe "esPolCero" $ do
    it "e1" $
      esPolCero polCero  `shouldBe`  True
    it "e2" $
      esPolCero ejPol1   `shouldBe`  False

  describe "consPol" $ do
    it "e1" $
      show (consPol 3 0 ejPol2)   `shouldBe`  "x^5 + 5*x^2 + 4*x"
    it "e2" $
      show (consPol 3 2 polCero)  `shouldBe`  "2*x^3"
    it "e3" $
      show (consPol 6 7 ejPol2)   `shouldBe`  "7*x^6 + x^5 + 5*x^2 + 4*x"
    it "e4" $
      show (consPol 4 7 ejPol2)   `shouldBe`  "x^5 + 7*x^4 + 5*x^2 + 4*x"
    it "e5" $
      show (consPol 5 7 ejPol2)   `shouldBe`  "8*x^5 + 5*x^2 + 4*x"

  describe "grado" $
    it "e1" $
      grado ejPol3  `shouldBe`  4

  describe "coefLider" $
    it "e1" $
      coefLider ejPol3  `shouldBe`  6

  describe "restoPol" $ do
    it "e1" $
      show (restoPol ejPol3)  `shouldBe`  "2*x"
    it "e2" $
      show (restoPol ejPol2)  `shouldBe`  "5*x^2 + 4*x"
