module Tema_20c_ColaDePrioridadConMonticulos_Spec (main, spec) where

import Tema_20.ColaDePrioridadConMonticulos
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "vacia" $
    it "e1" $
      show (vacia :: CPrioridad Int) `shouldBe` "CP Vacio"

  describe "inserta" $
    it "e1" $
      show (inserta 5 (foldr inserta vacia [3,1,7,2,9])) `shouldBe`
      "CP (M 1 2 (M 2 2 (M 9 1 Vacio Vacio) (M 7 1 Vacio Vacio)) (M 3 1 (M 5 1 Vacio Vacio) Vacio))"

  describe "primero" $
    it "e1" $
      primero (foldr inserta vacia [3,1,7,2,9])  `shouldBe`  1

  describe "resto" $
    it "e1" $
      show (resto (foldr inserta vacia [3,1,7,2,9])) `shouldBe`
      "CP (M 2 2 (M 9 1 Vacio Vacio) (M 3 1 (M 7 1 Vacio Vacio) Vacio))"

  describe "esVacia" $ do
    it "e1" $
      esVacia (foldr inserta vacia [3,1,7,2,9]) `shouldBe`  False
    it "e2" $
      esVacia (vacia :: CPrioridad Int) `shouldBe`  True
