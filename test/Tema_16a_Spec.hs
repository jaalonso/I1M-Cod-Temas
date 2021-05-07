module Tema_16a_Spec (main, spec) where

import Tema_16.ColaDePrioridadConListas
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ColaDePrioridadConListas" $ do
    it "e1" $
      show (foldr inserta vacia [3,1,7,2,9]) `shouldBe` "CP [1,2,3,7,9]"
    it "e2" $
      show (vacia :: CPrioridad Int) `shouldBe` "CP []"
    it "e3" $
      show (inserta 5 (foldr inserta vacia [3,1,7,2,9])) `shouldBe` "CP [1,2,3,5,7,9]"
    it "e4" $
      primero (foldr inserta vacia [3,1,7,2,9]) `shouldBe` 1
    it "e5" $
      show (resto (foldr inserta vacia [3,1,7,2,9])) `shouldBe` "CP [2,3,7,9]"
    it "e6" $
      esVacia (foldr inserta vacia [3,1,7,2,9]) `shouldBe` False
    it "e7" $
      esVacia (vacia :: CPrioridad Int) `shouldBe`  True
