module Tema_15b_Spec (main, spec) where

import Tema_15.ColaConDosListas
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ColaConDosListas" $ do
    it "e1" $
      escribeCola (foldr inserta vacia [1..10]) `shouldBe` "C [10,9,8,7,6,5,4,3,2,1]"
    it "e2" $
      show (foldr inserta vacia [1..10]) `shouldBe` "C [10,9,8,7,6,5,4,3,2,1]"
    it "e3" $
      show (vacia :: Cola Int) `shouldBe` "C []"
    it "e4" $
      show (inserta 12 (foldr inserta vacia [1..10])) `shouldBe` "C [10,9,8,7,6,5,4,3,2,1,12]"
    it "e6" $
      primero (foldr inserta vacia [1..10]) `shouldBe` 10
    it "e7" $
      show (resto (foldr inserta vacia [1..10])) `shouldBe` "C [9,8,7,6,5,4,3,2,1]"
    it "e8" $
      esVacia (foldr inserta vacia [1..10]) `shouldBe`  False
    it "e9" $
      esVacia vacia  `shouldBe`  True
