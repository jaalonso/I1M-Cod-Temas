module Tema_15a_Spec (main, spec) where

import Tema_15.ColaConListas
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ColaConListas.hs" $ do
    it "e1" $
      show (foldr inserta vacia [1..10]) `shouldBe` "C [10,9,8,7,6,5,4,3,2,1]"
    it "e1" $
      show (vacia :: Cola Int) `shouldBe` "C []"
    it "e1" $
      show (inserta 12 (foldr inserta vacia [1..10])) `shouldBe` "C [10,9,8,7,6,5,4,3,2,1,12]"
    it "e1" $
      primero (foldr inserta vacia [1..10])  `shouldBe`  10
    it "e1" $
      show (resto (foldr inserta vacia [1..10])) `shouldBe` "C [9,8,7,6,5,4,3,2,1]"
    it "e1" $
      esVacia (foldr inserta vacia [1..10]) `shouldBe` False
    it "e1" $
      esVacia vacia `shouldBe` True
