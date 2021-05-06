module Tema_14b_Spec (main, spec) where

import Tema_14.PilaConListas
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Tema_14b" $ do
    it "e1" $
      show (apila 1 (apila 2 (apila 3 vacia))) `shouldBe` "1|2|3|-"
    it "e2" $
      show (vacia :: Pila Int) `shouldBe` "-"
    it "e3" $
      show (apila 4 (apila 1 (apila 2 (apila 3 vacia)))) `shouldBe` "4|1|2|3|-"
    it "e4" $
      cima (apila 1 (apila 2 (apila 3 vacia))) `shouldBe` 1
    it "e5" $
      show (desapila (apila 1 (apila 2 (apila 3 vacia)))) `shouldBe` "2|3|-"
    it "e6" $
      esVacia (apila 1 (apila 2 (apila 3 vacia))) `shouldBe` False
    it "e7" $
      esVacia vacia `shouldBe` True
