module Tema_14a_PilaConTipoDeDatoAlgebraico_Spec (main, spec) where

import Tema_14.PilaConTipoDeDatoAlgebraico
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "PilaConTipoDeDatoAlgebraico" $ do
    it "e1" $
      escribePila (apila 1 (apila 2 (apila 3 vacia))) `shouldBe` "1|2|3|-"
    it "e2" $
      show (apila 1 (apila 2 (apila 3 vacia))) `shouldBe` "1|2|3|-"
    it "e3" $
      show (vacia :: Pila Int) `shouldBe` "-"
    it "e4" $
      show (apila 4 (apila 1 (apila 2 (apila 3 vacia)))) `shouldBe` "4|1|2|3|-"
    it "e5" $
      cima (apila 1 (apila 2 (apila 3 vacia)))  `shouldBe`  1
    it "e6" $
      show (desapila (apila 1 (apila 2 (apila 3 vacia)))) `shouldBe` "2|3|-"
    it "e7" $
      esVacia (apila 1 (apila 2 (apila 3 vacia))) `shouldBe`  False
    it "e8" $
      esVacia vacia `shouldBe`  True
