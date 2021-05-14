module Tema_20a_Monticulo_Spec (main, spec) where

import Tema_20.Monticulo
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "inserta" $
    it "e1" $
      show (inserta 3 ejM1) `shouldBe`
      "M 1 2 (M 4 1 (M 8 1 Vacio Vacio) Vacio) (M 3 1 (M 6 1 Vacio Vacio) Vacio)"

  describe "resto" $
    it "e1" $
      show (resto ejM1) `shouldBe`
      "M 4 2 (M 8 1 Vacio Vacio) (M 6 1 Vacio Vacio)"

  describe "valido" $ do
    it "e2" $
      valido ejM1  `shouldBe`  True
    it "e2" $
      valido ejM2  `shouldBe`  True
    it "e3" $
      valido ejM3  `shouldBe`  True
