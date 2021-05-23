module Tema_22c_RecorridoEnProfundidad_Spec (main, spec) where

import Tema_22.RecorridoEnProfundidad
import Test.Hspec

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "recorridoEnProfundidad" $
    it "e1" $
      recorridoEnProfundidad 1 ejG `shouldBe`  [1,2,3,6,5,4]

  describe "recorridoEnProfundidad'" $
    it "e1" $
      recorridoEnProfundidad' 1 ejG `shouldBe`  [1,2,3,6,5,4]
