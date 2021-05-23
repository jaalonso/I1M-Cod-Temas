module Tema_22c_RecorridoEnAnchura_Spec (main, spec) where

import Tema_22.RecorridoEnAnchura
import Test.Hspec

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "recorridoEnAnchura" $
    it "e1" $
      recorridoEnAnchura 1 ejG `shouldBe`  [1,2,3,4,6,5]
