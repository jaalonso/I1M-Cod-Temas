module Tema_24d_ProblemaDelViajante_Spec (main, spec) where

import Tema_24.ProblemaDelViajante
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "viajante" $ do
    it "e1" $
      viajante ej1 `shouldBe` (20,[6,4,1,3,2,5,6])
    it "e2" $
      viajante ej2 `shouldBe` (56,[6,3,2,1,5,4,6])
