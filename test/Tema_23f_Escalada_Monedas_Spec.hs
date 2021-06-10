module Tema_23f_Escalada_Monedas_Spec (main, spec) where

import Tema_23.Escalada_Monedas
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sucesoresMonedas" $
    it "e1" $
      sucesoresMonedas (199,[]) `shouldBe`
       [(198,[1]),(197,[2]),(194,[5]),(189,[10]),
        (179,[20]),(149,[50]),(99,[100])]

  describe "cambio" $
    it "e1" $
       cambio 199  `shouldBe`  [2,2,5,20,20,50,100]
