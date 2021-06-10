module Tema_23c_BEE_Reinas_Spec (main, spec) where

import Tema_23.BEE_Reinas
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "valida" $ do
    it "e1" $
      valida [(1,1)] (2,2)  `shouldBe`  False
    it "e2" $
      valida [(1,1)] (2,3)  `shouldBe`  True
  describe "sucesoresNR" $
    it "e1" $
      sucesoresNR (1,4,[]) `shouldBe`
        [(2,4,[(1,1)]),(2,4,[(1,2)]),(2,4,[(1,3)]),(2,4,[(1,4)])]
  describe "buscaEE_NR 8" $
    it "e1" $
      buscaEE_NR 8  `shouldBe`
        [(1,1),(2,5),(3,8),(4,6),(5,3),(6,7),(7,2),(8,4)]
  describe "nSolucionesNR" $
    it "e1" $
      nSolucionesNR 8  `shouldBe`  92
