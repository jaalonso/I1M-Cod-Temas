module Tema_20b_MonticuloPropiedades_Spec (main, spec) where

import Tema_20.MonticuloPropiedades
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Propiedades de monticulos" $ do
    it "p1" $
      property prop_genMonticulo
    it "p2" $
      property prop_monticuloNV
    it "p3" $
      property prop_inserta_es_valida
    it "p4" $
      property prop_inserta_no_vacio
    it "p5" $
      property prop_resto_es_valida
    it "p6" $
      property prop_resto_inserta
    it "p7" $
      property prop_menor_es_minimo
