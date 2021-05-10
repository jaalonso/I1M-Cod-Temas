module Tema_16b_ColaDePrioridadPropiedades_Spec (main, spec) where

import Tema_16.ColaDePrioridadPropiedades
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ColaDePrioridadPropiedades" $ do
    it "p1" $
      property prop_genCPrioridad_correcto
    it "p2" $
      property prop_inserta_conmuta
    it "p3" $
      property prop_primero_inserta_vacia
    it "p4" $
      property prop_primero_inserta
    it "p5" $
      property prop_resto_inserta_vacia
    it "p6" $
      property prop_resto_inserta
    it "p7" $
      property prop_vacia_es_vacia
    it "p8" $
      property prop_inserta_no_es_vacia
