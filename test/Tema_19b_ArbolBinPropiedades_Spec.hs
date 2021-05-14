module Tema_19b_ArbolBinPropiedades_Spec (main, spec) where

import Tema_19.ArbolBinPropiedades
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Propiedades de ABB" $ do
    it "p1" $
      property prop_genABB_correcto
    it "p2" $
      property prop_listaOrdenada_correcta
    it "p3" $
      property prop_orderedList_correcta
    it "p4" $
      property prop_vacio_es_ABB
    it "p5" $
      property prop_inserta_es_valida
    it "p6" $
      property prop_inserta_es_no_vacio
    it "p7" $
      property prop_elemento_de_inserta
    it "p8" $
      property prop_vacio_sin_elementos
    it "p9" $
      property prop_elementos_de_inserta
    it "p10" $
      property prop_elimina_es_valida
    it "p11" $
      property prop_elimina_agrega
    it "p12" $
      property prop_crea_es_valida
    it "p13" $
      property prop_crea'_es_valida
    it "p14" $
      property prop_elementos_crea
    it "p15" $
      property prop_elementos_crea'
    it "p16" $
      property prop_en_elementos
    it "p17" $
      property prop_menoresMinimo
