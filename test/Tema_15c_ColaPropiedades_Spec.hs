module Tema_15c_ColaPropiedades_Spec (main, spec) where

import Tema_15.ColaPropiedades
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Tema_15c" $ do
    it "p1" $
     property prop_genCola_correcto
    it "p2" $
     property prop_primero_inserta_vacia
    it "p3" $
     property prop_primero_inserta_no_vacia
    it "p4" $
     property prop_resto_inserta_vacia
    it "p5" $
     property prop_resto_inserta_en_no_vacia
    it "p6" $
     property prop_vacia_es_vacia
    it "p7" $
     property prop_inserta_no_es_vacia
    it "p8" $
     property prop_valida_vacia
    it "p9" $
     property prop_valida_inserta
    it "p10" $
     property prop_valida_resto
