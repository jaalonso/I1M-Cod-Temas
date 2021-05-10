module Tema_17d_ConjuntoPropiedades_Spec (main, spec) where

import Tema_17.ConjuntoPropiedades
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ConjuntoPropiedades" $ do
    it "p1" $
      property prop_independencia_repeticiones
    it "p2" $
      property prop_independencia_del_orden
    it "p3" $
      property prop_vacio_no_elementos
    it "p4" $
      property prop_pertenece_inserta
    it "p5" $
      property prop_elimina_vacio
    it "p6" $
      property prop_elimina_inserta
    it "p7" $
      property prop_vacio_es_vacio
    it "p8" $
      property prop_inserta_es_no_vacio
