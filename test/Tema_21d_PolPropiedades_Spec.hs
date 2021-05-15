module Tema_21d_PolPropiedades_Spec (main, spec) where

import Tema_21.PolPropiedades
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Propiedades de los polinomios" $ do
    it "p1" $
      property prop_polCero_es_cero
    it "p2" $
      property prop_consPol_no_cero
    it "p3" $
      property prop_consPol
    it "p4" $
      property prop_grado
    it "p5" $
      property prop_coefLider
    it "p6" $
      property prop_restoPol
