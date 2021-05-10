module Tema_14c_PilaPropiedades_Spec (main, spec) where

import Tema_14.PilaPropiedades
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Tema_14c" $ do
    it "p1" $
      property prop_cima_apila
    it "p2" $
      property prop_desapila_apila
    it "p3" $
      property prop_vacia_esta_vacia
    it "p4" $
      property prop_apila_no_es_vacia
