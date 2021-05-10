module Tema_18e_TablaPropiedades_Spec (main, spec) where

import Tema_18.TablaPropiedades
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Propiedades de las tablas" $ do
    it "p1" $
      property prop_modifica_modifica_1
    it "p2" $
      property prop_modifica_modifica_2
    it "p3" $
      property prop_valor_modifica_1
    it "p4" $
      property prop_valor_modifica_2
