module Tema_8_Spec (main, spec) where

import Tema_8
import Test.Hspec
import Prelude hiding ( (++)
                      , drop
                      , length
                      , null
                      , replicate
                      , sum
                      , take
                      )
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "longitud" $
    it "e1" $
      longitud [2,3,1]  `shouldBe`  3
  describe "intercambia" $
    it "e1" $
      intercambia (2,5)  `shouldBe` (5,2)
  describe "inversa" $
    it "e1" $
      inversa [3,2,5]  `shouldBe`  [5,2,3]
  describe "replicate" $
    it "e1" $
      replicate 3 5  `shouldBe`  [5,5,5]
  describe "propiedades" $ do
    it "e1" $
      property prop_intercambia
    it "e2" $
      property prop_inversa_unitaria
    it "e3" $
      property prop_doble_negacion
    it "e4" $
      property prop_length_replicate
    it "e5" $
      property prop_asociativa_conc
    it "e6" $
      property prop_identidad_concatenacion
    it "e7" $
      property prop_length_append
    it "e8" $
      property prop_take_drop
    it "e9" $
      property prop_null
    it "e10" $
      property prop_equiv_inversa
    it "e11" $
      property prop_sum_map
    it "e12" $
      property prop_map_length
