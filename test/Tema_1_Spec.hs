module Tema_1_Spec (main, spec) where

import Tema_1
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "doble" $ do
    it "e1" $
      doble 3 `shouldBe` 6
    it "e2" $
      doble (doble 3) `shouldBe` 12

  describe "prop_doble" $
    it "p1" $
      property prop_doble

  describe "prop_suma''" $
    it "p1" $
      property prop_prod_suma''

  describe "suma" $
    it "e1" $
      suma 4 `shouldBe` 10

  describe "sumaLista" $
    it "e1" $
      sumaLista [2,3,7] `shouldBe` 12

  describe "ordena" $
    it "e1" $
      ordena [4,6,2,5,3] `shouldBe` [2,3,4,5,6]
