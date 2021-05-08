module Tema_17a_Spec (main, spec) where

import Tema_17.ConjuntoConListasNoOrdenadasConDuplicados
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ConjuntoConListasNoOrdenadasConDuplicados" $ do
    it "e1" $
      escribeConjunto (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])
        `shouldBe` "{2,5,1,3,7,5,3,2,1,9,0}"
    it "e2" $
      show (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])
        `shouldBe` "{2,5,1,3,7,5,3,2,1,9,0}"
    it "e3" $
      show (vacio :: Conj Int)
        `shouldBe` "{}"
    it "e4" $
      show (inserta 5 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0]))
        `shouldBe` "{5,2,5,1,3,7,5,3,2,1,9,0}"
    it "e5" $
      show (elimina 3 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0]))
        `shouldBe` "{2,5,1,7,5,2,1,9,0}"
    it "e6" $
      pertenece 3 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])  `shouldBe`  True
    it "e7" $
      pertenece 4 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])  `shouldBe`  False
    it "e8" $
      esVacio (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0]) `shouldBe` False
    it "e9" $
      esVacio (vacio :: Conj Int) `shouldBe` True
