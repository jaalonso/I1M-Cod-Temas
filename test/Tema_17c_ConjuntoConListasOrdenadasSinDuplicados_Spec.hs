module Tema_17c_ConjuntoConListasOrdenadasSinDuplicados_Spec (main, spec) where

import Tema_17.ConjuntoConListasOrdenadasSinDuplicados
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ConjuntoConListasOrdenadasSinDuplicados" $ do
    it "e1" $
      escribeConjunto (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])
        `shouldBe` "{0,1,2,3,5,7,9}"
    it "e2" $
      show (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])
        `shouldBe` "{0,1,2,3,5,7,9}"
    it "e3" $
      show (vacio :: Conj Int)
        `shouldBe` "{}"
    it "e4" $
      esVacio (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0]) `shouldBe`  False
    it "e5" $
      esVacio (vacio :: Conj Int)  `shouldBe`  True
    it "e6" $
      pertenece 3 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])  `shouldBe`  True
    it "e7" $
      pertenece 4 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])  `shouldBe`  False
    it "e8" $
      show (inserta 5 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0]))
        `shouldBe` "{0,1,2,3,5,7,9}"
    it "e9" $
      show (inserta 4 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0]))
        `shouldBe` "{0,1,2,3,4,5,7,9}"
    it "e10" $
      show (elimina 3 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0]))
        `shouldBe` "{0,1,2,5,7,9}"
    it "e11" $
      show (elimina 4 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0]))
        `shouldBe` "{0,1,2,3,5,7,9}"
