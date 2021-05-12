module Tema_19a_ArbolBin_Spec (main, spec) where

import Tema_19.ArbolBin
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "escribeABB" $ do
    it "e1" $
      escribeABB (crea (reverse [5,2,6,4,8,3,9])) `shouldBe`
        " (5 (2 - (4 (3 - -) -)) (6 - (8 - (9 - -))))"
    it "e2" $
      escribeABB (foldr inserta vacio (reverse [5,2,4,3,8,6,7,10,9,11])) `shouldBe`
        " (5 (2 - (4 (3 - -) -)) (8 (6 - (7 - -)) (10 (9 - -) (11 - -))))"

  describe "ejemploABB" $ do
    it "e1" $
      show (ejemploABB 1) `shouldBe`
         " (5 (2 - (4 (3 - -) -)) (6 - (8 - (9 - -))))"
    it "e2" $
      show (ejemploABB 2) `shouldBe`
        " (5 (2 - (4 (3 - -) -)) (8 (6 - (7 - -)) (10 (9 - -) (11 - -))))"

  describe "vacio" $
    it "e1" $
      show (vacio :: ABB Int) `shouldBe`
        " -"

  describe "inserta" $
    it "e1" $
      show (inserta 7 (ejemploABB 1)) `shouldBe`
        " (5 (2 - (4 (3 - -) -)) (6 - (8 (7 - -) (9 - -))))"

  describe "crea" $
    it "e1" $
      show (crea [3,7,2]) `shouldBe`
        " (2 - (7 (3 - -) -))"

  describe "crea'" $
    it "e1" $
      show (crea' [2,3,7]) `shouldBe`
        " (3 (2 - -) (7 - -))"

  describe "elimina" $ do
    it "e1" $
      show (elimina 3 (ejemploABB 1)) `shouldBe`
        " (5 (2 - (4 - -)) (6 - (8 - (9 - -))))"
    it "e2" $
      show (elimina 2 (ejemploABB 1)) `shouldBe`
        " (5 (4 (3 - -) -) (6 - (8 - (9 - -))))"
    it "e3" $
      show (elimina 5 (ejemploABB 1)) `shouldBe`
        " (6 (2 - (4 (3 - -) -)) (8 - (9 - -)))"
    it "e4" $
      show (elimina 7 (ejemploABB 1)) `shouldBe`
        " (5 (2 - (4 (3 - -) -)) (6 - (8 - (9 - -))))"

  describe "valido" $ do
    it "e1" $
      valido (ejemploABB 1) `shouldBe` True
