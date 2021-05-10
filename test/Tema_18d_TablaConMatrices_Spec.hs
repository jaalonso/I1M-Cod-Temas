module Tema_18d_TablaConMatrices_Spec (main, spec) where

import Tema_18.TablaConMatrices
import Test.Hspec
import Control.Exception (evaluate)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "valor" $ do
    it "e1" $
      valor t1 6 `shouldBe` -3
    it "e2" $
      valor t2 2 `shouldBe` 4
    it "e3" $
      evaluate (valor t2 5) `shouldThrow`
        errorCall "Ix{Integer}.index: Index (5) out of range ((1,3))"

  describe "modifica" $ do
    it "e1" $
      valor t1 6 `shouldBe` -3
    it "e2" $
      valor (modifica (6,9) t1) 6 `shouldBe` 9

  describe "tabla" $ do
    it "e1" $
      show t1 `shouldBe` "Tbl (array (1,6) [(1,1),(2,2),(3,0),(4,-1),(5,-2),(6,-3)])"
    it "e2" $
      show t2 `shouldBe` "Tbl (array (1,3) [(1,5),(2,4),(3,7)])"

  describe "tabla" $ do
    it "e1" $
      tieneValor t2 3 `shouldBe` True
    it "e2" $
      tieneValor t2 4 `shouldBe` False

  where
      f x = if x < 3 then x else 3-x
      t1  = tabla [(i,f i) | i <- [1..6] ]
      t2  = tabla [(1,5),(2,4),(3,7)]
