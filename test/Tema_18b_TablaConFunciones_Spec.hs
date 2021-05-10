module Tema_18b_TablaConFunciones_Spec (main, spec) where

import Tema_18.TablaConFunciones
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
      valor t2 2 `shouldBe` 67
    it "e3" $
      evaluate (valor t2 5 ) `shouldThrow` errorCall "fuera de rango"

  describe "modifica" $ do
    it "e1" $
      valor t1 6 `shouldBe` -3
    it "e2" $
      valor (modifica (6,9) t1) 6 `shouldBe` 9

  describe "tabla" $ do
    it "e1" $
      show (tabla [(4,89), (1,90), (2,67)]) `shouldBe` "<<Una tabla>>"
    where
      f x = if x < 3 then x else 3-x
      t1  = tabla [(i,f i) | i <- [1..6] ]
      t2  = tabla [(4,89), (1,90), (2,67)]
