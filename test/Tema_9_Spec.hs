module Tema_9_Spec (main, spec) where

import Tema_9
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "izquierda" $
    it "e1" $
      izquierda (3,5)  `shouldBe`  (2,5)
  describe "multiplica" $
    it "e1" $
      multiplica (2,5)  `shouldBe`  10
  describe "copia" $
    it "e1" $
      copia 5  `shouldBe`  (5,5)
  describe "movimiento" $ do
    it "e1" $
      movimiento Izquierda (2,3)  `shouldBe`  (1,3)
    it "e2" $
      movimiento Derecha   (2,3)  `shouldBe`  (3,3)
    it "e3" $
      movimiento Arriba    (2,3)  `shouldBe`  (2,4)
    it "e4" $
      movimiento Abajo     (2,3)  `shouldBe`  (2,2)
  describe "movimientos" $
    it "e1" $
      movimientos [Izquierda, Arriba] (2,3)  `shouldBe`  (1,4)
  describe "cuadrado" $
    it "e1" $
      cuadrado 3  `shouldBe`  Rect 3 3
  describe "area" $ do
    it "e1" $
      area (Circulo 1)  `shouldBe`  3.1415927
    it "e2" $
      area (Circulo 2)  `shouldBe`  12.566371
    it "e3" $
      area (Rect 2 5)   `shouldBe`  10.0
    it "e4" $
      area (cuadrado 3) `shouldBe`   9
  describe "divisionSegura" $ do
    it "e1" $
      divisionSegura 6 3  `shouldBe`  Just 2
    it "e2" $
      divisionSegura 6 0  `shouldBe`  Nothing
  describe "headSegura" $ do
    it "e1" $
      headSegura [2,3,5]  `shouldBe`  Just 2
    it "e2" $
      headSegura ([]::[Int]) `shouldBe`  Nothing
  describe "nat2int" $
    it "e1" $
      nat2int (Suc (Suc (Suc Cero)))  `shouldBe`  3
  describe "int2nat" $
    it "e1" $
      int2nat 3  `shouldBe`  Suc (Suc (Suc Cero))
  describe "suma" $
    it "e1" $
      suma (Suc (Suc Cero)) (Suc Cero)  `shouldBe`  Suc (Suc (Suc Cero))
  describe "longitud" $
    it "e1" $
      longitud (Cons 2 (Cons 3 (Cons 5 Nil)))  `shouldBe`  3
  describe "ocurre" $ do
    it "e1" $
      ocurre 4  ejArbol  `shouldBe`  True
    it "e2" $
      ocurre 10 ejArbol  `shouldBe`  False
  describe "aplana" $
    it "e1" $
      aplana ejArbol  `shouldBe`  [1,3,4,5,6,7,9]
  describe "ocurreEnArbolOrdenado" $ do
    it "e1" $
      ocurreEnArbolOrdenado 4 ejArbol   `shouldBe`  True
    it "e2" $
      ocurreEnArbolOrdenado 10 ejArbol  `shouldBe`  False
  describe "valor" $ do
    it "e1" $
      valor [('A',False),('B',True)] p3  `shouldBe`  True
    it "e2" $
      valor [('A',True),('B',False)] p3  `shouldBe`  False
  describe "busca" $
    it "e1" $
      busca 2 [(1,'a'),(3,'d'),(2,'c')]  `shouldBe`  'c'
  describe "variables" $
    it "e1" $
      variables p3  `shouldBe`  "AAB"
  describe "esTautologia" $ do
    it "e1" $
      esTautologia p1  `shouldBe`  False
    it "e2" $
      esTautologia p2  `shouldBe`  True
    it "e3" $
      esTautologia p3  `shouldBe`  False
    it "e4" $
      esTautologia p4  `shouldBe`  True
  describe "valorEA" $
    it "e1" $
      valorEA (Suma (Suma (Num 2) (Num 3)) (Num 4))  `shouldBe`  9
  describe "eval" $ do
    it "e1" $
      eval (Suma (Suma (Num 2) (Num 3)) (Num 4)) []  `shouldBe`  9
    it "e2" $
      eval (Suma (Num 2) (Num 3)) [METE (Num 4)]     `shouldBe`  9
    it "e3" $
      eval (Num 3) [SUMA 2, METE (Num 4)]            `shouldBe`  9
    it "e4" $
      eval (Num 4) [SUMA 5]                          `shouldBe`  9
  describe "ejec" $ do
    it "e1" $
      ejec [METE (Num 3), METE (Num 4)] 2  `shouldBe`  9
    it "e2" $
      ejec [SUMA 2, METE (Num 4)]       3  `shouldBe`  9
    it "e3" $
      ejec [METE (Num 4)]               5  `shouldBe`  9
    it "e4" $
      ejec [SUMA 5]                     4  `shouldBe`  9
    it "e5" $
      ejec []                           9  `shouldBe`  9
  describe "evalua" $
    it "e1" $
      evalua (Suma (Suma (Num 2) (Num 3)) (Num 4))  `shouldBe`  9
