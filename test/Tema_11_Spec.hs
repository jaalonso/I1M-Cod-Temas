module Tema_11_Spec (main, spec) where

import Tema_11
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "valida" $ do
    it "e1" $
      valida Res 5 3  `shouldBe`  True
    it "e2" $
      valida Res 3 5  `shouldBe`  False
    it "e3" $
      valida Div 6 3  `shouldBe`  True
    it "e4" $
      valida Div 6 4  `shouldBe`  False
  describe "aplica" $ do
    it "e1" $
      aplica Sum 2 3  `shouldBe`  5
    it "e1" $
      aplica Div 6 3  `shouldBe`  2
  describe "numeros" $
    it "e1" $
      numeros (Apl Mul (Apl Sum (Num 2) (Num 3)) (Num 7))
      `shouldBe` [2,3,7]
  describe "valor" $ do
    it "e1" $
      valor (Apl Mul (Apl Sum (Num 2) (Num 3)) (Num 7)) `shouldBe` [35]
    it "e2" $
      valor (Apl Res (Apl Sum (Num 2) (Num 3)) (Num 7)) `shouldBe` []
    it "e3" $
      valor (Apl Sum (Apl Res (Num 2) (Num 3)) (Num 7)) `shouldBe` []
  describe "sublistas" $ do
    it "e1" $
      sublistas "bc" `shouldBe` ["","c","b","bc"]
    it "e2" $
      sublistas "abc" `shouldBe` ["","c","b","bc","a","ac","ab","abc"]
  describe "intercala" $ do
    it "e1" $
      intercala 'x' "bc"   `shouldBe`  ["xbc","bxc","bcx"]
    it "e2" $
      intercala 'x' "abc"  `shouldBe`  ["xabc","axbc","abxc","abcx"]
  describe "permutaciones" $ do
    it "e1" $
      permutaciones "bc"  `shouldBe`  ["bc","cb"]
    it "e2" $
      permutaciones "abc"  `shouldBe`  ["abc","bac","bca","acb","cab","cba"]
  describe "elecciones" $
    it "e1" $
      elecciones "abc" `shouldBe`
        ["","c","b","bc","cb","a","ac","ca","ab","ba",
         "abc","bac","bca","acb","cab","cba"]
  describe "solucion" $
    it "e1" $
      solucion ejExpr [1,3,7,10,25,50] 765  `shouldBe`  True
  describe "divisiones" $ do
    it "e1" $
      divisiones "bcd"  `shouldBe`  [("b","cd"),("bc","d")]
    it "e2" $
      divisiones "abcd"  `shouldBe`  [("a","bcd"),("ab","cd"),("abc","d")]
  describe "expresiones" $
    it "e1" $
      map show (take 18 (expresiones [2,3,5])) `shouldBe`
        ["2+(3+5)","2-(3+5)","2*(3+5)","2/(3+5)","2+(3-5)","2-(3-5)",
         "2*(3-5)","2/(3-5)","2+(3*5)","2-(3*5)","2*(3*5)","2/(3*5)",
         "2+(3/5)","2-(3/5)","2*(3/5)","2/(3/5)","(2+3)+5","(2+3)-5"]
  describe "combina" $
    it "e1" $
      map show (combina (Num 2) (Num 3)) `shouldBe`
        ["2+3","2-3","2*3","2/3"]
  describe "soluciones" $ do
    it "e1" $
      show (head (soluciones [1,3,7,10,25,50] 765)) `shouldBe`
         "3*((7*(50-10))-25)"
    -- it "e2" $
    --   length (soluciones [1,3,7,10,25,50] 765) `shouldBe` 780
    -- it "e3" $
    --   length (soluciones [1,3,7,10,25,50] 831) `shouldBe` 0
  describe "resultados" $
    it "e1" $
      map show (resultados [2,3,5]) `shouldBe`
        ["(2+(3+5),10)","(2*(3+5),16)","(2+(3*5),17)","(2*(3*5),30)","((2+3)+5,10)",
         "((2+3)*5,25)","((2+3)/5,1)","((2*3)+5,11)","((2*3)-5,1)","((2*3)*5,30)"]
  describe "combina'" $ do
    it "e1" $
      map show (combina' (Num 2,2) (Num 3,3)) `shouldBe`
        ["(2+3,5)","(2*3,6)"]
    it "e2" $
      map show (combina' (Num 3,3) (Num 2,2))  `shouldBe` ["(3+2,5)","(3-2,1)","(3*2,6)"]
    it "e3" $
      map show (combina' (Num 2,2) (Num 6,6)) `shouldBe` ["(2+6,8)","(2*6,12)"]
    it "e4" $
      map show (combina' (Num 6,6) (Num 2,2)) `shouldBe` ["(6+2,8)","(6-2,4)","(6*2,12)","(6/2,3)"]
  describe "soluciones'" $ do
    it "e1" $
      show (head (soluciones' [1,3,7,10,25,50] 765)) `shouldBe` "3*((7*(50-10))-25)"
    -- it "e2" $
    --   length (soluciones' [1,3,7,10,25,50] 765)`shouldBe` 780
    -- it "e3" $
    --   length (soluciones' [1,3,7,10,25,50] 831) `shouldBe` 0
  describe "resultados'" $
    it "e1" $
      map show (resultados' [5,3,2]) `shouldBe`
        ["(5-(3-2),4)","((5-3)+2,4)","((5-3)*2,4)","((5-3)/2,1)"]
  describe "combina''" $ do
    it "e1" $
      map show (combina'' (Num 2,2) (Num 3,3)) `shouldBe` ["(2+3,5)","(2*3,6)"]
    it "e2" $
      map show (combina'' (Num 3,3) (Num 2,2)) `shouldBe` ["(3-2,1)"]
    it "e3" $
      map show (combina'' (Num 2,2) (Num 6,6)) `shouldBe` ["(2+6,8)","(2*6,12)"]
    it "e4" $
      map show (combina'' (Num 6,6) (Num 2,2)) `shouldBe` ["(6-2,4)","(6/2,3)"]
  describe "soluciones''" $ do
    it "e1" $
      show (head (soluciones'' [1,3,7,10,25,50] 765)) `shouldBe` "3*((7*(50-10))-25)"
    -- it "e2" $
    --   length (soluciones'' [1,3,7,10,25,50] 765) `shouldBe` 49
    -- it "e3" $
    --   length (soluciones'' [1,3,7,10,25,50] 831) `shouldBe`       0
  describe "reinas" $
    it "e1" $
      reinas 4  `shouldBe` [[3,1,4,2],[2,4,1,3]]
  describe "hamming" $
    it "e1" $
      take 12 hamming `shouldBe` [1,2,3,4,5,6,8,9,10,12,15,16]
  describe "mezcla3" $
    it "e1" $
      mezcla3 [2,4,6,8,10] [3,6,9,12] [5,10] `shouldBe`
        [2,3,4,5,6,8,9,10,12]
  describe "mezcla2" $
    it "e1" $
      mezcla2 [2,4,6,8,10,12] [3,6,9,12]  `shouldBe`  [2,3,4,6,8,9,10,12]
