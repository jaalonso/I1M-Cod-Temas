module Tema_12_Analizador_Spec (main, spec) where

import Tema_12.Analizador
import Test.Hspec
import Control.Exception (evaluate)
import Data.Char (isLower)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Analizador" $ do
    it "e1" $
      analiza (resultado 3) "Hola"  `shouldBe`  [(3,"Hola")]
    it "e2" $
      analiza fallo "Hola"  `shouldBe` ([] :: [(Char, String)])
    it "e3" $
      analiza elemento "Hola"  `shouldBe`  [('H',"ola")]
    it "e4" $
      analiza elemento "" `shouldBe` ([] :: [(Char, String)])
    it "e5" $
      analiza (elemento +++ resultado 'd') "abc"  `shouldBe`  [('a',"bc")]
    it "e6" $
      analiza (fallo +++ resultado 'd') "abc"     `shouldBe`  [('d',"abc")]
    it "e7" $
      analiza (fallo +++ fallo) "abc" `shouldBe` ([] :: [(Char, String)])
    it "e8" $
      analiza (sat isLower) "hola"  `shouldBe`  [('h',"ola")]
    it "e9" $
      analiza (sat isLower) "Hola" `shouldBe` ([] :: [(Char, String)])
    it "e10" $
      analiza digito "123"  `shouldBe`  [('1',"23")]
    it "e11" $
      analiza digito "uno"  `shouldBe` ([] :: [(Char, String)])
    it "e12" $
      analiza minuscula "eva"  `shouldBe`  [('e',"va")]
    it "e13" $
      analiza minuscula "Eva"  `shouldBe` ([] :: [(Char, String)])
    it "e14" $
      analiza mayuscula "Eva"  `shouldBe`  [('E',"va")]
    it "e15" $
      analiza mayuscula "eva"  `shouldBe` ([] :: [(Char, String)])
    it "e16" $
      analiza letra "Eva"  `shouldBe`  [('E',"va")]
    it "e17" $
      analiza letra "eva"  `shouldBe`  [('e',"va")]
    it "e18" $
      analiza letra "123"  `shouldBe` ([] :: [(Char, String)])
    it "e19" $
      analiza alfanumerico "Eva"   `shouldBe`  [('E',"va")]
    it "e20" $
      analiza alfanumerico "eva"   `shouldBe`  [('e',"va")]
    it "e21" $
      analiza alfanumerico "123"   `shouldBe`  [('1',"23")]
    it "e22" $
      analiza alfanumerico " 123"  `shouldBe` ([] :: [(Char, String)])
    it "e23" $
      analiza (caracter 'E') "Eva"  `shouldBe`  [('E',"va")]
    it "e24" $
      analiza (caracter 'E') "eva"  `shouldBe`  ([] :: [(Char, String)])
    it "e25" $
      analiza (cadena "abc") "abcdef"  `shouldBe`  [("abc","def")]
    it "e26" $
      analiza (cadena "abc") "abdcef"  `shouldBe`  ([] :: [(String, String)])
    it "e27" $
      analiza (varios digito) "235abc"  `shouldBe`  [("235","abc")]
    it "e28" $
      analiza (varios digito) "abc235"  `shouldBe`  [("","abc235")]
    it "e29" $
      analiza (varios1 digito) "235abc"  `shouldBe`  [("235","abc")]
    it "e30" $
      analiza (varios1 digito) "abc235"  `shouldBe`  ([] :: [(String, String)])
    it "e31" $
      analiza ident "lunes12 de Ene"  `shouldBe`  [("lunes12"," de Ene")]
    it "e32" $
      analiza ident "Lunes12 de Ene"  `shouldBe`  ([] :: [(String, String)])
    it "e33" $
      analiza nat "14DeAbril"   `shouldBe`  [(14,"DeAbril")]
    it "e34" $
      analiza nat " 14DeAbril"  `shouldBe`  ([] :: [(Int, String)])
    it "e35" $
      analiza espacio "    a b c"  `shouldBe`  [((),"a b c")]
    it "e36" $
      analiza (unidad nat) " 14DeAbril"     `shouldBe`  [(14,"DeAbril")]
    it "e37" $
      analiza (unidad nat) " 14   DeAbril"  `shouldBe`  [(14,"DeAbril")]
    it "e38" $
      analiza identificador "  lunes12  de Ene"  `shouldBe`  [("lunes12","de Ene")]
    it "e39" $
      analiza natural "  14DeAbril"  `shouldBe`  [(14,"DeAbril")]
    it "e40" $
      analiza (simbolo "abc") "  abcdef"  `shouldBe`  [("abc","def")]
    it "e41" $
      analiza listaNat " [  2,  3, 5   ]"  `shouldBe`  [([2,3,5],"")]
    it "e42" $
      analiza listaNat " [  2,  3,]"       `shouldBe`  ([] :: [([Int], String)])
    it "e43" $
      analiza expr "2*3+5"     `shouldBe`  [(11,"")]
    it "e44" $
      analiza expr "2*(3+5)"   `shouldBe`  [(16,"")]
    it "e45" $
      analiza expr "2+3*5"     `shouldBe`  [(17,"")]
    it "e46" $
      analiza expr "2*3+5abc"  `shouldBe`  [(11,"abc")]
    it "e47" $
      analiza term "2*3+5"  `shouldBe`  [(6,"+5")]
    it "e48" $
      analiza factor "2*3+5"      `shouldBe`  [(2,"*3+5")]
    it "e49" $
      analiza factor "(2+3)*5"    `shouldBe`  [(5,"*5")]
    it "e50" $
      analiza factor "(2+3*7)*5"  `shouldBe`  [(23,"*5")]
    it "e51" $
      valor "2*3+5"      `shouldBe`  11
    it "e52" $
      valor "2*(3+5)"    `shouldBe`  16
    it "e53" $
      valor "2 * 3 + 5"  `shouldBe`  11
    it "e54" $
      evaluate (valor "2*3x+5y") `shouldThrow`
        errorCall "entrada sin usar x+5y"
    it "e55" $
      evaluate (valor "-1") `shouldThrow`
        errorCall "entrada no valida"
