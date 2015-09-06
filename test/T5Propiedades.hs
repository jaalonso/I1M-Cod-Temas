module T5Propiedades (tests) where

import I1M.Temas.T5
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

tests :: TestTree
tests = 
    testGroup "Propiedades del tema 5"
      [ testGroup "ejemplos"
         [ testCase "concat'" $
             concat' [[1,3],[2,5,6],[4,7]] @?= [1,3,2,5,6,4,7]
         , testCase "primeros" $ primeros [(1,3),(2,5),(6,3)]  @?=  [1,2,6]
         , testCase "length'" $ length' [4,2,5]  @?=  3
         , testCase "factores" $ factores 30  @?=  [1,2,3,5,6,10,15,30]
         , testCase "primo1" $ primo 30  @?= False
         , testCase "primo2" $ primo 31  @?= True 
         , testCase "primos" $ primos 31  @?= [2,3,5,7,11,13,17,19,23,29,31]  
         , testCase "busca" $
             busca 'b' [('a',1),('b',3),('c',5),('b',2)] @?= [3,2]
         , testCase "adyacentes" $
             adyacentes [2,5,3,7]  @?=  [(2,5),(5,3),(3,7)]
         , testCase "ordenada1" $ ordenada [1,3,5,6,7]  @?=  True
         , testCase "ordenada2" $ ordenada [1,3,6,5,7]  @?=  False  
         , testCase "posiciones" $ posiciones 5 [1,5,3,5,5,7]  @?=  [1,3,4]
         , testCase "minusculas" $
             minusculas "EstoEsUnaPrueba"  @?=  "stosnarueba"  
         , testCase "ocurrencias" $ ocurrencias 'a' "Salamanca"  @?=  4  
         , testCase "let2int1" $ let2int 'a'  @?=  0
         , testCase "let2int2" $ let2int 'd'  @?=  3
         , testCase "let2int3" $ let2int 'z'  @?=  25
         , testCase "int2let1" $ int2let 0   @?=  'a'
         , testCase "int2let2" $ int2let 3   @?=  'd'
         , testCase "int2let3" $ int2let 25  @?=  'z'
         , testCase "desplaza1" $ desplaza   3  'a'  @?=  'd'
         , testCase "desplaza2" $ desplaza   3  'y'  @?=  'b'
         , testCase "desplaza3" $ desplaza (-3) 'd'  @?=  'a'
         , testCase "desplaza4" $ desplaza (-3) 'b'  @?=  'y'
         , testCase "codifica1" $
             codifica   3  "En todo la medida"
               @?=  "Eq wrgr od phglgd"
         , testCase "codifica2" $
             codifica (-3) "Eq wrgr od phglgd"
               @?=  "En todo la medida"
         , testCase "porcentaje" $ porcentaje 2 5  @?=  40.0
         , testCase "frecuencias" $ 
             frecuencias "en todo la medida" 
               @?= [14.285715,0.0,0.0,21.428572,14.285715,0.0,0.0,0.0,
                    7.1428576,0.0,0.0,7.1428576,7.1428576,7.1428576,
                    14.285715,0.0,0.0,0.0,0.0,7.1428576,0.0,0.0,0.0,0.0,0.0,0.0]
         , testCase "chiCuadrado1" $ chiCuadrado [3,5,6] [3,5,6]  @?=  0.0
         , testCase "chiCuadrado2" $ chiCuadrado [3,5,6] [5,6,3]  @?=  3.9666667
         , testCase "rota" $ rota 2 "ramo"  @?=  "mora"
         , testCase "descifra" $
             descifra "Lt htruqnhfit ij qf anif jx ijxhzgwnw qt xnruqj vzj jx"
               @?= "Lo complicado de la vida es descubrir lo simple que es"
         ]
      , testGroup "propiedades"
         [ testProperty "P1" prop_desplaza
         , testProperty "P2" prop_codifica
         ]]


