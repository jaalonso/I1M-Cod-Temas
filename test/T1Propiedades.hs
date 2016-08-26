module T1Propiedades (tests) where

import I1M.Temas.T1
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

tests :: TestTree
tests = 
    testGroup "Propiedades del tema 1"
      [ testGroup "ejemplos"
         [ testCase "doble1" $ doble 3         @?= 6
         , testCase "doble2" $ doble (doble 3) @?= 12
         , testCase "suma"   $ suma 4          @?= 10
         ]
      , testGroup "propiedades"
         [ testProperty "P1" prop_doble1
         ]]

prop_doble1 :: Int -> Int -> Bool
prop_doble1 x y = doble (x+y) == (doble x) + (doble y)  

