module T2Propiedades (tests) where

import I1M.Temas.T2
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

tests :: TestTree
tests = 
    testGroup "Propiedades del tema 2"
      [ testGroup "ejemplos"
         [ testCase "doble"     $ doble 3       @?= 6
         , testCase "cuadruple" $ cuadruple 3   @?= 12
         , testCase "factorial" $ factorial 4   @?= 24
         , testCase "media"     $ media [1,5,3] @?= 3
         ]]

