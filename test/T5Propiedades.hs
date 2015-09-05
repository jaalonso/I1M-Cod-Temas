module T5Propiedades (tests) where

import I1M.Temas.T5
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

tests :: TestTree
tests = 
    testGroup "Propiedades del tema 5"
      [ testGroup "ejemplos"
         [ testCase "suma"    $ (suma (2,3))   @?= 5
         , testCase "deCeroA" $ deCeroA 5      @?= [0,1,2,3,4,5]
         , testCase "suma'1"  $ suma' 2 3      @?= 5
         , testCase "suma'2"  $ (suma' 2) 3    @?= 5
         , testCase "mult1"   $ mult 2 5 7     @?= 70 
         , testCase "mult2"   $ (mult 2) 5 7   @?= 70
         , testCase "mult3"   $ ((mult 2) 5) 7 @?= 70
         , testCase "suc"     $ suc 5          @?= 6 
         ]]

