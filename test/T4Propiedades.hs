module T4Propiedades (tests) where

import I1M.Temas.T4
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

tests :: TestTree
tests = 
    testGroup "Propiedades del tema 4"
      [ testGroup "ejemplos"
         [ testCase "isDigit1" $ isDigit '3'  @?=  True
         , textCase "isDigit2" $ isDigit 'c'  @?=  False
         , textCase "even1"    $ even 6  @?=  True
         , textCase "even2"    $ even 7  @?=  False
         , textCase "splitAt"  $ splitAt 2 [3,5,7,9,4]  @?=  ([3,5],[7,9,4])
         , textCase "abs"      $ abs (-5)  @?=  5
         , textCase "signum1"  $ signum (-5)  @?=  -1
         , textCase "signum2"  $ signum 0     @?=  0
         , textCase "signum3"  $ signum 7     @?=  1
         , textCase "abs'"     $ abs' (-5)  @?=  5
         , textCase "signum'1" $ signum' (-5)  @?=  -1
         , textCase "signum'2" $ signum' 0     @?=  0
         , textCase "signum'3" $ signum' 7     @?=  1
         , textCase "fst"      $ fst (5,3)  @?=  5
         , textCase "snd"      $ snd (5,3)  @?=  3
         , textCase "null1"    $ null []     @?=  True
         , textCase "null2"    $ null [3,2]  @?=  False
         , textCase "head"     $ head [3,2,5]  @?=  3
         , textCase "tail"     $ tail [3,2,5]  @?=  [2,5]
         , textCase "pred"     $ pred 5  @?=  4
         , textCase "suma"     $ suma 2 3  @?=  5
         , textCase "suma'"    $ suma' 2 3  @?=  5
         , textCase "const1"   $ const 2 3  @?=  2
         , textCase "const2"   $ const 2 7  @?=  2
         , textCase "const'1"  $ const' 2 3  @?=  2
         , textCase "const'2"  $ const' 2 7  @?=  2
         , textCase "impares"  $ impares 4  @?=  [1,3,5,7]
         , textCase "impares'" $ impares' 4  @?=  [1,3,5,7]
         , textCase "suma''"   $ suma'' 2 3  @?=  5
         , textCase "siguiente" $ siguiente 3  @?=  4
         , textCase "inverso"  $ inverso 5  @?=  0.2
         , textCase "doble"    $ doble 3  @?=  6 
         , textCase "mitad"    $ mitad 6  @?=  3.0
         ]]

