module T4Propiedades (tests) where

import Prelude 
    hiding ( (&&)
           , abs
           , const
           , even
           , fst
           , head
           , not
           , null
           , pred
           , signum
           , snd
           , splitAt
           , tail)

import I1M.Temas.T4
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

tests :: TestTree
tests = 
    testGroup "Propiedades del tema 4"
      [ testGroup "ejemplos"
         [ testCase "isDigit1" $ isDigit '3'  @?=  True
         , testCase "isDigit2" $ isDigit 'c'  @?=  False
         , testCase "even1"    $ even 6  @?=  True
         , testCase "even2"    $ even 7  @?=  False
         , testCase "splitAt"  $ splitAt 2 [3,5,7,9,4]  @?=  ([3,5],[7,9,4])
         , testCase "abs"      $ abs (-5)  @?=  5
         , testCase "signum1"  $ signum (-5)  @?=  -1
         , testCase "signum2"  $ signum 0     @?=  0
         , testCase "signum3"  $ signum 7     @?=  1
         , testCase "abs'"     $ abs' (-5)  @?=  5
         , testCase "signum'1" $ signum' (-5)  @?=  -1
         , testCase "signum'2" $ signum' 0     @?=  0
         , testCase "signum'3" $ signum' 7     @?=  1
         , testCase "fst"      $ fst (5,3)  @?=  5
         , testCase "snd"      $ snd (5,3)  @?=  3
         , testCase "null1"    $ null []     @?=  True
         , testCase "null2"    $ null [3,2]  @?=  False
         , testCase "head"     $ head [3,2,5]  @?=  3
         , testCase "tail"     $ tail [3,2,5]  @?=  [2,5]
         , testCase "pred"     $ pred 5  @?=  4
         , testCase "suma"     $ suma 2 3  @?=  5
         , testCase "suma'"    $ suma' 2 3  @?=  5
         , testCase "const1"   $ const 2 3  @?=  2
         , testCase "const2"   $ const 2 7  @?=  2
         , testCase "const'1"  $ const' 2 3  @?=  2
         , testCase "const'2"  $ const' 2 7  @?=  2
         , testCase "impares"  $ impares 4  @?=  [1,3,5,7]
         , testCase "impares'" $ impares' 4  @?=  [1,3,5,7]
         , testCase "suma''"   $ suma'' 2 3  @?=  5
         , testCase "siguiente" $ siguiente 3  @?=  4
         , testCase "inverso"  $ inverso 5  @?=  0.2
         , testCase "doble"    $ doble 3  @?=  6 
         , testCase "mitad"    $ mitad 6  @?=  3.0
         ]]

