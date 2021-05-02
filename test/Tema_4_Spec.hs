module Tema_4_Spec (main, spec) where

import Tema_4
import Test.Hspec

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

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "isDigit" $ do
    it "e1" $
      isDigit '3'  `shouldBe`  True
    it "e2" $
      isDigit 'c'  `shouldBe`  False
  describe "even" $ do
    it "e1" $
      even 6  `shouldBe`  True
    it "e2" $
      even 7  `shouldBe`  False
  describe "splitAt" $ do
    it "e1" $
      splitAt 2 [3,5,7,9,4]  `shouldBe`  ([3,5],[7,9,4])
  describe "abs" $ do
    it "e1" $
      abs (-5)  `shouldBe`  5
  describe "signum" $ do
    it "e1" $
      signum (-5)  `shouldBe`  -1
    it "e2" $
      signum 0     `shouldBe`  0
    it "e3" $
      signum 7     `shouldBe`  1
  describe "abs'" $ do
    it "e1" $
      abs' (-5)  `shouldBe`  5
  describe "signum'" $ do
    it "e1" $
      signum' (-5)  `shouldBe`  -1
    it "e2" $
      signum' 0     `shouldBe`  0
    it "e3" $
      signum' 7     `shouldBe`  1
  describe "fst" $ do
    it "e1" $
      fst (5,3)  `shouldBe`  5
  describe "snd" $ do
    it "e1" $
      snd (5,3)  `shouldBe`  3
  describe "test1" $ do
    it "e1" $
      test1 ['a','b','c']  `shouldBe`  True
    it "e2" $
      test1 ['b','a','c']  `shouldBe`  False
    it "e3" $
      test2 ['a','b']      `shouldBe`  True
    it "e4" $
      test2 ['a','b','c']  `shouldBe`  True
    it "e5" $
      test2 ['b','a','c']  `shouldBe`  False
  describe "null" $ do
    it "e1" $
      null []     `shouldBe`  True
    it "e2" $
      null [3,2]  `shouldBe`  False
  describe "head" $
    it "e1" $
      head [3,2,5]  `shouldBe`  3
  describe "tail" $
    it "e1" $
      tail [3,2,5]  `shouldBe`  [2,5]
  describe "pred" $
    it "e1" $
      pred 5  `shouldBe`  4
  describe "suma" $
    it "e1" $
      suma 2 3  `shouldBe`  5
  describe "suma'" $
    it "e1" $
      suma' 2 3  `shouldBe`  5
  describe "const" $ do
    it "e1" $
      const 2 3  `shouldBe`  2
    it "e2" $
      const 2 7  `shouldBe`  2
  describe "const'" $ do
    it "e1" $
      const' 2 3  `shouldBe`  2
    it "e2" $
      const' 2 7  `shouldBe`  2
  describe "impares" $
    it "e1" $
      impares 4  `shouldBe`  [1,3,5,7]
  describe "impares'" $
    it "e1" $
      impares' 4  `shouldBe`  [1,3,5,7]
  describe "suma''" $
    it "e1" $
      suma'' 2 3  `shouldBe`  5
  describe "siguiente" $
    it "e1" $
      siguiente 3  `shouldBe`  4
  describe "inverso" $
    it "e1" $
      inverso 5  `shouldBe`  0.2
  describe "doble" $
    it "e1" $
      doble 3  `shouldBe`  6
  describe "mitad" $
    it "e1" $
      mitad 6  `shouldBe`  3.0
