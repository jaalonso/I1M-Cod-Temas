module Tema_2_Spec (main, spec) where

import Tema_2
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "doble" $
    it "e1" $
      doble 3 `shouldBe` 6

  describe "cuadruple" $
    it "e1" $
      cuadruple 3 `shouldBe` 12

  describe "factorial" $
    it "e1" $
      factorial 4 `shouldBe` 24

  describe "media" $
    it "e1" $
      media [1,5.0,3]  `shouldBe`  3
