module Tema_18a_El_tipo_array_Spec (main, spec) where

import Tema_18.El_tipo_array
import Test.Hspec
import Data.Array

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "El_tipo_array" $ do
    it "e1" $
      cuadrados 5
        `shouldBe` listArray (0,5) [0,1,4,9,16,25]
    it "e2" $
      fibs 7
        `shouldBe` listArray (0,7) [1,1,2,3,5,8,13,21]
    it "e3" $
      histograma (0,5) [3,1,4,1,5,4,2,7]
        `shouldBe` listArray (0,5) [0,2,1,1,2,1]
