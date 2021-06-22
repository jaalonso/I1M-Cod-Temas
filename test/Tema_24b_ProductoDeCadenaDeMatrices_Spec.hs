module Tema_24b_ProductoDeCadenaDeMatrices_Spec (main, spec) where

import Tema_24.ProductoDeCadenaDeMatrices
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "pcm" $ do
    it "e1" $
      show (pcm [30,1,40,10,25]) `shouldBe` "(1400,(A1*((A2*A3)*A4)))"
