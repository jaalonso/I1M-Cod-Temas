module Tema_23b_DivideVencerasOrdRapida_Spec (main, spec) where

import Tema_23.DivideVencerasOrdRapida
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ordenaRapida" $ do
    it "e1" $
      ordenaRapida [3,1,4,1,5,9,2,8] `shouldBe` [1,1,2,3,4,5,8,9]
