module Tema_23a_DivideVencerasOrdMezcla_Spec (main, spec) where

import Tema_23.DivideVencerasOrdMezcla
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ordenaPorMezcla" $ do
    it "e1" $
      ordenaPorMezcla [3,1,4,1,5,9,2,8] `shouldBe` [1,1,2,3,4,5,8,9]
