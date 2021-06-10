module Tema_23g_Escalada_Prim_Spec (main, spec) where

import Tema_23.Escalada_Prim
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "prim" $
    it "e1" $
      prim g1 `shouldBe` [(2,4,55),(1,3,34),(2,5,32),(1,2,12)]
