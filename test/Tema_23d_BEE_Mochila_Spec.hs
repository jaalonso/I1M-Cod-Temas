module Tema_23d_BEE_Mochila_Spec (main, spec) where

import Tema_23.BEE_Mochila
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "valida" $ do
    it "e1" $
      buscaEE_Mochila [(2,3),(3,5),(4,6),(5,10)] 8 `shouldBe`
       ([(5,10.0),(3,5.0)],15.0)
    it "e2" $
      buscaEE_Mochila [(2,3),(3,5),(5,6)] 10 `shouldBe`
       ([(3,5.0),(3,5.0),(2,3.0),(2,3.0)],16.0)
    it "e3" $
      buscaEE_Mochila [(8,15),(15,10),(3,6),(6,13),(2,4),(4,8),(5,6),(7,7)] 35 `shouldBe`
       ([(6,13.0),(6,13.0),(6,13.0),(6,13.0),(6,13.0),(3,6.0),(2,4.0)],75.0)
    it "e4" $
      buscaEE_Mochila [(2,2.8),(3,4.4),(5,6.1)] 10 `shouldBe`
       ([(3,4.4),(3,4.4),(2,2.8),(2,2.8)],14.4)
