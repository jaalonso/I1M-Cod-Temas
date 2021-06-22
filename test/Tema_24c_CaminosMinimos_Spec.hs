module Tema_24c_CaminosMinimos_Spec (main, spec) where

import Tema_24.CaminosMinimos
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "caminosMinimos" $ do
    it "e1" $
      caminosMinimos ej1Grafo `shouldBe`
        [((1,2),(2,[1,3,2])),  ((1,3),(1,[1,3])),  ((1,4),(5,[1,3,6,4])),
         ((1,5),(7,[1,3,2,5])),((1,6),(3,[1,3,6])),((2,3),(1,[2,3])),
         ((2,4),(5,[2,3,6,4])),((2,5),(5,[2,5])),  ((2,6),(3,[2,3,6])),
         ((3,4),(4,[3,6,4])),  ((3,5),(6,[3,2,5])),((3,6),(2,[3,6])),
         ((4,5),(7,[4,6,5])),  ((4,6),(2,[4,6])),  ((5,6),(5,[5,6]))]
    it "e2" $
      caminosMinimos ej2Grafo `shouldBe`
        [((1,2),(1,[1,2])),     ((1,3),(4,[1,2,3])),     ((1,4),(6,[1,4])),
         ((1,5),(11,[1,4,5])),  ((1,6),(8,[1,4,6])),     ((2,3),(3,[2,3])),
         ((2,4),(10,[2,1,4])),  ((2,5),(15,[2,1,4,5])),  ((2,6),(12,[2,1,4,6])),
         ((3,4),(13,[3,2,1,4])),((3,5),(18,[3,2,1,4,5])),((3,6),(15,[3,2,1,4,6])),
         ((4,5),(5,[4,5])),     ((4,6),(2,[4,6])),       ((5,6),(3,[5,6]))]
