module Tema_24a_Fibonacci_Spec (main, spec) where

import Tema_24.Fibonacci
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Fibonacci" $ do
    it "e1" $
      fib 8 `shouldBe` 21
