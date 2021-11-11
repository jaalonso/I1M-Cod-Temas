module Tema_30_Programacion_dinamica_Spec (main, spec) where

import Tema_30
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fib" $ do
    it "e1" $
      fib1 6 `shouldBe` 8
    it "e2" $
      fib2 6 `shouldBe` 8

  describe "binomial" $ do
    it "e1" $
      binomial1 5 2 `shouldBe` 10
    it "e2" $
      binomial2 5 2 `shouldBe` 10

  describe "longitudSCM" $ do
    it "e1" $
      longitudSCM1 "amapola" "matamoscas" `shouldBe` 4
    it "e2" $
      longitudSCM1 "atamos" "matamoscas"  `shouldBe` 6
    it "e3" $
      longitudSCM1 "aaa" "bbbb"           `shouldBe` 0
    it "e4" $
      longitudSCM2 "amapola" "matamoscas" `shouldBe` 4
    it "e5" $
      longitudSCM2 "atamos" "matamoscas"  `shouldBe` 6
    it "e6" $
      longitudSCM2 "aaa" "bbbb"           `shouldBe` 0

  describe "scm" $ do
    it "e1" $
      scm1 "amapola" "matamoscas" `shouldBe` "amoa"
    it "e2" $
      scm1 "atamos" "matamoscas"  `shouldBe` "atamos"
    it "e3" $
      scm1 "aaa" "bbbb"           `shouldBe` ""
    it "e4" $
      scm2 "amapola" "matamoscas" `shouldBe` "amoa"
    it "e5" $
      scm2 "atamos" "matamoscas"  `shouldBe` "atamos"
    it "e6" $
      scm2 "aaa" "bbbb"           `shouldBe` ""

  describe "levenshtein" $ do
    it "e1" $
      levenshtein1 "casa"  "calle"    `shouldBe`  3
    it "e2" $
      levenshtein1 "calle" "casa"     `shouldBe`  3
    it "e3" $
      levenshtein1 "casa"  "casa"     `shouldBe`  0
    it "e4" $
      levenshtein1 "ana" "maria"      `shouldBe`  3
    it "e5" $
      levenshtein1 "agua" "manantial" `shouldBe`  7
    it "e6" $
      levenshtein2 "casa"  "calle"    `shouldBe`  3
    it "e7" $
      levenshtein2 "calle" "casa"     `shouldBe`  3
    it "e8" $
      levenshtein2 "casa"  "casa"     `shouldBe`  0
    it "e9" $
      levenshtein2 "ana" "maria"      `shouldBe`  3
    it "e10" $
      levenshtein2 "agua" "manantial" `shouldBe`  7
