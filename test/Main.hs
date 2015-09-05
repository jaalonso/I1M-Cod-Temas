-- Main.hs
-- Propiedades de los códigos de los temas,
-- José A. Alonso Jiménez <jalonso@us.es> y
-- Sevilla, 5 de Septiembre de 2015
-- ---------------------------------------------------------------------

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified T1Propiedades (tests)
import qualified T2Propiedades (tests)
import qualified T3Propiedades (tests)
import qualified T4Propiedades (tests)
import qualified T5Propiedades (tests)

main :: IO ()
main = defaultMain tests

tests =
    testGroup "Comprobaciones"
    [ T1Propiedades.tests
    , T2Propiedades.tests
    , T3Propiedades.tests
    , T4Propiedades.tests
    , T5Propiedades.tests
    ]
