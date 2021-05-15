-- PolPropiedades.hs
-- Propiedades del TAD de los polinomios.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tema_21.PolPropiedades where

-- Nota: Hay que elegir una implementación del TAD de polinomios
import Tema_21.PolRepTDA
-- import Tema_21.PolRepDensa
-- import Tema_21.PolRepDispersa
-- import I1M.Pol

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Generador de polinomios                                            --
-- ---------------------------------------------------------------------

-- (genPol n) es un generador de polinomios. Por ejemplo,
--    λ> sample (genPol 1)
--    7*x^9 + 9*x^8 + 10*x^7 + -14*x^5 + -15*x^2 + -10
--    -4*x^8 + 2*x
--    -8*x^9 + 4*x^8 + 2*x^6 + 4*x^5 + -6*x^4 + 5*x^2 + -8*x
--    -9*x^9 + x^5 + -7
--    8*x^10 + -9*x^7 + 7*x^6 + 9*x^5 + 10*x^3 + -1*x^2
--    7*x^10 + 5*x^9 + -5
--    -8*x^10 + -7
--    -5*x
--    5*x^10 + 4*x^4 + -3
--    3*x^3 + -4
--    10*x
genPol :: Int -> Gen (Polinomio Int)
genPol 0 = return polCero
genPol _ = do
  n <- choose (0,10)
  b <- choose (-10,10)
  p <- genPol (div n 2)
  return (consPol n b p)

instance Arbitrary (Polinomio Int) where
  arbitrary = sized genPol

-- ---------------------------------------------------------------------
-- Propiedades                                                        --
-- ---------------------------------------------------------------------

-- Propiedad. polCero es el polinomio cero.
prop_polCero_es_cero :: Bool
prop_polCero_es_cero =
  esPolCero polCero

-- Comprobación.
--    λ> quickCheck prop_polCero_es_cero
--    +++ OK, passed 100 tests.

-- Propiedad. Si n es mayor que el grado de p y b no es cero, entonces
-- (consPol n b p) es un polinomio distinto del cero.
prop_consPol_no_cero :: Int -> Int -> Polinomio Int -> Property
prop_consPol_no_cero n b p =
  n > grado p && b /= 0  ==>
  not (esPolCero (consPol n b p))

-- Comprobación.
--    λ> quickCheck prop_consPol_no_cero
--    +++ OK, passed 100 tests.

-- Propiedad. (consPol (grado p) (coefLider p) (restoPol p)) es igual a p.
prop_consPol :: Polinomio Int -> Bool
prop_consPol p =
  consPol (grado p) (coefLider p) (restoPol p) == p

-- Comprobación
--    λ> quickCheck prop_consPol
--    +++ OK, passed 100 tests.

-- Propiedad. Si n es mayor que el grado de p y b no es cero, entonces
-- el grado de (consPol n b p) es n.
prop_grado :: Int -> Int -> Polinomio Int -> Property
prop_grado n b p =
  n > grado p && b /= 0 ==>
  grado (consPol n b p) == n

-- Comprobación.
--    λ> quickCheck prop_grado
--    +++ OK, passed 100 tests.

-- Propiedad. Si n es mayor que el grado de p y b no es cero, entonces
-- el coeficiente líder de (consPol n b p) es b.
prop_coefLider :: Int -> Int -> Polinomio Int -> Property
prop_coefLider n b p =
  n > grado p && b /= 0 ==>
  coefLider (consPol n b p) == b

-- Comprobación.
--    λ> quickCheck prop_coefLider
--    +++ OK, passed 100 tests.

-- Propiedad. Si n es mayor que el grado de p y b no es cero, entonces
-- el resto de (consPol n b p) es p.
prop_restoPol :: Int -> Int -> Polinomio Int -> Property
prop_restoPol n b p =
  n > grado p && b /= 0 ==>
  restoPol (consPol n b p) == p

-- Comprobación.
--    λ> quickCheck prop_restoPol
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

return []

verificaPol :: IO Bool
verificaPol = $quickCheckAll

-- La verificación es
--    λ> verificaPol
--    === prop_polCero_es_cero from PolPropiedades.hs:53 ===
--    +++ OK, passed 1 test.
--
--    === prop_consPol_no_cero from PolPropiedades.hs:63 ===
--    +++ OK, passed 100 tests; 251 discarded.
--
--    === prop_consPol from PolPropiedades.hs:73 ===
--    +++ OK, passed 100 tests.
--
--    === prop_grado from PolPropiedades.hs:83 ===
--    +++ OK, passed 100 tests; 321 discarded.
--
--    === prop_coefLider from PolPropiedades.hs:94 ===
--    +++ OK, passed 100 tests; 340 discarded.
--
--    === prop_restoPol from PolPropiedades.hs:105 ===
--    +++ OK, passed 100 tests; 268 discarded.
--
--    True
