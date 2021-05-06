-- PilaPropiedades.hs
-- Propiedades del TAD pilas.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tema_14.PilaPropiedades where

-- Hay que elegir una implementación del TAD pilas.
import Tema_14.PilaConTipoDeDatoAlgebraico
-- import Tema_14.PilaConListas
-- import I1M.Pila

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Generador de pilas                                          --
-- ---------------------------------------------------------------------

-- genPila es un generador de pilas. Por ejemplo,
--    λ> sample genPila
--    -
--    0|0|-
--    -
--    -6|4|-3|3|0|-
--    -
--    9|5|-1|-3|0|-8|-5|-7|2|-
--    -3|-10|-3|-12|11|6|1|-2|0|-12|-6|-
--    2|-14|-5|2|-
--    5|9|-
--    -1|-14|5|-
--    6|13|0|17|-12|-7|-8|-19|-14|-5|10|14|3|-18|2|-14|-11|-6|-
genPila :: (Arbitrary a, Num a) => Gen (Pila a)
genPila = do
  xs <- listOf arbitrary
  return (foldr apila vacia xs)

-- El tipo pila es una instancia del arbitrario.
instance (Arbitrary a, Num a) => Arbitrary (Pila a) where
  arbitrary = genPila

-- ---------------------------------------------------------------------
-- Propiedades
-- ---------------------------------------------------------------------

-- Propiedad. La cima de la pila que resulta de apilar x en una pila p
-- es x.
prop_cima_apila :: Int -> Pila Int -> Bool
prop_cima_apila x p =
  cima (apila x p) == x

-- Comprobación.
--    λ> quickCheck prop_cima_apila
--    +++ OK, passed 100 tests.

-- Propiedad. La pila que resulta de desapilar después de apilar
-- cualquier elemento es la pila inicial.
prop_desapila_apila :: Int -> Pila Int -> Bool
prop_desapila_apila x p =
  desapila (apila x p) == p

-- Comprobación.
--    λ> quickCheck prop_desapila_apila
--    +++ OK, passed 100 tests.

-- Propiedad. La pila vacía está vacía.
prop_vacia_esta_vacia :: Bool
prop_vacia_esta_vacia = esVacia vacia

-- Comprobación.
--    λ> quickCheck prop_vacia_esta_vacia
--    +++ OK, passed 100 tests.

-- Propiedad. La pila que resulta de apilar un elemento en un pila
-- cualquiera no es vacía.
prop_apila_no_es_vacia :: Int -> Pila Int -> Bool
prop_apila_no_es_vacia x p = not (esVacia (apila x p))

-- Comprobación.
--    λ> quickCheck prop_apila_no_es_vacia
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

return []

verificaPila :: IO Bool
verificaPila = $quickCheckAll

-- La verificación es
--    λ> verificaPila
--    === prop_cima_apila from PilaPropiedades.hs:49 ===
--    +++ OK, passed 100 tests.
--
--    === prop_desapila_apila from PilaPropiedades.hs:59 ===
--    +++ OK, passed 100 tests.
--
--    === prop_vacia_esta_vacia from PilaPropiedades.hs:68 ===
--    +++ OK, passed 1 test.
--
--    === prop_apila_no_es_vacia from PilaPropiedades.hs:77 ===
--    +++ OK, passed 100 tests.
--
--    True
