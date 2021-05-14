-- MonticuloPropiedades.hs
-- Propiedades del TAD montículos.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tema_20.MonticuloPropiedades where

-- Importación de la implementación de los montículos que se desea
-- verificar.
import Tema_20.Monticulo
-- import I1M.Monticulo

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Generador de montículos                                            --
-- ---------------------------------------------------------------------

-- (creaMonticulo xs) es el montículo correspondiente a la lista xs. Por
-- ejemplo,
--    λ> creaMonticulo [6,1,4,8]
--    M 1 2 (M 4 1 (M 8 1 Vacio Vacio) Vacio) (M 6 1 Vacio Vacio)
creaMonticulo :: [Int] -> Monticulo Int
creaMonticulo = foldr inserta vacio

-- genMonticulo es un generador de montículos. Por ejemplo,
--    λ> sample genMonticulo
--    VacioM
--    M (-1) 1 (M 1 1 VacioM VacioM) VacioM
--    ...
genMonticulo :: Gen (Monticulo Int)
genMonticulo = do
  xs <- listOf arbitrary
  return (creaMonticulo xs)

-- Montículo es una instancia de la clase arbitraria.
instance Arbitrary (Monticulo Int) where
  arbitrary = genMonticulo

-- genMonticulo genera montículos válidos.
prop_genMonticulo :: Monticulo Int -> Bool
prop_genMonticulo = valido

-- Comprobación.
--    λ> quickCheck prop_genMonticulo
--    +++ OK, passed 100 tests.

-- monticuloNV es un generador de montículos no vacío. Por ejemplo,
--    λ> sample monticuloNV
--    M 0 1 VacioM VacioM
--    M 1 1 (M 1 1 (M 1 1 VacioM VacioM) VacioM) VacioM
--    M 0 2 (M 1 1 VacioM VacioM) (M 2 1 VacioM VacioM)
--    M (-4) 2 (M (-3) 1 VacioM VacioM) (M 1 1 VacioM VacioM)
--    M 3 1 VacioM VacioM
--    M (-8) 1 (M (-5) 1 VacioM VacioM) VacioM
monticuloNV :: Gen (Monticulo Int)
monticuloNV = do
  xs <- listOf arbitrary
  x <- arbitrary
  return (creaMonticulo (x:xs))

-- Prop. monticuloNV genera montículos no vacío.
prop_monticuloNV :: Monticulo Int -> Property
prop_monticuloNV _ =
  forAll monticuloNV (\m -> valido m && not (esVacio m))

-- Comprobación.
--    λ> quickCheck prop_monticuloNV
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Propiedades
-- ---------------------------------------------------------------------

-- Propiedades de vacio
-- --------------------

-- Propiedad. vacio es un montículo.
prop_vacio_es_monticulo :: Bool
prop_vacio_es_monticulo =
  esVacio (vacio :: Monticulo Int)

-- Comprobación.
--    λ> prop_vacio_es_monticulo
--    True

-- Propiedades de inserta
-- ----------------------

-- Propiedad. inserta produce montículos válidos.
prop_inserta_es_valida :: Int -> Monticulo Int -> Bool
prop_inserta_es_valida x m =
  valido (inserta x m)

-- Comprobación.
--    λ> quickCheck prop_inserta_es_valida
--    +++ OK, passed 100 tests.

-- Propiedad. Los montículos creados con inserta son no vacío.
prop_inserta_no_vacio :: Int -> Monticulo Int -> Bool
prop_inserta_no_vacio x m =
  not (esVacio (inserta x m))

-- Comprobación.
--    λ> quickCheck prop_inserta_no_vacio
--    +++ OK, passed 100 tests.

-- Propiedades de resto
-- --------------------

-- Propiedad. Al borrar el menor elemento de un montículo no vacío se
-- obtiene un montículo válido.
prop_resto_es_valida :: Monticulo Int -> Property
prop_resto_es_valida _ =
  forAll monticuloNV (valido . resto)

-- Comprobación.
--    λ> quickCheck prop_resto_es_valida
--    +++ OK, passed 100 tests.

-- Propiedad. El resto de (inserta x m) es m si m es el montículo vacío
-- o x es menor o igual que el menor elemento de m o (inserta x (resto m)),
-- en caso contrario.
prop_resto_inserta :: Int -> Monticulo Int -> Bool
prop_resto_inserta x m =
  resto (inserta x m)
  == if esVacio m || x <= menor m
     then m
     else inserta x (resto m)

-- Comprobación.
--    λ> quickCheck prop_resto_inserta
--    +++ OK, passed 100 tests.

-- Propiedades de menor
-- --------------------

-- Propiedad. (menor m) es el menor elemento del montículo m.
prop_menor_es_minimo :: Monticulo Int -> Bool
prop_menor_es_minimo m =
  esVacio m ||
  esVacio (resto m) ||
  menor m <= menor (resto m)

-- Comprobación.
--    λ> quickCheck prop_menor_es_minimo
--    +++ OK, passed 100 tests.


-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

return []

verificaMonticulo :: IO Bool
verificaMonticulo = $quickCheckAll

-- La verificación es
--    λ> verificaMonticulo
--    === prop_genMonticulo from MonticuloPropiedades.hs:46 ===
--    +++ OK, passed 100 tests.
--
--    === prop_monticuloNV from MonticuloPropiedades.hs:68 ===
--    +++ OK, passed 100 tests.
--
--    === prop_vacio_es_monticulo from MonticuloPropiedades.hs:84 ===
--    +++ OK, passed 1 test.
--
--    === prop_inserta_es_valida from MonticuloPropiedades.hs:96 ===
--    +++ OK, passed 100 tests.
--
--    === prop_inserta_no_vacio from MonticuloPropiedades.hs:105 ===
--    +++ OK, passed 100 tests.
--
--    === prop_resto_es_valida from MonticuloPropiedades.hs:118 ===
--    +++ OK, passed 100 tests.
--
--    === prop_resto_inserta from MonticuloPropiedades.hs:129 ===
--    +++ OK, passed 100 tests.
--
--    === prop_menor_es_minimo from MonticuloPropiedades.hs:144 ===
--    +++ OK, passed 100 tests.
--
--    True
