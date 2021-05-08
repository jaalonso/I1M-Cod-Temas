-- ConjuntoPropiedades.hs
-- Propiedades del TAD conjuntos.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tema_17.ConjuntoPropiedades where

-- Nota: Hay que elegir una implementación del TAD de conjuntos
import Tema_17.ConjuntoConListasNoOrdenadasConDuplicados
-- import Tema_17.ConjuntoConListasNoOrdenadasSinDuplicados
-- import Tema_17.ConjuntoConListasOrdenadasSinDuplicados
-- import I1M.Conjunto

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Generador de conjuntos                                          --
-- ---------------------------------------------------------------------

-- genConjunto es un generador de conjuntos. Por ejemplo,
--    ghci> sample genConjunto
--    {}
--    {}
--    {}
--    {3,-2,-2,-3,-2,4}
--    {-8,0,4,6,-5,-2}
--    {12,-2,-1,-10,-2,2,15,15}
--    {2}
--    {}
--    {-42,55,55,-11,23,23,-11,27,-17,-48,16,-15,-7,5,41,43}
--    {-124,-66,-5,-47,58,-88,-32,-125}
--    {49,-38,-231,-117,-32,-3,45,227,-41,54,169,-160,19}
genConjunto :: Gen (Conj Int)
genConjunto = do
  xs <- listOf arbitrary
  return (foldr inserta vacio xs)

-- Los conjuntos son concreciones de los arbitrarios.
instance Arbitrary (Conj Int) where
  arbitrary = genConjunto

-- ---------------------------------------------------------------------
-- Propiedades                                          --
-- ---------------------------------------------------------------------

-- Propiedades de inserta
-- ----------------------

-- Propiedad. El número de veces que se añada un elemento a un conjunto
-- no importa.
prop_independencia_repeticiones :: Int -> Conj Int -> Bool
prop_independencia_repeticiones x c =
  inserta x (inserta x c) == inserta x c

-- Comprobación.
--    λ> quickCheck prop_independencia_repeticiones
--    +++ OK, passed 100 tests.

-- Propiedad. El orden en que se añadan los elementos a un conjunto no
-- importa.
prop_independencia_del_orden :: Int -> Int -> Conj Int -> Bool
prop_independencia_del_orden x y c =
  inserta x (inserta y c) == inserta y (inserta x c)

-- Comprobación.
--    λ> quickCheck prop_independencia_del_orden
--    +++ OK, passed 100 tests.

-- Propiedades de pertenece
-- ------------------------

-- Propiedad. El conjunto vacío no tiene elementos.
prop_vacio_no_elementos :: Int -> Bool
prop_vacio_no_elementos x =
  not (pertenece x vacio)

-- Comprobación.
--    λ> quickCheck prop_vacio_no_elementos
--    +++ OK, passed 100 tests.

-- Propiedad. Un elemento pertenece al conjunto obtenido añadiendo x al
-- conjunto c syss es igual a x o pertenece a c.
prop_pertenece_inserta :: Int -> Int -> Conj Int -> Bool
prop_pertenece_inserta x y c =
  pertenece y (inserta x c) == (x==y) || pertenece y c

-- Comprobación.
--    λ> quickCheck prop_pertenece_inserta
--    +++ OK, passed 100 tests.

-- Propiedades de elimina
-- ----------------------

-- Propiedad. Al eliminar cualquier elemento del conjunto vacío se
-- obtiene el conjunto vacío.
prop_elimina_vacio :: Int -> Bool
prop_elimina_vacio x =
  elimina x vacio == vacio

-- Comprobación.
--    λ> quickCheck prop_elimina_vacio
--    +++ OK, passed 100 tests.

-- Propiedad. El resultado de eliminar x en el conjunto obtenido
-- añadiéndole x al conjunto c es c menos x, si x e y son iguales y es el
-- conjunto obtenido añadiéndole y a c menos x, en caso contrario.
prop_elimina_inserta :: Int -> Int -> Conj Int -> Bool
prop_elimina_inserta x y c =
  elimina x (inserta y c)
  == if x == y
     then elimina x c
     else inserta y (elimina x c)

-- Comprobación
--    λ> quickCheck prop_elimina_inserta
--    +++ OK, passed 100 tests.

-- Propiedades de esVacio
-- ----------------------

-- Propiedad. vacio es vacío.
prop_vacio_es_vacio :: Bool
prop_vacio_es_vacio =
  esVacio (vacio :: Conj Int)

-- Comprobación.
--    λ> quickCheck prop_vacio_es_vacio
--    +++ OK, passed 100 tests.

-- Propiedad. Los conjuntos construidos con inserta no son vacío.
prop_inserta_es_no_vacio :: Int -> Conj Int -> Bool
prop_inserta_es_no_vacio x c =
  not (esVacio (inserta x c))

-- Comprobación
--    λ> quickCheck prop_inserta_es_no_vacio
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

return []

verificaConjunto :: IO Bool
verificaConjunto = $quickCheckAll

-- La verificación es
--    λ> verificaConjunto
--    === prop_independencia_repeticiones from ConjuntoPropiedades.hs:55 ===
--    +++ OK, passed 100 tests.
--
--    === prop_independencia_del_orden from ConjuntoPropiedades.hs:65 ===
--    +++ OK, passed 100 tests.
--
--    === prop_vacio_no_elementos from ConjuntoPropiedades.hs:77 ===
--    +++ OK, passed 100 tests.
--
--    === prop_pertenece_inserta from ConjuntoPropiedades.hs:87 ===
--    +++ OK, passed 100 tests.
--
--    === prop_elimina_vacio from ConjuntoPropiedades.hs:100 ===
--    +++ OK, passed 100 tests.
--
--    === prop_elimina_inserta from ConjuntoPropiedades.hs:111 ===
--    +++ OK, passed 100 tests.
--
--    === prop_vacio_es_vacio from ConjuntoPropiedades.hs:126 ===
--    +++ OK, passed 1 test.
--
--    === prop_inserta_es_no_vacio from ConjuntoPropiedades.hs:135 ===
--    +++ OK, passed 100 tests.
--
--    True
