-- ArbolBinPropiedades.hs
-- Generador de árboles binarios de búsqueda y propiedades del TAD.
-- Tablas mediante matrices.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tema_19.ArbolBinPropiedades where

import Tema_19.ArbolBin

import Data.List (nub, sort)
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Generador de ABB                                                   --
-- ---------------------------------------------------------------------

-- genABB es un generador de árboles binarios de búsqueda. Por ejemplo,
--    λ> sample genABB
--     -
--     (1 (-1 - -) -)
--     (1 - -)
--     (-1 (-3 - -) (1 - (4 - -)))
--     -
--     (10 (-7 - -) -)
--     (1 (-9 - -) (7 (5 - -) (10 - -)))
--     ...
genABB :: Gen (ABB Int)
genABB = do
  xs <- listOf arbitrary
  return (foldr inserta vacio xs)

-- Los árboles binarios de búsqueda son instancias de la clase
-- arbitraria.
instance Arbitrary (ABB Int) where
  arbitrary = genABB

-- Propiedad. Todo los elementos generados por genABB son árboles binarios
-- de búsqueda.
prop_genABB_correcto :: ABB Int -> Bool
prop_genABB_correcto = valido

-- Comprobación.
--    λ> quickCheck prop_genABB_correcto
--    +++ OK, passed 100 tests.

-- listaOrdenada es un generador de listas ordenadas de números
-- enteros. Por ejemplo,
--    λ> sample listaOrdenada
--    [1]
--    [1]
--    [-2,-1,0]
--    [-1,0,1]
--    [-8,-5,-4,-3,3,4,8]
--    [-6,-3,8]
--    [-14,-13]
--    [-31,-23,-16,-13,-11,-5,1,4,11,14,15,21,26,29]
--    []
--    []
--    []
listaOrdenada :: Gen [Int]
listaOrdenada =
  frequency [(1,return []),
             (4,do xs <- orderedList
                   n <- arbitrary
                   return (nub ((case xs of
                                   []  -> n
                                   x:_ -> n `min` x)
                                :xs)))]

-- (ordenada xs) se verifica si xs es una lista ordenada creciente. Por
-- ejemplo,
--    ordenada [3,5,9]  ==  True
--    ordenada [3,9,5]  ==  False
ordenada :: [Int] -> Bool
ordenada xs = and [x < y | (x,y) <- zip xs (tail xs)]

-- Propiedad. El generador listaOrdenada produce listas ordenadas.
prop_listaOrdenada_correcta :: [Int] -> Property
prop_listaOrdenada_correcta _ =
  forAll listaOrdenada ordenada

-- Comprobación:
--    λ> quickCheck prop_listaOrdenada_correcta
--    +++ OK, passed 100 tests.

-- Propiedad. Al eliminar las repeticiones en las listas producidas por el
-- generador orderedList se obtienen listas ordenadas.
prop_orderedList_correcta :: [Int] -> Property
prop_orderedList_correcta _ =
  forAll orderedList (ordenada . nub)

-- Comprobación:
--    λ> quickCheck prop_orderedList_correcta
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Propiedades                                                        --
-- ---------------------------------------------------------------------

-- Propiedades de vacio
-- --------------------

-- Prop. vacio es un ABB.
prop_vacio_es_ABB :: Bool
prop_vacio_es_ABB =
  valido (vacio :: ABB Int)

-- Comprobación:
--    λ> quickCheck prop_vacio_es_ABB
--    +++ OK, passed 100 tests.

-- Propiedades de inserta
-- ----------------------

-- Propiedad. Si a es un ABB, entonces (inserta v a) también lo es.
prop_inserta_es_valida :: Int -> ABB Int -> Bool
prop_inserta_es_valida v a =
  valido (inserta v a)

-- Comprobación:
--    λ> quickCheck prop_inserta_es_valida
--    +++ OK, passed 100 tests.

-- Propiedad. El árbol que resulta de añadir un elemento a un ABB es no
-- vacío.
prop_inserta_es_no_vacio :: Int -> ABB Int -> Bool
prop_inserta_es_no_vacio x a =
  inserta x a /= vacio

-- Comprobación.
--    λ> quickCheck prop_inserta_es_no_vacio
--    +++ OK, passed 100 tests.

-- Propiedad. Para todo x y a, x es un elemento de (inserta x a).
prop_elemento_de_inserta :: Int -> ABB Int -> Bool
prop_elemento_de_inserta x a =
  pertenece x (inserta x a)

-- Comprobación:
--    λ> quickCheck prop_elemento_de_inserta
--    +++ OK, passed 100 tests.

-- Propiedades de pertenece
-- ------------------------

-- Propiedad. En en árbol vacio no hay ningún elemento.
prop_vacio_sin_elementos :: Int -> Bool
prop_vacio_sin_elementos x =
  not (pertenece x vacio)

-- Comprobación:
--    λ> quickCheck prop_vacio_sin_elementos
--    +++ OK, passed 100 tests.

-- Propiedad. Los elementos de (inserta x a) son x y los elementos de
-- a.
prop_elementos_de_inserta :: Int -> Int -> ABB Int -> Bool
prop_elementos_de_inserta x y a =
  pertenece y (inserta x a) == (x == y) || pertenece y a

-- Comprobación.
--    λ> quickCheck prop_elementos_de_inserta
--    +++ OK, passed 100 tests.

-- Propiedades de elimina
-- ----------------------

-- Propiedad. Si a es un ABB, entonces (elimina v a) también lo es.
prop_elimina_es_valida :: Int -> ABB Int -> Bool
prop_elimina_es_valida v a =
  valido (elimina v a)

-- Comprobación:
--    λ> quickCheck prop_elimina_es_valida
--    +++ OK, passed 100 tests.

-- Prop. El resultado de eliminar el elemento x en (inserta x a) es
-- (elimina x a).
prop_elimina_agrega :: Int -> ABB Int -> Bool
prop_elimina_agrega x a =
  elimina x (inserta x a) == elimina x a

-- Comprobación
--    λ> quickCheck prop_elimina_agrega
--    +++ OK, passed 100 tests.

-- Propiedades de crea
-- -------------------

-- Propiedad. (crea xs) es un ABB.
prop_crea_es_valida :: [Int] -> Bool
prop_crea_es_valida xs =
  valido (crea xs)

-- Comprobación:
--    λ> quickCheck prop_crea_es_valida
--    +++ OK, passed 100 tests.

-- Propiedades de crea'
-- --------------------

-- Propiedad. Para todas las listas ordenadas xs, se tiene que (crea' xs)
-- es un ABB.
prop_crea'_es_valida :: [Int] -> Property
prop_crea'_es_valida _ =
  forAll listaOrdenada (valido . crea')

-- Comprobación:
--    λ> quickCheck prop_crea'_es_valida
--    +++ OK, passed 100 tests.

-- Propiedades de elementos
-- ------------------------

-- Propiedad. (elementos (crea xs)) es igual a la lista xs ordenada y
-- sin repeticiones.
prop_elementos_crea :: [Int] -> Bool
prop_elementos_crea xs =
  elementos (crea xs) == sort (nub xs)

-- Comprobación
--    λ> quickCheck prop_elementos_crea
--    +++ OK, passed 100 tests.

-- Propiedad. Si ys es una lista ordenada sin repeticiones, entonces
-- (elementos (crea' ys)) es igual ys.
prop_elementos_crea' :: [Int] -> Bool
prop_elementos_crea' xs =
  elementos (crea' ys) == ys
  where ys = sort (nub xs)

-- Comprobación
--    λ> quickCheck prop_elementos_crea'
--    +++ OK, passed 100 tests.

-- Propiedad. Un elemento pertenece a (elementos a) syss es un valor de a.
prop_en_elementos :: Int -> ABB Int -> Bool
prop_en_elementos v a =
  pertenece v a == elem v (elementos a)

-- Comprobación:
--    λ> quickCheck prop_en_elementos
--    +++ OK, passed 100 tests.

-- Propiedades de menor
-- --------------------

-- Propiedad. (menor a) es menor o igual que todos los elementos de ABB
-- a.
prop_menoresMinimo ::Int -> ABB Int -> Bool
prop_menoresMinimo _ a =
  and [menor a <= v | v <- elementos a]

-- Comprobación.
--    λ> quickCheck prop_menoresMinimo
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

return []

verificaABB :: IO Bool
verificaABB = $quickCheckAll

-- La verificación es
--    λ> verificaABB
--    === prop_genABB_correcto from ArbolBinPropiedades.hs:44 ===
--    +++ OK, passed 100 tests.
--
--    === prop_listaOrdenada_correcta from ArbolBinPropiedades.hs:83 ===
--    +++ OK, passed 100 tests.
--
--    === prop_orderedList_correcta from ArbolBinPropiedades.hs:93 ===
--    +++ OK, passed 100 tests.
--
--    === prop_vacio_es_ABB from ArbolBinPropiedades.hs:109 ===
--    +++ OK, passed 1 test.
--
--    === prop_inserta_es_valida from ArbolBinPropiedades.hs:121 ===
--    +++ OK, passed 100 tests.
--
--    === prop_inserta_es_no_vacio from ArbolBinPropiedades.hs:131 ===
--    +++ OK, passed 100 tests.
--
--    === prop_elemento_de_inserta from ArbolBinPropiedades.hs:140 ===
--    +++ OK, passed 100 tests.
--
--    === prop_vacio_sin_elementos from ArbolBinPropiedades.hs:152 ===
--    +++ OK, passed 100 tests.
--
--    === prop_elementos_de_inserta from ArbolBinPropiedades.hs:162 ===
--    +++ OK, passed 100 tests.
--
--    === prop_elimina_es_valida from ArbolBinPropiedades.hs:174 ===
--    +++ OK, passed 100 tests.
--
--    === prop_elimina_agrega from ArbolBinPropiedades.hs:184 ===
--    +++ OK, passed 100 tests.
--
--    === prop_crea_es_valida from ArbolBinPropiedades.hs:196 ===
--    +++ OK, passed 100 tests.
--
--    === prop_crea'_es_valida from ArbolBinPropiedades.hs:209 ===
--    +++ OK, passed 100 tests.
--
--    === prop_elementos_crea from ArbolBinPropiedades.hs:222 ===
--    +++ OK, passed 100 tests.
--
--    === prop_elementos_crea' from ArbolBinPropiedades.hs:232 ===
--    +++ OK, passed 100 tests.
--
--    === prop_en_elementos from ArbolBinPropiedades.hs:242 ===
--    +++ OK, passed 100 tests.
--
--    === prop_menoresMinimo from ArbolBinPropiedades.hs:255 ===
--    +++ OK, passed 100 tests.
--
--    True
