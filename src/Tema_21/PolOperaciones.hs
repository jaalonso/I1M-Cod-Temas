-- PolOperaciones.hs
-- Operaciones con polinomios.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Tema_21.PolOperaciones (module Pol,
                               module Tema_21.PolOperaciones) where

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

-- Nota: Hay que elegir una implementación del TAD de los polinomios.
import Tema_21.PolRepTDA as Pol
-- import Tema_21.PolRepDensa as Pol
-- import Tema_21.PolRepDispersa as Pol
-- import I1M.Pol as Pol

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejemplos                                                           --
-- ---------------------------------------------------------------------

-- Ejemplos de polinomios:
ejPol1, ejPol2, ejPol3, ejTerm :: Polinomio Int
ejPol1 = consPol 4 3 (consPol 2 (-5) (consPol 0 3 polCero))
ejPol2 = consPol 5 1 (consPol 2 5 (consPol 1 4 polCero))
ejPol3 = consPol 4 6 (consPol 1 2 polCero)
ejTerm = consPol 1 4 polCero

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
genPol :: (Arbitrary a, Num a, Eq a) => Int -> Gen (Polinomio a)
genPol 0 = return polCero
genPol _ = do
  n <- choose (0,10)
  b <- arbitrary
  p <- genPol (div n 2)
  return (consPol n b p)

instance (Arbitrary a, Num a, Eq a) => Arbitrary (Polinomio a) where
  arbitrary = sized genPol

-- ---------------------------------------------------------------------
-- Funciones sobre términos                                           --
-- ---------------------------------------------------------------------

-- (creaTermino n a) es el término a*x^n. Por ejemplo,
--    creaTermino 2 5  ==  5*x^2
creaTermino :: (Num t, Eq t) => Int -> t -> Polinomio t
creaTermino n a = consPol n a polCero

-- (termLider p) es el término líder del polinomio p. Por ejemplo,
--    ejPol2            ==  x^5 + 5*x^2 + 4*x
--    termLider ejPol2  ==  x^5
termLider :: (Num t, Eq t) => Polinomio t -> Polinomio t
termLider p = creaTermino (grado p) (coefLider p)

-- ---------------------------------------------------------------------
-- Suma de polinomios                                                 --
-- ---------------------------------------------------------------------

-- (sumaPol p q) es la suma de los polinomios p y q. Por ejemplo,
--    ejPol1                 ==  3*x^4 + -5*x^2 + 3
--    ejPol2                 ==  x^5 + 5*x^2 + 4*x
--    sumaPol ejPol1 ejPol2  ==  x^5 + 3*x^4 + 4*x + 3
sumaPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
sumaPol p q
  | esPolCero p = q
  | esPolCero q = p
  | n1 > n2      = consPol n1 a1 (sumaPol r1 q)
  | n1 < n2      = consPol n2 a2 (sumaPol p r2)
  | otherwise    = consPol n1 (a1+a2) (sumaPol r1 r2)
  where n1 = grado p
        a1 = coefLider p
        r1 = restoPol p
        n2 = grado q
        a2 = coefLider q
        r2 = restoPol q

-- Propiedad. El polinomio cero es el elemento neutro de la suma.
prop_neutroSumaPol :: Polinomio Int -> Bool
prop_neutroSumaPol p =
  sumaPol polCero p == p

-- Comprobación con QuickCheck.
--    λ> quickCheck prop_neutroSumaPol
--    OK, passed 100 tests.

-- Propiedad. La suma es conmutativa.
prop_conmutativaSuma :: Polinomio Int -> Polinomio Int -> Bool
prop_conmutativaSuma p q =
  sumaPol p q == sumaPol q p

-- Comprobación:
--    λ> quickCheck prop_conmutativaSuma
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Producto de polinomios                                             --
-- ---------------------------------------------------------------------

-- (multPorTerm t p) es el producto del término t por el polinomio
-- p. Por ejemplo,
--    ejTerm                     ==  4*x
--    ejPol2                     ==  x^5 + 5*x^2 + 4*x
--    multPorTerm ejTerm ejPol2  ==  4*x^6 + 20*x^3 + 16*x^2
multPorTerm :: (Num t, Eq t) => Polinomio t -> Polinomio t -> Polinomio t
multPorTerm term pol
  | esPolCero pol = polCero
  | otherwise     = consPol (n+m) (a*b) (multPorTerm term r)
  where n = grado term
        a = coefLider term
        m = grado pol
        b = coefLider pol
        r = restoPol pol

-- (multPol p q) es el producto de los polinomios p y q. Por
-- ejemplo,
--    λ> ejPol1
--    3*x^4 + -5*x^2 + 3
--    λ> ejPol2
--    x^5 + 5*x^2 + 4*x
--    λ> multPol ejPol1 ejPol2
--    3*x^9 + -5*x^7 + 15*x^6 + 15*x^5 + -25*x^4 + -20*x^3 + 15*x^2 + 12*x
multPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
multPol p q
  | esPolCero p = polCero
  | otherwise    = sumaPol (multPorTerm (termLider p) q)
                           (multPol (restoPol p) q)

-- Propiedad. El producto de polinomios es conmutativo.
prop_conmutativaProducto :: Polinomio Int -> Polinomio Int -> Bool
prop_conmutativaProducto p q =
  multPol p q == multPol q p

-- La comprobación es
--    λ> quickCheck prop_conmutativaProducto
--    OK, passed 100 tests.

-- El producto es distributivo respecto de la suma.
prop_distributivaProductoSuma :: Polinomio Int -> Polinomio Int
                                 -> Polinomio Int -> Bool
prop_distributivaProductoSuma p q r =
  multPol p (sumaPol q r) == sumaPol (multPol p q) (multPol p r)

-- Comprobación:
--    λ> quickCheck prop_distributivaProductoSuma
--    OK, passed 100 tests.

-- polUnidad es el polinomio unidad. Por ejemplo,
--    λ> polUnidad
--    1
polUnidad :: (Num t, Eq t) => Polinomio t
polUnidad = consPol 0 1 polCero

-- Propiedad. El polinomio unidad es el elemento neutro del producto.
prop_polUnidad :: Polinomio Int -> Bool
prop_polUnidad p =
  multPol p polUnidad == p

-- Comprobación:
--    λ> quickCheck prop_polUnidad
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Valor de un polinomio en un punto                                  --
-- ---------------------------------------------------------------------

-- (valor p c) es el valor del polinomio p al sustituir su variable por
-- c. Por ejemplo,
--    ejPol1             ==  3*x^4 + -5*x^2 + 3
--    valor ejPol1 0     ==  3
--    valor ejPol1 1     ==  1
--    valor ejPol1 (-2)  ==  31
valor:: (Num a, Eq a) => Polinomio a -> a -> a
valor p c
  | esPolCero p = 0
  | otherwise   =  b*c^n + valor r c
  where n = grado p
        b = coefLider p
        r = restoPol p

-- ---------------------------------------------------------------------
-- Verificación de raices de polinomios                               --
-- ---------------------------------------------------------------------

-- (esRaiz c p) se verifica si c es una raiz del polinomio p. por
-- ejemplo,
--    ejPol3           ==  6*x^4 + 2*x
--    esRaiz 1 ejPol3  ==  False
--    esRaiz 0 ejPol3  ==  True
esRaiz :: (Num a, Eq a) => a -> Polinomio a -> Bool
esRaiz c p = valor p c == 0

-- ---------------------------------------------------------------------
-- Derivación de polinomios                                           --
-- ---------------------------------------------------------------------

-- (derivada p) es la derivada del polinomio p. Por ejemplo,
--    ejPol2           ==  x^5 + 5*x^2 + 4*x
--    derivada ejPol2  ==  5*x^4 + 10*x + 4
derivada :: (Eq a, Num a) => Polinomio a -> Polinomio a
derivada p
  | n == 0     = polCero
  | otherwise  = consPol (n-1) (b * fromIntegral n) (derivada r)
  where n = grado p
        b = coefLider p
        r = restoPol p

-- Propiedad. La derivada de la suma es la suma de las derivadas.
prop_derivada :: Polinomio Int -> Polinomio Int -> Bool
prop_derivada p q =
  derivada (sumaPol p q) == sumaPol (derivada p) (derivada q)

-- Comprobación
--    λ> quickCheck prop_derivada
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Resta de polinomios                                                --
-- ---------------------------------------------------------------------

-- (resta p q) es la el polinomio obtenido restándole a p el q. Por
-- ejemplo,
--    ejPol1                  ==  3*x^4 + -5*x^2 + 3
--    ejPol2                  ==  x^5 + 5*x^2 + 4*x
--    restaPol ejPol1 ejPol2  ==  -1*x^5 + 3*x^4 + -10*x^2 + -4*x + 3
restaPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
restaPol p q  =
  sumaPol p (multPorTerm (creaTermino 0 (-1)) q)

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

return []

verificaPolOperaciones :: IO Bool
verificaPolOperaciones = $quickCheckAll

-- La verificación es
--    λ> verificaPolOperaciones
--    === prop_neutroSumaPol from PolOperaciones.hs:103 ===
--    +++ OK, passed 100 tests.
--
--    === prop_conmutativaSuma from PolOperaciones.hs:112 ===
--    +++ OK, passed 100 tests.
--
--    === prop_conmutativaProducto from PolOperaciones.hs:154 ===
--    +++ OK, passed 100 tests.
--
--    === prop_distributivaProductoSuma from PolOperaciones.hs:163 ===
--    +++ OK, passed 100 tests.
--
--    === prop_polUnidad from PolOperaciones.hs:179 ===
--    +++ OK, passed 100 tests.
--
--    === prop_derivada from PolOperaciones.hs:233 ===
--    +++ OK, passed 100 tests.
--
--    True
