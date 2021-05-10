-- El_tipo_array.hs
-- El tipo array.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_18.El_tipo_array where

import Data.Array

-- (cuadrados n) es un vector de n+1 elementos tal que su elemento
-- i-ésimo es i². Por ejemplo,
--    λ> cuadrados 5
--    array (0,5) [(0,0),(1,1),(2,4),(3,9),(4,16),(5,25)]
cuadrados :: Int -> Array Int Int
cuadrados n = array (0,n) [(i,i^2) | i <- [0..n]]

-- (fibs n) es el vector formado por los n primeros términos de la
-- sucesión de Fibonacci. Por ejemplo,
--    λ> fibs 7
--    array (0,7) [(0,1),(1,1),(2,2),(3,3),
--                 (4,5),(5,8),(6,13),(7,21)]
fibs :: Int -> Array Int Int
fibs n = a where
  a = array (0,n)
            ([(0,1),(1,1)] ++
             [(i,a!(i-1)+a!(i-2)) | i <- [2..n]])

-- (histograma r is) es el vector formado contando cuantas veces
-- aparecen los elementos del rango r en la lista de índices is. Por
-- ejemplo,
--    λ> histograma (0,5) [3,1,4,1,5,4,2,7]
--    array (0,5) [(0,0),(1,2),(2,1),(3,1),(4,2),(5,1)]
histograma :: (Ix a, Num b) => (a,a) -> [a] -> Array a b
histograma r is =
   accumArray (+) 0 r [(i,1) | i <- is, inRange r i]
