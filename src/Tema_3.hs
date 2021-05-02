-- Tema_3.hs
-- Tema 3: Tipos y clases.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_3 where

-- (suma (x,y)) es la suma de x e y. Por ejemplo,
--    suma (2,3) == 5
suma :: (Int,Int) -> Int
suma (x,y) = x+y

-- (deCeroA n) es la lista de los números desde 0 hasta n. Por
-- ejemplo,
--    deCeroA 5 == [0,1,2,3,4,5]
deCeroA :: Int -> [Int]
deCeroA n = [0..n]

-- (suma' x y) es la suma de x e y. Por ejemplo,
--    suma' 2 3   == 5
--    (suma' 2) 3 == 5
suma' :: Int -> (Int -> Int)
suma' x y = x+y

-- (mult x y z) es el producto de x, y y z. Por ejemplo,
--    mult 2 5 7     == 70
--    (mult 2) 5 7   == 70
--    ((mult 2) 5) 7 == 70
mult :: Int -> (Int -> (Int -> Int))
mult x y z = x*y*z

-- (suc n) es el siguiente de n. Por ejemplo,
--    suc 5 == 6
suc :: Int -> Int
suc = suma' 1
