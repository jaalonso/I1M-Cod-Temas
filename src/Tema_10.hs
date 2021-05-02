-- Tema_10.hs
-- Tema 10: Evaluación perezosa.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_10 where

-- inf representa el infinito.
inf :: Int
inf = 1 + inf

-- Evaluaciones con infinito:
--    fst (0,inf)  =>  0

-- (cuadrado n) es el cuadrado de n.
cuadrado :: Int -> Int
cuadrado n = n * n

-- unos es la lista con infinitos unos,
--    unos         ==  [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, ...
--    head unos    ==  1
--    take 3 unos  ==  [1,1,1]
unos :: [Int]
unos = 1 : unos

-- ---------------------------------------------------------------------
-- § Primos mediante la criba de Erastótenes                          --
-- ---------------------------------------------------------------------

-- primos es la lista de los números primos, calculados mediante la
-- criba de Erastótenes. Por ejemplo,
--    take 15 primos  ==  [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47]
primos :: [Int ]
primos = criba [2..]

criba :: [Int] -> [Int]
criba (p:xs) = p : criba [x | x <- xs, x `mod` p /= 0]

--   primos
-- = criba [2..]
-- = criba (2 : [3..])
-- = 2 : (criba [x | x <- [3..],
--                   x `mod` 2 /= 0])
-- = 2 : (criba (3 : [x | x <- [4..],
--                        x `mod` 2 /= 0]))
-- = 2 : 3 : (criba [x | x <- [4..],
--                       x `mod` 2 /= 0,
--                       x `mod` 3 /= 0])
-- = 2 : 3 : (criba (5 : [x | x <- [6..],
--                            x `mod` 2 /= 0,
--                            x `mod` 3 /= 0])
-- = 2 : 3 : 5 : (criba ([x | x <- [6..],
--                            x `mod` 2 /= 0,
--                            x `mod` 3 /= 0,
--                            x `mod` 5 /= 0])
-- = ...

-- (sumaAcu v xs) es la suma de v y los elementos de xs, usando
-- acumulador. Por ejemplo,
--    ghci> sumaAcu 3 [4,7,2]
--    16
--    ghci> sumaAcu 0 [1..1000000]
--    *** Exception: stack overflow
sumaAcu :: Integer -> [Integer] -> Integer
sumaAcu v []     = v
sumaAcu v (x:xs) = sumaAcu (v+x) xs

-- (sumaAcu' v xs) es la suma de v y los elementos de xs, usando
-- acumulador y evaluación impaciente. Por ejemplo,
--    ghci> sumaAcu' 3 [4,7,2]
--    16
--    ghci> sumaAcu' 0 [1..1000000]
--    1784293664
sumaAcu' :: Integer -> [Integer] -> Integer
sumaAcu' v []     = v
sumaAcu' v (x:xs) = (sumaAcu' $! (v+x)) xs

-- Versión estricta de foldl en el Preludio:
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f a []     = a
foldl' f a (x:xs) = (foldl' f $! f a x) xs

-- (sumaAcu' v xs) es la suma de v y los elementos de xs, usando
-- foldl'. Por ejemplo,
--    ghci> sumaAcu' 3 [4,7,2]
--    16
--    ghci> sumaAcu'' 0 [1..10000]
--    50005000
sumaAcu'' = foldl' (+)
