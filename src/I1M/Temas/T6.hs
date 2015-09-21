-- I1M 2010-11: tema-6.hs
-- Tema 6: Funciones recursivas.
-- José A. Alonso Jiménez <jalonso@us,es>
-- Sevilla, 7 de Noviembre de 2010
-- =====================================================================

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------

import Prelude hiding (product, reverse, length, (++), zip, drop, init)
import Data.Char  

-- ---------------------------------------------------------------------
-- Recursión numérica                                                 --
-- ---------------------------------------------------------------------

-- (factorial n) es el factorial de n. Por ejemplo,
--    factorial 3  ==  6
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

-- (m `por` n) es el producto de m por n. Por ejemplo,
--    3 `por` 2  ==  6
por :: Int -> Int -> Int
m `por` 0       = 0
m `por` n = m + (m `por` (n-1))

-- ---------------------------------------------------------------------
-- Recusión sobre lista                                               --
-- ---------------------------------------------------------------------

-- (product xs) es el producto de los números de xs. Por ejemplo,
--    product [7,5,2] == 70
product :: Num a => [a] -> a
product []     = 1
product (n:ns) = n * product ns

-- (length xs) es el número de elementos de xs. Por ejemplo,
--    length [2,4,5] == 3
length :: [a] -> Int
length []     = 0
length (_:xs) = 1 + length xs

-- (reverse xs) es la inversa de xs. Por ejemplo,
--    reverse [2,5,3]  ==  [3,5,2]  
reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x]

-- (xs ++ ys) es la concatenación de xs e ys. Por ejemplo,
--    [2,5] ++ [3,5,6]  ==  [2,5,3,5,6]
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)

-- (inserta e xs) inserta el elemento e en la lista xs delante del
-- primer elemento de xs mayor o igual que e. Por ejemplo,
--    inserta 5 [2,4,7,3,6,8,10] == [2,4,5,7,3,6,8,10]  
inserta :: Ord a => a -> [a] -> [a]
inserta e []                  = [e]
inserta e (x:xs) | e <= x     = e : (x:xs) 
                 | otherwise  = x : inserta e xs    

-- (ordena_por_insercion xs) es la lista xs ordenada mediante inserción,
-- Por ejemplo, 
--    ordena_por_insercion [2,4,3,6,3] == [2,3,3,4,6]  
ordena_por_insercion :: Ord a => [a] -> [a]
ordena_por_insercion []     = []
ordena_por_insercion (x:xs) = 
    inserta x (ordena_por_insercion xs)   

-- ---------------------------------------------------------------------
-- Recursión sobre varios argumentos                                  --
-- ---------------------------------------------------------------------

-- (zip xs ys) es la lista de los pares de los elementos de xs e ys en
-- la misma posición. Por ejemplo,
--    zip [1,3,5] [2,4,6,8]  ==  [(1,2),(3,4),(5,6)]
zip :: [a] -> [b] -> [(a, b)]
zip []     _      = []
zip _      []     = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

-- (drop n xs) es la lista obtenida eliminando los n primeros elementos
-- de xs. Por ejemplo,
--    drop 2 [5,7,9,4] == [9,4]
--    drop 5 [1,4]     ==  [] 
drop :: Int -> [a] -> [a]
drop 0 xs     = xs
drop _ []     = []
drop n (x:xs) = drop (n-1) xs

-- ---------------------------------------------------------------------
-- Recursión múltiple                                                 --
-- ---------------------------------------------------------------------

-- (fibonacci n) es el n--ésimo término de la sucesión de Fibonacci. Por
-- ejemplo, 
--    fibonacci 8  ==  21  
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- (ordena xs) es la lista obtenida ordenando xs meciante el algoritmo
-- de ordenación rápida. Por ejemplo,
--    ordena [2,5,4,7]  ==  [2,4,5,7]
ordena :: (Ord a) => [a] -> [a]
ordena [] = []
ordena (x:xs) = 
    (ordena menores) ++ [x] ++ (ordena mayores)
    where menores = [a | a <- xs, a <= x]
          mayores = [b | b <- xs, b > x]

-- ---------------------------------------------------------------------
-- Recursión mutua                                                    --
-- ---------------------------------------------------------------------

-- (par x) se verifica si x es par e (impar x) se verifica si x es
-- impar. Por ejemplo,
--    par 3    ==  False
--    impar 3  ==  True
par :: Int -> Bool
par 0 = True
par n = impar (n-1)

impar :: Int -> Bool
impar 0  = False
impar n  = par (n-1)

-- (pares xs) son los elementos de xs que ocupan posiciones pares e
-- (impares xs) son los elementos de xs que ocupan posiciones
-- impares. Por ejemplo,
--    pares [1,3,5,7]  ==  [1,5]  
pares :: [a] -> [a]
pares []     = []
pares (x:xs) = x : impares xs

impares :: [a] -> [a]
impares []     = []
impares (_:xs) = pares xs

-- ---------------------------------------------------------------------
-- Heurísticas para las definiciones recursivas                       --
-- ---------------------------------------------------------------------

-- (init xs) es la lista xs sin el último elemento. Por ejemplo,
--    init [3,2,5]  ==  [2,5] 
init :: [a] -> [a]
init [_]    = []
init (x:xs) = x : init xs
