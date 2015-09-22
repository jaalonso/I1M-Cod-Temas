-- I1M T8.hs
-- Tema 8: Razonamiento sobre programas.
-- José A. Alonso Jiménez <jalonso@us,es>
-- Sevilla, 26 de Julio de 2009 (Revisión del 22-9-15)
-- =====================================================================

module I1M.Temas.T8 where

-- ---------------------------------------------------------------------
-- Importaciones                                                      --
-- ---------------------------------------------------------------------

import Prelude hiding ( (++) 
                      , drop
                      , length
                      , null
                      , replicate
                      , sum
                      , take
                      )
import Test.QuickCheck
import Test.QuickCheck.Function

-- (longitud xs) es el número de elementos de xs. Por ejemplo,
--    longitud [2,3,1]  ==  3
longitud :: [a] -> Int
longitud []     = 0                 -- longitud.1
longitud (_:xs) = 1 + longitud xs   -- longitud.2

-- (intercambia p) es el par obtenido intercambiando las componentes del
-- par p. Por ejemplo,
--    intercambia (2,5)  == (5,2)
intercambia :: (a,b) -> (b,a)
intercambia (x,y) = (y,x)       -- intercambia

-- Propiedad: La función intercambia es idempotente
prop_intercambia :: Eq a => a -> a -> Bool
prop_intercambia x y = 
    intercambia (intercambia (x,y)) == (x,y)

-- Comprobación:
--    ghci> quickCheck prop_intercambia
--    +++ OK, passed 100 tests.

-- (inversa xs) es la inversa de la lista xs. Por ejemplo,
--    inversa [3,2,5]  ==  [5,2,3]
inversa :: [a] -> [a]
inversa []     = []                  -- inversa.1
inversa (x:xs) = inversa xs ++ [x]   -- inversa.2

-- Propiedad: La inversa de una lista unitaria es ella misma. 
prop_inversa_unitaria :: Eq a => a -> Bool
prop_inversa_unitaria x =
    inversa [x] == [x]

-- Comprobación:
--    ghci> quickCheck prop_inversa_unitaria
--    +++ OK, passed 100 tests.

-- (no x) es la negación de x.
no :: Bool -> Bool
no False = True
no True  = False

-- Propiedad: La negación es idempotente.
prop_doble_negacion x =
  no (no x) == x

-- Comprobación:
--    ghci> quickCheck prop_doble_negacion
--    +++ OK, passed 100 tests.

-- (replicate n x) es la lista formda por `n` elementos iguales a
-- x. Por ejemplo, 
--    replicate 3 5  ==  [5,5,5]  
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

-- Propiedad: La longitud de (replicate n x) es n.
prop_length_replicate :: Int -> Int -> Bool
prop_length_replicate n xs =
    length (replicate m xs) == m
    where m = abs n

-- Comprobación
--    ghci> quickCheck prop_length_replicate
--    OK, passed 100 tests.

-- (xs ++ ys) es la concatenación de xs e ys.
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys             -- ++.1
(x:xs) ++ ys = x : (xs ++ ys) -- ++.2

-- Propiedad: La concatenación es asociativa.
prop_asociativa_conc :: [Int] -> [Int] -> [Int] -> Bool
prop_asociativa_conc xs ys zs = 
    xs ++ (ys ++ zs)==(xs ++ ys) ++ zs

-- Comprobación:
--    ghci> quickCheck prop_asociatividad_conc
--    OK, passed 100 tests.

-- Propiedad: xs ++ [] = xs
prop_identidad_concatenacion :: [Int] -> Bool
prop_identidad_concatenacion xs = xs ++ [] == xs  

-- Comprobación:
--    ghci> quickCheck prop_identidad_concatenacion
--    OK, passed 100 tests.

-- (length xs) es el número de elementos de xs.
length :: [a] -> Int
length []     = 0               -- length.1
length (x:xs) = 1 + length xs   -- length.2

-- Propiedad: length(xs ++ ys) = (length xs) + (length ys)
prop_length_append :: [Int] -> [Int] -> Bool
prop_length_append xs ys = 
   length(xs ++ ys)==(length xs)+(length ys) 

-- Comprobación:
--    ghci> quickCheck prop_length_append
--    OK, passed 100 tests.

-- (take n xs) es la lista formada por los n primeros elementos de xs.
take :: Int -> [a] -> [a]
take 0 _       = []                  -- take.1
take _ []      = []                  -- take.2
take n (x:xs)  = x : take (n-1) xs   -- take.3

-- (drop n xs) es la lista obtenida eliminando los n primeros elementos
-- de xs.
drop :: Int -> [a] -> [a]
drop 0 xs      = xs                  -- drop.1
drop _ []      = []                  -- drop,2
drop n (_:xs)  = drop (n-1) xs       -- drop.3

-- Propiedad: take n xs ++ drop n xs = xs
prop_take_drop :: Int -> [Int] -> Property
prop_take_drop n xs = 
    n >= 0 ==> take n xs ++ drop n xs == xs

-- Comprobación:
--    ghci> quickCheck prop_take_drop
--    OK, passed 100 tests.

-- (null xs) se verifica si xs no tiene elementos.
null :: [a] -> Bool
null []           = True            -- null.1
null (_:_)        = False           -- null.2

-- Propiedad: xs no tiene elementos syss (xs ++ xs) no los tiene. 
prop_null :: [Int] -> Bool
prop_null xs =
    null xs == null (xs ++ xs)

-- Comprobación
--    ghci> quickCheck prop_null
--    +++ OK, passed 100 tests.

-- (inversa1 xs) es la inversa de xs.
inversa1 :: [a] -> [a]
inversa1 []     = []                                      -- inversa1.1
inversa1 (x:xs) = inversa1 xs ++ [x]                      -- inversa1.2

-- (inversa2 xs) es la inversa de xs definida con un acumulador.
inversa2 :: [a] -> [a]
inversa2 xs = inversa2Aux xs []                           -- inversa2.1
    where inversa2Aux []     ys = ys                      -- inversa2Aux.1
          inversa2Aux (x:xs) ys = inversa2Aux xs (x:ys)   -- inversa2Aux.2

-- Propiedad: La funciones inversa1 e inversa2 son equivalentes.
prop_equiv_inversa :: [Int] -> Bool
prop_equiv_inversa xs = inversa1 xs == inversa2 xs

-- Comprobación
--    ghci> quickCheck prop_equiv_inversa
--    +++ OK, passed 100 tests.

-- (sum xs) es la suma de los elementos de xs.
sum :: [Int] -> Int
sum []     = 0                
sum (x:xs) = x + sum xs       

-- Propiedad: sum (map (2*) xs) = 2 * sum xs
prop_sum_map :: [Int] -> Bool
prop_sum_map xs = sum (map (2*) xs) == 2 * sum xs

--  Comprobación:
--    ghci> quickCheck prop_sum_map
--    +++ OK, passed 100 tests.

-- Propiedad La aplicación de una función a los elementos de una lista
-- conserva su longitud:
prop_map_length (Fun _ f) xs =
    length (map f xs) == length xs

-- Comprobación
--    ghci> quickCheck prop_map_length
--    +++ OK, passed 100 tests.
