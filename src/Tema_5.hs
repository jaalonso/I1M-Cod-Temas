-- Tema_5.hs
-- Tema 5: Definiciones de listas por comprensión
-- José A. Alonso Jiménez https://jaalonso.github.com
-- ---------------------------------------------------------------------

module Tema_5 where

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------

import Data.Char (chr, isLower, ord)
import Test.QuickCheck (quickCheck)

-- ---------------------------------------------------------------------
-- Generadores                                                        --
-- ---------------------------------------------------------------------

-- (concat' xss) es la concatenación de la lista de listas xss. Por
-- ejemplo,
--    concat' [[1,3],[2,5,6],[4,7]]  ==  [1,3,2,5,6,4,7]
concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

-- (primeros ps) es la lista de los primeros elementos de la lista de
-- pares ps. Por ejemplo,
--    primeros [(1,3),(2,5),(6,3)]  ==  [1,2,6]
primeros :: [(a, b)] -> [a]
primeros ps =  [x | (x,_) <- ps]

-- (length' xs) es la longitud de xs. Por ejemplo,
--    length' [4,2,5]  ==  3
length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

-- ---------------------------------------------------------------------
-- Guardas                                                            --
-- ---------------------------------------------------------------------

-- (factores n) es la lista de los factores del número n. Por ejemplo,
--    factores 30  ==  [1,2,3,5,6,10,15,30]
factores :: Int -> [Int]
factores n = [x | x <- [1..n], n `mod` x == 0]

-- (primo n) se verifica si n es primo. Por ejemplo,
--    primo 30  == False
--    primo 31  == True
primo :: Int -> Bool
primo n = factores n == [1, n]

-- (primos n) es la lista de los primos menores o iguales que n. Por
-- ejemplo,
--    primos 31  == [2,3,5,7,11,13,17,19,23,29,31]
primos :: Int -> [Int]
primos n = [x | x <- [2..n], primo x]

-- (busca c t) es la lista de los valores de la lista de asociación t
-- cuyas claves valen c. Por ejemplo,
--    busca 'b' [('a',1),('b',3),('c',5),('b',2)]  ==  [3,2]
busca :: Eq a => a -> [(a, b)] -> [b]
busca c t = [v | (c', v) <- t, c' == c]

-- ---------------------------------------------------------------------
-- La función zip                                                     --
-- ---------------------------------------------------------------------

-- (adyacentes xs) es la lista de los pares de elementos adyacentes de
-- la lista xs. Por ejemplo,
--    adyacentes [2,5,3,7]  ==  [(2,5),(5,3),(3,7)]
adyacentes :: [a] -> [(a, a)]
adyacentes xs = zip xs (tail xs)

-- (ordenada xs) se verifica si la lista xs está ordenada. Por ejemplo,
--    ordenada [1,3,5,6,7]  ==  True
--    ordenada [1,3,6,5,7]  ==  False
ordenada :: Ord a => [a] -> Bool
ordenada xs = and [x <= y | (x,y) <- adyacentes xs]

-- (posiciones x xs) es la lista de las posiciones ocupadas por el
-- elemento x en la lista xs. Por ejemplo,
--    posiciones 5 [1,5,3,5,5,7]  ==  [1,3,4]
posiciones :: Eq a => a -> [a] -> [Int]
posiciones x xs = [i | (x',i) <- zip xs [0..], x == x']

-- ---------------------------------------------------------------------
-- Comprensión de cadenas                                             --
-- ---------------------------------------------------------------------

-- (minusculas c) es la cadena formada por las letras minúsculas de la
-- cadena c. Por ejemplo,
--    minusculas "EstoEsUnaPrueba"  ==  "stosnarueba"
minusculas :: String -> String
minusculas xs = [x | x <- xs, isLower x]

-- (ocurrencias x xs) es el número de veces que ocurre el carácter x en
-- la cadena xs. Por ejemplo,
--    ocurrencias 'a' "Salamanca"  ==  4
ocurrencias :: Char -> String -> Int
ocurrencias x xs = length [x' | x' <- xs, x == x']

-- ---------------------------------------------------------------------
-- Cifrado César                                                      --
-- ---------------------------------------------------------------------

-- Codificación y descodificación
-- ------------------------------

-- (let2int c) es el entero correspondiente a la letra minúscula c. Por
-- ejemplo,
--    let2int 'a'  ==  0
--    let2int 'd'  ==  3
--    let2int 'z'  ==  25
let2int :: Char -> Int
let2int c = ord c - ord 'a'

-- (int2let n) es la letra minúscula correspondiente al entero n. Por
-- ejemplo,
--    int2let 0   ==  'a'
--    int2let 3   ==  'd'
--    int2let 25  ==  'z'
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

-- (desplaza n c) es el carácter obtenido desplazando n caracteres el
-- carácter c. Por ejemplo,
--    desplaza   3  'a'  ==  'd'
--    desplaza   3  'y'  ==  'b'
--    desplaza (-3) 'd'  ==  'a'
--    desplaza (-3) 'b'  ==  'y'
desplaza :: Int -> Char -> Char
desplaza n c | isLower c = int2let ((let2int c + n) `mod` 26)
             | otherwise =  c

-- (codifica n xs) es el resultado de codificar el texto xs con un
-- desplazamiento n. Por ejemplo,
--    codifica   3  "En todo la medida"   ==  "Eq wrgr od phglgd"
--    codifica (-3) "Eq wrgr od phglgd"   ==  "En todo la medida"
codifica :: Int -> String -> String
codifica n xs = [desplaza n x | x <- xs]

-- Propiedad: Al desplazar −n un carácter desplazado n, se obtiene
-- el carácter inicial.
prop_desplaza :: Int -> Char -> Bool
prop_desplaza n c =
  desplaza (-n) (desplaza n c') == c'
  where normaliza d | d `elem` ['a'..'z'] = d
                    | otherwise           = 'a'
        c' = normaliza c

-- La comprobación es
--    λ> quickCheck prop_desplaza
--    +++ OK, passed 100 tests.

-- Propiedad: Al codificar con −n una cadena codificada con n, se
-- obtiene la cadena inicial.
prop_codifica :: Int -> String -> Bool
prop_codifica n xs =
  codifica (-n) (codifica n xs') == xs'
  where normaliza c | c `elem` ['a'..'z'] = c
                    | otherwise           = 'a'
        xs' = [normaliza c | c <- xs]

-- La comprobación es
--    λ> quickCheck prop_codifica
--    +++ OK, passed 100 tests.

-- Análisis de frecuencia
-- ----------------------

-- tabla es la lista de la frecuencias de las letras en castellano, Por
-- ejemplo, la frecuencia de la 'a' es del 12.53%, la de la 'b' es 1.42%.
tabla :: [Float]
tabla = [12.53, 1.42, 4.68, 5.86, 13.68, 0.69, 1.01, 0.70, 6.25,
          0.44, 0.01, 4.97, 3.15,  6.71, 8.68, 2.51, 0.88, 6.87,
          7.98, 4.63, 3.93, 0.90,  0.02, 0.22, 0.90, 0.52]

-- (porcentaje n m) es el porcentaje de n sobre m. Por ejemplo,
--    porcentaje 2 5  ==  40.0
porcentaje :: Int -> Int -> Float
porcentaje n m = (fromIntegral n / fromIntegral m) * 100

-- (frecuencias xs) es la frecuencia de cada una de las minúsculas de la
-- cadena xs. Por ejemplo,
--    > frecuencias "en todo la medida"
--    [14.3,0,0,21.4,14.3,0,0,0,7.1,0,0,7.1,
--     7.1,7.1,14.3,0,0,0,0,7.1,0,0,0,0,0,0]
frecuencias :: String -> [Float]
frecuencias xs =
    [porcentaje (ocurrencias x xs) n | x <- ['a'..'z']]
    where n = length (minusculas xs)

-- (chiCuadrado os es) es la medida chi cuadrado de la discrepancia
-- entre la distribución observada os y la esperada es. Por ejemplo,
--    chiCuadrado [3,5,6] [3,5,6]  ==  0.0
--    chiCuadrado [3,5,6] [5,6,3]  ==  3.9666667
chiCuadrado :: [Float] -> [Float] -> Float
chiCuadrado os es = sum [((o - e) ^ 2) / e | (o,e) <- zip os es]

-- (rota n xs) es la lista obtenida rotando n posiciones los elementos
-- de la lista xs. Por ejemplo,
--    rota 2 "ramo"  ==  "mora"
rota :: Int -> [a] -> [a]
rota n xs = drop n xs ++ take n xs

-- (descifra xs) es la cadena obtenida descodificando la cadena xs por
-- el desplazamiento que produce una distribución de minúsculas con
-- la menor deviación chi cuadrado respecto de la tabla de distribución
-- de las vocales en castellano. Por ejemplo,
--   > descifra "Lt htruqnhfit ij qf anif jx ijxhzgwnw qt xnruqj vzj jx"
--   "Lo complicado de la vida es descubrir lo simple que es"
descifra :: String -> String
descifra xs =  codifica (-factor) xs
  where
    factor = head (posiciones (minimum tabChi) tabChi)
    tabChi = [chiCuadrado (rota n tabla') tabla | n <- [0..25]]
    tabla' = frecuencias xs

-- ---------------------------------------------------------------------
-- § Verificación de propiedades                                      --
-- ---------------------------------------------------------------------

-- Las propiedades son
verifica_Tema_5 :: IO ()
verifica_Tema_5 =
  sequence_ [quickCheck prop_desplaza,
             quickCheck prop_codifica]

-- Su verificación es
--    λ> verifica_Tema_5
--    +++ OK, passed 100 tests.
--    +++ OK, passed 100 tests.
