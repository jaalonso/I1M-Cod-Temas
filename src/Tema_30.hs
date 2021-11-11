-- Tema_30.hs
-- Tema 30: Resolución de problemas mediante programación dinámica
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_30 where

-- ---------------------------------------------------------------------
-- § Introducción                                                     --
-- ---------------------------------------------------------------------

-- El objetivo de este tema es introducir la programación dinámica mediante
-- una colección de problemas.
--
-- Se usarán la librería Data.Array

import Data.Array

-- ---------------------------------------------------------------------
-- § Los números de Fibonacci                                         --
-- ---------------------------------------------------------------------

-- Los primeros términos de la sucesión de Fibonacci son
--    0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, ...
--
-- Definir la función
--    fib :: Integer -> Integer
-- tal que (fib n) es el n-ésimo término de la sucesión de Fibonacci. Por
-- ejemplo,
--    fib 6 == 8

-- Definición por recursión
-- ========================

fib1 :: Integer -> Integer
fib1 0 = 0
fib1 1 = 1
fib1 n = fib1 (n-1) + fib1 (n-2)

-- Definición con programación dinámica
-- ====================================

fib2 :: Integer -> Integer
fib2 n = vectorFib2 n ! n

-- (vectorFib2 n) es el vector con índices de 0 a n tal que el valor
-- de la posición i es el i-ésimo número de Finonacci. Por ejemplo,
--    λ> vectorFib2 7
--    array (0,7) [(0,0),(1,1),(2,1),(3,2),(4,3),(5,5),(6,8),(7,13)]
vectorFib2 :: Integer -> Array Integer Integer
vectorFib2 n = v where
  v = array (0,n) [(i,f i) | i <- [0..n]]
  f 0 = 0
  f 1 = 1
  f m = v!(m-1) + v!(m-2)

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> fib1 34
--    5702887
--    (13.00 secs, 3,780,606,208 bytes)
--    λ> fib2 34
--    5702887
--    (0.00 secs, 0 bytes)

-- ---------------------------------------------------------------------
-- § Coeficientes binomiales                                          --
-- ---------------------------------------------------------------------

-- El coeficiente binomial n sobre k es el número de subconjuntos de k
-- elementos escogidos de un conjunto con n elementos.
--
-- Definir la función
--    binomial :: Integer -> Integer -> Integer

-- Definición por recursión
-- ========================

binomial1 :: Integer -> Integer -> Integer
binomial1 _ 0 = 1
binomial1 n k
  | n == k    = 1
  | otherwise = binomial1 (n-1) (k-1) + binomial1 (n-1) k

-- Definición con programación dinámica
-- ====================================

binomial2 :: Integer -> Integer -> Integer
binomial2 n k = matrizBinomial2 n k ! (n,k)

-- (matrizBinomial2 n k) es la matriz de orden (n+1)x(k+1) tal que el
-- valor en la posición (i,j) (con j <= i) es el coeficiente binomial i
-- sobre j. Por ejemplo,
--    λ> [[(matrizBinomial2 3 3)!(i,j) | j <- [0..i]] | i <- [0..3]]
--    [[1],[1,1],[1,2,1],[1,3,3,1]]
matrizBinomial2 :: Integer -> Integer -> Array (Integer,Integer) Integer
matrizBinomial2 n k = q where
  q = array ((0,0),(n,k)) [((i,j),f i j) | i <- [0..n], j <- [0..k]]
  f _ 0 = 1
  f i j
    | i == j    = 1
    | otherwise = q!(i-1,j-1) + q!(i-1,j)

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> binomial1 24 12
--    2704156
--    (4.16 secs, 1,479,653,016 bytes)
--    λ> binomial2 24 12
--    2704156
--    (0.00 secs, 0 bytes)

-- ---------------------------------------------------------------------
-- § Longitud de la subsecuencia común máxima (SCM)                   --
-- ---------------------------------------------------------------------

-- Si a una secuencia X de elementos (pongamos por ejemplo, caracteres) le
-- quitamos algunos de ellos y dejamos los que quedan en el orden en el que
-- aparecían originalmente tenemos lo que se llama una subsecuencia de X. Por
-- ejemplo, "aaoa" es una subsecuencia de la secuencia "amapola".
--
-- El término también se aplica cuando quitamos todos los elementos (es
-- decir, la secuencia vacía es siempre subsecuencia de cualquier
-- secuencia) o cuando no quitamos ninguno (lo que significa que
-- cualquier secuencia es siempre subsecuencia de sí misma).
--
-- Dadas dos secuencias X e Y, decimos que Z es una subsecuencia común
-- de X e Y si Z es subsecuencia de X y de Y. Por ejemplo, si X =
-- "amapola" e Y = "matamoscas", la secuencia "aaoa" es una de las
-- subsecuencias comunes de X e Y más larga, con longitud 4, ya que no
-- hay ninguna subsecuencia común a X e Y de longitud mayor que
-- 4. También son subsecuencias comunes de longitud 4 "maoa" o "amoa".
--
-- Se desea encontrar la longitud de las subsecuencias comunes más
-- largas de dos secuencias de caracteres dadas.
--
-- Definir la función
--    longitudSCM :: Eq a => [a] -> [a] -> Int
-- tal que (longitudSCM xs ys) es la longitud de la subsecuencia
-- máxima de xs e ys. Por ejemplo,
--    longitudSCM "amapola" "matamoscas" == 4
--    longitudSCM "atamos" "matamoscas"  == 6
--    longitudSCM "aaa" "bbbb"           == 0

-- Definición por recursión
-- ========================

longitudSCM1 :: Eq a => [a] -> [a] -> Int
longitudSCM1 [] _ = 0
longitudSCM1 _ [] = 0
longitudSCM1 (x:xs) (y:ys)
  | x == y    = 1 + longitudSCM1 xs ys
  | otherwise = max (longitudSCM1 (x:xs) ys) (longitudSCM1 xs (y:ys))

-- Definición con programación dinámica
-- ====================================

longitudSCM2 :: Eq a => [a] -> [a] -> Int
longitudSCM2 xs ys = matrizLongitudSCM2 xs ys ! (n,m)
  where n = length xs
        m = length ys

-- (matrizLongitudSCM2 xs ys) es la matriz de orden (n+1)x(m+1) (donde n
-- y m son los números de elementos de xs e ys, respectivamente) tal que
-- el valor en la posición (i,j) es la longitud de la SCM de los i
-- primeros elementos de xs y los j primeros elementos de ys. Por ejemplo,
--    λ> elems (matrizLongitudSCM2 "amapola" "matamoscas")
--    [0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,1,1,1,1,2,2,2,2,2,2,
--     0,1,2,2,2,2,2,2,2,3,3,0,1,2,2,2,2,2,2,2,3,3,0,1,2,2,2,2,3,3,3,3,3,
--     0,1,2,2,2,2,3,3,3,3,3,0,1,2,2,3,3,3,3,3,4,4]
-- Gráficamente,
--       m a t a m o s c a s
--    [0,0,0,0,0,0,0,0,0,0,0,
-- a   0,0,1,1,1,1,1,1,1,1,1,
-- m   0,1,1,1,1,2,2,2,2,2,2,
-- a   0,1,2,2,2,2,2,2,2,3,3,
-- p   0,1,2,2,2,2,2,2,2,3,3,
-- o   0,1,2,2,2,2,3,3,3,3,3,
-- l   0,1,2,2,2,2,3,3,3,3,3,
-- a   0,1,2,2,3,3,3,3,3,4,4]
matrizLongitudSCM2 :: Eq a => [a] -> [a] -> Array (Int,Int) Int
matrizLongitudSCM2 xs ys = q
  where
    n = length xs
    m = length ys
    v = listArray (1,n) xs
    w = listArray (1,m) ys
    q = array ((0,0),(n,m)) [((i,j), f i j) | i <- [0..n], j <- [0..m]]
      where f 0 _ = 0
            f _ 0 = 0
            f i j | v ! i == w ! j = 1 + q ! (i-1,j-1)
                  | otherwise      = max (q ! (i-1,j)) (q ! (i,j-1))

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> longitudSCM1 (take 18 (cycle [1,3])) (take 18 (cycle [2,3]))
--    9
--    (21.59 secs, 8,868,816,536 bytes)
--    λ> longitudSCM2 (take 18 (cycle [1,3])) (take 18 (cycle [2,3]))
--    9
--    (0.01 secs, 0 bytes)

-- ---------------------------------------------------------------------
-- § Subsecuencia común máxima (SCM)                                  --
-- ---------------------------------------------------------------------

-- Definir la función
--    scm :: Eq a => [a] -> [a] -> [a]
-- tal que (scm xs ys) es una de las subsecuencias comunes de longitud
-- máxima de xs e ys. Por ejemplo,
--    scm "amapola" "matamoscas" == "amoa"
--    scm "atamos" "matamoscas"  == "atamos"
--    scm "aaa" "bbbb"           == ""

-- Definición por recursión
-- ========================

scm1 :: Eq a => [a] -> [a] -> [a]
scm1 [] _ = []
scm1 _ [] = []
scm1 (x:xs) (y:ys)
  | x == y    = x : scm1 xs ys
  | otherwise = mayor (scm1 (x:xs) ys) (scm1 xs (y:ys))

-- (mayor xs ys) es la cadena más larga de xs e ys.
--    mayor "hola" "buenas"  ==  "buenas"
--    mayor "hola" "pera"    ==  "hola"
mayor :: [a] -> [a] -> [a]
mayor xs ys
  | length xs >= length ys = xs
  | otherwise              = ys

-- Definición con programación dinámica
-- ====================================

scm2 :: Eq a => [a] -> [a] -> [a]
scm2 xs ys = reverse ((matrizSCM2 xs ys) ! (n,m))
  where n = length xs
        m = length ys

-- (matrizSCM2 xs ys) es la matriz de orden (n+1)x(m+1) (donde n
-- y m son los números de elementos de xs e ys, respectivamente) tal que
-- el valor en la posición (i,j) es una SCM de los i primeros
-- elementos de xs y los j primeros elementos de ys. Por ejemplo,
--    λ> elems (matrizSCM2 "amapola" "matamoscas")
--    ["","","","","","","","","","","","","","a","a","a","a","a","a",
--     "a","a","a","","m","a","a","a","ma","ma","ma","ma","ma","ma","",
--     "m","am","am","aa","ma","ma","ma","ma","ama","ama","","m","am",
--     "am","aa","ma","ma","ma","ma","ama","ama","","m","am","am","aa",
--     "ma","oma","oma","oma","ama","ama","","m","am","am","aa","ma",
--     "oma","oma","oma","ama","ama","","m","am","am","aam","aam","oma",
--     "oma","oma","aoma","aoma"]
-- Gráficamente,
--        m   a    t    a     m     o     s     c     a      s
--    ["","" ,""  ,""  ,""   ,""   ,""   ,""   ,""   ,""    ,"",
-- a   "","" ,"a" ,"a" ,"a"  ,"a"  ,"a"  ,"a"  ,"a"  ,"a"   ,"a",
-- m   "","m","a" ,"a" ,"a"  ,"ma" ,"ma" ,"ma" ,"ma" ,"ma"  ,"ma",
-- a   "","m","am","am","aa" ,"ma" ,"ma" ,"ma" ,"ma" ,"ama" ,"ama",
-- p   "","m","am","am","aa" ,"ma" ,"ma" ,"ma" ,"ma" ,"ama" ,"ama",
-- o   "","m","am","am","aa" ,"ma" ,"oma","oma","oma","ama" ,"ama",
-- l   "","m","am","am","aa" ,"ma" ,"oma","oma","oma","ama" ,"ama",
-- a   "","m","am","am","aam","aam","oma","oma","oma","aoma","aoma"]
matrizSCM2 :: Eq a => [a] -> [a] -> Array (Int,Int) [a]
matrizSCM2 xs ys = q where
  q = array ((0,0),(n,m)) [((i,j), f i j) | i <- [0..n], j <- [0..m]]
  n = length xs
  m = length ys
  v = listArray (1,n) xs
  w = listArray (1,m) ys
  f 0 _ = []
  f _ 0 = []
  f i j | v ! i == w ! j = (v!i) : (q ! (i-1,j-1))
        | otherwise      = mayor (q ! (i-1,j)) (q ! (i,j-1))

-- Comparación de eficiencia
-- =========================

-- La comparación es
-- λ> length (scm1 (take 16 (cycle [1,3])) (take 16 (cycle [2,3])))
-- 8
-- (11.30 secs, 1,716,231,120 bytes)
-- λ> length (scm2 (take 16 (cycle [1,3])) (take 16 (cycle [2,3])))
-- 8
-- (0.02 secs, 0 bytes)

-- ---------------------------------------------------------------------
-- § Distancia de Levenshtein                                         --
-- ---------------------------------------------------------------------


-- La distancia de Levenshtein (o distancia de edición) es el número mínimo
-- de operaciones requeridas para transformar una cadena de caracteres en
-- otra. Las operaciones de edición que se pueden hacer son:
-- + insertar un carácter (por ejemplo, de "abc" a "abca")
-- + eliminar un carácter (por ejemplo, de "abc" a "ac")
-- + sustituir un carácter (por ejemplo, de "abc" a "adc")
--
-- Por ejemplo, la distancia de Levenshtein entre "casa" y "calle" es de
-- 3 porque se necesitan al menos tres ediciones elementales para
-- cambiar uno en el otro:
--    "casa"  --> "cala"  (sustitución de 's' por 'l')
--    "cala"  --> "calla" (inserción de 'l' entre 'l' y 'a')
--    "calla" --> "calle" (sustitución de 'a' por 'e')
--
-- Definir la función
--    levenshtein :: String -> String -> Int
-- tal que (levenshtein xs ys) es la distancia de Levenshtein entre xs e
-- ys. Por ejemplo,
--    levenshtein "casa"  "calle"    ==  3
--    levenshtein "calle" "casa"     ==  3
--    levenshtein "casa"  "casa"     ==  0
--    levenshtein "ana" "maria"      ==  3
--    levenshtein "agua" "manantial" ==  7

-- Definición por recursión
-- ========================

levenshtein1 :: String -> String -> Int
levenshtein1 "" ys = length ys
levenshtein1 xs "" = length xs
levenshtein1 c1@(x:xs) c2@(y:ys)
  | x == y    = levenshtein1 xs ys
  | otherwise = 1 + minimum [ levenshtein1 xs c2
                            , levenshtein1 c1 ys
                            , levenshtein1 xs ys]

-- Definición con programación dinámica
-- ====================================

levenshtein2 :: String -> String -> Int
levenshtein2 xs ys = (matrizLevenshtein xs ys) ! (m,n)
  where  m = length xs
         n = length ys

-- (matrizLevenshtein xs ys) es la matriz cuyo número de filas es la
-- longitud de xs, cuyo número de columnas es la longitud de ys y en
-- valor en la posición (i,j) es la distancia de Levenshtein entre los
-- primeros i caracteres de xs y los j primeros caracteres de ys. Por
-- ejemplo,
--    λ> elems (matrizLevenshtein "casa" "calle")
--    [0,1,2,3,4,5,1,0,1,2,3,4,2,1,0,1,2,3,3,2,1,1,2,3,4,3,2,2,2,3]
-- Gráficamente,
--       c a l l e
--     0,1,2,3,4,5,
--  c  1,0,1,2,3,4,
--  a  2,1,0,1,2,3,
--  s  3,2,1,1,2,3,
--  a  4,3,2,2,2,3
matrizLevenshtein :: String -> String -> Array (Int,Int) Int
matrizLevenshtein xs ys = q where
  q = array ((0,0),(m,n)) [((i,j), f i j) | i <- [0..m], j <- [0..n]]
  m = length xs
  n = length ys
  f 0 j = j
  f i 0 = i
  f i j | xs !! (i-1) == ys !! (j-1) = q ! (i-1,j-1)
        | otherwise                  = 1 + minimum [ q ! (i-1,j)
                                                   , q ! (i,j-1)
                                                   , q ! (i-1,j-1)]

-- Comparación de eficiencia
-- =========================

-- La comparación es
--    λ> levenshtein1 (show (2^33)) (show (3^33))
--    12
--    (16.19 secs, 11,766,254,536 bytes)
--    λ> levenshtein2 (show (2^33)) (show (3^33))
--    12
--    (0.02 secs, 0 bytes)
