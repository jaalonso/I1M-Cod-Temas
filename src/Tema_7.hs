-- Tema_7.hs
-- Tema 7: Funciones de orden superior.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_7 where

-- ---------------------------------------------------------------------
-- Importaciones                                                      --
-- ---------------------------------------------------------------------

import Prelude hiding (id, foldr, foldl, (.))
import Test.QuickCheck
import Data.Char

-- ---------------------------------------------------------------------
-- Funciones de orden superior                                        --
-- ---------------------------------------------------------------------

-- (dosVeces f x) es el resultado de aplicar f a f x. Por ejemplo,
--    dosVeces (*3) 2           ==  18
--    dosVeces reverse [2,5,7]  ==  [2,5,7]
dosVeces :: (a -> a) -> a -> a
dosVeces f x = f (f x)

-- id es la función identidad.
id :: a -> a
id x =  x

-- ---------------------------------------------------------------------
-- Procesamiento de listas                                            --
-- ---------------------------------------------------------------------

-- (map f xs) es la lista obtenida aplicando f a cada elemento de
-- xs. Por ejemplo,
--    map (*2) [3,4,7]     == [6,8,14]
--    map sqrt [1,2,4]     == [1.0,1.4142135623731,2.0]
--    map even [1..5]      == [False,True,False,True,False]

-- Definición de map por comprensión:
mapC :: (a -> b) -> [a] -> [b]
mapC f xs =  [f x | x <- xs]

-- Definición de map por recursión:
mapR :: (a -> b) -> [a] -> [b]
mapR _ []     = []
mapR f (x:xs) = f x : mapR f xs

-- Propiedad:  sum (map (2*) xs) = 2 * sum xs
prop_sum_map :: [Int] -> Bool
prop_sum_map xs = sum (map (2*) xs) == 2 * sum xs

-- Comprobación con QuickCheck:
--    λ> quickCheck prop_sum_map
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- La función filter                                                  --
-- ---------------------------------------------------------------------

-- (filter p xs) es la lista de los elementos de xs que cumplen la
-- propiedad p. Por ejemplo,
--    filter even [1,3,5,4,2,6,1] == [4,2,6]
--    filter (>3) [1,3,5,4,2,6,1] == [5,4,6]

-- Definición de filter por comprensión:
filterC :: (a -> Bool) -> [a] -> [a]
filterC p xs =  [x | x <- xs, p x]

-- Definición de filter por recursión:
filterR :: (a -> Bool) -> [a] -> [a]
filterR _ []                 = []
filterR p (x:xs) | p x       = x : filterR p xs
                 | otherwise = filterR p xs

-- (sumaCuadradosPares xs) es la suma de los cuadrados de los números
-- pares de la lista xs. Por ejemplo,
--    sumaCuadradosPares [1..5]  ==  20
sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares xs = sum (map (^2) (filter even xs))

-- Definición por comprensión:
sumaCuadradosPares' :: [Int] -> Int
sumaCuadradosPares' xs = sum [x^2 | x <- xs, even x]

-- ---------------------------------------------------------------------
-- Función de plegado por la derecha: foldr                           --
-- ---------------------------------------------------------------------

-- Ejemplos de definiciones recursivas sobre listas:
sum' :: [Integer] -> Integer
sum' []         = 0
sum' (x:xs)     = x + sum' xs

product' :: [Integer] -> Integer
product' []     = 1
product' (x:xs) = x * product' xs

or' :: [Bool] -> Bool
or' []          = False
or' (x:xs)      = x || or' xs

and' :: [Bool] -> Bool
and' []         = True
and' (x:xs)     = x && and' xs

-- Definiciones con foldr
sum'', product'' :: [Integer] -> Integer
or'', and''      :: [Bool] -> Bool
sum''     = foldr (+) 0
product'' = foldr (*) 1
or''      = foldr (||) False
and''     = foldr (&&) True

-- Definición del patrón foldr
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v []     =  v
foldr f v (x:xs) =  f x (foldr f v xs)

-- (longitud xs) es el número de elementos de xs Por ejemplo,
--    longitud [4,2,5]  ==  3
longitud :: [a] -> Int
longitud = foldr (\_ n -> 1+n) 0

-- (inversa xs) es la inversa de la lista xs. Por ejemplo,
--    inversa [4,2,5]  ==  [5,2,4]
inversa :: [a] -> [a]
inversa = foldr (\x xs -> xs ++ [x]) []

-- (conc xs ys) es la conctenación de las listas xs es ys. Por ejemplo,
--    conc [4,2,5] [7,9]  ==  [4,2,5,7,9]
conc :: [a] -> [a] -> [a]
conc xs ys = (foldr (:) ys) xs

-- ---------------------------------------------------------------------
-- Función de plegado por la izquierda: foldl                         --
-- ---------------------------------------------------------------------

-- (suma xs) es la suma de la lista xs. Por ejemplo,
--    suma [2,3,7]  ==  12
suma :: [Integer] -> Integer
suma = sumaAux 0
  where sumaAux v []     = v
        sumaAux v (x:xs) = sumaAux (v+x) xs

-- Definiciones con foldl:
suma3, product3 :: [Integer] -> Integer
or3, and3      :: [Bool] -> Bool
suma3    = foldl (+) 0
product3 = foldl (*) 1
or3      = foldl (||) False
and3     = foldl (&&) True

-- Definición de foldl:
foldl :: (a -> b -> a) -> a -> [b ] -> a
foldl _ v []     =  v
foldl f v (x:xs) =  foldl f (f v x ) xs

-- ---------------------------------------------------------------------
-- Composición de funciones                                           --
-- ---------------------------------------------------------------------

-- (f . g) es la composición de f y g.
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g  = \x -> f (g x)

-- Definiciones sin composición:
parSC                :: Integer -> Bool
dosVecesSC           :: (a -> a) -> a -> a
sumaCuadradosParesSC :: [Integer] -> Integer
parSC n                 = not (odd n)
dosVecesSC f x          = f (f x)
sumaCuadradosParesSC ns = sum (map (^2) (filter even ns))

-- Definiciones con composición:
parCC                :: Integer -> Bool
dosVecesCC           :: (a -> a) -> a -> a
sumaCuadradosParesCC :: [Integer] -> Integer
parCC                = not . odd
dosVecesCC f         = f . f
sumaCuadradosParesCC = sum . map (^2) . filter even

-- (composicionLista fs) es la composición de la lista de funciones
-- fs. Por ejemplo,
--    composicionLista [(*2),(^2)] 3       ==  18
--    composicionLista [(^2),(*2)] 3       ==  36
--    composicionLista [(/9),(^2),(*2)] 3  ==  4.0
composicionLista :: [a -> a] -> (a -> a)
composicionLista =  foldr (.) id

-- ---------------------------------------------------------------------
-- Caso de estudio: Codificación binaria y transmisión de cadenas     --
-- ---------------------------------------------------------------------

-- Objetivo: Definir una función que convierta una cadena en una lista
-- de ceros y unos junto con otra función que realice la conversión
-- opuesta.

-- Los números binarios se representan mediante listas de bits en orden
-- inverso. Un bit es cero o uno. Por ejemplo, el número 1101 se
-- representa por [1,0,1,1].

--  El tipo Bit es el de los bits.
type Bit = Int

-- Cambio de bases
-- ===============

-- (bin2int x) es el número decimal correspondiente al número binario
-- x. Por ejemplo,
--    bin2int [1,0,1,1]  ==  13
bin2int :: [Bit] -> Int
bin2int =  foldr (\x y -> x + 2*y) 0

-- Puede definirse por recursión
--    bin2intR [1,0,1,1]  ==  13
bin2intR :: [Bit] -> Int
bin2intR [] = 0
bin2intR (x:xs) = x + 2 * (bin2intR xs)

-- Puede definirse por comprensión
--    bin2intC [1,0,1,1]  ==  13
bin2intC :: [Bit] -> Int
bin2intC xs = sum [x*2^n | (x,n) <- zip xs [0..]]

-- (int2bin x) es el número binario correspondiente al número decimal
-- x. Por ejemplo,
--    int2bin 13  ==  [1,0,1,1]
int2bin :: Int -> [Bit]
int2bin n | n < 2     = [n]
          | otherwise = n `mod` 2 : int2bin (n `div` 2)

-- Propiedad: Al pasar un número natural a binario con int2bin y el
-- resultado a decimal con bin2int se obtiene el número inicial.
prop_int_bin :: Int -> Bool
prop_int_bin x =
  bin2int (int2bin y) == y
  where y = abs x

-- Comprobación:
--    > quickCheck prop_int_bin
--    +++ OK, passed 100 tests.

-- Codificación y descodificación
-- ==============================

-- Un octeto es un grupo de ocho bits.

-- (creaOcteto bs) es el octeto correspondiente a la lista de bits bs;
-- es decir, los 8 primeros elementos de bs si su longitud es mayor o
-- igual que 8 y la lista de 8 elemento añadiendo ceros al final de bs
-- en caso contrario. Por ejemplo,
--    creaOcteto [1,0,1,1,0,0,1,1,1,0,0,0]  ==  [1,0,1,1,0,0,1,1]
--    creaOcteto [1,0,1,1]                  ==  [1,0,1,1,0,0,0,0]
creaOcteto :: [Bit] -> [Bit]
creaOcteto bs = take 8 (bs ++ repeat 0)

-- creaOcteto se puede definir sin usar repeat:
--    creaOcteto' [1,0,1,1,0,0,1,1,1,0,0,0]  ==  [1,0,1,1,0,0,1,1]
--    creaOcteto' [1,0,1,1]                  ==  [1,0,1,1,0,0,0,0]
creaOcteto' :: [Bit] -> [Bit]
creaOcteto' bs =  take 8 (bs ++ replicate 8 0)

-- (codifica c) es la codificación de la cadena c como una lista de bits
-- obtenida convirtiendo cada carácter en un número Unicode,
-- convirtiendo cada uno de dichos números en un octeto y concatenando
-- los octetos para obtener una lista de bits. Por ejemplo,
--    λ> codifica "abc"
--    [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
codifica :: String -> [Bit]
codifica =  concat . map (creaOcteto . int2bin . ord)

-- (separaOctetos bs) es la lista obtenida separando la lista de bits bs
-- en listas de 8 elementos. Por ejemplo,
--    λ> separaOctetos [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0]
--    [[1,0,0,0,0,1,1,0],[0,1,0,0,0,1,1,0]]
separaOctetos :: [Bit] -> [[Bit]]
separaOctetos [] = []
separaOctetos bs = take 8 bs : separaOctetos (drop 8 bs)

-- (descodifica bs) es la cadena correspondiente a la lista de bits
-- bs. Por ejemplo,
--    λ> descodifica [1,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,1,1,0,0,0,1,1,0]
--    "abc"
descodifica :: [Bit] -> String
descodifica =  map (chr . bin2int) . separaOctetos

-- Los canales de transmisión pueden representarse mediante funciones
-- que transforman cadenas de bits en cadenas de bits.

-- (transmite c t) es la cadena obtenida transmitiendo la cadena t a
-- través del canal c. Por ejemplo,
--    λ> transmite id "Texto por canal correcto"
--    "Texto por canal correcto"
transmite :: ([Bit] -> [Bit]) -> String -> String
transmite canal =  descodifica . canal . codifica

-- Propiedad: Al trasmitir cualquier cadena por el canal identidad se
-- obtiene la cadena.
prop_transmite :: ASCIIString -> Bool
prop_transmite (ASCIIString cs) =
  transmite id cs == cs

-- Comprobación de la corrección:
--    λ> quickCheck prop_transmite
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- § Verificación de propiedades                                      --
-- ---------------------------------------------------------------------

-- Las propiedades son
verifica_Tema_7 :: IO ()
verifica_Tema_7 =
  sequence_ [quickCheck prop_sum_map,
             quickCheck prop_int_bin,
             quickCheck prop_transmite]

-- Su verificación es
