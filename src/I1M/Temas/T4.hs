-- I1M T4.hs
-- Tema 4: Definición de funciones.
-- José A. Alonso Jiménez <jalonso@us,es>
-- =====================================================================

module I1M.Temas.T4 where

import Prelude 
    hiding ( (&&)
           , abs
           , const
           , even
           , fst
           , head
           , not
           , null
           , pred
           , signum
           , snd
           , splitAt
           , tail)

-- ---------------------------------------------------------------------
-- Definiciones por composición                                       --
-- ---------------------------------------------------------------------

-- (isDigit c) se verifica si c es un dígito. Por ejemplo.
--    isDigit '3'  ==  True
--    isDigit 'c'  ==  False
isDigit :: Char -> Bool
isDigit c = (c >= '0') && (c <= '9')

-- (even n) se verifica si n es par. Por ejemplo,
--    even 6  ==  True
--    even 7  ==  False
even :: (Integral a) => a -> Bool
even n =  n `rem` 2 == 0

-- (splitAt n xs) es el par formado por los n primeros elementos de xs y
-- los restantes elementos. Por ejemplo,
--    splitAt 2 [3,5,7,9,4]  ==  ([3,5],[7,9,4])
splitAt :: Int -> [a] -> ([a],[a])
splitAt n xs = (take n xs, drop n xs)  

-- ---------------------------------------------------------------------
-- Definiciones con condicionales                                     --
-- ---------------------------------------------------------------------

-- (abs x) es el valor absoluto de x. Por ejemplo,
--    abs (-5)  ==  5
abs :: Int -> Int
abs n = if n >= 0 then n else -n  

-- (signum x) es el signo de x. Por ejemplo,
--    signum (-5)  ==  -1
--    signum 0     ==  0
--    signum 7     ==  1
signum :: Int -> Int
signum n = if n < 0 then (-1) else 
              if n == 0 then 0 else 1  

-- ---------------------------------------------------------------------
-- Definiciones con ecuaciones con guardas                            --
-- ---------------------------------------------------------------------

-- (abs' x) es el valor absoluto de x. Por ejemplo,
--    abs' (-5)  ==  5
abs' n | n >= 0    = n
       | otherwise = -n

-- (signum' x) es el signo de x. Por ejemplo,
--    signum' (-5)  ==  -1
--    signum' 0     ==  0
--    signum' 7     ==  1
signum' n | n < 0     = -1
          | n == 0    = 0
          | otherwise = 1

-- ---------------------------------------------------------------------
-- Definiciones con equiparación de patrones                          --
-- ---------------------------------------------------------------------

-- (not x) es la negación de x.
not :: Bool -> Bool
not True  =  False
not False =  True

-- (x && y) es la conjunción de x e y.
(&&)  :: Bool -> Bool -> Bool
True  && True  = True
True  && False = False
False && True  = False
False && False = False

-- (x &&1 y) es la conjunción de x e y.
(&&&) :: Bool -> Bool -> Bool
True  &&& True =  True
_     &&& _    =  False

-- (x &&&& y) es la conjunción de x e y.
(&&&&) :: Bool -> Bool -> Bool
True  &&&& x =  x
False &&&& _ =  False

-- (fst p) es la primera componente del par p. Por ejemplo,
--    fst (5,3)  ==  5
fst :: (a,b) -> a
fst (x,_) = x

-- (snd p) es la segunda componente del par p. Por ejemplo,
--    snd (5,3)  ==  3
snd :: (a,b) -> b
snd (_,y) = y  

-- (test1 xs) se verifica si xs es una lista de 3 caracteres que empieza
-- por 'a'.  
test1 :: [Char ] -> Bool
test1 ['a',_,_] = True
test1 _         = False

-- (test2 xs) se verifica si  es una lista de caracteres que empieza por
-- 'a'.   
test2 :: [Char ] -> Bool
test2 ('a':_) = True
test2 _       = False

-- (null xs) se verifica si xs es la lista vacía. Por ejemplo,
--    null []     ==  True
--    null [3,2]  ==  False
null :: [a] -> Bool
null []    = True
null (_:_) = False

-- (head xs) es el primer elemento de xs. Por ejemplo.
--    head [3,2,5]  ==  3
head :: [a] -> a
head (x:_) =  x

-- (tail xs) es el resto de xs. Por ejemplo,
--    tail [3,2,5]  ==  [2,5]
tail :: [a] -> [a]
tail (_:xs) = xs

-- (pred n) es el predecesor de n. Por ejemplo,
--    pred 5  ==  4
pred :: Int -> Int
pred 0 = 0
pred n = n-1

-- ---------------------------------------------------------------------
-- Expresiones lambda                                                 --
-- ---------------------------------------------------------------------

-- (suma x y) es la suma de x e y. Por ejemplo,
--    suma 2 3  ==  5
suma x y = x+y  

-- (suma' x y) es la suma de x e y. Por ejemplo,
--    suma' 2 3  ==  5
suma' = \x -> (\y -> x+y)  

-- (const x y) es x. Por ejemplo,
--    const 2 3  ==  2
--    const 2 7  ==  2
const :: a -> b -> a
const x y = x

-- (const' x y) es x. Por ejemplo,
--    const' 2 3  ==  2
--    const' 2 7  ==  2
const' :: a -> (b -> a)
const' x = \_ -> x

-- (impares n) es la lista de los primeros nÃºmeros impares. Por ejemplo,
--    impares 4  ==  [1,3,5,7]
impares n = map f [0..n-1]
    where f x = 2*x+1

-- (impares' n) es la lista de los primeros nÃºmeros impares. Por ejemplo,
--    impares' 4  ==  [1,3,5,7]
impares' n = map (\x -> 2*x+1) [0..n-1]

-- ---------------------------------------------------------------------
-- Secciones                                                          --
-- ---------------------------------------------------------------------

-- (suma'' x y) es la suma de x e y. Por ejemplo,
--    suma'' 2 3  ==  5
suma'' = (+)

-- (siguiente x) es el siguiente de x. Por ejemplo,
--    siguiente 3  ==  4
siguiente = (1+)

-- (inverso x) es el inverso de x. Por ejemplo,
--    inverso 5  ==  0.2
inverso = (1/)

-- (doble x) es el doble de x. Por ejemplo, 
--    doble 3  ==  6 
doble = (2*)

-- (mitad x) es la mitad de x. Por ejemplo,
--    mitad 6  ==  3.0
mitad = (/2)  
