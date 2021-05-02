-- Tema_2.hs
-- Tema 2: Introducción a la programación con Haskell
-- José A. Alonso Jiménez https://jaalonso.github.io
-- =====================================================================

module Tema_2 where

-- (doble x) es el doble de x. Por ejemplo,
--    doble 3 == 6
doble :: Integer -> Integer
doble x = x+x

-- (cuadruple x) es el cuadruple de x. Por ejemplo,
--    cuadruple 3 == 12
cuadruple :: Integer -> Integer
cuadruple x = doble (doble x)

-- (factorial x) es el factorial de x. Por ejemplo,
--    factorial 4 == 24
factorial :: Integer -> Integer
factorial n = product [1..n]

-- (media ns) es la media aritmética de la lista de números ns. Por
-- ejemplo,
--    media [1,5.0,3]  ==  3
media :: [Float] -> Float
media ns = sum ns / fromIntegral (length ns)
