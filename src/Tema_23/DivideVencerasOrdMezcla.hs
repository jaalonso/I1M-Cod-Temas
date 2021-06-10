-- DivideVencerasOrdMezcla.hs
-- Divide y vencerás. ordenación por mezcla.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_23.DivideVencerasOrdMezcla  where

-- Hay que elegir una implementación
import Tema_23.DivideVenceras
-- import I1M.DivideVenceras

-- (ordenaPorMezcla xs) es la lista obtenida ordenando xs por el
-- procedimiento de ordenación por mezcla. Por ejemplo,
--    λ> ordenaPorMezcla [3,1,4,1,5,9,2,8]
--    [1,1,2,3,4,5,8,9]
ordenaPorMezcla :: Ord a => [a] -> [a]
ordenaPorMezcla = divideVenceras ind id divide combina
  where
    ind xs            = length xs <= 1
    divide xs         = [take n xs, drop n xs]
      where n = length xs `div` 2
    combina _ [l1,l2] = mezcla l1 l2
    combina _ _       = error "Imposible"

-- (mezcla xs ys) es la lista obtenida mezclando xs e ys. Por ejemplo,
--    mezcla [1,3] [2,4,6]  ==  [1,2,3,4,6]
mezcla :: Ord a => [a] -> [a] -> [a]
mezcla [] b = b
mezcla a [] = a
mezcla a@(x:xs) b@(y:ys)
  | x <= y    = x : mezcla xs b
  | otherwise = y : mezcla a ys
