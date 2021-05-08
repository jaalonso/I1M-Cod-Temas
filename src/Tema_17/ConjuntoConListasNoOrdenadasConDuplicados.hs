-- ConjuntoConListasNoOrdenadasConDuplicados.hs
-- Implementación de conjuntos mediante listas no ordenadas con duplicados.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_17.ConjuntoConListasNoOrdenadasConDuplicados
  (Conj,
   vacio,          -- Conj a
   inserta,        -- Eq a => a -> Conj a -> Conj a
   elimina,        -- Eq a => a -> Conj a -> Conj a
   pertenece,      -- Eq a => a -> Conj a -> Bool
   esVacio,        -- Conj a -> Bool
   escribeConjunto -- Show a => Conj a -> String
  ) where

-- Conjuntos como listas no ordenadas con repeticiones:
newtype Conj a = Cj [a]

-- (escribeConjunto c) es la cadena correspondiente al conjunto c. Por
-- ejemplo,
--    λ> escribeConjunto (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])
--    "{2,5,1,3,7,5,3,2,1,9,0}"
escribeConjunto :: Show a => Conj a -> String
escribeConjunto (Cj [])     = "{}"
escribeConjunto (Cj [x])    = "{" ++ show x ++ "}"
escribeConjunto (Cj (x:xs)) = "{" ++ show x ++ aux xs
  where aux [] = "}"
        aux (y:ys) = "," ++ show y ++ aux ys

-- Procedimiento de escritura de conjuntos.
instance Show a => Show (Conj a) where
  show = escribeConjunto

-- Ejemplo de conjunto: El conjunto obtenido añadiéndole al conjunto
-- vacío los elementos 2, 5, 1, 3, 7, 5, 3, 2, 1, 9 y 0 es
--    λ> foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0]
--    {2,5,1,3,7,5,3,2,1,9,0}

-- vacio es el conjunto vacío. Por ejemplo,
--    λ> vacio
--    {}
vacio :: Conj a
vacio = Cj []

-- (inserta x c) es el conjunto obtenido añadiendo el elemento x al
-- conjunto c. Por ejemplo,
--    λ> inserta 5 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])
--    {5,2,5,1,3,7,5,3,2,1,9,0}
inserta :: Eq a => a -> Conj a -> Conj a
inserta x (Cj ys) = Cj (x:ys)

-- (elimina x c) es el conjunto obtenido eliminando el elemento x
-- del conjunto c. Por ejemplo,
--    λ> elimina 3 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])
--    {2,5,1,7,5,2,1,9,0}
elimina :: Eq a => a -> Conj a -> Conj a
elimina x (Cj ys) = Cj (filter (/= x) ys)

-- (pertenece x c) se verifica si x pertenece al conjunto c. Por ejemplo,
--    pertenece 3 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])  ==  True
--    pertenece 4 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])  ==  False
pertenece :: Eq a => a -> Conj a -> Bool
pertenece x (Cj xs) = x `elem` xs

-- (esVacio c) se verifica si c es el conjunto vacío. Por ejemplo,
--    esVacio (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0]) == False
--    esVacio vacio == True
esVacio :: Conj a -> Bool
esVacio (Cj xs) = null xs

-- (subconjunto c1 c2) se verifica si c1 es un subconjunto de c2. Por
-- ejemplo,
--    subconjunto (Cj [1,3,2,1]) (Cj [3,1,3,2])  ==  True
--    subconjunto (Cj [1,3,4,1]) (Cj [3,1,3,2])  ==  False
subconjunto :: Eq a => Conj a -> Conj a -> Bool
subconjunto (Cj xs) (Cj ys) = sublista xs ys
  where sublista [] _      = True
        sublista (z:zs) vs = elem z vs && sublista zs vs

-- (igualConjunto c1 c2) se verifica si los conjuntos c1 y c2 son
-- iguales. Por ejemplo,
--    igualConjunto (Cj [1,3,2,1]) (Cj [3,1,3,2])  ==  True
--    igualConjunto (Cj [1,3,4,1]) (Cj [3,1,3,2])  ==  False
igualConjunto :: Eq a => Conj a -> Conj a -> Bool
igualConjunto c c' =
  subconjunto c c' && subconjunto c' c

--- Los conjuntos son comparables por igualdad.
instance Eq a => Eq (Conj a) where
  (==) = igualConjunto
