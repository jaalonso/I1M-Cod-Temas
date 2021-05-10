-- ConjuntoConListasOrdenadasSinDuplicados.hs
-- Implementación de conjuntos mediante listas ordenadas sin repeticiones.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_17.ConjuntoConListasOrdenadasSinDuplicados
  (Conj,
   vacio,         -- Conj a
   esVacio,       -- Conj a -> Bool
   pertenece,     -- Ord a => a -> Conj a -> Bool
   inserta,       -- Ord a => a -> Conj a -> Conj a
   elimina,       -- Ord a => a -> Conj a -> Conj a
   escribeConjunto -- Show a => Conj a -> String
  ) where

-- Los conjuntos como listas ordenadas sin repeticiones.
newtype Conj a = Cj [a]
  deriving Eq

-- (escribeConjunto c) es la cadena correspondiente al conjunto c. Por
-- ejemplo,
--    λ> escribeConjunto (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])
--    "{0,1,2,3,5,7,9}"
escribeConjunto :: Show a => Conj a -> String
escribeConjunto (Cj [])     = "{}"
escribeConjunto (Cj [x])    = "{" ++ show x ++ "}"
escribeConjunto (Cj (x:xs)) = "{" ++ show x ++ aux xs
  where aux []     = "}"
        aux (y:ys) = "," ++ show y ++ aux ys

-- Procedimiento de escritura de conjuntos.
instance Show a => Show (Conj a) where
  show = escribeConjunto

-- Ejemplo de conjunto: El conjunto obtenido añadiéndole al conjunto
-- vacío los elementos 2, 5, 1, 3, 7, 5, 3, 2, 1, 9 y 0 es
--    λ> foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0]
--    {0,1,2,3,5,7,9}

-- vacio es el conjunto vacío. Por ejemplo,
--    λ> vacio
--    {}
vacio :: Conj a
vacio = Cj []

-- (esVacio c) se verifica si c es el conjunto vacío. Por ejemplo,
--    esVacio (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0]) ==  False
--    esVacio vacio  ==  True
esVacio :: Conj a -> Bool
esVacio (Cj xs) = null xs

-- (pertenece x c) se verifica si x pertenece al conjunto c. Por ejemplo,
--    pertenece 3 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])  ==  True
--    pertenece 4 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])  ==  False
pertenece :: Ord a => a -> Conj a -> Bool
pertenece x (Cj s) = x `elem` takeWhile (<= x) s

-- (inserta x c) es el conjunto obtenido añadiendo el elemento x al
-- conjunto c. Por ejemplo,
--    λ> inserta 5 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])
--    {0,1,2,3,5,7,9}
--    λ> inserta 4 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])
--    {0,1,2,3,4,5,7,9}
inserta :: Ord a => a -> Conj a -> Conj a
inserta x (Cj s) = Cj (agrega x s)
  where agrega z []                    = [z]
        agrega z s'@(y:ys) | z > y      = y : agrega z ys
                           | z < y      = z : s'
                           | otherwise  = s'

-- (elimina x c) es el conjunto obtenido eliminando el elemento x
-- del conjunto c. Por ejemplo,
--    λ> elimina 3 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])
--    {0,1,2,5,7,9}
--    λ> elimina 4 (foldr inserta vacio [2,5,1,3,7,5,3,2,1,9,0])
--    {0,1,2,3,5,7,9}
elimina :: Ord a => a -> Conj a -> Conj a
elimina x (Cj s) = Cj (elimina' x s)
  where elimina' _ []                    = []
        elimina' z s'@(y:ys) | z > y     = y : elimina' z ys
                             | z < y     = s'
                             | otherwise = ys
