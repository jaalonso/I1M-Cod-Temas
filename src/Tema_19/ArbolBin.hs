-- ArbolBin.hs
-- TAD de árboles binarios de búsqueda e implementación.
-- Tablas mediante matrices.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

-- Un árbol binario de búsqueda (ABB) es un árbol binario tal que el
-- valor de cada nodo es mayor que los valores de su subárbol izquierdo
-- y es menor que los valores de su subárbol derecho y, además, ambos
-- subárboles son árboles binarios de búsqueda. Por ejemplo, al
-- almacenar los valores de [2,3,4,5,6,8,9] en un ABB se puede obtener
-- los siguientes ABB:
--
--       5                     5
--      / \                   / \
--     /   \                 /   \
--    2     6               3     8
--     \     \             / \   / \
--      4     8           2   4 6   9
--     /       \
--    3         9
--
-- El objetivo principal de los ABB es reducir el tiempo de acceso a los
-- valores.

module Tema_19.ArbolBin
  (ABB,
   vacio,      -- ABB
   inserta,    -- (Ord a, Show a) => a -> ABB a -> ABB a
   elimina,    -- (Ord a, Show a) => a -> ABB a -> ABB a
   crea,       -- (Ord a, Show a) => [a] -> ABB a
   crea',      -- (Ord a, Show a) => [a] -> ABB a
   menor,      -- Ord a => ABB a -> a
   elementos,  -- (Ord a, Show a) => ABB a -> [a]
   pertenece,  -- (Ord a, Show a) => a -> ABB a -> Bool
   valido,     -- (Ord a, Show a) => ABB a -> Bool
   escribeABB, -- Show a => ABB a -> String
   ejemploABB  -- Int -> ABB Int
  ) where

-- Los ABB como tipo de dato algebraico.
data ABB a = Vacio
           | Nodo a (ABB a) (ABB a)
  deriving Eq

-- (escribeABB a) es la cadena correspondiente a la pila a. Por
-- ejemplo,
--    λ> escribeABB (crea (reverse [5,2,6,4,8,3,9]))
--    " (5 (2 - (4 (3 - -) -)) (6 - (8 - (9 - -))))"
--    λ> escribeABB (foldr inserta vacio (reverse [5,2,4,3,8,6,7,10,9,11]))
--    " (5 (2 - (4 (3 - -) -)) (8 (6 - (7 - -)) (10 (9 - -) (11 - -))))"
escribeABB :: Show a => ABB a -> String
escribeABB Vacio        = " -"
escribeABB (Nodo x i d) = " (" ++ show x ++ escribeABB i ++ escribeABB d ++ ")"

-- Procedimiento de escritura de pilas.
instance Show a => Show (ABB a) where
  show = escribeABB

-- Ejemplos de ABB
--    λ> ejemploABB 1
--     (5 (2 - (4 (3 - -) -)) (6 - (8 - (9 - -))))
--    λ> ejemploABB 2
--     (5 (2 - (4 (3 - -) -)) (8 (6 - (7 - -)) (10 (9 - -) (11 - -))))
ejemploABB :: Int -> ABB Int
ejemploABB 1 = crea (reverse [5,2,6,4,8,3,9])
ejemploABB 2 = foldr inserta vacio (reverse [5,2,4,3,8,6,7,10,9,11])
ejemploABB _ = error "No definido"

-- vacio es el ABB vacío. Por ejemplo,
--    λ> vacio
--     -
vacio :: ABB a
vacio = Vacio

-- (pertenece v' a) se verifica si v' es el valor de algún nodo del ABB
-- a. Por ejemplo,
--   pertenece 3 (ejemploABB 1)  ==  True
--   pertenece 7 (ejemploABB 1)  ==  False
pertenece :: (Ord a,Show a) => a -> ABB a -> Bool
pertenece _ Vacio         = False
pertenece v' (Nodo v i d) | v == v'   = True
                          | v' < v    = pertenece v' i
                          | otherwise = pertenece v' d

-- pertenece requiere O(n) paso en el peor caso O(n) y O(log n) en el mejor,
-- donde n es el número de nodos del ABB.

-- (inserta v a) es el árbol obtenido añadiendo el valor v al ABB a, si
-- no es uno de sus valores. Por ejemplo,
--    λ> inserta 7 (ejemploABB 1)
--     (5 (2 - (4 (3 - -) -)) (6 - (8 (7 - -) (9 - -))))
inserta :: (Ord a,Show a) => a -> ABB a -> ABB a
inserta v' Vacio        = Nodo v' Vacio Vacio
inserta v' (Nodo v i d) | v' == v   = Nodo v i d
                        | v' < v    = Nodo v (inserta v' i) d
                        | otherwise = Nodo v i (inserta v' d)

-- (crea vs) es el ABB cuyos valores son vs. Por ejemplo,
--    λ> crea [3,7,2]
--     (2 - (7 (3 - -) -))
crea :: (Ord a,Show a) => [a] -> ABB a
crea = foldr inserta Vacio

-- (crea' vs) es el ABB de menor profundidad cuyos valores son los de
-- la lista ordenada vs. Por ejemplo,
--    λ> crea' [2,3,7]
--     (3 (2 - -) (7 - -))
crea' :: (Ord a,Show a) => [a] -> ABB a
crea' [] = Vacio
crea' vs = Nodo x (crea' l1) (crea' l2)
  where n      = length vs `div` 2
        l1     = take n vs
        (x:l2) = drop n vs

-- (elementos a) es la lista de los valores de los nodos del ABB en el
-- recorrido inorden. Por ejemplo,
--   elementos (ejemploABB 1)  ==  [2,3,4,5,6,8,9]
--   elementos (ejemploABB 2)  ==  [2,3,4,5,6,7,8,9,10,11]
elementos :: (Ord a,Show a) => ABB a -> [a]
elementos Vacio        = []
elementos (Nodo v i d) = elementos i ++ [v] ++ elementos d

-- (elimina v a) es el ABB obtenido borrando el valor v del ABB a. Por
-- ejemplo,
--    λ> (ejemploABB 1)
--     (5 (2 - (4 (3 - -) -)) (6 - (8 - (9 - -))))
--    λ> elimina 3 (ejemploABB 1)
--     (5 (2 - (4 - -)) (6 - (8 - (9 - -))))
--    λ> elimina 2 (ejemploABB 1)
--     (5 (4 (3 - -) -) (6 - (8 - (9 - -))))
--    λ> elimina 5 (ejemploABB 1)
--     (6 (2 - (4 (3 - -) -)) (8 - (9 - -)))
--    λ> elimina 7 (ejemploABB 1)
--     (5 (2 - (4 (3 - -) -)) (6 - (8 - (9 - -))))
elimina  :: (Ord a,Show a) => a -> ABB a -> ABB a
elimina _  Vacio = Vacio
elimina v' (Nodo v i Vacio) | v'==v = i
elimina v' (Nodo v Vacio d) | v'==v = d
elimina v' (Nodo v i d)     | v' < v    = Nodo v (elimina v' i) d
                            | v' > v    = Nodo v i (elimina v' d)
                            | otherwise = Nodo k i (elimina k d)
  where k = menor d

-- (menor a) es el mínimo valor del ABB a. Por ejemplo,
--   menor (ejemploABB 1)  ==  2
menor :: Ord a => ABB a -> a
menor (Nodo v Vacio _) = v
menor (Nodo _ i _)     = menor i
menor Vacio            = error "Imposible"

-- (menorTodos v a) se verifica si v es menor que todos los elementos
-- del ABB a.
menorTodos :: (Ord a, Show a) => a -> ABB a -> Bool
menorTodos _ Vacio = True
menorTodos v a        = v < minimum (elementos a)

-- (mayorTodos v a) se verifica si v es mayor que todos los elementos
-- del ABB a.
mayorTodos :: (Ord a, Show a) => a -> ABB a -> Bool
mayorTodos _ Vacio = True
mayorTodos v a     = v > maximum (elementos a)

-- (valido a) se verifica si a es un ABB correcto. Por ejemplo,
--    valido (ejemploABB 1) == True
valido :: (Ord a, Show a) => ABB a -> Bool
valido Vacio        = True
valido (Nodo v a b) = mayorTodos v a &&
                      menorTodos v b &&
                      valido a &&
                      valido b
