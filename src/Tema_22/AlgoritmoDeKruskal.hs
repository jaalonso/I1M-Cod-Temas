-- AlgoritmoDeKruskal.hs
-- Algoritmo de Kruskal
-- ---------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tema_22.AlgoritmoDeKruskal where

-- ---------------------------------------------------------------------
-- Importaciones                                                      --
-- ---------------------------------------------------------------------

-- Nota: Seleccionar una implementación del TAD grafo.
import Tema_22.GrafoConVectorDeAdyacencia
-- import Tema_22.GrafoConMatrizDeAdyacencia
-- import I1M.Grafo

-- Elegir la 1ª si se ha elegido una del Tema 22 y actualizarla para
-- usar la misma implementación.
import Tema_22.RecorridoEnAnchura
-- import I1M.RecorridoEnAnchura

import qualified Data.Map as M
import Data.List
import Data.Ix
import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejemplos                                                           --
-- ---------------------------------------------------------------------

g1, g2, g3, g4 :: Grafo Int Int
g1 = creaGrafo ND (1,5) [(1,2,12),(1,3,34),(1,5,78),
                         (2,4,55),(2,5,32),
                         (3,4,61),(3,5,44),
                         (4,5,93)]
g2 = creaGrafo ND (1,5) [(1,2,13),(1,3,11),(1,5,78),
                         (2,4,12),(2,5,32),
                         (3,4,14),(3,5,44),
                         (4,5,93)]
g3 = creaGrafo ND (1,7) [(1,2,5),(1,3,9),(1,5,15),(1,6,6),
                         (2,3,7),
                         (3,4,8),(3,5,7),
                         (4,5,5),
                         (5,6,3),(5,7,9),
                         (6,7,11)]
g4 = creaGrafo ND (1,7) [(1,2,5),(1,3,9),(1,5,15),(1,6,6),
                         (2,3,7),
                         (3,4,8),(3,5,1),
                         (4,5,5),
                         (5,6,3),(5,7,9),
                         (6,7,11)]

-- ---------------------------------------------------------------------
-- Algoritmo de Kruskal                                               --
-- ---------------------------------------------------------------------

-- (kruskal g) es el árbol de expansión mínimo del grafo g calculado
-- mediante el algoritmo de Kruskal. Por ejemplo,
--    kruskal g1  ==  [(55,2,4),(34,1,3),(32,2,5),(12,1,2)]
--    kruskal g2  ==  [(32,2,5),(13,1,2),(12,2,4),(11,1,3)]
--    kruskal g3  ==  [(9,5,7),(7,2,3),(6,1,6),(5,4,5),(5,1,2),(3,5,6)]
--    kruskal g4  ==  [(9,5,7),(6,1,6),(5,4,5),(5,1,2),(3,5,6),(1,3,5)]
kruskal :: (Ix v, Num p, Ord p) => Grafo v p -> [(p,v,v)]
kruskal g = aux (sort [(p,x,y) | (x,y,p) <- aristas g])
                (M.fromList [(x,x) | x <- nodos g])
                []
                (length (nodos g) - 1)
  where aux _  _ ae 0 = ae
        aux [] _ _  _ = error "Imposible"
        aux ((p,x,y):as) d ae n
          | actualizado = aux as d' ((p,x,y):ae) (n-1)
          | otherwise   = aux as d ae n
          where (actualizado,d') = buscaActualiza (x,y) d

-- (raiz d n) es la raíz de n en el diccionario. Por ejemplo,
--    raiz (M.fromList [(1,1),(3,1),(4,3),(5,4),(2,6),(6,6)]) 5  == 1
--    raiz (M.fromList [(1,1),(3,1),(4,3),(5,4),(2,6),(6,6)]) 2  == 6
raiz :: (Eq n, Ord n) => M.Map n n -> n -> n
raiz d x | v == x    = v
         | otherwise = raiz d v
  where v = d M.! x

-- (buscaActualiza a d) es el par formado por False y el diccionario d,
-- si los dos vértices de la arista a tienen la misma raíz en d y el par
-- formado por True y la tabla obtenida añadiéndole a d la arista
-- formada por el vértice de a de mayor raíz y la raíz del vértice de a
-- de menor raíz. Y actualizando las raices de todos los elementos
-- afectados por la raíz añadida. Por ejemplo,
--   λ> d = M.fromList [(1,1),(2,1),(3,3),(4,4),(5,5),(6,5),(7,7)]
--   λ> buscaActualiza (5,4) d
--   (True,fromList [(1,1),(2,1),(3,3),(4,4),(5,4),(6,4),(7,7)])
--   λ> d' = snd it
--   λ> buscaActualiza (6,1) d'
--   (True,fromList [(1,1),(2,1),(3,3),(4,1),(5,1),(6,1),(7,7)])
buscaActualiza :: (Eq n, Ord n) => (n,n) -> M.Map n n -> (Bool,M.Map n n)
buscaActualiza (x,y) d
  | x' == y'  = (False, d)
  | y' <  x'  = (True, modificaR x (d M.! x) y' d)
  | otherwise = (True, modificaR y (d M.! y) x' d)
  where x' = raiz d x
        y' = raiz d y

-- (modificaR x y y' d) actualiza d como sigue:
-- + el valor de todas las claves z con valor y es y'
-- + el valor de todas las claves z con (z > x) con valor x es y'
modificaR :: (Eq n, Ord n) => n -> n -> n -> M.Map n n -> M.Map n n
modificaR x y y' d = aux2 ds (aux1 cs d)
  where cs = M.keys d
        ds = filter (>x) cs
        aux1 [] tb = tb
        aux1 (a:as) tb | tb M.! a == y = aux1 as (M.update (\_ -> Just y') a tb)
                       | otherwise     = aux1 as tb
        aux2 [] tb = tb
        aux2 (b:bs) tb | tb M.! b == x = aux2 bs (M.update (\_ -> Just y') b tb)
                       | otherwise     = aux2 bs tb

-- Traza del diccionario correspondiente al grafo g3
-- =================================================

-- Lista de aristas, ordenadas según su peso:
-- [(3,5,6),(5,1,2),(5,4,5),(6,1,6),(7,2,3),(7,3,5),(8,3,4),(9,1,3),(9,5,7),(11,6,7),(15,1,5)]
--
-- Inicial
--   fromList [(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7)]
--
-- Después de añadir la arista (5,6) de peso 3
--   fromList [(1,1),(2,2),(3,3),(4,4),(5,5),(6,5),(7,7)]
--
-- Después de añadir la arista (1,2) de peso 5
--   fromList [(1,1),(2,1),(3,3),(4,4),(5,5),(6,5),(7,7)]
--
-- Después de añadir la arista (4,5) de peso 5
--   fromList [(1,1),(2,1),(3,3),(4,4),(5,4),(6,4),(7,7)]
--
-- Después de añadir la arista (1,6) de peso 6
--   fromList [(1,1),(2,1),(3,3),(4,1),(5,1),(6,1),(7,7)]
--
-- Después de añadir la arista (2,3) de peso 7
--   fromList [(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,7)]
--
-- Las posibles aristas a añadir son:
--    (+) la (3,5) con peso 7, que no es posible pues la raíz de 3
--        coincide con la raíz de 5, por lo que formaría un ciclo
--    (+) la (3,4) con peso 8, que no es posible pues la raíz de 3
--        coincide con la raíz de 4, por lo que formaría un ciclo
--    (+) la (1,3) con peso 9, que no es posible pues la raíz de 3
--        coincide con la raíz de 1, por lo que formaría un ciclo
--    (+) la (5,7) con peso 9, que no forma ciclo
--
-- Después de añadir la arista (5,7) con peso 9
--    fromList [(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1)]
--
-- No es posible añadir más aristas, pues formarían ciclos.

-- ---------------------------------------------------------------------
-- El algoritmo de Prim                                                  --
-- ---------------------------------------------------------------------

-- (prim g) es el árbol de expansión mínimo del grafo g calculado
-- mediante el algoritmo de Prim. Por ejemplo,
--    prim g1  == [(55,2,4),(34,1,3),(32,2,5),(12,1,2)]
--    prim g2  == [(32,2,5),(12,2,4),(13,1,2),(11,1,3)]
--    prim g3  == [(9,5,7),(7,2,3),(5,5,4),(3,6,5),(6,1,6),(5,1,2)]
prim :: (Ix v, Num p, Ord p) => Grafo v p -> [(p,v,v)]
prim g = prim' [n]              -- Nodos colocados
               ns               -- Nodos por colocar
               []               -- Árbol de expansión
               (aristas g)      -- Aristas del grafo
  where
    (n:ns) = nodos g
    prim' _ _ _  []  = []
    prim' _ [] ae _  = ae
    prim' t r  ae as = prim' (v':t) (delete v' r) (e:ae) as
      where e@(_,_, v') = minimum [(c,u,v)| (u,v,c) <- as,
                                             u `elem` t,
                                             v `elem` r]

-- ---------------------------------------------------------------------
-- § Generador de grafos                                              --
-- ---------------------------------------------------------------------

-- (generaGND n ps) es el grafo completo de orden n tal que los pesos
-- están determinados por ps. Por ejemplo,
--    λ> generaGND 3 [4,2,5]
--    (ND,array (1,3) [(1,[(2,4),(3,2)]),
--                     (2,[(1,4),(3,5)]),
--                      3,[(1,2),(2,5)])])
--    λ> generaGND 3 [4,-2,5]
--    (ND,array (1,3) [(1,[(2,4)]),(2,[(1,4),(3,5)]),(3,[(2,5)])])
generaGND :: Int -> [Int] -> Grafo Int Int
generaGND n ps  = creaGrafo ND (1,n) l3
  where l1 = [(x,y) | x <- [1..n], y <- [1..n], x < y]
        l2 = zip l1 ps
        l3 = [(x,y,z) | ((x,y),z) <- l2, z > 0]

-- genGND es un generador de grafos dirigidos. Por ejemplo,
--    λ> sample genGND
--    (ND,array (1,1) [(1,[])])
--    (ND,array (1,3) [(1,[(2,3),(3,13)]),(2,[(1,3)]),(3,[(1,13)])])
--    ...
genGND :: Gen (Grafo Int Int)
genGND = do n <- choose (1,50)
            xs <- vectorOf (n*n) arbitrary
            return (generaGND n xs)

-- Los grafos está contenido en la clase de los objetos generables
-- aleatoriamente.
instance Arbitrary (Grafo Int Int) where
  arbitrary = genGND

-- ---------------------------------------------------------------------
-- § Verificación                                                     --
-- ---------------------------------------------------------------------

prop_AE :: Grafo Int Int -> Property
prop_AE g =
  not (null (aristas g)) && conexo g ==>
  nodosAE (kruskal g) == ns &&
  nodosAE (prim g) == ns &&
  pesoAE (kruskal g) == pesoAE (prim g)
  where ns = nodos g
        nodosAE ae = sort (nub (concat [[x,y] | (_,x,y) <- ae]))
        pesoAE xs = sum [p | (p,_,_) <- xs]

-- (conexo g) se verifica si el grafo ni dirigido g es conexo. Por
-- ejemplo,
--    conexo (creaGrafo False (1,3) [(1,2,0),(3,2,0)])  ==  True
--    conexo (creaGrafo False (1,4) [(1,2,0),(3,4,0)])  ==  False
conexo :: (Ix a, Num p, Eq p) => Grafo a p -> Bool
conexo g = length (recorridoEnAnchura i g) == n
    where xs = nodos g
          i  = head xs
          n  = length xs

-- La comprobación es
--    λ> quickCheck prop_AE
--    +++ OK, passed 100 tests.
