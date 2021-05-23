-- GrafoConVectorDeAdyacencia.hs
-- Representación del TAD grafo mediante vectores de adyacencia.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Tema_22.GrafoConVectorDeAdyacencia
  (Orientacion (..),
   Grafo,
   creaGrafo,  -- (Ix v,Num p) => Orientacion -> (v,v) -> [(v,v,p)] ->
               --                 Grafo v p
   dirigido,   -- (Ix v,Num p) => (Grafo v p) -> Bool
   adyacentes, -- (Ix v,Num p) => (Grafo v p) -> v -> [v]
   nodos,      -- (Ix v,Num p) => (Grafo v p) -> [v]
   aristas,    -- (Ix v,Num p) => (Grafo v p) -> [(v,v,p)]
   aristaEn,   -- (Ix v,Num p) => (Grafo v p) -> (v,v) -> Bool
   peso        -- (Ix v,Num p) => v -> v -> (Grafo v p) -> p
  ) where

-- Librería auxiliar
import Data.Array

-- Orientacion es D (dirigida) ó ND (no dirigida).
data Orientacion = D | ND
  deriving (Eq, Show)

-- (Grafo v p) es un grafo con vértices de tipo v y pesos de tipo p.
data Grafo v p = G Orientacion (Array v [(v,p)])
  deriving (Eq, Show)

-- (creaGrafo o cs as) es un grafo (dirigido o no, según el valor de o),
-- con el par de cotas cs y listas de aristas as (cada arista es un trío
-- formado por los dos vértices y su peso). Ver el ejemplo a continuación.
creaGrafo :: (Ix v, Num p) => Orientacion -> (v,v) -> [(v,v,p)] -> Grafo v p
creaGrafo o cs vs =
  G o (accumArray (\xs x -> xs++[x]) [] cs
                  ((if o == D then []
                    else [(x2,(x1,p))|(x1,x2,p) <- vs, x1 /= x2]) ++
                   [(x1,(x2,p)) | (x1,x2,p) <- vs]))

-- ejGrafoND es el grafo
--             12
--        1 -------- 2
--        | \78     /|
--        |  \   32/ |
--        |   \   /  |
--      34|     5    |55
--        |   /   \  |
--        |  /44   \ |
--        | /     93\|
--        3 -------- 4
--             61
-- representado mediante un vector de adyacencia; es decir,
--    λ> ejGrafoND
--    G ND array (1,5) [(1,[(2,12),(3,34),(5,78)]),
--                      (2,[(1,12),(4,55),(5,32)]),
--                      (3,[(1,34),(4,61),(5,44)]),
--                      (4,[(2,55),(3,61),(5,93)]),
--                      (5,[(1,78),(2,32),(3,44),(4,93)])])
ejGrafoND :: Grafo Int Int
ejGrafoND = creaGrafo ND (1,5) [(1,2,12),(1,3,34),(1,5,78),
                                (2,4,55),(2,5,32),
                                (3,4,61),(3,5,44),
                                (4,5,93)]

-- ejGrafoD es el mismo grafo que ejGrafoND pero orientando las aristas;
-- es decir,
--    λ> ejGrafoD
--    G D array (1,5) [(1,[(2,12),(3,34),(5,78)]),
--                     (2,[(4,55),(5,32)]),
--                     (3,[(4,61),(5,44)]),
--                     (4,[(5,93)]),
--                     (5,[])])
ejGrafoD :: Grafo Int Int
ejGrafoD = creaGrafo D (1,5) [(1,2,12),(1,3,34),(1,5,78),
                              (2,4,55),(2,5,32),
                              (3,4,61),(3,5,44),
                              (4,5,93)]

-- (dirigido g) se verifica si g es dirigido. Por ejemplo,
--    dirigido ejGrafoD   ==  True
--    dirigido ejGrafoND  ==  False
dirigido :: (Ix v,Num p) => Grafo v p -> Bool
dirigido (G o _) = o == D

-- (nodos g) es la lista de todos los nodos del grafo g. Por ejemplo,
--    nodos ejGrafoND  ==  [1,2,3,4,5]
--    nodos ejGrafoD   ==  [1,2,3,4,5]
nodos :: (Ix v,Num p) => Grafo v p -> [v]
nodos (G _ g) = indices g

-- (adyacentes g v) es la lista de los vértices adyacentes al nodo v en
-- el grafo g. Por ejemplo,
--    adyacentes ejGrafoND 4  ==  [2,3,5]
--    adyacentes ejGrafoD  4  ==  [5]
adyacentes :: (Ix v,Num p) => Grafo v p -> v -> [v]
adyacentes (G _ g) v = map fst (g!v)

-- (aristaEn g a) se verifica si a es una arista del grafo g. Por
-- ejemplo,
--    aristaEn ejGrafoND (5,1)  ==  True
--    aristaEn ejGrafoND (4,1)  ==  False
--    aristaEn ejGrafoD (5,1)   ==  False
--    aristaEn ejGrafoD (1,5)   ==  True
aristaEn :: (Ix v,Num p) => Grafo v p -> (v,v) -> Bool
aristaEn g (x,y) = y `elem` adyacentes g x

-- (peso v1 v2 g) es el peso de la arista que une los vértices v1 y v2
-- en el grafo g. Por ejemplo,
--    peso 1 5 ejGrafoND  ==  78
--    peso 1 5 ejGrafoD   ==  78
peso :: (Ix v,Num p) => v -> v -> Grafo v p -> p
peso x y (G _ g) = head [c | (a,c) <- g!x , a == y]

-- (aristas g) es la lista de las aristas del grafo g. Por ejemplo,
--    λ> aristas ejGrafoD
--    [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),
--     (3,5,44),(4,5,93)]
--    λ> aristas ejGrafoND
--    [(1,2,12),(1,3,34),(1,5,78),(2,1,12),(2,4,55),(2,5,32),
--     (3,1,34),(3,4,61),(3,5,44),(4,2,55),(4,3,61),(4,5,93),
--     (5,1,78),(5,2,32),(5,3,44),(5,4,93)]
aristas :: (Ix v,Num p) => Grafo v p -> [(v,v,p)]
aristas (G o g) = [(v1,v2,w) | v1 <- nodos (G o g) , (v2,w) <- g!v1]
