-- TablaConMatrices.hs
-- Tablas mediante matrices.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_18.TablaConMatrices
  (Tabla,
   tabla,     -- Eq i => [(i,v)] -> Tabla i v
   valor,     -- Eq i => Tabla i v -> i -> v
   modifica,  -- Eq i => (i,v) -> Tabla i v -> Tabla i v
   tieneValor -- Ix i => Tabla i v -> i -> Bool
  ) where

import Data.Array (Array, Ix, array, (//), (!), indices, bounds, inRange)

-- Las tablas como matrices.
newtype Tabla i v = Tbl (Array i v)
  deriving (Show, Eq)

-- Ejemplos de tablas:
--    λ> f x = if x < 3 then x else 3-x
--    λ> t1 = tabla [(i,f i) | i <- [1..6]]
--    λ> t1
--    Tbl (array (1,6) [(1,1),(2,2),(3,0),(4,-1),(5,-2),(6,-3)])
--    λ> t2 = tabla [(1,5),(2,4),(3,7)]
--    λ> t2

-- (tabla ivs) es la tabla correspondiente a la lista de asociación
-- ivs (que es una lista de pares formados por los índices y los
-- valores). Por ejemplo,
--    λ> tabla [(1,5),(3,7),(2,4)]
--    Tbl (array (1,3) [(1,5),(2,4),(3,7)])
tabla :: Ix i => [(i,v)] -> Tabla i v
tabla ivs = Tbl (array (m,n) ivs)
  where is = [i | (i,_) <- ivs]
        m  = minimum is
        n  = maximum is

-- (valor t i) es el valor del índice i en la tabla t. Por ejemplo,
--    λ> f x = if x < 3 then x else 3-x
--    λ> t1 = tabla [(i,f i) | i <- [1..6]]
--    λ> valor t1 6
--    -3
--    λ> valor t2 2
--    4
--    λ> valor t2 5
--    *** Exception: Ix{Integer}.index: Index (5) out of range ((1,3))
valor :: Ix i => Tabla i v -> i -> v
valor (Tbl t) i = t ! i

-- (modifica (i,x) t) es la tabla obtenida modificando en la tabla t el
-- valor de i por x. Por ejemplo,
--    λ> f x = if x < 3 then x else 3-x
--    λ> t1 = tabla [(i,f i) | i <- [1..6]]
--    λ> valor t1 6
--    -3
--    λ> valor (modifica (6,9) t1) 6
--    9
modifica :: Ix i => (i,v) -> Tabla i v -> Tabla i v
modifica (i,v) (Tbl t) | i `elem` indices t = Tbl (t // [(i,v)])
                       | otherwise          = Tbl t

-- (cotas t) son las cotas de la tabla t. Por ejemplo,
--    λ> t2 = tabla [(4,89), (1,90), (2,67)]
--    λ> cotas t2
--    (1,3)
cotas :: Ix i => Tabla i v -> (i,i)
cotas (Tbl t) = bounds t

-- (tieneValor t x) se verifica si x es una clave de la tabla t. Por ejemplo,
--    λ> t2 = tabla [(4,89), (1,90), (2,67)]
--    λ> tieneValor t2 3
--    True
--    λ> tieneValor t2 4
--    False
tieneValor :: Ix i => Tabla i v -> i -> Bool
tieneValor t = inRange (cotas t)
