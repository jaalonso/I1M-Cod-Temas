-- TablaConListasDeAsociacion.hs
-- Tablas mediante listas de asociación.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_18.TablaConListasDeAsociacion
  (Tabla,
   tabla,   -- Eq i => [(i,v)] -> Tabla i v
   valor,   -- Eq i => Tabla i v -> i -> v
   modifica -- Eq i => (i,v) -> Tabla i v -> Tabla i v
  ) where

-- Las tablas como listas de asociación.
newtype Tabla i v = Tbl [(i,v)]
  deriving Show

-- Ejemplos de tabla:
--    λ> f x = if x < 3 then x else 3-x
--    λ> t1 = tabla [(i,f i) | i <- [1..6]]
--    λ> t1
--    Tbl [(1,1),(2,2),(3,0),(4,-1),(5,-2),(6,-3)]
--    λ> t2 = tabla [(4,89), (1,90), (2,67)]
--    λ> t2
--    Tbl [(4,89),(1,90),(2,67)]

-- (tabla ivs) es la tabla correspondiente a la lista de asociación
-- ivs (que es una lista de pares formados por los índices y los
-- valores). Por ejemplo,
--    λ> tabla [(4,89), (1,90), (2,67)]
--    Tbl [(4,89),(1,90),(2,67)]
tabla :: Eq i => [(i,v)] -> Tabla i v
tabla ivs = Tbl ivs

-- (valor t i) es el valor del índice i en la tabla t. Por ejemplo,
--    λ> f x = if x < 3 then x else 3-x
--    λ> t1 = tabla [(i,f i) | i <- [1..6]]
--    λ> valor t1 6
--    -3
--    λ> t2 = tabla [(4,89), (1,90), (2,67)]
--    λ> valor t2 2
--    67
--    λ> valor t2 5
--    *** Exception: fuera de rango
valor :: Eq i => Tabla i v -> i -> v
valor (Tbl []) _        = error "fuera de rango"
valor (Tbl ((j,v):r)) i | i == j    = v
                        | otherwise = valor (Tbl r) i

-- (modifica (i,x) t) es la tabla obtenida modificando en la tabla t el
-- valor de i por x. Por ejemplo,
--    λ> f x = if x < 3 then x else 3-x
--    λ> t1 = tabla [(i,f i) | i <- [1..6]]
--    λ> valor t1 6
--    -3
--    λ> valor (modifica (6,9) t1) 6
--    9
modifica :: Eq i => (i,v) -> Tabla i v -> Tabla i v
modifica p (Tbl [])                 = (Tbl [p])
modifica p'@(i,_) (Tbl (p@(j,_):r)) | i == j     = Tbl (p':r)
                                    | otherwise  = Tbl (p:r')
  where Tbl r' = modifica p' (Tbl r)

-- ---------------------------------------------------------------------
-- Igualdad                                                           --
-- ---------------------------------------------------------------------

-- Las tablas son comparables por igualdad. Por ejemplo,
--    λ> f x = if x < 3 then x else 3-x
--    λ> t1 = tabla [(i,f i) | i <- [1..6]]
--    λ> t2 = tabla [(4,89), (1,90), (2,67)]
--    λ> t1 == t2
--    False
--    λ> t3 = tabla [(1,1),(3,0),(2,2),(4,-1),(5,-2),(6,-3)]
--    λ> t1 == t3
--    True
instance (Eq i, Eq v) => Eq (Tabla i v) where
  (Tbl [])        == (Tbl []) = True
  (Tbl ((i,v):t)) == Tbl t'   = elem (i,v) t' &&
                                Tbl t == Tbl [p | p <- t', p /= (i,v)]
  _               == _        = False
