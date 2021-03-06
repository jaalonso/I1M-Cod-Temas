-- TablaConFunciones.hs
-- Tablas como funciones.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_18.TablaConFunciones
  (Tabla,
   tabla,   -- Eq i => [(i,v)] -> Tabla i v
   valor,   -- Eq i => Tabla i v -> i -> v
   modifica -- Eq i => (i,v) -> Tabla i v -> Tabla i v
  ) where

-- Las tablas como funciones.
newtype Tabla i v = Tbl (i -> v)

-- Procedimiento de escritura.
instance Show (Tabla i v) where
  showsPrec _ _ = showString "<<Una tabla>>"

-- Ejemplos de tablas:
--    λ> f x = if x < 3 then x else 3-x
--    λ> t1 = tabla [(i,f i) | i <- [1..6] ]
--    λ> t1
--    <<Una tabla>>

-- (valor t i) es el valor del índice i en la tabla t. Por ejemplo,
--    λ> f x = if x < 3 then x else 3-x
--    λ> t1 = tabla [(i,f i) | i <- [1..6] ]
--    λ> valor t1 6
--    -3
--    λ> t2 = tabla [(4,89), (1,90), (2,67)]
--    λ> valor t2 2
--    67
--    λ> valor t2 5
--    *** Exception: fuera de rango
valor :: Eq i => Tabla i v -> i -> v
valor (Tbl f) = f

-- (modifica (i,v) t) es la tabla obtenida modificando en la tabla t el
-- valor de i por v. Por ejemplo,
--    λ> f x = if x < 3 then x else 3-x
--    λ> t1 = tabla [(i,f i) | i <- [1..6] ]
--    λ> valor t1 6
--    -3
--    λ> valor (modifica (6,9) t1) 6
--    9
modifica :: Eq i => (i,v) -> Tabla i v -> Tabla i v
modifica (i,v) (Tbl f) = Tbl g
  where g j | j == i    = v
            | otherwise = f j

-- (tabla ivs) es la tabla correspondiente a la lista de asociación
-- ivs (que es una lista de pares formados por los índices y los
-- valores). Por ejemplo,
--    λ> tabla [(4,89), (1,90), (2,67)]
--    <<Una tabla>>
tabla :: Eq i => [(i,v)] -> Tabla i v
tabla = foldr modifica (Tbl (\_ -> error "fuera de rango"))
