-- ColaConListas.hs
-- Implementación de las colas mediante listas.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_15.ColaConListas
  (Cola,
   vacia,   -- Cola a
   inserta, -- a -> Cola a -> Cola a
   primero, -- Cola a -> a
   resto,   -- Cola a -> Cola a
   esVacia, -- Cola a -> Bool
   valida   -- Cola a -> Bool
  ) where

-- Colas como listas:
newtype Cola a = C [a]
  deriving (Show, Eq)

-- Ejemplo de cola: La cola obtenida añadiéndole a la cola vacía los
-- números del 1 al 10 es
--    λ> foldr inserta vacia [1..10]
--    C [10,9,8,7,6,5,4,3,2,1]

-- vacia es la cola vacía. Por ejemplo,
--    λ> vacia
--    C []
vacia :: Cola a
vacia = C []

-- (inserta x c) es la cola obtenida añadiendo x al final de la cola
-- c. Por ejemplo,
--    λ> inserta 12 (foldr inserta vacia [1..10])
--    C [10,9,8,7,6,5,4,3,2,1,12]
inserta :: a -> Cola a -> Cola a
inserta x (C c) = C (c ++ [x])

-- (primero c) es el primer elemento de la cola c. Por ejemplo,
--    primero (foldr inserta vacia [1..10])  ==  10
primero :: Cola a -> a
primero (C (x:_)) = x
primero (C [])    = error "primero: cola vacia"

-- (resto c) es la cola obtenida eliminando el primer elemento de la
-- cola c. Por ejemplo,
--    λ> resto (foldr inserta vacia [1..10])
--    C [9,8,7,6,5,4,3,2,1]
resto :: Cola a -> Cola a
resto (C (_:xs)) = C xs
resto (C [])     = error "resto: cola vacia"

-- (esVacia c) se verifica si c es la cola vacía. Por ejemplo,
--    esVacia (foldr inserta vacia [1..10]) == False
--    esVacia vacia  == True
esVacia :: Cola a -> Bool
esVacia (C xs)  = null xs

-- (valida c) se verifica si c representa una cola válida. Con esta
-- representación, todas las colas son válidas.
valida :: Cola a -> Bool
valida _ = True
