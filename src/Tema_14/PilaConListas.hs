-- PilaConListas.hs
-- Implementación de las pilas mediante listas.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_14.PilaConListas
  (Pila,
   vacia,      -- Pila a
   apila,      -- a -> Pila a -> Pila a
   cima,       -- Pila a -> a
   desapila,   -- Pila a -> Pila a
   esVacia,    -- Pila a -> Bool
   escribePila -- Show a => Pila a -> String
  ) where

-- Representación de las pilas mediante listas.
newtype Pila a = P [a]
  deriving Eq

-- (escribePila p) es la cadena correspondiente a la pila p. Por
-- ejemplo,
--    escribePila (apila 1 (apila 2 (apila 3 vacia))) == "1|2|3|-"
escribePila :: Show a => Pila a -> String
escribePila (P [])     = "-"
escribePila (P (x:xs)) = show x ++ "|" ++ escribePila (P xs)

-- Procedimiento de escritura de pilas.
instance Show a => Show (Pila a) where
  show = escribePila

-- Ejemplo de pila:
--    λ> apila 1 (apila 2 (apila 3 vacia))
--    1|2|3|-

-- vacia es la pila vacía. Por ejemplo,
--    λ> vacia
--    -
vacia   :: Pila a
vacia = P []

-- (apila x p) es la pila obtenida añadiendo x encima de la pila p. Por
-- ejemplo,
--    λ> apila 4 (apila 1 (apila 2 (apila 3 vacia)))
--    4|1|2|3|-
apila :: a -> Pila a -> Pila a
apila x (P xs) = P (x:xs)

-- (cima p) es la cima de la pila p. Por ejemplo,
--    cima (apila 1 (apila 2 (apila 3 vacia)))  ==  1
cima :: Pila a -> a
cima (P [])    = error "cima de la pila vacia"
cima (P (x:_)) = x

-- (desapila p) es la pila obtenida suprimiendo la cima de la pila
-- p. Por ejemplo,
--    λ> desapila (apila 1 (apila 2 (apila 3 vacia)))
--    2|3|-
desapila :: Pila a -> Pila a
desapila (P [])     = error "desapila la pila vacia"
desapila (P (_:xs)) = P  xs

-- (esVacia p) se verifica si p es la pila vacía. Por ejemplo,
--    esVacia (apila 1 (apila 2 (apila 3 vacia))) ==  False
--    esVacia vacia                               ==  True
esVacia :: Pila a -> Bool
esVacia (P xs) = null xs
