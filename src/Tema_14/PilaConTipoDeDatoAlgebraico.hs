-- PilaConTipoDeDatoAlgebraico.hsPilaConListas.hs
-- Implementación de las pilas mediante tipos de datos algebraicos.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_14.PilaConTipoDeDatoAlgebraico
  (Pila,
   vacia,      -- Pila a
   apila,      -- a -> Pila a -> Pila a
   cima,       -- Pila a -> a
   desapila,   -- Pila a -> Pila a
   esVacia,    -- Pila a -> Bool
   escribePila -- Show a => Pila a -> String
  ) where

-- Tipo de dato algebraico de las pilas:
data Pila a = Vacia
            | P a (Pila a)
  deriving Eq

-- (escribePila p) es la cadena correspondiente a la pila p. Por
-- ejemplo,
--    escribePila (apila 1 (apila 2 (apila 3 vacia))) == "1|2|3|-"
escribePila :: Show a => Pila a -> String
escribePila Vacia   = "-"
escribePila (P x s) = show x ++ "|" ++ escribePila s

-- Procedimiento de escritura de pilas.
instance Show a => Show (Pila a) where
  show = escribePila

-- Ejemplo de pila:
--    λ> apila 1 (apila 2 (apila 3 vacia))
--    1|2|3|-

-- vacia es la pila vacía. Por ejemplo,
--    λ> vacia
--    -
vacia :: Pila a
vacia = Vacia

-- (apila x p) es la pila obtenida añadiendo x encima de la pila p. Por
-- ejemplo,
--    λ> apila 4 (apila 1 (apila 2 (apila 3 vacia)))
--    4|1|2|3|-
apila :: a -> Pila a -> Pila a
apila x p = P x p

-- (cima p) es la cima de la pila p. Por ejemplo,
--    cima (apila 1 (apila 2 (apila 3 vacia)))  ==  1
cima :: Pila a -> a
cima Vacia   = error "la pila vacia no tiene cima"
cima (P x _) =  x

-- (desapila p) es la pila obtenida suprimiendo la cima de la pila
-- p. Por ejemplo,
--    λ> desapila (apila 1 (apila 2 (apila 3 vacia)))
--    2|3|-
desapila :: Pila a -> Pila a
desapila Vacia   = error "no se puede desapila la pila vacia"
desapila (P _ p) = p

-- (esVacia p) se verifica si p es la pila vacía. Por ejemplo,
--    esVacia (apila 1 (apila 2 (apila 3 vacia))) ==  False
--    esVacia vacia                               ==  True
esVacia :: Pila a -> Bool
esVacia Vacia = True
esVacia _     = False
