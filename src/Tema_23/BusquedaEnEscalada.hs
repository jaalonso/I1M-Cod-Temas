-- BusquedaEnEscalada.hs
-- Búsqueda en escalada.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_23.BusquedaEnEscalada where

-- ---------------------------------------------------------------------
-- Importaciones                                                      --
-- ---------------------------------------------------------------------

-- Nota: Hay que elegir una implementación de las colas de prioridad
-- import Tema_16.ColaDePrioridadConListas
import Tema_20.ColaDePrioridadConMonticulos
-- import I1M.ColaDePrioridad

-- (buscaEscalada s o e) es la lista de soluciones del problema de espacio de
-- estado definido por la función sucesores (s), el objetivo (o) y el
-- estado inicial (e), obtenidas buscando por escalada.
buscaEscalada :: Ord nodo =>
                 (nodo -> [nodo])   -- sucesores
                 -> (nodo -> Bool)  -- es final
                 -> nodo            -- nodo actual
                 -> [nodo]          -- soluciones
buscaEscalada sucesores esFinal x = busca' (inserta x vacia) where
  busca' c
    | esVacia c           = []
    | esFinal (primero c) = [primero c]
    | otherwise           = busca' (foldr inserta vacia (sucesores (primero c)))
