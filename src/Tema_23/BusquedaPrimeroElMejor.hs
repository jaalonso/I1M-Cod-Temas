-- BusquedaPrimeroElMejor.hs
-- Búsqueda por primero el mejor.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_23.BusquedaPrimeroElMejor (buscaPM)  where

-- ---------------------------------------------------------------------
-- Importaciones                                                      --
-- ---------------------------------------------------------------------

-- Nota: Hay que elegir una implementación de las colas de prioridad.
import Tema_20.ColaDePrioridadConMonticulos
-- import I1M.ColaDePrioridad

-- (buscaPM s o e) es la lista de soluciones del problema de espacio de
-- estado definido por la función sucesores (s), el objetivo (o) y el
-- estado inicial (e), obtenidas buscando por primero el mejor.
buscaPM :: (Ord nodo) =>
           (nodo -> [nodo])   -- sucesores
           -> (nodo -> Bool)  -- esFinal
           -> nodo            -- nodo actual
           -> [nodo]          -- solución
buscaPM sucesores esFinal x = busca' (inserta x vacia)
 where
   busca' c
    | esVacia c = []
    | esFinal (primero c) =
        primero c : busca' (resto c)
    | otherwise =
        busca' (foldr inserta (resto c) (sucesores (primero c)))
