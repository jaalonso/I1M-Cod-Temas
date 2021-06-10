-- BusquedaEnEspaciosDeEstados.hs
-- Búsqueda en espacios de estados.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_23.BusquedaEnEspaciosDeEstados (buscaEE) where

-- ---------------------------------------------------------------------
-- Importaciones                                                      --
-- ---------------------------------------------------------------------

-- Nota: Hay que elegir una implementación de las pilas.
import Tema_14.PilaConListas
-- import Tema_14.PilaConTipoDeDatoAlgebraico
-- import I1M.Pilas

-- ---------------------------------------------------------------------
-- Descripción de los problemas de espacios de estados                --
-- ---------------------------------------------------------------------

-- (buscaEE s o e) es la lista de soluciones del problema de espacio de
-- estado definido por la función sucesores (s), el objetivo (o) y el
-- estado inicial (e).
buscaEE :: (nodo -> [nodo]) -- sucesores
        -> (nodo -> Bool)   -- esFinal
        -> nodo             -- nodo actual
        -> [nodo]           -- soluciones
buscaEE sucesores esFinal x = busca' (apila x vacia)
  where
    busca' p
      | esVacia p        = []
      | esFinal (cima p) = cima p : busca' (desapila p)
      | otherwise        = busca' (foldr apila
                                         (desapila p)
                                         (sucesores (cima p)))
