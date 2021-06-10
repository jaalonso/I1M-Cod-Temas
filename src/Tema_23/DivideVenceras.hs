-- DivideVenceras.hs
-- Divide y vencerás.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_23.DivideVenceras (divideVenceras) where

-- (divideVenceras ind resuelve divide combina pbInicial) resuelve el
-- problema pbInicial mediante la técnica de divide y vencerás, donde
-- + (ind pb) se verifica si el problema pb es indivisible
-- + (resuelve pb) es la solución del problema indivisible pb
-- + (divide pb) es la lista de subproblemas de pb
-- + (combina pb ss) es la combinación de las soluciones ss de los
--      subproblemas del problema pb.
-- + pbInicial es el problema inicial
divideVenceras :: (p -> Bool)
               -> (p -> s)
               -> (p -> [p])
               -> (p -> [s] -> s)
               -> p
               -> s
divideVenceras ind resuelve divide combina = dv'
  where
    dv' pb
      | ind pb    = resuelve pb
      | otherwise = combina pb [dv' sp | sp <- divide pb]
