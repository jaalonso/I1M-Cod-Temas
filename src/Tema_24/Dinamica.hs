-- Dinamica.hs
-- Patrón de la programación dinámica.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_24.Dinamica (module Tabla, dinamica)  where

-- Hay que elegir una implementación de TAD Tabla
-- import Tema_18.TablaConFunciones as Tabla
-- import Tema_18.TablaConListasDeAsociacion as Tabla
-- import Tema_18.TablaConMatrices as Tabla
import I1M.Tabla as Tabla

import Data.Array

dinamica :: Ix i => (Tabla i v -> i -> v) -> (i,i) -> Tabla i v
dinamica calcula cotas = t
  where t = tabla [(i,calcula t i) | i <- range cotas]
