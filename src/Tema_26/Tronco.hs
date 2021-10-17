-- Tronco.hs
-- Tronco de un árbol.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 23 de mayo de 2013
-- ---------------------------------------------------------------------

module Tema_26.Tronco where

import Graphics.Gloss

main :: IO ()
main = display (InWindow "Tronco de arbol" (700,800) (20,20)) black dibujo

dibujo :: Picture
dibujo = Color aquamarine (Translate 0 (-300) tronco)

tronco :: Picture
tronco = Polygon [(30,0), (15,300), (-15,300), (-30,0)]
