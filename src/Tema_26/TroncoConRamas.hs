-- TroncoConRamas.hs
-- Tronco de árbol con ramas.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 23 de mayo de 2014
-- ---------------------------------------------------------------------

module Tema_26.TroncoConRamas where

import Graphics.Gloss

main :: IO ()
main = display (InWindow "Tronco de arbol" (700,800) (20,20)) black dibujo

dibujo :: Picture
dibujo = Color green (Translate 0 (-300) troncoConRamas)

tronco :: Picture
tronco = Polygon [(30,0), (15,300), (-15,300), (-30,0)]

troncoConRamas :: Picture
troncoConRamas =
  Pictures [ tronco,
             Translate 0 300 rama,
             Translate 0 240 (Rotate 20    rama),
             Translate 0 180 (Rotate (-20) rama),
             Translate 0 120 (Rotate 40    rama),
             Translate 0  60 (Rotate (-40) rama)
           ]
  where rama = Scale 0.5 0.5 tronco
