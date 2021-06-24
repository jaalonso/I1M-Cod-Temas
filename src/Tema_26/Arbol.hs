-- Arbol.hs
-- Construcción iterada de un árbol.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 4 de Agosto de 2011
-- ---------------------------------------------------------------------

import Graphics.Gloss

main :: IO ()
main = display (InWindow "Arbol" (700,800) (20,20)) black dibujo

dibujo :: Picture
dibujo = Color green (Translate 0 (-300) arbol)

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

arbol :: Picture
arbol =
  Pictures [ tronco,
             Translate 0 300 ramas,
             Translate 0 240 (Rotate 20    ramas),
             Translate 0 180 (Rotate (-20) ramas),
             Translate 0 120 (Rotate 40    ramas),
             Translate 0  60 (Rotate (-40) ramas)
           ]
  where ramas = Scale 0.5 0.5 troncoConRamas
