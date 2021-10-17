-- ArbolFractal.hs
-- Árbol como un fractal.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 4 de Agosto de 2011
-- ---------------------------------------------------------------------

module Tema_26.ArbolFractal where

import Graphics.Gloss
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Árbol fractal.  Introduce el paso [0..6]: "
  paso <- readLn
  display (InWindow "Arbol fractal" (700,800) (20,20)) black (dibujo paso)

dibujo :: Int -> Picture
dibujo paso = Color green (Translate 0 (-300) (arbol paso))

tronco :: Picture
tronco = Polygon [(30,0), (15,300), (-15,300), (-30,0)]

arbol :: Int -> Picture
arbol 0 = tronco
arbol n = Pictures [tronco,
                    Translate 0 300 menor,
                    Translate 0 240 (Rotate 20    menor),
                    Translate 0 180 (Rotate (-20) menor),
                    Translate 0 120 (Rotate 40    menor),
                    Translate 0  60 (Rotate (-40) menor) ]
    where menor = Scale 0.5 0.5 (arbol (n-1))
