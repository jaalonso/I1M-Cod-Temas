-- ArbolColoreado.hs
-- Árbol como un fractal coloreado (1).
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 4 de Agosto de 2011
-- ---------------------------------------------------------------------

module Tema_26.ArbolFractalColoreado where

import Graphics.Gloss
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Árbol fractal.  Introduce el grado [0..6]: "
  grado <- readLn
  display (InWindow "Arbol fractal" (700,800) (20,20)) black (dibujo grado)

dibujo :: Int -> Picture
dibujo grado = Translate 0 (-300) (arbol grado marron)

tronco :: Color -> Picture
tronco color' = Color color' (Polygon [(30,0), (15,300), (-15,300), (-30,0)])

arbol :: Int -> Color -> Picture
arbol 0 color' = tronco color'
arbol n color' = Pictures [tronco color',
                           Translate 0 300 arbolMenor,
                           Translate 0 240 (Rotate   20  arbolMenor),
                           Translate 0 180 (Rotate (-20) arbolMenor),
                           Translate 0 120 (Rotate   40  arbolMenor),
                           Translate 0  60 (Rotate (-40) arbolMenor) ]
    where arbolMenor = Scale 0.5 0.5 (arbol (n-1) (masVerde color'))

marron :: Color
marron = makeColorI 139 100  35 255

-- (masVerde c) es el color obtenido mezclando los colores c y verde en
-- las proporciones 1 y 0.1.
masVerde :: Color -> Color
masVerde color' = mixColors 1.0 0.1 color' green
