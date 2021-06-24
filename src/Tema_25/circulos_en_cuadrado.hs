-- circulos_en_cuadrado.hs
-- Círculos en un cuadrado.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 20 de Mayo de 2013
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio. ¿Qué dibujo genera el siguiente programa?
-- ---------------------------------------------------------------------

import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (500,300) (20,20)) white dibujo

dibujo :: Picture
dibujo = pictures [translate x y (circle 10) | 
                   x <- [-200,-100..200],
                   y <- [-200,-100..200]]
