-- circulos_expandiendose.hs
-- Círculos expandiéndose.
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
dibujo = pictures [rotate angulo (translate (5*x) 0 (circle x))
                  | x      <- [10,20.. 40],
                    angulo <- [ 0,45..360]]
