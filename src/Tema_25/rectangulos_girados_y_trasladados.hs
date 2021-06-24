-- rectangulos_girados_y_trasladados.hs
-- Rectángulos girados y trasladados.
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
dibujo = pictures [translate x 0 (rotate x (rectangleWire 200 200)) | 
                   x <- [-45,-35..45]]
