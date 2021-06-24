-- circulos_trasladados.hs
-- Círculos por comprensión y trasladados. 
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 20 de Mayo de 2013
-- ---------------------------------------------------------------------

import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (500,300) (20,20)) white dibujo

dibujo :: Picture
dibujo = pictures [translate x 0 (circle 80) | x <- [-100,-60..100]]
