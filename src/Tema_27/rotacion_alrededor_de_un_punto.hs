-- rotacion_alrededor_de_un_punto.hs
-- Rotación alrededor de un punto.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 21 de Mayo de 2013
-- ---------------------------------------------------------------------

import Graphics.Gloss

main :: IO ()
main = animate (InWindow "Rotacion alrededor de un punto" (1800,820) (90,90))
               green animacion

animacion :: Float -> Picture
animacion t = rotate (60 * t) (translate 400 0 (circleSolid 10))
