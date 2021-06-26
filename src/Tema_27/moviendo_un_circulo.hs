-- moviendo_un_circulo.hs
-- Moviendo un círculo.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 21 de Mayo de 2013
-- ---------------------------------------------------------------------

import Graphics.Gloss

main :: IO ()
main = animate (InWindow "Moviendo un circulo" (1800,820) (90,90)) green animacion

animacion :: Float -> Picture
animacion t = translate (50 * t - 900) 0 (color red (circleSolid 25))

-- Nota: La variable t toma como valor el tiempo transcurrido. Sus
-- valores son [0,0.5..]. Por tanto, los desplazamientos son [0,25,..].
