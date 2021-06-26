-- rotando_un_cuadrado.hs
-- Rotando un cuadrado.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 21 de Mayo de 2013
-- ---------------------------------------------------------------------

import Graphics.Gloss

main :: IO ()
main = animate (InWindow "Rotando un cuadrado" (500,500) (20,20)) green animacion

animacion :: Float -> Picture
animacion t = rotate ((-100) * t) (rectangleSolid 100 100)

-- Nota: La variable t toma como valor el tiempo transcurrido. Sus
-- valores son [0,0.5..]. Por tanto, los giros son de [0,30,..]. 