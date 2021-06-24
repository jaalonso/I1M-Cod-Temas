-- elefante.hs
-- Dibujo de un elefante.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 20 de Mayo de 2013
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio. Dibujar un elefante como en la figura elefante.png
-- ---------------------------------------------------------------------

import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (500,300) (20,20)) white dibujo

dibujo :: Picture
dibujo = elefante

elefante = 
  pictures 
   [rotate 20 (scale 3 2 (translate 30 40 (circleSolid 25))),  -- cabeza
    translate 150 (-20) (rectangleSolid 40 80),                -- trompa
    translate (-10) 40 (scale 1.5 1 (circleSolid 80)),         -- cuerpo
    translate 50 (-50)(rectangleSolid 40 70),                  -- pata delantera
    translate (-60) (-50) (rectangleSolid 40 70),              -- pata trasera
    translate (-140) 50 (rotate (-100) (rectangleSolid 10 40)) -- cola
   ]
