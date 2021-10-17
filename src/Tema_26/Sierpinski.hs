-- Sierpinski.hs
-- Triángulo de Sierpinski
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 19 de Agosto de 2013
-- ---------------------------------------------------------------------

module Tema_26.Sierpinski where

import Graphics.Gloss
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Sierpinski. Introduce el paso [0..6]: "
  paso <- readLn
  display (InWindow ("Sierpinski - Paso " ++ show paso)
                    (500,500) (20,20)) black (dibujo paso)

dibujo :: Int -> Picture
dibujo paso = Color aquamarine $
              Translate (-150) (-125) $
              sierpinski paso

-- Longitud del lado inicial
longitud = 300 :: Float

sierpinski :: Int -> Picture
sierpinski 0 =
    Polygon [(0,0),
             (longitud/2, longitud * sqrt 3 /2),
             (longitud, 0)]
sierpinski n =
  Pictures [nuevoSierpinski,
            Translate (longitud/2) 0 nuevoSierpinski,
            Translate (longitud/4) (longitud * sqrt 3 /4) nuevoSierpinski]
  where nuevoSierpinski = Scale 0.5 0.5 (sierpinski (n-1))
