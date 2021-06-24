-- Dragon.hs
-- La curva dragón
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 24 de Mayo de 2014
-- ---------------------------------------------------------------------

import Graphics.Gloss
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Dragon. Introduce el paso [0..6]: "
  paso <- readLn
  display (InWindow ("Dragon - Paso " ++ show paso)
                    (500,500) (20,20)) black (dibujo paso (255*2^paso))

dibujo :: Int -> Float -> Picture
dibujo paso longitud =
  Color aquamarine $
  Translate (-100) (-50) $
  dragon paso longitud

dragon :: Int -> Float -> Picture
dragon 0 longitud = Line [(0,0),(longitud,0)]
dragon n longitud = Pictures [arriba, enfrente, abajo, derecha]
  where arriba      = Rotate (-90)
                      prevDragon
        enfrente    = Translate prevLong prevLong $
                      Rotate 180
                      prevDragon
        abajo       = Translate prevLong prevLong $
                      Rotate 90
                      prevDragon
        derecha     = Translate (prevLong * 2) 0 $
                      Rotate 180
                      prevDragon
        prevDragon  = Scale 0.25 0.25 (dragon (n-1) longitud)
        prevLong    = longitud / (2^(n+1))
