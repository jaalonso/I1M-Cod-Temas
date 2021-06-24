-- Curva_de_Koch.hs
-- Fractal de la curva de Koch.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 4 de Agosto de 2011
-- ---------------------------------------------------------------------

import Graphics.Gloss
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Curva de Koch. Introduce el paso [0..6]: "
  paso <- readLn
  display (InWindow ("Curva de Koch - Paso " ++ show paso)
                    (500,500) (20,20)) black (dibujo paso)

-- Longitud de los lados del triángulo inicial
longitud :: Float
longitud = 360

dibujo :: Int -> Picture
dibujo paso =
    Color aquamarine $                                  -- colorea
    Translate (-longitud/2) (-(longitud * sqrt 3)/6) $  -- centra el fractal
    curva paso

curva :: Int -> Picture
curva 0 = Line [(0, 0), (longitud, 0)]
curva n =
  Pictures [nuevaCurva,
            Translate (longitud/3) 0                        (Rotate (-60) nuevaCurva),
            Translate (longitud/2) ((longitud * sqrt 3)/6)  (Rotate   60  nuevaCurva),
            Translate (2 * longitud/3) 0                    nuevaCurva ]
  where nuevaCurva = Scale (1/3) (1/3) (curva (n-1))
