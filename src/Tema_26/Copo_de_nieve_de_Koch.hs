-- CopoDeNieveDeKoch.hs
-- Fractal del copo de nieve de Koch.
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 4 de Agosto de 2011
-- ---------------------------------------------------------------------

module Tema_26.Copo_de_nieve_de_Koch where

import Graphics.Gloss
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Fractal copo de nieve. Introduce el paso [0..6]: "
  paso <- readLn
  display (InWindow "Fractal copo de nieve" (500,500) (20,20)) black (dibujo paso)

-- Longitud de los lados del triángulo inicial
longitud = 360 :: Float

dibujo :: Int -> Picture
dibujo paso =
  Color aquamarine $                                  -- colorea
  Translate (-longitud/2) ((longitud * sqrt 3)/6) $   -- centra el fractal
  copoDeNieve paso

curva :: Int -> Picture
curva 0 = Line [(0,0), (longitud, 0)]
curva n =
  Pictures [nuevaCurva,
            Translate (longitud/3)     0                        (Rotate (-60) nuevaCurva),
            Translate (longitud/2)     ((longitud * sqrt 3)/6)  (Rotate   60  nuevaCurva),
            Translate (2 * longitud/3) 0                        nuevaCurva]
  where nuevaCurva = Scale (1/3) (1/3) (curva (n-1))

copoDeNieve :: Int -> Picture
copoDeNieve n =
  Pictures [unaCurva ,
            Translate longitud     0                          (Rotate   120  unaCurva),
            Translate (longitud/2) (-((longitud * sqrt 3)/2)) (Rotate (-120) unaCurva)]
  where unaCurva = curva n
