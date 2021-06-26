import Graphics.Gloss

main :: IO ()
main = animate (InWindow "Arbol animado" (1800,820) (90,90))
               blue animacion

animacion :: Float -> Picture
animacion t =
  Pictures [ Translate 0 150    rectanguloDeFondo
           , Translate 0 0      rectanguloDeFondo
           , Translate 0 (-150) rectanguloDeFondo
           , Translate 0 (-150) (fractalArbol 7 t)
           ]

rectanguloDeFondo :: Picture
rectanguloDeFondo =
  Pictures [ Color red   (rectangleSolid 400 100)
           , Color white (rectangleWire  400 100) ]

colorContornoRamas :: Color
colorContornoRamas = makeColor 0.3 0.3 1.0 1.0

colorRamas :: Color
colorRamas = makeColor 0.0 1.0 0.0 0.5

fractalArbol :: Int -> Float -> Picture
fractalArbol 0 _ = Blank
fractalArbol n t =
  Pictures [ Color colorRamas (rectangleUpperSolid 20 300)
           , Color colorContornoRamas (rectangleUpperWire 20 300)
           , Translate 0 30
             $ Rotate  (200 * sin t / fromIntegral n)
             $ Scale   0.9 0.9
             $ fractalArbol (n-1) t
           , Translate 0 70
             $ Rotate  (-200 * sin t / fromIntegral n)
             $ Scale      0.8 0.8
             $ fractalArbol (n-1) t ]
