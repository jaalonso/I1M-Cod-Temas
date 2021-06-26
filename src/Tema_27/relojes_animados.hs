import Graphics.Gloss

main :: IO ()
main = animate (InWindow "Relojes animados" (1800,820) (90,90))
               black animacion

animacion :: Float -> Picture
animacion t = Color white
            $ Scale 110 110
            $ Rotate (t * 2*pi)
            $ fractalReloj 5 t

-- El fractal básico está formado por tres círculos desplazados desde el
-- origen de la siguiente manera.
--
--         1
--         |
--         .
--       /   \
--      2     3
--
-- El sentido de giro cambia a medida que aumenta n. Los componentes de
-- las iteraciones más altas también giran más rápido.

fractalReloj :: Int -> Float -> Picture
fractalReloj 0 _ = Blank
fractalReloj n s = Pictures [circ1, circ2, circ3, lineas] where
  a       = 1 / sin (2 * pi / 6)
  b       = a * cos (2 * pi / 6)
  n'      = fromIntegral n
  rot     = if even n
            then   50 * s * log (1 + n')
            else (-50 * s * log (1 + n'))
  circNm1 = Pictures [ circle 1
                     , Scale (a/2.5) (a/2.5) (fractalReloj (n-1) s)
                     , if n > 2
                       then Color cyan
                            $ Translate (-0.15) 1
                            $ Scale 0.001 0.001
                            $ Text (show s)
                       else Blank ]
  circ1   = Translate 0 a         $ Rotate rot    circNm1
  circ2   = Translate 1 (-b)      $ Rotate (-rot)         circNm1
  circ3   = Translate (-1) (-b)   $ Rotate rot    circNm1
  lineas  = Pictures [ Line [(0, 0), ( 0,  a)]
                     , Line [(0, 0), ( 1, -b)]
                     , Line [(0, 0), (-1, -b)] ]
