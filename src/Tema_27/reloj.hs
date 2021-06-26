-- reloj.hs
-- Reloj
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 21 de Mayo de 2013
-- ---------------------------------------------------------------------

import Graphics.Gloss

main :: IO ()
main = animate (InWindow "Reloj" (1800,820) (90,90))
               green animacion

animacion :: Float -> Picture
animacion t = reloj t

reloj :: Float -> Picture
reloj t = pictures [ fondo,
                     minutero t,
                     segundero t ]

fondo :: Picture
fondo = color (dark white) (circleSolid 250)

minutero :: Float -> Picture
minutero t = rotate (0.1 * t) (line [(0,0), (0,250)])

segundero :: Float -> Picture
segundero t = rotate (6 * t) (line [(0,0), (0,250)])

-- Giro del minutero:
-- * 360º ~ 1 hora = 60 minutos = 3.600 segundos
-- * 1 minuto ~ (360/3.600)º = 0.1º
--
-- Giro del segundero:
-- * 360º ~ 1 minuto = 60 segundos
-- * 1 segundo ~ (360/60)º = 6º
