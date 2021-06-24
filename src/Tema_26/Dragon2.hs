-- Dragon.hs
-- La curva dragón
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 24 de Mayo de 2014
-- ---------------------------------------------------------------------

import Graphics.Gloss
-- import System.IO

main :: IO ()
main = do display (InWindow "Dragon" (500,500) (20,20)) white (dragon 1)

dragon :: Int -> Picture
dragon 0 = Line [(-200,0),(200,0)]
dragon n = Pictures [Translate (-100) 100 (Rotate  45 sub)] --,
                     -- Translate    100 100 (Rotate 135 sub)]
  where sub = Scale r r (dragon (n-1))
        r   = (fromIntegral 1) /(sqrt 2)
