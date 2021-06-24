import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (300,300) (20,20)) white coronaCircular

coronaCircular :: Picture
coronaCircular = thickCircle 100 30
