import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (300,300) (-20,-20)) white bucle

bucle :: Picture
bucle = lineLoop [(-123,-99),(15,17),(107,-73)]
