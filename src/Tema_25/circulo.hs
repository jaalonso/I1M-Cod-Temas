import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (300,300) (20,20)) white circulo

circulo :: Picture
circulo = circle 100
