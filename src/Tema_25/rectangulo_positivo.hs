import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (300,300) (20,20)) white rectanguloPositivo

rectanguloPositivo :: Picture
rectanguloPositivo = rectangleUpperWire 200 100
