import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (300,300) (20,20)) white rectangulo

rectangulo :: Picture
rectangulo = rectangleWire 200 100
