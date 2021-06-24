import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (300,300) (20,20)) white cuadrado

cuadrado :: Picture
cuadrado = Polygon [(72,72),(144,72),(144,144),(72,144),(72,72)]
