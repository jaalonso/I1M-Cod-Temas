import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (300,300) (20,20)) white dibujo

dibujo :: Picture
dibujo = Pictures [cuadrado2, cuadrado1]

cuadrado1 :: Picture
cuadrado1 = Line [(72,72),(144,72),(144,144),(72,144),(72,72)]

cuadrado2 :: Picture
cuadrado2 = Line [(0,0),(100,0),(100,100),(0,100),(0,0)]
