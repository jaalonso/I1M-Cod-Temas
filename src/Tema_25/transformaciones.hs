import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (300,300) (20,20)) white dibujo

dibujo :: Picture
dibujo = Pictures [cuadrado, titulo]

cuadrado :: Picture
cuadrado = Line [(72,72),(144,72),(144,144),(72,144),(72,72)]

titulo :: Picture
titulo = Rotate (-10) $
         Translate (-70) 0 $ 
         Scale 0.2 0.2 $ 
         Text "Un cuadrado"
