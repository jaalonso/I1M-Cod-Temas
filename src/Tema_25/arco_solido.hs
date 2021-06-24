import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (300,300) (20,20)) white arcoSolido

arcoSolido :: Picture
arcoSolido = arcSolid 0 90 100
