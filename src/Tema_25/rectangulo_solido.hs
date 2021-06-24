import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (300,300) (20,20)) white rectanguloSolido

rectanguloSolido :: Picture
rectanguloSolido = rectangleSolid 200 100
