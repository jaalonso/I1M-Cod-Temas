import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (300,300) (20,20)) white rectanguloPositivoSolido

rectanguloPositivoSolido :: Picture
rectanguloPositivoSolido = rectangleUpperSolid 200 100
