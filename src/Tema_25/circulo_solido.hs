import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (300,300) (20,20)) white circuloSolido

circuloSolido :: Picture
circuloSolido = circleSolid 100
