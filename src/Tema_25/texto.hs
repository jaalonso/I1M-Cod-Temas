import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (800,300) (20,20)) white texto

texto :: Picture
texto = Text "Figura"
