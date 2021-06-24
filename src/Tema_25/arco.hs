import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (300,300) (20,20)) white arco

arco :: Picture
arco = arc 0 90 100
