import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (300,300) (20,20)) white rectangulo

rectangulo :: Picture
rectangulo = lineLoop (rectanglePath 200 100)
