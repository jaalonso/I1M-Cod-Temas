import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (300,300) (20,20)) white dibujo

dibujo :: Picture
dibujo = pictures [ circle x | x <- [10,20..100] ]
