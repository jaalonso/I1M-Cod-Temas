import Graphics.Gloss

main :: IO ()
main = display (InWindow "Dibujo" (300,300) (20,20)) white sectorCircular

sectorCircular :: Picture
sectorCircular = sectorWire 0 90 100
