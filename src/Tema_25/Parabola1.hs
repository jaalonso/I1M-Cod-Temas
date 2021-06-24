-- Parabola.hs
-- Representación de la parábola y=x^2
-- José A. Alonso Jiménez <jalonso@us.es>
-- Sevilla, 4 de Agosto de 2011
-- ---------------------------------------------------------------------

import Graphics.Gloss

main  :: IO ()
main  =  display (InWindow "y = x^2" (500,500) (20,20)) white dibujo

dibujo :: Picture 
dibujo = Color red (Translate 0 (-200) parabola)

parabola :: Picture
parabola = Line (map puntosParabola [-200, -190 .. 200]) 
    where puntosParabola x = (x, x^2/100)

-- Referencia: "Drawing a parabola y = x^2" de Clem Baker-Finch
-- http://cs.anu.edu.au/student/comp1100/lectures/15-Graphics/scripts/Parabola.hs 

