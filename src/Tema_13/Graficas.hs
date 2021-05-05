-- Graficas.hs
-- Representación gráfica de funciones con gnuplot.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_13.Graficas where

import Graphics.Gnuplot.Simple

-- Representación de la función coseno
dib0 :: IO ()
dib0 = plotFunc [] [0,0.01..10 :: Double] cos

-- (xRango n (a,b)) es la lista de los puntos obtenidos al dividir el
-- segmento (a,b) en n partes iguales. Por ejemplo,
--    xRango 5 (0,10)  ==  [0.0,2.0,4.0,6.0,8.0,10.0]
xRango :: Integer -> (Double,Double) -> [Double]
xRango = linearScale

-- Dibuja la gráfica de la función seno en el fichero ej.png
dib1a :: IO ()
dib1a = plotFunc [PNG "ej.png"] (xRango 500 (-10,10)) sin

-- Dibuja la gráfica de la función seno con retícula vertical
dib1b :: IO ()
dib1b = plotFunc [Grid (Just ["x"])] (xRango 500 (-10,10)) sin

-- Dibuja la gráfica de la función seno con retícula horizontal
dib1c :: IO ()
dib1c = plotFunc [Grid (Just ["y"])] (xRango 500 (-10,10)) sin

-- Dibuja la gráfica de la función seno con retícula horizontal y vertical
dib1d :: IO ()
dib1d = plotFunc [Grid (Just [])] (xRango 500 (-10,10)) sin

-- Dibuja la gráfica de la función seno con título y sin etiqueta
dib1e :: IO ()
dib1e = plotFunc [Title "La funcion seno", Key Nothing]
                 (xRango 1000 (-10,10)) sin

-- Dibuja la gráfica de la función seno con una etiqueta en el eje X
dib1f :: IO ()
dib1f = plotFunc [XLabel "Eje horizontal"] (xRango 1000 (-10,10)) sin

-- Dibuja los 30 primeros términos de la sucesión de Fibonacci:
dib2 :: IO ()
dib2 = plotList [] (take 30 fibs)
  where fibs :: [Double]
        fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Dibuja la gráfica de las funciones seno y coseno:
dib3 :: IO ()
dib3 = plotFuncs [] (xRango 1000 (-10,10)) [sin, cos]

-- Dibuja una función definida en forma paramétrica:
dib4 :: IO ()
dib4 = plotParamFunc [Key Nothing]
                     (xRango 1000 (0,2*pi))
                     (\t -> (12*sin t - 4*sin(3*t),
                             13*cos t - 5*cos(2*t) - 2*cos(3*t) - cos(4*t)))

-- Representación de superficies con plotFunc3d
dib5 :: IO ()
dib5 = plotFunc3d [] [] xs xs (\x y -> exp(-(x*x+y*y)))
  where xs = [-2,-1.8..2::Double]
