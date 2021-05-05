-- Vida.hs
-- El juego de la vida
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_13.Vida where

-- Descripción del juego de la vida
-- + El tablero del juego de la vida es una malla formada por cuadrados
--   ("células") que se pliega en todas las direcciones.
-- + Cada célula tiene 8 células vecinas, que son las que están próximas a ella,
--   incluso en las diagonales.
-- + Las células tienen dos estados: están "vivas" o "muertas".
-- + El estado del tablero evoluciona a lo largo de unidades de tiempo discretas.
-- + Las transiciones dependen del número de células vecinas vivas:
--   + Una célula muerta con exactamente 3 células vecinas vivas "nace" (al turno
--     siguiente estará viva).
--   + Una célula viva con 2 ó 3 células vecinas vivas sigue viva, en otro caso
--     muere.

import Data.List (nub)

type Pos = (Int,Int)

irA :: Pos -> IO ()
irA (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

escribeEn :: Pos -> String -> IO ()
escribeEn p xs = do
  irA p
  putStr xs

limpiaPantalla :: IO ()
limpiaPantalla = putStr "\ESC[2J"

-- El tablero del juego de la vida
type Tablero = [Pos]

-- Dimensiones:
ancho, alto :: Int
ancho = 5
alto  = 5

-- Ejemplo de tablero:
ejTablero :: Tablero
ejTablero = [(2,3),(3,4),(4,2),(4,3),(4,4)]

-- (vida n t) simula el juego de la vida a partir del tablero t con un
-- tiempo entre generaciones proporcional a n. Por ejemplo,
--    vida 100000 ejTablero
-- Nota. Esta función se debe de ejecutar en una terminal.
vida :: Int -> Tablero -> IO ()
vida n t = do
  limpiaPantalla
  escribeTablero t
  espera n
  vida n (siguienteGeneracion t)

-- Escritura del tablero:
escribeTablero :: Tablero -> IO ()
escribeTablero t =
  sequence_ [escribeEn p "O" | p <- t]

-- Espera entre generaciones:
espera :: Int -> IO ()
espera n =
  sequence_ [return () | _ <- [1..n]]

-- siguienteGeneracion t) es el tablero de la siguiente generación al tablero
-- t. Por ejemplo,
--    λ> siguienteGeneracion ejTablero
--    [(4,3),(3,4),(4,4),(3,2),(5,3)]
siguienteGeneracion :: Tablero -> Tablero
siguienteGeneracion t = supervivientes t ++ nacimientos t

-- (supervivientes t) es la listas de posiciones de t que sobreviven;
-- i.e. posiciones con 2 ó 3 vecinos vivos. Por ejemplo,
--    supervivientes ejTablero  ==  [(4,3),(3,4),(4,4)]
supervivientes :: Tablero -> [Pos]
supervivientes t = [p | p <- t,
                        nVecinosVivos t p `elem` [2,3]]

-- (nVecinosVivos t c) es el número de vecinos vivos de la célula
-- c en el tablero t. Por ejemplo,
--    nVecinosVivos ejTablero (3,3)  ==  5
--    nVecinosVivos ejTablero (3,4)  ==  3
nVecinosVivos :: Tablero -> Pos -> Int
nVecinosVivos t = length . filter (tieneVida t) . vecinos

-- (vecinos p) es la lista de los vecinos de la célula en la
-- posición p. Por ejemplo,
--    vecinos (2,3) == [(1,2),(2,2),(3,2),(1,3),(3,3),(1,4),(2,4),(3,4)]
--    vecinos (1,2) == [(5,1),(1,1),(2,1),(5,2),(2,2),(5,3),(1,3),(2,3)]
--    vecinos (5,2) == [(4,1),(5,1),(1,1),(4,2),(1,2),(4,3),(5,3),(1,3)]
--    vecinos (2,1) == [(1,5),(2,5),(3,5),(1,1),(3,1),(1,2),(2,2),(3,2)]
--    vecinos (2,5) == [(1,4),(2,4),(3,4),(1,5),(3,5),(1,1),(2,1),(3,1)]
--    vecinos (1,1) == [(5,5),(1,5),(2,5),(5,1),(2,1),(5,2),(1,2),(2,2)]
--    vecinos (5,5) == [(4,4),(5,4),(1,4),(4,5),(1,5),(4,1),(5,1),(1,1)]
vecinos :: Pos -> [Pos]
vecinos (x,y) = map modular [(x-1,y-1), (x,y-1), (x+1,y-1),
                             (x-1,y),            (x+1,y),
                             (x-1,y+1), (x,y+1), (x+1,y+1)]

-- (modular p) es la posición correspondiente a p en el tablero considerando
-- los plegados. Por ejemplo,
--    modular (6,3)  ==  (1,3)
--    modular (0,3)  ==  (5,3)
--    modular (3,6)  ==  (3,1)
--    modular (3,0)  ==  (3,5)
modular :: Pos -> Pos
modular (x,y) = (1 + (x-1) `mod` ancho,
                 1 + (y-1) `mod` alto)

-- (tieneVida t p) se verifica si la posición p del tablero t tiene
-- vida. Por ejemplo,
--    tieneVida ejTablero (1,1)  ==  False
--    tieneVida ejTablero (2,3)  ==  True
tieneVida :: Tablero -> Pos -> Bool
tieneVida t p = p `elem` t

-- (noTieneVida t p) se verifica si la posición p del tablero t no tiene
-- vida. Por ejemplo,
--    noTieneVida ejTablero (1,1)  ==  True
--    noTieneVida ejTablero (2,3)  ==  False
noTieneVida :: Tablero -> Pos -> Bool
noTieneVida t p = not (tieneVida t p)

-- (nacimientos t) es la lista de los nacimientos de tablero t; i.e. las
-- posiciones sin vida con 3 vecinos vivos. Por ejemplo,
--    nacimientos ejTablero  ==  [(3,2),(5,3)]
nacimientos' :: Tablero -> [Pos]
nacimientos' t = [(x,y) | x <- [1..ancho],
                          y <- [1..alto],
                          noTieneVida t (x,y),
                          nVecinosVivos t (x,y) == 3]

-- La función anterior se puede mejorar como sigue.
nacimientos :: Tablero -> [Pos]
nacimientos t = [p | p <- nub (concatMap vecinos t),
                     noTieneVida t p,
                     nVecinosVivos t p == 3]

-- Nota: El programa se debe de ejecutar en una terminar. Los pasos son
-- + Abrir la terminal.
-- + Cambiar al directorio donde se encuentra
-- + Iniciar Haskell con
--      stack exec -- ghci
-- + Cargar el programa con
--      :load Vida
-- + Ejecutar el programa con
--      vida 100000 ejTablero
