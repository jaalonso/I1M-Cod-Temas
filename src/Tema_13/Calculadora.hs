-- Calculadora.hs
-- Calculadora aritmérica
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_13.Calculadora where

import I1M.Analizador
import System.IO

-- getCh lee un carácter sin eco.
getCh :: IO Char
getCh = do
  hSetEcho stdin False
  c <- getChar
  hSetEcho stdin True
  return c

-- limpiaPantalla limpia la pantalla.
limpiaPantalla :: IO ()
limpiaPantalla = putStr "\ESC[2J"

-- Un posición (con tipo Pos) es un par de enteros.
type Pos = (Int,Int)

-- (irA p) coloca el cursor en la posición p.
irA :: Pos -> IO ()
irA (x,y) = putStr ("\ESC[" ++
                    show y ++ ";" ++ show x ++
                    "H")

-- (escribeEn p c) escribe la cadena c en la posición p.
escribeEn :: Pos -> String -> IO ()
escribeEn p xs = do irA p
                    putStr xs

-- calculadora es el programa principar consistente
calculadora :: IO ()
calculadora = do
  limpiaPantalla
  escribeCalculadora
  limpiar

escribeCalculadora :: IO ()
escribeCalculadora =
 do limpiaPantalla
    sequence_ [escribeEn (1,y) xs
               | (y,xs) <- zip [1..13] imagenCalculadora]
    putStrLn ""


-- imagenCalculadora es la imagen de la calculadora. Los primeros cuatro
-- botones permiten escribir las órdenes:
-- + q para salir (quit),
-- + c para limpiar la agenda (clear),
-- + d para borrar un carácter (delete) y
-- + = para evaluar una expresión.
-- Los restantes botones permiten escribir las expresiones.
imagenCalculadora :: [String]
imagenCalculadora =
  ["+---------------+",
   "|               |",
   "+---+---+---+---+",
   "| q | c | d | = |",
   "+---+---+---+---+",
   "| 1 | 2 | 3 | + |",
   "+---+---+---+---+",
   "| 4 | 5 | 6 | - |",
   "+---+---+---+---+",
   "| 7 | 8 | 9 | * |",
   "+---+---+---+---+",
   "| 0 | ( | ) | / |",
   "+---+---+---+---+"]

limpiar :: IO ()
limpiar = calc ""

calc :: String -> IO ()
calc xs = do
  escribeEnPantalla xs
  c <- getCh
  if c `elem` botones
    then procesa c xs
    else calc xs

escribeEnPantalla :: String -> IO ()
escribeEnPantalla xs = do
  escribeEn (3,2) "             "
  escribeEn (3,2) (reverse (take 13 (reverse xs)))

botones :: String
botones = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/"
    extra    = "QCD \ESC\BS\DEL\n"

procesa :: Char -> String -> IO ()
procesa c xs
   | c `elem` "qQ\ESC"    = salir
   | c `elem` "dD\BS\DEL" = borrar xs
   | c `elem` "=\n"       = evaluar xs
   | c `elem` "cC"        = limpiar
   | otherwise            = agregar c xs

salir :: IO ()
salir = irA (1,14)

borrar :: String -> IO ()
borrar "" = calc ""
borrar xs = calc (init xs)

evaluar :: String -> IO ()
evaluar xs =
  case analiza expr xs of
    [(n,"")] -> calc (show n)
    _        -> calc xs

agregar :: Char -> String -> IO ()
agregar c xs = calc (xs ++ [c])

-- Nota: El programa se debe de ejecutar en una terminar. Los pasos son
-- + Abrir la terminal.
-- + Cambiar al directorio donde se encuentra
-- + Iniciar Haskell con
--      stack exec -- ghci
-- + Cargar el programa con
--      :load Calculadora
-- + Ejecutar el programa con
--      calculadora
