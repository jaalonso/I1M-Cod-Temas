-- Ficheros.hs
-- Manejo de ficheros.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_13.Ficheros where

import Data.Char (toUpper)
import Data.List (sort)
import Text.Printf (printf)

-- (muestraContenidoFichero f) muestra en pantalla el contenido del
-- fichero f. Por ejemplo,
--    λ> muestraContenidoFichero "Ejemplo_1.txt"
--    Este fichero tiene tres lineas
--    esta es la segunda y
--    esta es la tercera.
muestraContenidoFichero :: FilePath -> IO ()
muestraContenidoFichero f = do
  cs <- readFile f
  putStrLn cs

-- (aMayucula f1 f2) lee el contenido del fichero f1 y escribe su
-- contenido en mayúscula en el fichero f2. Por ejemplo,
--    λ> muestraContenidoFichero "Ejemplo_1.txt"
--    Este fichero tiene tres lineas
--    esta es la segunda y
--    esta es la tercera.
--
--    λ> aMayuscula "Ejemplo_1.txt" "Ejemplo_3.txt"
--    λ> muestraContenidoFichero "Ejemplo_3.txt"
--    ESTE FICHERO TIENE TRES LINEAS
--    ESTA ES LA SEGUNDA Y
--    ESTA ES LA TERCERA.
aMayuscula :: FilePath -> FilePath -> IO ()
aMayuscula f1 f2 = do
  contenido <- readFile f1
  writeFile f2 (map toUpper contenido)

-- (ordenaFichero f1 f2) lee el contenido del fichero f1 y escribe su
-- contenido ordenado en el fichero f2. Por ejemplo,
--    λ> muestraContenidoFichero "Ejemplo_4a.txt"
--    Juan Ramos
--    Ana Ruiz
--    Luis Garcia
--    Blanca Perez
--
--    λ> ordenaFichero "Ejemplo_4a.txt" "Ejemplo_4b.txt"
--    λ> muestraContenidoFichero "Ejemplo_4b.txt"
--    Ana Ruiz
--    Blanca Perez
--    Juan Ramos
--    Luis Garcia
ordenaFichero :: FilePath -> FilePath -> IO ()
ordenaFichero f1 f2 = do
  cs <- readFile f1
  writeFile f2 ((unlines . sort . lines) cs)

-- (tablaCuadrados f n) escribe en el fichero f los cuadrados de los n
-- primeros números. Por ejemplo.
--    λ> tablaCuadrados "cuadrados.txt" 9
--    λ> muestraContenidoFichero "cuadrados.txt"
--    (1,1) (2,4) (3,9) (4,16) (5,25) (6,36) (7,49) (8,64) (9,81)
tablaCuadrados :: FilePath -> Int -> IO ()
tablaCuadrados f n =
  writeFile f (listaDeCuadrados n)

listaDeCuadrados :: Int -> String
listaDeCuadrados n =
  unwords (map show [(x,x*x) | x <- [1..n]])

-- (tablaCuadrados2 f n) escribe en el fichero f los cuadrados de los n
-- primeros números, uno por línea. Por ejemplo.
--    λ> tablaCuadrados2 "cuadrados.txt" 5
--    λ> muestraContenidoFichero "cuadrados.txt"
--    (1,1)
--    (2,4)
--    (3,9)
--    (4,16)
--    (5,25)
tablaCuadrados2 :: FilePath -> Int -> IO ()
tablaCuadrados2 f n =
  writeFile f (listaDeCuadrados2 n)

listaDeCuadrados2 :: Int -> String
listaDeCuadrados2 n =
  unlines (map show [(x,x*x) | x <- [1..n]])

--(tablaLogaritmos f ns) escribe en el fichero f los cuadrados de los
--números de ns, uno por línea. Por ejemplo.
--    λ> tablaLogaritmos "Tabla_de_logaritmos.txt" [1,3..20]
--    λ> muestraContenidoFichero "Tabla_de_logaritmos.txt"
--    +----+----------------+
--    | n  | log(n)         |
--    +----+----------------+
--    |  1 | 0.000000000000 |
--    |  3 | 1.098612288668 |
--    |  5 | 1.609437912434 |
--    |  7 | 1.945910149055 |
--    |  9 | 2.197224577336 |
--    | 11 | 2.397895272798 |
--    | 13 | 2.564949357462 |
--    | 15 | 2.708050201102 |
--    | 17 | 2.833213344056 |
--    | 19 | 2.944438979166 |
--    +----+----------------+
tablaLogaritmos :: FilePath -> [Int] -> IO ()
tablaLogaritmos f ns = do
  writeFile f (tablaLogaritmosAux ns)

tablaLogaritmosAux :: [Int] -> String
tablaLogaritmosAux ns =
     linea
  ++ cabecera
  ++ linea
  ++ concat [printf "| %2d | %.12f |\n" n x
            | n <- ns
            , let x = log (fromIntegral n) :: Double]
  ++ linea

linea, cabecera :: String
linea    = "+----+----------------+\n"
cabecera = "| n  | log(n)         |\n"
