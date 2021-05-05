-- FuncionesES.hs
-- Funciones de entrada/salida.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_13.FuncionesES where

import Prelude hiding ( getLine
                      , putStr
                      , putStrLn
                      , sequence_
                      )

-- ejSecuenciacion lee dos caracteres y devuelve el par formado por
-- ellos. Por ejemplo,
--    λ> ejSecuenciacion
--    b f
--    ('b','f')
ejSecuenciacion :: IO (Char,Char)
ejSecuenciacion = do
  x <- getChar
  _ <- getChar
  y <- getChar
  return (x,y)

-- getLine lee cadenas del teclado.
getLine :: IO String
getLine = do
  x <- getChar
  if x == '\n' then return []
               else do xs <- getLine
                       return (x:xs)

-- (putStr c) escribe la cadena c en la pantalla. Por ejemplo,
--    λ> putStr "abc "
--    abc λ>
putStr :: String -> IO ()
putStr []     = return ()
putStr (x:xs) = do putChar x
                   putStr xs

-- (putStrLn c) escribe la cadena c y un salto de línea en la
-- pantalla. Por ejemplo,
--    λ> putStrLn "abc "
--    abc
--    λ>
putStrLn :: String -> IO ()
putStrLn xs = do
  putStr xs
  putChar '\n'

-- (sequence_ as) ejecuta la lista de acciones as. Por ejemplo,
--    λ> sequence_ [putStrLn "uno", putStrLn "dos"]
--    uno
--    dos
--    λ> it
--    ()
sequence_ :: [IO a] -> IO ()
sequence_ []     = return ()
sequence_ (a:as) = do _ <- a
                      sequence_ as

-- longitudCadena pide una cadena y escribe el número de caracteres que
-- tiene. Por ejemplo,
--    λ> longitudCadena
--    Escribe una cadena: "Hoy es lunes"
--    La cadena tiene 14 caracteres
longitudCadena :: IO ()
longitudCadena = do
  putStr "Escribe una cadena: "
  xs <- getLine
  putStr "La cadena tiene "
  putStr (show (length xs))
  putStrLn " caracteres"
