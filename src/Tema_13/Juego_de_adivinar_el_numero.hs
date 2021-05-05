-- Juego_de_adivinar_el_numero.hs
-- Juego de adivinar el número.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_13.Juego_de_adivinar_el_numero where

import System.Random (randomRIO)

-- ---------------------------------------------------------------------
-- § Adivina la máquina                                               --
-- ---------------------------------------------------------------------

-- Descripción: El programa le pide al jugador humano que piense un
-- número entre 1 y 100 y trata de adivinar el número que ha pensado
-- planteándole conjeturas a las que el jugador humano responde con
-- mayor, menor o exacto según que el número pensado sea mayor, menor o
-- igual que el número conjeturado por la máquina.
--
-- Ejemplo de sesión:
--    λ> juego
--    Piensa un numero entre el 1 y el 100.
--    Es 50? [mayor/menor/exacto] mayor
--    Es 75? [mayor/menor/exacto] menor
--    Es 62? [mayor/menor/exacto] mayor
--    Es 68? [mayor/menor/exacto] exacto
--    Fin del juego

juego :: IO ()
juego = do
  putStrLn "Piensa un numero entre el 1 y el 100."
  adivina 1 100
  putStrLn "Fin del juego"

adivina :: Int -> Int -> IO ()
adivina a b = do
  putStr ("Es " ++ show conjetura ++ "? [mayor/menor/exacto] ")
  s <- getLine
  case s of
    "mayor"  -> adivina (conjetura+1) b
    "menor"  -> adivina a (conjetura-1)
    "exacto" -> return ()
    _        -> adivina a b
    where
      conjetura = (a+b) `div` 2

-- ---------------------------------------------------------------------
-- § Adivina el humano                                                --
-- ---------------------------------------------------------------------

-- Descripción: En el segundo juego la máquina genera un número
-- aleatorio entre 1 y 100 y le pide al jugador humano que adivine el
-- número que ha pensado planteándole conjeturas a las que la máquina
-- responde con mayor, menor o exacto según que el número pensado sea
-- mayor, menor o igual que el número conjeturado por el jugador humano.

-- Ejemplo de sesión:
--    λ> juego2
--    Tienes que adivinar un numero entre 1 y 100
--    Escribe un numero: 50
--     es bajo.
--    Escribe un numero: 75
--     es alto.
--    Escribe un numero: 62
--     Exactamente

juego2 :: IO ()
juego2 = do
  n <- randomRIO (1::Int,100)
  putStrLn "Tienes que adivinar un numero entre 1 y 100"
  adivina' n

adivina' :: Int -> IO ()
adivina' n = do
  putStr "Escribe un numero: "
  c <- getLine
  let x = read c
  case compare x n of
    LT -> do putStrLn " es bajo."
             adivina' n
    GT -> do putStrLn " es alto."
             adivina' n
    EQ -> putStrLn " Exactamente"
