-- Tema_9.hs
-- Tema 9: Declaración de tipos y clases.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_9 where

import Data.List (nub)

-- ---------------------------------------------------------------------
-- Declaraciones de tipos                                             --
-- ---------------------------------------------------------------------

-- Las posiciones son pares de números enteros.
type Pos = (Int,Int)

-- (izquierda p)~ es la posición a la izquierda de la posición p. Por
-- ejemplo,
--    izquierda (3,5)  ==  (2,5)
izquierda :: Pos -> Pos
izquierda (x,y) = (x-1,y)

-- (Par a) es el tipo de pares de elementos de tipo a:
type Par a = (a,a)

-- (multiplica p) es el producto del par de enteros p. Por ejemplo,
--     multiplica (2,5)  ==  10
multiplica :: Par Int -> Int
multiplica (x,y) = x*y

-- (copia x) es el par formado con dos copias de x. Por ejemplo,
--    copia 5  ==  (5,5)
copia :: a -> Par a
copia x = (x,x)

-- ---------------------------------------------------------------------
-- Declaraciones de datos                                             --
-- ---------------------------------------------------------------------

-- Definición del tipo de movimientos:
data Mov = Izquierda | Derecha | Arriba | Abajo

-- (movimiento m p) es la posición resultante de realizar el movimiento m en
-- la posición p. Por ejemplo,
--    movimiento Izquierda (2,3)  ==  (1,3)
--    movimiento Derecha   (2,3)  ==  (3,3)
--    movimiento Arriba    (2,3)  ==  (2,4)
--    movimiento Abajo     (2,3)  ==  (2,2)
movimiento :: Mov -> Pos -> Pos
movimiento Izquierda (x,y) = (x-1,y)
movimiento Derecha   (x,y) = (x+1,y)
movimiento Arriba    (x,y) = (x,y+1)
movimiento Abajo     (x,y) = (x,y-1)

-- (movimientos ms p) es la posición resultante de realizar los movimientos
-- de la lista de mocimientos ms en la posición p. Por ejemplo,
--    movimientos [Izquierda, Arriba] (2,3)  ==  (1,4)
movimientos :: [Mov] -> Pos -> Pos
movimientos [] p     = p
movimientos (m:ms) p = movimientos ms (movimiento m p)

-- (opuesto m) es el movimiento opuesto de m.
opuesto :: Mov -> Mov
opuesto Izquierda = Derecha
opuesto Derecha   = Izquierda
opuesto Arriba    = Abajo
opuesto Abajo     = Arriba

-- Una figura es un círculo con su radio o un rectángulo son su base y
-- su altura.
data Figura = Circulo Float | Rect Float Float
  deriving (Eq, Show)

-- (cuadrado n) es el cuadrado de lado n. Por ejemplo,
--    cuadrado 3  ==  Rect 3 3
cuadrado :: Float -> Figura
cuadrado n = Rect n n

-- (area f) es el área de la figura f. Por ejemplo,
--    area (Circulo 1)  ==  3.1415927
--    area (Circulo 2)  ==  12.566371
--    area (Rect 2 5)   ==  10.0
--    area (cuadrado 3) ==   9
area :: Figura -> Float
area (Circulo r) = pi*r^2
area (Rect x y)  = x*y

-- (divisionSegura m n) es la división de m entre n si n no es cero y
-- fallo en caso contrario. Por ejemplo,
--    divisionSegura 6 3  ==  Just 2
--    divisionSegura 6 0  ==  Nothing
divisionSegura :: Int -> Int -> Maybe Int
divisionSegura _ 0 = Nothing
divisionSegura m n = Just (m `div` n)

-- (headSegura xs) es la cabeza de xs si xs es no vacía y fallo en caso
-- contrario. Por ejemplo,
--    headSegura [2,3,5]  ==  Just 2
--    headSegura []       ==  Nothing
headSegura :: [a] -> Maybe a
headSegura [] = Nothing
headSegura xs = Just (head xs)

-- ---------------------------------------------------------------------
-- Tipos recursivos                                                   --
-- ---------------------------------------------------------------------

-- Los naturales se construyen con el cero y la función sucesor.
data Nat = Cero | Suc Nat
  deriving (Eq, Show)

-- (nat2int n) es el número entero correspondiente al número natural
-- n. Por ejemplo,
--    nat2int (Suc (Suc (Suc Cero)))  ==  3
nat2int :: Nat -> Int
nat2int Cero    = 0
nat2int (Suc n) = 1 + nat2int n

-- (int2nat n) es el número natural correspondiente al número entero
-- n. Por ejemplo,
--    int2nat 3  ==  Suc (Suc (Suc Cero))
int2nat :: Int -> Nat
int2nat 0 = Cero
int2nat n = Suc (int2nat (n-1))

-- (suma m n) es la suma de los número naturales m y n. Por ejemplo,
--    suma (Suc (Suc Cero)) (Suc Cero)  ==  Suc (Suc (Suc Cero))
suma :: Nat -> Nat -> Nat
suma Cero    n = n
suma (Suc m) n = Suc (suma m n)

-- (Lista a) es el tipo de las lista de elementos de tipo a.
data Lista a = Nil | Cons a (Lista a)

-- (longitud xs) es la longitud de la lista xs. Por ejemplo,
--    longitud (Cons 2 (Cons 3 (Cons 5 Nil)))  ==  3
longitud :: Lista a -> Int
longitud Nil         = 0
longitud (Cons _ xs) = 1 + longitud xs

-- Arbol es el tipo de los árboles binarios sobre los enteros.
data Arbol = Hoja Int | Nodo Arbol Int Arbol

-- El árbol binario
--         5
--        / \
--       /   \
--      3     7
--     / \   / \
--    1   4 6   9
-- se define por
ejArbol :: Arbol
ejArbol = Nodo (Nodo (Hoja 1)
                     3
                     (Hoja 4))
               5
               (Nodo (Hoja 6)
                     7
                     (Hoja 9))

-- (ocurre m a) se verifica si m ocurre en el árbol a. Por ejemplo,
--    ocurre 4  ejArbol  ==  True
--    ocurre 10 ejArbol  ==  False
ocurre :: Int -> Arbol -> Bool
ocurre m (Hoja n)     = m == n
ocurre m (Nodo i n d) = m == n || ocurre m i || ocurre m d

-- (aplana a) es la lista obtenida aplanando el árbol a. Por ejemplo,
--    aplana ejArbol  ==  [1,3,4,5,6,7,9]
aplana :: Arbol -> [Int]
aplana (Hoja n)     = [n]
aplana (Nodo i n d) = aplana i ++ [n] ++ aplana d

-- (ocurreEnArbolOrdenado m a) se verifica si m ocurre en el árbol
-- ordenado a. Por ejemplo,
--    ocurreEnArbolOrdenado 4 ejArbol   ==  True
--    ocurreEnArbolOrdenado 10 ejArbol  ==  False
ocurreEnArbolOrdenado :: Int -> Arbol -> Bool
ocurreEnArbolOrdenado m (Hoja n)  =  m == n
ocurreEnArbolOrdenado m (Nodo i n d)
     | m == n      =  True
     | m < n       =  ocurreEnArbolOrdenado m i
     | otherwise   =  ocurreEnArbolOrdenado m d

-- data Arbol a   = Hoja a | Nodo (Arbol a) (Arbol a)
-- data Arbol a   = Hoja | Nodo (Arbol a) a (Arbol a)
-- data Arbol a b = Hoja a | Nodo (Arbol a b) b (Arbol a b)
-- data Arbol a   = Nodo a [Arbol a]

-- ---------------------------------------------------------------------
-- Sistema de decisión de tautologías                                 --
-- ---------------------------------------------------------------------

-- Las fórmulas proposicionales se definen por:
--    * Las constantes booleanas son fórmulas proposicionales.
--    * Las fórmulas atómicas son fórmulas proposicionales.
--    * Si F es una fómula proposicional, entonces -F también los es.
--    * Si F y F son fórmulas proposicionales, entonces (F /\ G) y
--      (F -> G) también lo son.
data FProp = Const Bool
           | Var Char
           | Neg FProp
           | Conj FProp FProp
           | Impl FProp FProp
  deriving Show

-- Ejemplos de representación de fórmulas proposicionales: Las fórmulas
--    * p1 := A /\ -A
--    * p2 := (A /\ B) -> A
--    * p3 := A -> (A /\ B)
--    * p4 := (A -> (A -> B)) -> B
-- se representan por
p1, p2, p3, p4 :: FProp
p1 = Conj (Var 'A') (Neg (Var 'A'))
p2 = Impl (Conj (Var 'A') (Var 'B')) (Var 'A')
p3 = Impl (Var 'A') (Conj (Var 'A') (Var 'B'))
p4 = Impl (Conj (Var 'A') (Impl (Var 'A') (Var 'B'))) (Var 'B')

-- Las interpretaciones son listas formadas por el nombre de una
-- variable proposicional y un valor de verdad.
type Interpretacion = [(Char, Bool)]

-- (valor i p) es el valor de la proposición p en la interpretación
-- i. Por ejemplo,
--    valor [('A',False),('B',True)] p3  ==  True
--    valor [('A',True),('B',False)] p3  ==  False
valor :: Interpretacion -> FProp -> Bool
valor _ (Const b)  = b
valor i (Var x)    = busca x i
valor i (Neg p)    = not (valor i p)
valor i (Conj p q) = valor i p && valor i q
valor i (Impl p q) = valor i p <= valor i q

-- (busca c t) es el valor del primer par de t cuya clave es igual a
-- c. Por ejemplo,
--    busca 2 [(1,'a'),(3,'d'),(2,'c')]  ==  'c'
busca :: Eq c => c -> [(c,v)] -> v
busca c t = head [v | (c',v) <- t, c == c']

-- (variables p) es la lista de los nombres de las variables de la
-- fórmula p. Por ejemplo,
--    variables p3  ==  "AAB"
variables :: FProp -> [Char]
variables (Const _)  = []
variables (Var x)    = [x]
variables (Neg p)    = variables p
variables (Conj p q) = variables p ++ variables q
variables (Impl p q) = variables p ++ variables q

-- (interpretacionesVar n) es la lista de las interpretaciones con n
-- variables. Por ejemplo,
--    λ> interpretacionesVar 2
--    [[False,False],
--     [False,True],
--     [True,False],
--     [True,True]]
interpretacionesVar :: Int -> [[Bool]]
interpretacionesVar 0 = [[]]
interpretacionesVar n = map (False:) bss ++ map (True:) bss
  where bss = interpretacionesVar (n-1)

-- (interpretaciones p) es la lista de las interpretaciones de la
-- fórmula p. Por ejemplo,
--    λ> interpretaciones p3
--    [[('A',False),('B',False)],
--     [('A',False),('B',True)],
--     [('A',True),('B',False)],
--     [('A',True),('B',True)]]
interpretaciones :: FProp -> [Interpretacion]
interpretaciones p =
  map (zip vs) (interpretacionesVar (length vs))
  where vs = nub (variables p)

-- Una definición alternativa es
interpretaciones' :: FProp -> [Interpretacion]
interpretaciones' p =
  [zip vs i | i <- is]
  where vs = nub (variables p)
        is = (interpretacionesVar (length vs))

-- (esTautologia p) se verifica si la fórmula p es una tautología. Por
-- ejemplo,
--    esTautologia p1  ==  False
--    esTautologia p2  ==  True
--    esTautologia p3  ==  False
--    esTautologia p4  ==  True
esTautologia :: FProp -> Bool
esTautologia p = and [valor i p | i <- interpretaciones p]

-- ---------------------------------------------------------------------
-- Máquina abstracta de cálculo aritmético                            --
-- ---------------------------------------------------------------------

-- Una expresión es un número entero o la suma de dos expresiones.
data Expr =  Num Int | Suma Expr Expr

-- (valorEA x) es el valor de la expresión aritmética x. Por ejemplo,
--    valorEA (Suma (Suma (Num 2) (Num 3)) (Num 4))  ==  9
valorEA :: Expr -> Int
valorEA (Num n)    = n
valorEA (Suma x y) = valorEA x + valorEA y

-- La pila de control de la máquina abstracta es una lista de operaciones.
type PControl = [Op]

-- La operaciones son meter un número en la pila o sumar un número con
-- el primero de la pila.
data Op = METE Expr | SUMA Int

-- (eval x p) evalúa la expresión x con la pila de control p. Por
-- ejemplo,
--    eval (Suma (Suma (Num 2) (Num 3)) (Num 4)) []  ==  9
--    eval (Suma (Num 2) (Num 3)) [METE (Num 4)]     ==  9
--    eval (Num 3) [SUMA 2, METE (Num 4)]            ==  9
--    eval (Num 4) [SUMA 5]                          ==  9
eval :: Expr -> PControl -> Int
eval (Num n)    p = ejec p n
eval (Suma x y) p = eval x (METE y : p)

-- (ejec p n) ejecuta la lila de control p sobre el entero n. Por
-- ejemplo,
--    ejec [METE (Num 3), METE (Num 4)] 2  ==  9
--    ejec [SUMA 2, METE (Num 4)]       3  ==  9
--    ejec [METE (Num 4)]               5  ==  9
--    ejec [SUMA 5]                     4  ==  9
--    ejec []                           9  ==  9
ejec :: PControl -> Int -> Int
ejec []           n = n
ejec (METE y : p) n = eval y (SUMA n : p)
ejec (SUMA n : p) m = ejec p (n+m)

-- (evalua e) evalua la expresión e con la máquina abstracta. Por
-- ejemplo,
--    evalua (Suma (Suma (Num 2) (Num 3)) (Num 4))  ==  9
evalua :: Expr -> Int
evalua e = eval e []
