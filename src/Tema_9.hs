-- Tema_9.hs
-- Tema 9: Declaración de tipos y clases.
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

module Tema_9 where

import Data.List (nub)

-- ---------------------------------------------------------------------
-- Declaraciones de tipos                                             --
-- ---------------------------------------------------------------------

-- Las posiciones son listas de números enteros.
type Pos = (Int,Int)

-- La listas de asociación son listas de pares formados por una clave y
-- un valor.
type Asoc c v = [(c,v)]

-- busca 2 [(1,'a'),(3,'d'),(2,'c')]  ==  'c'
busca :: Eq c => c -> Asoc c v -> v
busca c t = head [v | (c',v) <- t, c == c']

-- ---------------------------------------------------------------------
-- Declaraciones de datos                                             --
-- ---------------------------------------------------------------------

data Movimento = Izquierda | Derecha | Arriba | Abajo

-- (mueve m p) es la posición resultante de realizar el movimiento m en
-- la posición p. Por ejemplo,
--    mueve Izquierda (2,3)  ==  (1,3)
--    mueve Derecha   (2,3)  ==  (3,3)
--    mueve Arriba    (2,3)  ==  (2,4)
--    mueve Abajo     (2,3)  ==  (2,2)
mueve :: Movimento -> Pos -> Pos
mueve Izquierda (x,y) = (x-1,y)
mueve Derecha   (x,y) = (x+1,y)
mueve Arriba    (x,y) = (x,y+1)
mueve Abajo     (x,y) = (x,y-1)

-- (mueven ms p) es la posición resultante de realizar los movimientos
-- de la lista de mocimientos ms en la posición p. Por ejemplo,
--    mueven [Izquierda, Arriba] (2,3)  ==  (1,4)
mueven :: [Movimento] -> Pos -> Pos
mueven [] p     = p
mueven (m:ms) p = mueven ms (mueve m p)

-- (inverso m) es el movimiento inverso de m.
inverso :: Movimento -> Movimento
inverso Izquierda = Derecha
inverso Derecha   = Izquierda
inverso Arriba    = Abajo
inverso Abajo     = Arriba

data Figura = Circulo Float | Rect Float Float

--    :t Circulo  =>  Circulo :: Float -> Figura
--    :t Rect  =>  Rect :: Float -> Float -> Figura

-- (cuadrado n) es el cuadrado de lado n.
cuadrado :: Float -> Figura
cuadrado n = Rect n n

-- (area f) es el área de la figura f. Por ejemplo,
--    area (Circulo 1)  ==  3.1415927
--    area (Circulo 2)  ==  12.566371
--    area (Rect 2 5)   ==  10.0
area :: Figura -> Float
area (Circulo r) = pi*r^2
area (Rect x y)  = x*y

-- data Maybe a = Nothing | Just a

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

data Nat = Cero | Suc Nat
           deriving Show

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
-- Determinador de tautologías                                        --
-- ---------------------------------------------------------------------

-- Las fórmulas proposicionales se definen por:
--    * Las constantes booleanas son fórmulas proposicionales.
--    * Las fórmulas atómicas son fórmulas proposicionales.
--    * Si F es una fómula proposicional, entonces -F también los es.
--    * Si F y F son fórmulas proposicionales, entonces (F /\ G) y
--      (F -> G) también lo son.
data Prop = Const Bool
          | Var Char
          | Neg Prop
          | Conj Prop Prop
          | Impl Prop Prop
          deriving Show

-- Ejemplos de representación de fórmulas proposicionales: Las fórmulas
--    * p1 := A /\ -A
--    * p2 := (A /\ B) -> A
--    * p3 := A -> (A /\ B)
--    * p4 := (A -> (A -> B)) -> B
-- se representan por
p1, p2, p3, p4 :: Prop
p1 = Conj (Var 'A') (Neg (Var 'A'))
p2 = Impl (Conj (Var 'A') (Var 'B')) (Var 'A')
p3 = Impl (Var 'A') (Conj (Var 'A') (Var 'B'))
p4 = Impl (Conj (Var 'A') (Impl (Var 'A') (Var 'B'))) (Var 'B')

-- Las interpretaciones son listas formadas por el nombre de una
-- variable proposicional y un valor de verdad.
type Interpretacion = Asoc Char Bool

-- (valor i p) es el valor de la proposición p en la interpretación
-- i. Por ejemplo,
--    valor [('A',False),('B',True)] p3  ==  True
--    valor [('A',True),('B',False)] p3  ==  False
valor :: Interpretacion -> Prop -> Bool
valor _ (Const b)  = b
valor i (Var x)    = busca x i
valor i (Neg p)    = not (valor i p)
valor i (Conj p q) = valor i p && valor i q
valor i (Impl p q) = valor i p <= valor i q

-- (variables p) es la lista de los nombres de las variables de la
-- fórmula p. Por ejemplo,
--    variables p3  ==  "AAB"
variables :: Prop -> [Char]
variables (Const _)  = []
variables (Var x)    = [x]
variables (Neg p)    = variables p
variables (Conj p q) = variables p ++ variables q
variables (Impl p q) = variables p ++ variables q

-- (interpretacionesVar n) es la lista de las interpretaciones con n
-- variables. Por ejemplo,
--    *Main> interpretacionesVar 2
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
--    *Main> interpretaciones p3
--    [[('A',False),('B',False)],
--     [('A',False),('B',True)],
--     [('A',True),('B',False)],
--     [('A',True),('B',True)]]
interpretaciones :: Prop -> [Interpretacion]
interpretaciones p =
    map (zip vs) (interpretacionesVar (length vs))
    where vs = nub (variables p)

-- Una definición alternativa es
interpretaciones' :: Prop -> [Interpretacion]
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
esTautologia :: Prop -> Bool
esTautologia p = and [valor i p | i <- interpretaciones p]

-- ---------------------------------------------------------------------
-- Máquina abstracta                                                  --
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

{-
Ejemplo de evaluación:
  eval (Suma (Suma (Num 2) (Num 3)) (Num 4)) []
= eval (Suma (Num 2) (Num 3)) [EVAL (Num 4)]
= eval (Num 2) [EVAL (Num 3), EVAL (Num 4)]
= ejec [EVAL (Num 3), EVAL (Num 4)] 2
= eval (Num 3) [SUMA 2, EVAL (Num 4)]
= ejec [SUMA 2, EVAL (Num 4)] 3
= ejec [EVAL (Num 4)] (2+3)
= ejec [EVAL (Num 4)] 5
= eval (Num 4) [SUMA 5]
= ejec [SUMA 5] 4
= ejec [] (5+4)
= ejec [] 9
= 9
-}

-- ---------------------------------------------------------------------
-- Declaraciones de clases e instancias                               --
-- ---------------------------------------------------------------------

-- Declaración de la clase Eq en el Preludio:
--    class Eq a where
--        (==), (/=) :: a -> a -> Bool
--
--        -- Minimal complete definition: (==) or (/=)
--        x == y = not (x/=y)
--        x /= y = not (x==y)

-- Declaración de Bool instancia de Eq en el Preludio:
--    instance Eq Bool where
--        False == False = True
--        True  == True  = True
--        _     == _     = False

-- Declaración de Ord como superclase de Eq en el Preludio:
--    class (Eq a) => Ord a where
--        compare                :: a -> a -> Ordering
--        (<), (<=), (>=), (>)   :: a -> a -> Bool
--        max, min               :: a -> a -> a
--
--        -- Minimal complete definition: (<=) or compare
--        -- using compare can be more efficient for complex types
--        compare x y | x==y      = EQ
--                    | x<=y      = LT
--                    | otherwise = GT
--
--        x <= y                  = compare x y /= GT
--        x <  y                  = compare x y == LT
--        x >= y                  = compare x y /= LT
--        x >  y                  = compare x y == GT
--
--        max x y   | x <= y      = y
--                  | otherwise   = x
--        min x y   | x <= y      = x
--                  | otherwise   = y

-- Declaración de Bool instancia de Ord
-- instance Ord Bool where
--      False <= _     = True
--      True  <= True  = True
--      True  <= False = False

-- Clases derivadas
-- ----------------

-- Declaración de instancias de Bool mediante derivación.
--    data Bool = False | True
--    	    deriving (Eq, Ord, Read, Show)

-- Ejemplos:
--    False == False        ==  True
--    False < True          ==  True
--    show False            ==  "False"
--    read "False" :: Bool  ==  False

-- Tipos monádicos.
-- ----------------

-- Declaración de la clase de las mónadas en el Preludio:
--    class Monad m where
--        return :: a -> m a
--        (>>=)  :: m a -> (a -> m b) -> m b
--        (>>)   :: m a -> m b -> m b
--        fail   :: String -> m a
--
--        -- Minimal complete definition: (>>=), return
--        p >> q  = p >>= \ _ -> q
--        fail s  = error s
