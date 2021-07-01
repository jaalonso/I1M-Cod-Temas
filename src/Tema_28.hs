-- Tema_28.hs:
-- Tema 28: Análisis de complejidad de algoritmos
-- José A. Alonso Jiménez https://jaalonso.github.com
-- =====================================================================

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Clases de complejidad
-- ---------------------------------------------------------------------

-- Nota: La función f pertenece a la clase de complejidad de g (es decir
-- f está en O(g)) si
--     limsup   |f(x)/g(x)| < inf
--     x -> inf

-- ---------------------------------------------------------------------
-- Reglas para las ecuaciones de coste                                --
-- ---------------------------------------------------------------------

-- Notación:
-- * T(e) es el coste de evaluar la expresión e.
-- * T_f  es el número de pasos de la función f.

-- Reglas de evaluación de costes:
-- + T(c) = 0, donde c es una constante
-- + T(v) = 0, donde v es una variable
-- + T(if a then b else c) = T(a) + (if a then T(b) else T(c))
-- + T(p a1 a2 ... an) = T(a1) + T(a2) + ... + T(an), donde p es primitiva
-- + T(f a1 a2 ... an) = T(a1) + T(a2) + ... + T(an) + (T_f a1 a2 ... an)

-- ---------------------------------------------------------------------
-- Ejemplo de coste constante O(1)                                    --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función
--    f :: Integer -> Integer
-- tal que (f n) es n*(n+1)/2. Por ejemplo,
--    f 5  ==  15
-- ---------------------------------------------------------------------

f :: Integer -> Integer
f n = n*(n+1) `div` 2

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Calcular el tiempo necesario para calcular las
-- expresiones (f n) para n en [1000,2000..8000].
-- ---------------------------------------------------------------------

{-
   n    | segs.
   -----+------
   1000 | 0.01
   2000 | 0.01
   3000 | 0.01
   4000 | 0.01
   5000 | 0.01
   6000 | 0.01
   7000 | 0.01
   8000 | 0.01
-}

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. A la vista del ejercicio anterior, ¿qué relación hay
-- entre el aumento de n y el del tiempo de cálculo de (suma n)?
-- ---------------------------------------------------------------------

-- Hay una relación constante. Aproximadamente,
--    tiempo(f n) = 0.01

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Escribir las ecuaciones de recurrencia del coste de
-- la función f.
-- ---------------------------------------------------------------------

{-
 T(n) = 1
-}

-- ---------------------------------------------------------------------
-- Ejercicio 2.5. Una función es de complejidad constante si T(f)
-- pertence a la clase O(1). Demostrar que la función f es de
-- complejidad constante.
-- ---------------------------------------------------------------------

{-
 Demostración:
    T(f) = 1
    limpsup |1/1| = 1 < inf
-}

-- ---------------------------------------------------------------------
-- Ejemplo de complejidad lineal O(n)                              --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir por recursión la función
--    suma :: Integer -> Integer
-- tal que (suma n) es la suma de los números de 1 hasta n. Por ejemplo,
--    suma 5  ==  15
-- ---------------------------------------------------------------------

suma :: Integer -> Integer
suma 0 = 0
suma n = n + suma (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Calcular el tiempo necesario para calcular las
-- expresiones (suma n) para n en [1000,2000..8000].
-- ---------------------------------------------------------------------

{-
   n    | segs
   -----+------
   1000 | 0.02
   2000 | 0.03
   3000 | 0.04
   4000 | 0.04
   5000 | 0.05
   6000 | 0.06
   7000 | 0.07
   8000 | 0.08
-}

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. A la vista del ejercicio anterior, ¿qué relación hay
-- entre el aumento de n y el del tiempo de cálculo de (suma n)?
-- ---------------------------------------------------------------------

-- Hay una relación lineal. Aproximadamente,
--    tiempo(suma n) = n * 0.00001

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Escribir las ecuaciones de recurrencia del coste de
-- la función suma.
-- ---------------------------------------------------------------------

{-
 T(1)   = 1
 T(n+1) = 1 + T(n)
-}

-- ---------------------------------------------------------------------
-- Ejercicio 2.5. Demostrar que el coste de la función suma es T(n) = n.
-- ---------------------------------------------------------------------

{-
 Demostración: Basta comprobar que T(n) = n cumple las ecuaciones del
 coste de la función suma.

 T(1) = 1
 T(n+1) = n+1
        = 1+n
        = 1+T(n)
-}

-- ---------------------------------------------------------------------
-- Ejercicio 2.6. Una función es de complejidad lineal si T(n) pertenece
-- a O(n). Demostrar que la función suma es de complejidad lineal.
-- ---------------------------------------------------------------------

{-
 Demostración:
    limsup |T(n)/n| = limsup |n/n| = 1 < inf
-}

-- ---------------------------------------------------------------------
-- Ejercicio 2.7. Comprobar con QuickCheck que las funciones suma y f
-- son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_suma :: Integer -> Property
prop_suma n =
   n >= 0 ==> suma n == f n

-- La comprobación es
--    *Main> quickCheck prop_suma
--    OK, passed 100

-- ---------------------------------------------------------------------
-- Ejemplo de complejidad cuadrática O(n^2)                           --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir por recursión la función
--    sumaSuma :: Integer -> Integer
-- tal que (sumaSuma n) es la suma de las sumas de 0 a n; es decir,
--    sumaSuma n = (suma 0) + (suma 1) + ... + (suma n)
-- Por ejemplo,
--    sumaSuma 5  ==  35
-- ---------------------------------------------------------------------

sumaSuma :: Integer -> Integer
sumaSuma 0 = 0
sumaSuma n = suma n + sumaSuma (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Calcular el tiempo necesario para calcular las
-- expresiones (sumaSuma n) para n en [100,200..600].
-- ---------------------------------------------------------------------

{-
   n   | segs
   ----+-----
   100 | 0.06
   200 | 0.17
   300 | 0.35
   400 | 0.62
   500 | 0.96
   600 | 1.37
-}

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Calcular con Maxima el polinomio de interpolación para
-- los valores de 100, 200 y 300 y comprobarlo con los restantes
-- valores.
-- ---------------------------------------------------------------------

-- La interpolación con Maxima es
--    (%i1) load(interpol)$
--    (%i2) p_sumaSuma : [[100,0.06],[200,0.17],[300,0.35]]$
--    (%i3) lagrange(p_sumaSuma)$
--    (%i4) expand(%);
--    3.4999999999999953*10^-6*x^2+5.0000000000001432*10^-5*x+0.02

-- Por tanto, el polinomio es
t_sumaSuma n = 3.5e-6*n^2+5e-5*n+0.02

-- La comprobación es
--    λ> [(n,t_sumaSuma n) | n <- [100,200..600]]
--    [(100,06),(200,0.17),(300,0.35),(400,0.6),(500,0.92),(600,1.31)]

-- ---------------------------------------------------------------------
-- Ejercicio 3.4. Escribir las ecuaciones de recurrencia del coste de
-- la función sumaSuma.
-- ---------------------------------------------------------------------

{-
 T(1)   = 1
 T(n+1) = T(suma(n+1))+T(n)
        = n+1+T(n)
-}

-- ---------------------------------------------------------------------
-- Ejercicio 3.5. Demostrar por inducción que para la función sumaSuma,
--    T(n) <= n^2.
-- ---------------------------------------------------------------------

{-
 Demostración:

 Caso base:
    T(1) = 1 <= 1^2

 Caso inductivo: Suponemos la hipótesis de inducción
     T(n) <= n^2.
 Hay que demostrar que
     T(n+) <= (n+1)^2.
 En efecto,
    T(n+1)
    =  n+1+T(n)     [por coste de sumaSuma.2]
    <= n+1+n^2      [por hip. de inducción]
    <= n^2+2*n+1    [por álgebra]
    = (n+1)^2       [por álgebra]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 3.6. Una función es de complejidad cuadrática si T(n)
-- pertenece a O(n^2). Demostrar que la función sumaSuma es de
-- complejidad cuadrática.
-- ---------------------------------------------------------------------

{-
 Demostración
    limsup |T(n)/n^2| <= limsup |n^2/n^2| = 1 < inf
-}

-- ---------------------------------------------------------------------
-- Ejemplo de complejidad exponencial O(2^n)                           --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función
--    nEsimaAproximacion :: Float -> Float -> Int -> Float
-- tal (nEsimaAproximacion a x n) es el n-ésimo término de la sucesión
-- x_n definida por
--    x_0 = x
--    x_(n+1) = (a/x_n + x_n)/2
-- ---------------------------------------------------------------------

nEsimaAproximacion :: Float -> Float -> Int -> Float
nEsimaAproximacion a x 0     = x
nEsimaAproximacion a x (n+1) =
    (a / (nEsimaAproximacion a x n) + (nEsimaAproximacion a x n)) / 2.0

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Calcular el valor de las siguientes expresiones
--    nEsimaAproximacion  9 1  5
--    nEsimaAproximacion 16 1  5
--    nEsimaAproximacion 16 1 10
-- ---------------------------------------------------------------------

-- El cálculo es
--    λ> nEsimaAproximacion  9 1 5
--    3.0
--    λ> nEsimaAproximacion  16 1 5
--    4.0000005
--    λ> nEsimaAproximacion  16 1 10
--    4.0

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Definir el operador
--    (~=) :: Float -> Float -> Bool
-- tal que (x ~= y) si |x-y| < 0.001. Por ejemplo,
--    3.05 ~= 3.07        ==  False
--    3.00005 ~= 3.00007  == True
-- ---------------------------------------------------------------------

infix 5 ~=
(~=) :: Float -> Float -> Bool
x ~= y = abs(x-y) < 0.001

-- ---------------------------------------------------------------------
-- Ejecicio 6.4. Comprobar con QuickCheck que para todo número positivo
-- a, existe un número natural n tal que
--    (nEsimaAproximacion a 1 n)^2 ~= a
-- ---------------------------------------------------------------------

-- La propiedad es
prop_nEsimaAproximacion a =
    a >= 0 ==> or [(nEsimaAproximacion a 1 n)^2 ~= a | n <- [1..]]

-- La comprobación es
--    λ> quickCheck prop_nEsimaAproximacion
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.5. Demostrar que para todo a >= 0, el límite de la
-- sucesión
--    x_n = (nEsimaAproximacion a 1 n)
-- es la raíz cuadrada de a.
-- ---------------------------------------------------------------------

{-
 Demostración: Se tiene que
    x_(n+1) = (a/x_n + x_n)/2
 Sea y el límite de x_n. Entonces
    y = (a/y+y)/2
 Luego,
    2y = a/y+y
    y = a/y
    y^2 = a
    y = sqrt(a)
-}

-- ---------------------------------------------------------------------
-- Ejercicio 4.6. Calcular el tiempo y espacio necesario para calcular
-- las siguientes expresiones (nEsimaAproximacion 100 1 n) para n en
-- [14..19]
-- ---------------------------------------------------------------------

{-
   n  | segs  | bytes
   ---+-------+------------
   14 | 0.14  |   3.942.916
   15 | 0.27  |   7.577.012
   16 | 0.53  |  15.451.212
   17 | 1.04  |  31.200.484
   18 | 2.03  |  62.697.812
   19 | 4.08  | 125.084.900
   20 | 8.10  | 249.859.732
-}

-- ---------------------------------------------------------------------
-- Ejercicio 4.7. A la vista del ejercicio anterior, ¿qué relación hay
-- entre el aumento de n y el del tiempo y el espacio?
-- ---------------------------------------------------------------------

-- Por cada número que aumenta n se duplica el tiempo y el espacio. Por
-- tanto la relación es del orden 2^n.

-- ---------------------------------------------------------------------
-- Ejercicio 4.8. Escribir las ecuaciones de recurrencia del coste de
-- la función nEsimaAproximacion.
-- ---------------------------------------------------------------------

{-
 T(0) = 1
 T(n+1) = 2*T(n)
-}

-- ---------------------------------------------------------------------
-- Ejercicio 4.9. Demostrar por inducción que para la función sumaSuma,
--    T(n) = 2^n.
-- ---------------------------------------------------------------------

{-
 Demostración:
 Caso base:
    T(0) = 1 = 2^0

 Caso inductivo: Suponemos la hipótesis de inducción
     T(n) = 2^n.
 Hay que demostrar que
     T(n+1) = 2^(n+1).
 En efecto,
     T(n+1)
     = 2*T(n)   [por coste de nEsimaAproximacion]
     = 2*2^n    [por hip. de inducción]
     = 2^(n+1)  [por álgebra]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 4.10. Una función es de complejidad exponencial (de base 2)
-- si T(n) pertenece a O(2^n). Demostrar que la función
-- nEsimaAproximacion es de complejidad exponencial (de base 2).
-- ---------------------------------------------------------------------

{-
 Demostración
    limsup |T(n)/2^n| = limsup |2^n/2^n| = 1 < inf
-}

-- ---------------------------------------------------------------------
-- Ejemplo de complejidad logarítmica O(log n)                        --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir la función
--    potencia :: Integer -> Integer -> Integer
-- tal que (potencia x n) es x^n, usando las propiedades
--    x^n = (x*x)^(n/2),   si n es par
--    x^n = x*(x*x)^(n/2), si n es impar
-- ---------------------------------------------------------------------

potencia :: Integer -> Integer -> Integer
potencia x 0 = 1
potencia x n | even n    = potencia (x*x) (div n 2)
             | otherwise = x * potencia (x*x) (div n 2)

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Calcular el tiempo necesario para calcular
-- (potencia 2 n) para n [1024,2048,4096,8192,16384,32768].
-- ---------------------------------------------------------------------

{-
     n    | segs
     -----+-----
     1024 | 0.01
     2048 | 0.02
     4096 | 0.04
     8192 | 0.07
    16384 | 0.14
    32768 | 0.26
-}

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Escribir las ecuaciones de recurrencia del coste de
-- la función potencia.
-- ---------------------------------------------------------------------

{-
 T(1) = 1
 T(n) = 1 + T(n/2)
-}

-- ---------------------------------------------------------------------
-- Ejercicio 5.4. Demostrar por inducción que para la función sumaSuma,
--    T(n) = log n.
-- Nota. El logaritmo es en base 2.
-- ---------------------------------------------------------------------

{-
 Demostración:
 Caso base:
    T(1) = 1 <= log 1

 Caso inductivo:
    T(n)
    =  1 + T(n/2)
    <= 1 + log(n/2)
    =  1 + log n - 1
    = log n
-}

-- ---------------------------------------------------------------------
-- Complejidad de reverse                                             --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. La función inversa puede definirse por
--    reverse [] = []
--    reverse (x:xs) = reverse xs ++ [x]
-- Escribir las ecuaciones de recurrencia del coste de la función
-- reverse.
-- ---------------------------------------------------------------------

{-
 T_reverse(0)   = 1
 T_reverse(n+1) = T_++(n,1) + T_reverse(n) + 1
                = n + 1 + T_reverse(n)
-}

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Resolver con Maxima las ecuaciones de recurrencia del
-- coste de la función reverse y calcular la complejidad de la función
-- reverse.
-- ---------------------------------------------------------------------

-- La solución con Maxima es
--    (%i1) load("solve_rec")$
--    (%i2) solve_rec(t[n+1]=n+1+t[n],t[n]);
--    (%o2) t[n]=(n*(n+1))/2+%k[1]
--    (%i3) expand(%);
--    (%o3) t[n]=n^2/2+n/2+%k[1]

-- Por tanto, reverse es de O(n^2).
