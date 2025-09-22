-- funciones para que se vea mas choncho xD
sumita :: Int -> Int -> Int
sumita x 0 = x
sumita x y = sumita (x + 1) (y - 1)

sumitaCon1 :: Int -> Int -> Int
sumitaCon1 x y = 1 + sumita x y

-- Naturales
data Natural = Cero | S Natural deriving Show

a_natural :: Int -> Natural
a_natural 0 = Cero
a_natural x = S(a_natural $ x - 1)

a_entero :: Natural -> Int
a_entero Cero = 0
a_entero (S x) = 1 + a_entero x

suma_nat :: Natural -> Natural -> Natural
suma_nat Cero n = n
suma_nat (S n) m = S $ suma_nat n m

mult_nat :: Natural -> Natural -> Natural
mult_nat _ Cero = Cero
mult_nat n (S m) = suma_nat n (mult_nat n m)

-- Recursión
fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n - 1) + fibo (n - 2)

multiplicacion :: Int -> Int -> Int
multiplicacion _ 0 = 0
multiplicacion n m = n + multiplicacion n (m - 1)

potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia n m = n * potencia n (m - 1)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

divishon :: (Int, Int) -> (Int, Int)
divishon (a, b)
    | a < b = (0, a)
    | otherwise =
        let (q, r) = divishon (a - b, b)
        in (q + 1, r)

maximo :: [Int] -> Int
maximo [a] = a
maximo (x:xs)
    | x > mayorResto = x
    | otherwise = mayorResto
    where
        mayorResto = maximo xs

-- Árboles
data ArbolB a = Vacio | Nodo a (ArbolB a) (ArbolB a) deriving (Eq, Show)

hoja :: a -> ArbolB a
hoja x = Nodo x Vacio Vacio

numeroNodos :: ArbolB a -> Int
numeroNodos Vacio = 0
numeroNodos (Nodo _ i d) = sumitaCon1 (numeroNodos i) (numeroNodos d)

profundidad :: ArbolB a -> Int
profundidad Vacio = 0
profundidad (Nodo _ i d) = 1 + max (profundidad i) (profundidad d)
