doubleMe :: Float -> Float 
doubleMe x = x + x

--Ejercicio 1
--a)
--f :: Int -> Int
--f n | n==1 =8
--    | n==4 =131
--    | n==16 =16
--b)
--g :: Int -> Int
--g n | n==8 =16
--    | n==16 =4
--    | n==131 =1
--c) h = f o g
--h :: Int -> Int
--h x = f(g x)
--c) k = g o f
--k :: Int -> Int
--k x = g(f x)

--Ejercicio 2

--a) Absoluto

absoluto :: Int -> Int
absoluto x | x<0 = -x
           | otherwise =x

--b) maximoAbsoluto
maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y | absX > absY = absX
                   | otherwise = absY
            where absX = absoluto x
                  absY = absoluto y

--c) maximo3
maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | x > y && x > z = x
              | y > x && y > z = y 
              | otherwise =z 

--d)algunoEs0 hecho con pattern matching
algunoEs0 :: Float -> Float -> Bool 
algunoEs0 0 _ = True -- "_" significa que no me importa el valor de esa variable 
algunoEs0 _ 0 = True
algunoEs0 _ _ = False

--d)algunoEs0 hecho con logica
algunoEs0v2 :: Float -> Float -> Bool 
algunoEs0v2 x y = x ==0 || y==0

--e)ambosSon0 con patter matching
ambosSon0 :: Float -> Float -> Bool
ambosSon0 0 0 = True
ambosSon0 _ _= False

--e)ambosSon0 con logica
ambosSon0v2 :: Float -> Float -> Bool
ambosSon0v2 x y = x==0 && y==0

--f)mismoIntervalo
mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo x y | x<=3 && y<=3 =True
                   | x>3 && y>3 && x<=7 && y<=7 = True 
                   | x>7 && y>7 = True
                   | otherwise =False

--g)sumaDistintos
sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos x y z | x/=y && y/=z && x/=z = x + y +z
                    | x == y && x==z = 0
                    | y == z =x
                    | x == z =y
                    | otherwise =z

--h)esMultiploDe
esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = mod x y == 0

--i)digitoUnidades
digitoUnidades :: Int -> Int
digitoUnidades x = mod (absoluto x) 10

--j)digitoDecenas
digitoDecenas :: Int -> Int
digitoDecenas x = digitoDecenas (div x 10)

estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados x y | mod x y == 0 =True
                      | otherwise =False

--ej 4)
--a)prodInt
prodInt :: (Float,Float) -> (Float,Float) -> Float
prodInt (a,b) (c,d) = (a*b) * (c *d)

--b)todoMenor
todoMenor :: (Float,Float) -> (Float,Float) -> Bool
todoMenor (a,b) (c,d) = a < c && b < d

--c)distanciaPuntos
distanciaPuntos :: (Float,Float) -> Float
distanciaPuntos (x,y) = abs (x - y)

--d)sumaTerna
sumaTerna :: (Int,Int,Int) -> Int
sumaTerna (a,b,c) = a + b + c

--5)todosMenores
todosMenores :: (Int, Int, Int) ->Bool
todosMenores (a,b,c) = (fdos(a) > gdos(a)) && (fdos(b) > gdos(b)) && (fdos(c)>gdos(c))

fdos :: Int -> Int
fdos n | n<7 = n*n 
    | otherwise = 2 * n -1

gdos :: Int -> Int
gdos x | mod x 2 ==0 =div x 2
    | otherwise = 3 *x +1

fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

--Sumatoria Doble
sumatoriaDoble :: Integer -> Integer -> Integer
sumatoriaDoble _ 0 =0
sumatoriaDoble n m = sumatoriaInterna (n-1) m

sumatoriaInterna :: Integer -> Integer -> Integer
sumatoriaInterna _ 0 =0
sumatoriaInterna n m =n^m + sumatoriaInterna n (m - 1)

sacarEspaciosRepetidos :: [Char] -> [Char]
sacarEspaciosRepetidos [] = [] 
sacarEspaciosRepetidos [x] = [x]
sacarEspaciosRepetidos (x:y:xs) | (x==y && x==' ') = sacarEspaciosRepetidos (y:xs)
                                | otherwise = x:(sacarEspaciosRepetidos (y:xs))

