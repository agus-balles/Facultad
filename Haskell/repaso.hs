eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs) | pertenece x xs = eliminarRepetidos (xs)
                         | otherwise = x: eliminarRepetidos xs

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece x (j:js) | x == j = True
                   | otherwise = pertenece x js

sumatoriaDoble :: Integer -> Integer -> Integer 
sumatoriaDoble 1 m = sumatoriaInterna 1 m
sumatoriaDoble n m = sumatoriaInterna n m+ sumatoriaDoble (n-1) m

sumatoriaInterna :: Integer -> Integer -> Integer
sumatoriaInterna q 1 = q
sumatoriaInterna 0 _ = 0
sumatoriaInterna q m = q^m + sumatoriaInterna q (m-1)

productoria :: [Integer] -> Integer
productoria [] = 1
productoria [x] = x
productoria (x:xs) = x * productoria xs

