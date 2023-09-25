--Practica 5

-- Ejercicio 1

--1)
longuitudDeUnaLista :: [a] -> Int
longuitudDeUnaLista (x:xs) = length (x:xs)

--2)
ultimoDeUnaLista :: [t] -> t
ultimoDeUnaLista [x] = x
ultimoDeUnaLista (x:xs) = ultimoDeUnaLista xs

--3)
principioDeUnaLista :: [t] -> [t]
principioDeUnaLista [x] = []
principioDeUnaLista (x:xs) = x: principioDeUnaLista xs

--4)
reversoDeUnaLista :: [t] -> [t]
reversoDeUnaLista [x] = [x]
reversoDeUnaLista (x:xs) = ultimoDeUnaLista (x:xs) : reversoDeUnaLista (principioDeUnaLista (x:xs))
-- Ejercicio 2

--1)
pertenece :: Eq t => t -> [t] -> Bool
pertenece _ [] = False
pertenece n (x:xs)  | n == x = True
                    | n /= x = pertenece n xs

--2)
todosIguales :: Eq t => [t] -> Bool
todosIguales [x] = True
todosIguales (x:y:xs) | x == y = todosIguales (x:xs)
                      | x /= y = False

--3)
todosDistintos :: Eq t => [t] -> Bool
todosDistintos [x] = True
todosDistintos (x:y:xs) | x /= y = todosDistintos (y:xs)
                        | x == y = False

--4)
hayRepetidos :: [Integer] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | pertenceALaLista x xs = True
                    | otherwise = hayRepetidos xs
    where
        pertenceALaLista :: Integer -> [Integer] -> Bool
        pertenceALaLista n [] = False
        pertenceALaLista n (x:xs) | n == x = True
                                  | otherwise = pertenceALaLista n xs

--5)
quitarPrimeraAparicion :: Eq t => t -> [t] -> [t]
quitarPrimeraAparicion _ [_] = []
quitarPrimeraAparicion n (x:xs) | n == x = xs
                                | otherwise = quitarPrimeraAparicion n xs

--6)
quitarTodasLasApariciones :: Eq t => t -> [t] -> [t]
quitarTodasLasApariciones n (x:xs) | n /=x && xs == [] = [x]
                                   | n==x && xs == [] =[]
                                   | n == x = quitarTodasLasApariciones n xs
                                   | otherwise = x : quitarTodasLasApariciones n xs

--7)
eliminarRepetidos :: Eq t => [t] -> [t]
eliminarRepetidos [n] = [n]
eliminarRepetidos (x:xs) | esIgual x xs = eliminarRepetidos xs
                         | otherwise = x : eliminarRepetidos xs

esIgual :: Eq t => t -> [t] -> Bool
esIgual _ [] = False
esIgual n (y:xs) | n == y = True
                 | otherwise = esIgual n xs

--8)
mismosElementos :: Eq t => [t] -> [t] -> Bool
mismosElementos (x:xs) (j:js) | (x:xs) == (j:js) = True
                              | (x:xs) == [] && (j:js) == [] = True
                              | esIgual x (j:js) = mismosElementos xs (j:js)
                              | esIgual j (x:xs) = mismosElementos js (x:xs)
                              | otherwise = False


-- Ejercicio 3
--3)
maximoDeUnaLista :: [Integer] -> Integer
maximoDeUnaLista [x] = x
maximoDeUnaLista (x:y:xs) | x > y = maximoDeUnaLista (x:xs)
                          | otherwise = maximoDeUnaLista (y:xs)

    --9)
ordenarListaCrecientemente :: [Integer] -> [Integer]
ordenarListaCrecientemente [x] = [x]
ordenarListaCrecientemente (x:xs) | x >= maximoDeUnaLista xs = (ordenarListaCrecientemente xs) ++ [x]
                                  | otherwise = ordenarListaCrecientemente (xs ++ [x])

--5)
    --2)
descomponerNumeroEnPrimos :: [Integer] -> [[Integer]]
descomponerNumeroEnPrimos [] = []
descomponerNumeroEnPrimos (x:xs) = (damePrimos x):(descomponerNumeroEnPrimos xs)
    where
        damePrimos :: Integer -> [Integer]
        damePrimos n | menorDivisor n == n = [n]
                     | otherwise = (menorDivisor n) :(damePrimos(div n(menorDivisor n)))

menorDivisor :: Integer -> Integer
menorDivisor x = menorDivisorAux x 2
    where
        menorDivisorAux :: Integer -> Integer -> Integer
        menorDivisorAux x y | y == x = x
                            | mod x y == 0 = y
                            | mod x y /= 0 = menorDivisorAux x (y+1)
