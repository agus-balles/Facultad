import Distribution.Simple.Utils (xargs)
longitud :: [t] -> Integer
longitud [] = 0
longitud [x] = 1
longitud (x:xs) = 1 + longitud xs

ultimo :: [t] -> t 
ultimo [x] = x 
ultimo (x:xs) = ultimo xs

principio :: [t] -> [t]
principio [x] = []
principio (x:xs) = [x] ++ principio xs

reverso :: [t] -> [t]
reverso [x] = [x]
reverso (x:xs) = reverso xs ++ [x]

------------------------------------------------------------------------------

pertenece :: (Eq t) => t -> [t] -> Bool 
pertenece _ [] = False
pertenece j (x:xs) | j == x = True
                   | otherwise =pertenece j xs

todosIguales :: (Eq t) => [t] -> Bool
todosIguales [x] =True
todosIguales (x:y:xs) | x == y = todosIguales (y:xs)
                      | otherwise = False
                    
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [x] = True
todosDistintos (x:xs) | pertenece x xs = False
                      | otherwise = todosDistintos xs 

hayRepetidos :: (Eq t) => [t] -> Bool 
hayRepetidos [x] = False
hayRepetidos (x:xs) | pertenece x xs = True
                    | otherwise = hayRepetidos xs

quitar :: (Eq t) => t -> [t] -> [t]
quitar x (j:js) | not (pertenece x (j:js)) =(j:js)
                | x == j = js
                | otherwise = [j] ++ (quitar x js)

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos x (j:js) | not (pertenece x (j:js)) =(j:js)
                     | x == j =quitarTodos x js
                     | otherwise =[j] ++ quitarTodos x js

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs) | pertenece x xs = eliminarRepetidos xs 
                         | otherwise = [x] ++ eliminarRepetidos xs 

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos _ [] = False
mismosElementos [] _ =False
mismosElementos (x:xs) (j:js) = mismaLista (eliminarRepetidos (x:xs)) (eliminarRepetidos (j:js))

mismaLista :: (Eq t) => [t] -> [t] -> Bool
mismaLista [x] [j] = x == j 
mismaLista (x:xs) (j:js) | pertenece x (j:js) = mismaLista xs (quitar x (j:js))
                         | otherwise = False

-----------------------------------------------------------------------------------------------------------------------------------------------------------------

maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:y:xs) | x >= y = maximo (x:xs)
                | otherwise = maximo (y:xs)

sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ [] = []
sumarN x [j] = [x+j]
sumarN x (j:js) = [j+x] ++ (sumarN x js)

sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (j:js) = sumarN j (j:js)

sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo (j:js) = sumarN (ultimo (j:js)) (j:js)

pares :: [Integer] -> [Integer]
pares [] = []
pares [j] | mod j 2 == 0 = [j]
          | otherwise = []
pares (j:js) | mod j 2 == 0 = [j] ++ pares js
             | otherwise = pares js

multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN n [x] | mod x n == 0 = [x]
multiplosDeN n (x:xs) | mod x n == 0 = [x] ++ multiplosDeN n xs
                      | otherwise = multiplosDeN n xs

ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar [x] = [x]
ordenar (x:xs) = (ordenar (quitarTodos x xs)) ++ [(maximo (x:xs))]

---------------------------------------------------------------------------------------------------------
--1)
sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x:y:xs) | x == ' ' && x==y = sacarBlancosRepetidos (y:xs)
                               | otherwise = [x] ++ sacarBlancosRepetidos (y:xs)
--2)
contarPalabras :: [Char] -> Integer
contarPalabras (x:xs) = cuantasPalabras (eliminarEspaciosIniFin (sacarBlancosRepetidos (x:xs)))

cuantasPalabras :: [Char] -> Integer
cuantasPalabras [x] = 1
cuantasPalabras (x:xs) | x == ' ' = 1 + cuantasPalabras xs
                       | otherwise = cuantasPalabras xs

eliminarEspaciosIniFin :: [Char] -> [Char]
eliminarEspaciosIniFin (x:xs) | x == ' ' = eliminarEspaciosFin xs
                              | otherwise = eliminarEspaciosFin (x:xs) 

eliminarEspaciosFin :: [Char] -> [Char]
eliminarEspaciosFin [] = []
eliminarEspaciosFin [x] | x == ' ' = []
                        | otherwise = [x]
eliminarEspaciosFin (x:xs) = [x] ++ eliminarEspaciosFin xs
--3)
palabras :: [Char] -> [[Char]]
palabras [] = []
palabras xs = (primeraPalabra (eliminarEspaciosIniFin (sacarBlancosRepetidos xs))) : palabras  (sinPrimeraPalabra (eliminarEspaciosIniFin (sacarBlancosRepetidos xs)))


primeraPalabra :: [Char] -> [Char]
primeraPalabra [] = []
primeraPalabra (x:xs) | x /= ' ' = [x] ++ primeraPalabra xs
                      | otherwise = []

sinPrimeraPalabra :: [Char] -> [Char]
sinPrimeraPalabra [] = []
sinPrimeraPalabra (x:xs) | x /= ' ' = sinPrimeraPalabra xs
                         | otherwise = xs

--4)
palabraMasLarga :: [Char] -> [Char]
palabraMasLarga [] = []
palabraMasLarga xs = masLargaAux (palabras xs)

masLargaAux :: [[Char]] -> [Char]
masLargaAux [] = []
masLargaAux [x] = x
masLargaAux (x:y:xs) | length x >= length y = masLargaAux (x:xs)
                     | otherwise = masLargaAux (y:xs)

--5)
aplanar :: [[Char]] -> [Char]
aplanar [] = []
aplanar [x] = x
aplanar (x:xs) = x ++ aplanar xs

--6)
aplanarConEspacios :: [[Char]] -> [Char]
aplanarConEspacios [] = []
aplanarConEspacios [x] = x
aplanarConEspacios (x:xs) = x ++ [' '] ++ aplanarConEspacios xs

-------------------------------------------------------------------
--5)
--1)
sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada (x:xs) =x: sumaAcumuladaAux (x:xs)

sumaAcumuladaAux :: (Num t) => [t] -> [t]
sumaAcumuladaAux (x:y:[]) = [x+y]
sumaAcumuladaAux (x:y:xs) =  x+y : sumaAcumuladaAux ((x+y):xs)

--2)
--ej input [2, 10, 6] es [[2], [2, 5], [2, 3]]
menorDivisor :: Integer -> Integer 
menorDivisor n = menorDivisorAux n 2

menorDivisorAux :: Integer -> Integer  -> Integer
menorDivisorAux n m |n<m =1 
                    |mod n m ==0 = m
                    | otherwise = menorDivisorAux n (m+1)

esPrimo :: Integer -> Bool
esPrimo n = menorDivisor n == n 

descomponerEnPrimos :: [Integer] -> [[Integer]]
descomponerEnPrimos [] = []
descomponerEnPrimos (x:xs) = primosDeUnNumero 2 x : descomponerEnPrimos xs

primosDeUnNumero :: Integer -> Integer -> [Integer]
primosDeUnNumero n x | n>x =[] 
                     | esPrimo n && mod x n == 0 =n : primosDeUnNumero (n+1) x
                     | otherwise = primosDeUnNumero (n+1) x

