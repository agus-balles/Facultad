-- Practica 4
-- 1)
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Time.Format.ISO8601 (yearFormat)

{-# HLINT ignore "Redundant bracket" #-}
fibonacci :: Integer -> Integer
fibonacci x     | x == 0 = 0
                | x == 1 = 1
                | otherwise = fibonacci (x-1) + fibonacci (x-2)

-- 2)
parteEntera :: Float -> Integer
parteEntera x   | 0 <= x && x < 1 = 0
                | x >= 1 = 1 + parteEntera(x-1)
                | otherwise = parteEntera(x+1) - 1 

-- 3)
esDivisible :: Integer -> Integer -> Bool
esDivisible x y     | y == 0 = False
                    | x == 0 = True
                    | x < y = False
                    | otherwise = esDivisible (x-y) y                    

-- 4) 
sumaImparesHasta :: Integer -> Integer
sumaImparesHasta n
    | n <= 0    = 0  -- Caso base: Si n es menor o igual a 0, la suma es 0.
    | otherwise = sumaImparesHasta (n - 1) + (2 * n - 1)

--5)
factorialSkip :: Integer -> Integer
factorialSkip n | n == (-1) || n == 0 = 1
                | otherwise = n * factorialSkip (n - 2)

--6)
sumaDigitos :: Integer -> Integer
sumaDigitos n | div n 10 ==0 =n
              | otherwise = mod n 10 + sumaDigitos (div n 10)

-- 7)
todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n   | 0 <= n && n <= 9 = True  
                        | unidades n == decenas n = todosDigitosIguales (sinUltimaUnidad n)
                        | otherwise = False
    where
        unidades :: Integer -> Integer
        unidades n = mod n 10 

        decenas :: Integer -> Integer
        decenas n = unidades (sinUltimaUnidad n)

        sinUltimaUnidad :: Integer -> Integer
        sinUltimaUnidad n = div n 10

--8)
iesimoDigito :: Integer -> Integer -> Integer 
iesimoDigito n i = mod(div n (10^(cantidadDeDigitos n- i))) 10


cantidadDeDigitos :: Integer -> Integer
cantidadDeDigitos x | div x 10 == 0 = 1
                    | otherwise = 1 + cantidadDeDigitos (div x 10)


-- 9) 9449, 9494 , 9944, 9944, 9494, 9449           123321
--    5367, 5376, 5736, 7536, 7356, 7365

--9)
esCapicua :: Integer -> Bool
esCapicua x | cantidadDeDigitos x == 1 = True
            | mod x 10 == iesimoDigito x 1 = esCapicua (mod sinUltimoDig (10^cantidadDeDigitos sinUltimoDig))
            |otherwise = False --Nota: Parece que anda pero creo que esta mal, probar con numeros grandes.
            where sinUltimoDig= div x 10

--10)
--a)
sumatoriaA :: Integer -> Integer 
sumatoriaA 0 = 1
sumatoriaA i = 2^i + sumatoriaA (i-1)

--b)
sumatoriaB :: Integer -> Integer -> Integer
sumatoriaB 1 q = q
sumatoriaB i q = q^i + sumatoriaB (i-1) q

--c)
sumatoriaC :: Integer -> Integer -> Integer
sumatoriaC 0 _ = 0
sumatoriaC 1 q = q
sumatoriaC i q = sumatoriaB (2*i) q

--d)
sumatoriaD :: Integer -> Integer -> Integer
sumatoriaD 0 _ = 0
sumatoriaD _ 0 = 0
sumatoriaD i q = sumatoriaDAux i (2*i) q

sumatoriaDAux :: Integer -> Integer -> Integer -> Integer
sumatoriaDAux i n q | i == n = q^n
                    | otherwise = q^i + sumatoriaDAux (i+1) n q

--11)eAprox
eAprox :: Integer -> Float
eAprox 1 = 1
eAprox n = (1 / (fromIntegral (factorial n))) + eAprox (n-1)

factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n-1)

--12)
raizDe2Aprox :: Integer -> Float
raizDe2Aprox n = (raizDe2AproxAux n) -1

raizDe2AproxAux :: Integer -> Float
raizDe2AproxAux 1 = 2
raizDe2AproxAux n = 2 + (1 / (raizDe2AproxAux (n - 1)))

--13)
sumatoriaDoble :: Integer -> Integer -> Integer
sumatoriaDoble 0 _ = 0
sumatoriaDoble n m = sumatoriaInterna n m + sumatoriaDoble (n-1) m

sumatoriaInterna :: Integer -> Integer -> Integer
sumatoriaInterna _ 0 =0
sumatoriaInterna i j = i^j + sumatoriaInterna i (j - 1)

--14)
sumaPotencias :: Integer -> Integer ->Integer-> Integer
sumaPotencias 0 _ _ = 0
sumaPotencias _ 0 _ = 0
sumaPotencias q n m =sumatoriaInternaPotencias q n m + sumaPotencias q (n-1) m

sumatoriaInternaPotencias :: Integer -> Integer -> Integer -> Integer
sumatoriaInternaPotencias _ _ 0 =0
sumatoriaInternaPotencias i j k = i^(j+k) + sumatoriaInternaPotencias i j (k-1)

--15)
sumaRacionales :: Integer -> Integer -> Float
sumaRacionales 0 _ = 0
sumaRacionales n m =  sumatoriaInternaRacionales n m + sumaRacionales (n-1) m

sumatoriaInternaRacionales :: Integer -> Integer -> Float
sumatoriaInternaRacionales _ 0 =0
sumatoriaInternaRacionales p q =  fromIntegral p / fromIntegral q +   sumatoriaInternaRacionales p (q - 1)

--16) 
menorDivisor :: Integer -> Integer 
menorDivisor n = menorDivisorAux n 2

menorDivisorAux :: Integer -> Integer  -> Integer
menorDivisorAux n m |n<m =1 
                    |mod n m ==0 = m
                    | otherwise = menorDivisorAux n (m+1)

esPrimo :: Integer -> Bool
esPrimo n = menorDivisor n == n 

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m | n == m =True
                | not (divisorComunDesde n m 2) = True
                | otherwise = False

divisorComunDesde :: Integer -> Integer ->Integer -> Bool
divisorComunDesde n m y |y > n && y > m = False
                        |mod n y == 0 && mod m y == 0 =True
                        | otherwise = divisorComunDesde n m (y+1)

nEsimoPrimo :: Integer ->Integer
nEsimoPrimo n = primoNumero 2 1 n

primoNumero :: Integer -> Integer -> Integer -> Integer
primoNumero n m j | m == j && esPrimo n = n 
                  | esPrimo n = primoNumero (n+1) (m+1) j 
                  | otherwise = primoNumero (n+1) m j
    
esFibonacci :: Integer -> Bool
esFibonacci 0 =False
esFibonacci n = fibonacciIgual n (fibonacci 1)

fibonacciIgual :: Integer -> Integer -> Bool
fibonacciIgual n m | n== fibonacci m =True
                   | n< fibonacci m =False
                   |otherwise =fibonacciIgual n (m+1)



mayorDigitoPar :: Integer -> Integer
mayorDigitoPar x   | cantidadDeDigitos x == 1 && (mod x 2 ==0) =x
                   | cantidadDeDigitos x == 1 && (mod x 2 /=0) =(-1)
                   | mod y 2 /= 0 = mayorDigitoPar (div x 10) 
                   | mod z 2 /= 0 = mayorDigitoPar (mod x (10^cantidadDeDigitos (div x 10)))
                   | y >= z && cantidadDeDigitos x == 0 =y
                   | y < z && cantidadDeDigitos x == 0 =z
                   | y >= z = mayorDigitoPar (mod x (10^cantidadDeDigitos (div x 10)))
                   | y < z = mayorDigitoPar (div x 10) 
                   where y = mod x 10
                         z = iesimoDigito x 1

esSumaInicialDePrimos :: Int-> Bool 
esSumaInicialDePrimos 0 = False
esSumaInicialDePrimos 1 = False
esSumaInicialDePrimos n = sumaHastaPrimo 0 2 n

sumaHastaPrimo :: Int -> Int -> Int -> Bool
sumaHastaPrimo x y z | x > z = False
                     | x == z = True
                     | esPrimo (fromIntegral y) = sumaHastaPrimo (x+y) (y+1) z
                     | otherwise = sumaHastaPrimo x (y+1) z


