--ej input) [("Lucas","Juan"),("Lucas","Martin"),("Alvaro","Lucas")]
--expected output= True

relacionesValidas :: [(String,String)] -> Bool
relacionesValidas [] = True
relacionesValidas (x:xs)  |componentesIguales x =False
                          | esUnico x xs =relacionesValidas xs
                          | otherwise =False

componentesIguales :: (String,String) -> Bool
componentesIguales (a,b) = a == b

esUnico :: (String,String) -> [(String,String)] -> Bool
esUnico _ [] = True
esUnico (a,b) (x:xs) | (a,b) == x || (b,a) == x = False
                     | otherwise = esUnico (a,b) xs

------------------------------------------------------------------
--ej input) [("Lucas","Juan"),("Lucas","Martin"),("Alvaro","Abril")]
--expected output) ["Lucas","Juan","Martin","Alvaro","Abril"]
personas :: [(String,String)] -> [String]
personas [] = []
personas (x:xs) = eliminarRepetidos (pasarAlista (x:xs))

pasarAlista :: [(String, String)] -> [String]
pasarAlista ((a,b):[]) = [a,b]
pasarAlista ((a,b):xs) = [a,b] ++ pasarAlista xs 

eliminarRepetidos :: [String] -> [String]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) | pertenece x xs = eliminarRepetidos xs 
                         | otherwise = x : eliminarRepetidos xs

pertenece :: String -> [String] -> Bool
pertenece x [] = False
pertenece x (j:js) | x == j =True
                   | otherwise = pertenece x js

------------------------------------------------------------------
amigosDe :: String -> [(String,String)] -> [String]
amigosDe _ [] = []
amigosDe x ((a,b):xs)  | x ==a =b : amigosDe x xs
                       | x == b = a : amigosDe x xs
                       | otherwise = amigosDe x xs

-------------------------------------------------

personaConMasAmigos :: [(String,String)] -> String
personaConMasAmigos (x:xs) = masAmigosAux (personas (x:xs)) (x:xs)

masAmigosAux :: [String] -> [(String,String)] -> String 
masAmigosAux (x:[]) _ = x
masAmigosAux (x:y:xs) (j:js) | length (amigosDe x (j:js)) >= length (amigosDe y (j:js)) = masAmigosAux (x:xs) (j:js)
                             | otherwise = masAmigosAux (y:xs) (j:js)
