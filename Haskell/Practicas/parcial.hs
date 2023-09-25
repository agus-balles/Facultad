{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
esIgual :: (String, String) -> (String, String) -> Bool
esIgual (a,b) (c,d) | a == c && b == d = True
                    | a == d && b == c = True
                    | otherwise = False


pertenece :: (String, String) -> [(String, String)] -> Bool  
pertenece (a,b) [] = False
pertenece (a,b) (x:xs)  | esIgual (a,b) x = True
                        | otherwise = pertenece (a,b) xs

relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [] = True
relacionesValidas [x] = True
relacionesValidas (x:xs)    | pertenece x xs = False
                            | otherwise = relacionesValidas xs

-----------------------------------------------------------------------------------
pasarAlista :: [(String, String)] -> [String]
pasarAlista [] = []
pasarAlista ((a,b):xs) = [a,b] ++ pasarAlista (xs)

perteneceLista :: String -> [String] -> Bool
perteneceLista x [] = False
perteneceLista x (y:xs) | x == y = True
                        | otherwise = perteneceLista x xs

eliminarRepetidos :: [String] -> [String]
eliminarRepetidos [] = []
eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs)    | perteneceLista x xs = eliminarRepetidos xs     
                            | otherwise = [x] ++ (eliminarRepetidos xs)

personas :: [(String, String)] -> [String]
personas [] = []
personas [(x,y)] = [x,y]
personas (x:xs) = eliminarRepetidos(pasarAlista (x:xs))

--ej input: persona:"Julio" relaciones:[(Julio,Pedro),(Julio,Pancho),(Andres,Pancho),(Abril,Julio)]
--expected output):["Pedro", "Pancho", "Abril"]

amigosDe :: String -> [(String, String)] -> [String]
amigosDe _ [] = []
amigosDe x (j:js) | amigoEnLista x j = (devuelveSoloAmigo x j) : amigosDe x js
                  | otherwise = amigosDe x js

amigoEnLista :: String -> (String , String) -> Bool
amigoEnLista x (a,b) = x == a || x==b 

devuelveSoloAmigo :: String -> (String , String) -> String
devuelveSoloAmigo x (a,b) | x == a = b
                          | x==b =a

--ej input: relaciones:[("Julio","Pedro"),("Pepe","Pancho"),("Andres","Pancho"),("Abril","Julio"),("Julio","Nahi")]
--expected output: "Julio"
personaConMasAmigos :: [(String,String)] -> String 
personaConMasAmigos [] = ""
personaConMasAmigos (x:xs) = masAmigosTotal (personas (x:xs)) (x:xs)

masAmigosTotal :: [String] -> [(String,String)] -> String 
masAmigosTotal [x] _ = x
masAmigosTotal (x:y:xs) (j:js) | length (amigosDe x (j:js)) >= length (amigosDe y (j:js)) = masAmigosTotal (x:xs) (j:js)
                               | otherwise = masAmigosTotal (y:xs) (j:js)