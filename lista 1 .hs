-- 2 ) dobro de um numero

dobro :: Int -> Int 
dobro x = x + x 

-- 3 ) quadriplica usando dobro 

quadriplica :: Int -> Int 
quadriplica x = (dobro (dobro x))

-- 4 ) hipotenusa

hipotenusa :: Float -> Float -> Float 
hipotenusa a1 a2 = sqrt(a1^2 + a2^2 )

-- 5 ) dst entre 2 pontos 

dst2 :: Float -> Float -> Float -> Float -> Float
dst2 x1 y1 x2 y2 = sqrt(x1-x2)^2 + (y1-y2)^2

-- 6) verifica se é par 

verificapar :: Int -> Bool
verificapar x = if (mod x 2 == 0) then True else False 

-- 6b) verifica se é impar usando verificapar

verificaimpar :: Int -> Bool
verificaimpar x = if not (verificapar x) then True else False 

-- 7) farenheit para celsius

converte :: Float -> Float
converte f = ((f - 32) / 1.8)

--8) maior valor entre 2 numeros 

maiorentre2 :: Int -> Int -> Int
maiorentre2 x y = if x > y  then x else y 

--9) maior valor entre 3 numeros 

maior3 :: Integer -> Integer -> Integer -> Integer 
maior3 x y z 
    | x >= y && x >= z = x 
    | y >= z = y   
    | otherwise = z

--10) 10) Escreva uma função que recebe um valor numérico e retorne o valor 1 se a entrada
-- for maior que zero, -1 se for negativo, 0 se for zero.

retorna :: Int -> Int 
retorna x 
   | x == 0 = 0
   | x < 0 = -1
   | x > 0 =  1
   
-- 11) Seja a função ehDigito definida abaixo para um valor do tipo caractere. Faça uma
-- função que verifique se um caractere é uma letra.

ehDigito :: Char -> Bool
ehDigito c = if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) then True else False 

--12) Escreva uma função que, dados 3 valores, retorne se eles correspondem ou não aos
-- lados de um triângulo retângulo

triangretang:: Float -> Float -> Float -> Float
triangretang x y z 
   | x > y && x > z && (x == (hipotenusa y z )) = True
   | y > z && ( y == (hipotenusa x z )) = True 
   | z == (hipotenusa x y ) == true 
   |  otherwise = False 
   
   
 



