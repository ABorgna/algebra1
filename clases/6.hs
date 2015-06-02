iguales :: (Int,Int) -> Bool
iguales (x,y) = x == y

first :: (a,b,c) -> a
first (x,y,z) = x

first' (x,_,_) = x

longitud :: [a] -> Integer
longitud [] = 0
longitud (x:[]) = 1
longitud (x:y:[]) = 2
longitud (x:y:z:[]) = 3
longitud (_:_:_:xs) = 3 + longitud xs

longitud' = foldr (const (+1)) 0

iniciales :: [Char] -> [Char] -> [Char]
iniciales nombre apellido = [n,a]
    where (n:_) = nombre
          (a:_) = apellido

iniciales' (n:_) (a:_) = [n,a]
