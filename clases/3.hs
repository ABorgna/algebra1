module M3 where

listar :: a -> a -> a -> [a]
listar a b c = [a,b,c]

rangoDePaso :: (Enum t, Num t) => t -> t -> t -> [t]
rangoDePaso a z b = [a,a+b..z]

fib :: Int -> Integer
fib  = (fibs!!)

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) (fibs) (tail fibs)

reverso :: [a] -> [a]
reverso = foldl (flip (:)) []

capicua :: Eq a => [a] -> Bool
capicua xs = xs == reverso xs

productoria :: [Integer] -> Integer
productoria = foldr (*) 1

comb :: Integral a => a -> a -> a
comb n m | m == 0 = 1
         | n == m = 1
         | n < m = error "n es menor a m"
         | otherwise = comb (n-1) (m-1) + comb (n-1) m
