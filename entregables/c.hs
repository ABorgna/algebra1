import Data.List (find)
import Data.Maybe (fromJust)

-- | El triangulo de lados a b c es rectangulo?
esPitagorica :: (Ord a, Floating a) => a -> a -> a -> Bool
esPitagorica a b c = a**2 * b**2 == c**2

-- | Pendiente de la recta entre los puntos
pendiente :: Floating a => (a,a) -> (a,a) -> a
pendiente (x0,y0) (x1,y1) = (y1-y0) / (x1-x0)

-- |
sumaImparesCuadradosMenoresA :: Integer -> Integer
sumaImparesCuadradosMenoresA n = sum [1,3.. floor $ sqrt $ fromIntegral n]

-- |
prodInterno :: [Float] -> [Float] -> Float
prodInterno = fmap sum . zipWith (*)

-- |
isOrderedBy :: (a -> a -> Bool) -> [a] -> Bool
isOrderedBy cmp xs@(x:y:_) = x `cmp` y && isOrderedBy cmp (tail xs)
isOrderedBy _ _ = True

isOrdered :: Ord a => [a] -> Bool
isOrdered = isOrderedBy (<=)

-- |
primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

isPrime :: Integer -> Bool
isPrime p = not $ any (divides p) $ takeWhile (<=s) primes
    where s = floor $ sqrt $ fromIntegral p

divides :: Integer -> Integer -> Bool
divides n m = n `rem` m == 0

factorize :: Integer -> [Integer]
factorize 1 = []
factorize n =  f : factorize (n `div` f)
    where f = fromJust $ find (divides n) primes

-- |
data Lista a = Vacia | Agregar a (Lista a)

iguales :: Eq a => Lista a -> Lista a -> Bool
iguales Vacia Vacia = True
iguales (a:as) (b:bs) = a == b || iguales as bs
iguales _ _ = False
