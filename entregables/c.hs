import Control.Monad (filterM)
import Data.List (find,group,nub,tails)
import Data.Maybe (fromJust)

-- | 1.5
-- El triangulo de lados a b c es rectangulo?
esPitagorica :: (Eq a, Floating a) => a -> a -> a -> Bool
esPitagorica a b c = a**2 * b**2 == c**2

-- | 2.2
-- Pendiente de la recta entre los puntos
pendiente :: (Float,Float) -> (Float,Float) -> Float
pendiente (x0,y0) (x1,y1) = (y1-y0) / (x1-x0)

-- | 3.4
sumaImparesCuadradosMenoresA :: Integral a => a -> a
sumaImparesCuadradosMenoresA n = sum $ takeWhile (\x -> x^2 < n) [1,3..]

-- | 4.2
prodInterno :: [Float] -> [Float] -> Float
prodInterno = fmap sum . zipWith (*)

-- | 5.5
-- La consigna no especifica el criterio de ordenamiento
isOrderedBy :: (a -> a -> Bool) -> [a] -> Bool
isOrderedBy cmp xs@(x:y:_) = x `cmp` y && isOrderedBy cmp (tail xs)
isOrderedBy _ _ = True

isOrdered :: Ord a => [a] -> Bool
isOrdered = isOrderedBy (<=)

-- | 7.4
primes :: [Integer]
primes = 2 : filter isPrime [3,5..]
    where isPrime p = let s = floor $ sqrt $ fromIntegral p
                      in not $ any (divides p) $ takeWhile (<= s) primes

factorizar :: Integer -> [Integer]
factorizar 1 = []
factorizar n =  f : factorizar (n `quot` f)
    where f = fromJust $ find (divides n) primes

divides :: Integer -> Integer -> Bool
divides n m = n `rem` m == 0

-- | 8.2.4
data Lista a = Vacia | Agregar a (Lista a)

iguales :: Eq a => Lista a -> Lista a -> Bool
iguales Vacia Vacia = True
iguales (Agregar a as) (Agregar b bs) = a == b || iguales as bs
iguales _ _ = False

-- | 9.5
divisores :: Integer -> [Integer]
divisores = map (product . concat) . mapM tails . group . factorizar

seisDivisores :: [Integer]
seisDivisores = filter ((==6) . length . divisores) [1..1000]

