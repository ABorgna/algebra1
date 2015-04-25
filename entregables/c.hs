import Data.List (sort)

-- | El triangulo de lados a b c es rectangulo?
esPitagorica :: (Ord a, Floating a) => a -> a -> a -> Bool
esPitagorica a b c = let (d:f:h:[]) = map (**2) $ sort [a,b,c] in h == d+f

-- | Pendiente de la recta entre los puntos
pendiente :: Floating a => (a,a) -> (a,a) -> a
pendiente (x0,y0) (x1,y1) = (y1-y0) / (x1-x0)

-- | 
sumaImparesCuadradosMenoresA :: Integer -> Integer
sumaImparesCuadradosMenoresA n = sum $ takeWhile (\e->e^2<n) [1,3..]

