
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
isOrderedBy cmp xs@(x:y:_) = x `cmp` y && isOrdered cmp (tail xs)
isOrderedBy _ _ = True

isOrdered :: Ord a => [a] -> Bool
isOrdered = isOrderedBy (<=)
