import Data.List (sort)

-- | El triangulo de lados a b c es rectangulo?
esPitagorica :: (Ord a, Floating a) => a -> a -> a -> Bool
esPitagorica a b c = let (d:f:h:[]) = map (**2) $ sort [a,b,c] in h == d+f

