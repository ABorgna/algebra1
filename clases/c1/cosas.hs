import Data.List (nub)

esPositivo :: (Num a, Ord a) => a -> Bool
esPositivo = (>0)

ad1if :: (Num a, Ord a) => a -> a
ad1if n = if n < 10 then n else n+1

nand :: Bool -> Bool -> Bool
nand True True = False
nand    _    _ = True

nor :: Bool -> Bool -> Bool
nor False False = True
nor     _     _ = False

raices :: (Floating a, Eq a) => a -> a ->  a -> [a]
raices a b c = nub $ do 
    s <- [-1,1]
    return $ (-b + s * sqrt (b**2 - 4 * a * c)) / (2/a)
