import Data.List (nub)

mcd :: Integral a => a -> a -> a
mcd a 0 = abs a
mcd a b = mcd b $ rem a b

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece a (x:xs) = a == x || pertenece a xs

pertenece' a = any (==a)

hayRepetidos :: Eq a => [a] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) = x `elem` xs || hayRepetidos xs

menores :: Ord a => a -> [a] -> [a]
menores a = filter (<a)

quitar :: Eq a => a -> [a] -> [a]
quitar _ [] = []
quitar a (x:xs) = if a == x then xs else x: quitar a xs

maximo :: Ord a => [a] -> a
maximo = foldr1 (max)

enBase :: Integral a => a -> a -> [a]
enBase _ 0 = []
enBase b n = let (d,r) = quotRem n b in enBase b d ++ [r]

deBase :: Integral a => a -> [a] -> a
deBase b = foldl (\s e -> s*b+e) 0

-- | Lychrel
capicuaPara :: [Int] -> [Int]
capicuaPara xs = if xs == r then xs else capicuaPara next
    where r = reverse xs
          next = enBase 10 $ deBase 10 $ zipWith (+) xs r

