import Data.Tuple (swap)

potencia :: Float -> Int -> Float
potencia b = (!!) $ iterate (*b) 1

divisor :: Int -> Int -> (Int,Int)
divisor a b = (d,a-d)
    where d = last $ takeWhile ((<=a).(*b)) [0..]

divisores :: Int -> [Int]
divisores x = 1:x:[n | n <- [2..x `div` 2] , x `mod` n == 0]

esPrimo :: Int -> Bool
esPrimo = (>2) . length . divisores


