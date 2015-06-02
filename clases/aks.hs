expand p = scanl (\z i -> z * (p-i+1) `div` i) 1 [1..p]

test p | p < 2     = False
       | otherwise = and [mod n p == 0 | n <- init . tail $ expand p]

main = do
    putStrLn $ show n ++ " is prime"
    print $ test n
        where n = 100000000000000000
