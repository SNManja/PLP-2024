valorAbsoluto :: Float -> Float 
valorAbsoluto n | n >= 0 = n
                | otherwise = -n

bisiesto :: Int -> Bool
bisiesto a = (a `mod` 4 == 0) && ((a `mod` 400 == 0) || (a `mod` 100 /= 0)) 

factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial (n-1)

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos 1 = 0
cantDivisoresPrimos n = length (filter (\x -> esPrimo x && n `mod` x == 0) [2..n])

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = not (any (\x -> (n `mod` x == 0)) [2..(isqrt n)])
