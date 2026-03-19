

factorial :: Int -> Int
factorial 1 = 1
factorial x = x * factorial ( pred x )


isFactor :: (Int, Int) -> Bool
isFactor (m, n) = ((m `mod` n) == 0)

factors :: Int -> [Int]
factors n = [x | x <- [1..n], isFactor (n, x)]

isPrime :: Int -> Bool
isPrime n = length ( factors n ) < 3

primes :: Int -> [Int]
primes n = [x | x <- [1..n], isPrime x]