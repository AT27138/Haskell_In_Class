import Data.Char

-- factorial obvs

factorial :: Int -> Int
factorial 1 = 1
factorial x = x * factorial ( pred x )

--


-- prime stuff (lesson 2)

isFactor :: (Int, Int) -> Bool
isFactor (m, n) = ((m `mod` n) == 0)

factors :: Int -> [Int]
factors n = [x | x <- [1..n], isFactor (n, x)]

isPrime :: Int -> Bool
isPrime n = length ( factors n ) < 3

primes :: Int -> [Int]
primes n = [x | x <- [1..n], isPrime x]

--

-- lesson 3

isPalindrome :: String -> Bool
isPalindrome s = s == reverse ( s )

caesarShift :: (Char, Int) -> Int
caesarShift (c, i) = ( ord c ) + i

caesarHash :: Int -> Char
caesarHash c
    |   c < ( ( ord 'A' ) + 26)   = chr ( c )
    |   otherwise     = chr ( c - 26 )

caesarCipher :: (String, Int) -> String
caesarCipher (str, shft) = [ caesarHash (caesarShift (c, shft - 26)) | c <- str, isAlpha c]


--
