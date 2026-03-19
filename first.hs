

factorial :: Int -> Int
factorial 1 = 1
factorial x = x * factorial ( pred x )


factors :: Int -> [Int]
factors n = [x | x <- [1..n] | n mod x == 0]