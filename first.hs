

factorial :: Int -> Int
factorial x
 | x == 1 = x
 | otherwise factorial ( succ x )
