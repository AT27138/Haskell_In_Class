

factorial :: Int -> Int
factorial 1 = 1
factorial x = factorial `succ` x
