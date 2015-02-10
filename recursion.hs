

prod :: [Int] -> Int
prod [x] = x
prod (x:xs) = x * prod xs
