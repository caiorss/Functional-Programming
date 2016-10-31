
data Season = Winter | Spring | Summer | Fall deriving(Show)

toInt :: Season -> Int
toInt Winter  = 0
toInt Spring  = 1
toInt Summer  = 2
toInt Fall    = 3

fromInt :: Int -> Season
fromInt 0 = Winter
fromInt 1 = Spring
fromInt 2 = Summer
fromInt 3 = Fall

next :: Season -> Season
next x = fromInt ( (toInt(x) + 1) `mod` 4 )

next2 Winter = Spring
next2 Spring = Summer
next2 Summer = Fall
next2 Fall   = Winter

--eqSeason :: 
eqSeason x y = (toInt x == toInt y)
