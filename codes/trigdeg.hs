-- Applied Haskell
--
-- rearning Haskell by implementing Statical, 
--  Finance Functions 
--
------------------------------------------

-- Arithmetic Mean of a Sequence
--
-- mean :: Fractional b => [b] -> b
mean lst = sum lst / fromIntegral (length lst)

-- Geometric Mean of Squence 
geomean lst = (product lst)^(fromIntegral (length lst))

-- Convert from decimal to percent
to_percent lst = map (100.0 *) lst


-- Lagged Difference of a time serie
-- lagddif [xi] = [x_i+1 - x_i]
lagdiff lst = zipWith (-) (tail lst) lst
--
-- Growth of a Time Serie
growth lst = zipWith (/) (lagdiff lst) lst
---
--- Perncentual Growth
growthp = to_percent . growth

-- Time Serie Test Data
timeserie = [0.1, 0.23, 0.18, 0.34, 0.55, 0.167, 0.45, 0.56 ]


-- Task:http://rosettacode.org/wiki/Averages/Mean_angle#Haskell 
-- http://rosettacode.org/wiki/Averages/Mean_angle#Haskell 
--
-- Degree Trigonometric Functions
--
deg2rad deg = deg*pi/180.0
rad2deg rad = rad*180.0/pi

sind = sin . deg2rad
cosd = cos . deg2rad
tand = tan . deg2rad
atand = rad2deg . atan
atan2d y x = rad2deg (atan2 y x )

avg_angle angles = atan2d y x
    where
    y = mean (map sind angles) 
    x = mean (map cosd angles)

-- Vectorial Dot Product
--
-- *Main> dotprod [10.23, 23.3, -103.44, 10.23] [90.0, 23.12, 30.4, 80.0 ] 
-- -866.7799999999993
--  
-- *Main> dotprod [1, 3, -5] [4, -2, -1]
-- 3
----------------------------------------------------------------
-- http://rosettacode.org/wiki/Dot_product#Haskell 
dotprod v1 v2 = sum ( zipWith  (*) v1 v2 )

-- *Main> sin ( units 90 "deg")
-- 1.0
-- *Main> 
-- *Main> sin ( units 45 "deg")
-- 0.7071067811865475
-- *Main> 
-- *Main> 
-- 
units angle sym | sym == "deg" = angle*pi/180.0
                | sym == "rad" = angle


-- Numerical Derivate Function
--
-- *Main> f 2
--  4.0
-- *Main> df 2
-- 6.000020000040961
--  
--
derv dx f x = (f(x+dx) - f(x))/dx

f x = 2*x**2 - 2*x
df = derv 1e-5 f

-- *Main> sum [1..100]
--  5050
--  
sum2 :: [Integer] -> Integer
sum2 []     = 0
sum2 (x:xs) = x + sum xs


prod :: [Integer] -> Integer
prod []     = 1
prod (x:xs) = x * prod xs


-- root itmax f guess =  let newguess =  
