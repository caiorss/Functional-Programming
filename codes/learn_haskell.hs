{-
 -  Author:         Caio Rodrigues.
 -  Description:    Learning Haskell Script
 -  Editor:         VIM/Gvim
 - ------------------------------------------
 -
 - To load this module enter in the shell 
 -      $ ghci
 -      
 -      Prelude> :load learning_haskell.hs
 -      
 -      To Reload the modules
 -      Prelude>  :reload 
 -
 -
 - In the Haskell shell you must type let
 -  :let add a  b = a + b
 -
 - References:
 -
 - Pattern Matching:
 -      http://stackoverflow.com/questions/2225774/haskell-pattern-matching-what-is-it 
 -      https://www.cs.auckland.ac.nz/references/haskell/haskell-intro-html/patterns.html  
 -}


add a b = a + b
add10 = add 10

--  Factorial
--
-- > map fact [1..10]
--  [1,2,6,24,120,720,5040,40320,362880,3628800]
--
--  fact :: (Eq a, Num a) => a -> a
--
fact 0 = 1
fact n = n*fact(n-1)

-- Fibbonacci Function in recursive way
-- 
--
fib 0 = 1
fib 1 = 1
fib n | n>= 2
    = fib(n-1) + fib(n-2)

-- *Main> 
-- *Main> let sign x | x > 0 = 1 | x == 0 = 0 | x < 0 = -1
-- *Main> 
-- *Main> map sign [-4..4]
-- [-1,-1,-1,-1,0,1,1,1,1]
-- *Main> 
-- *Main> 
-- 
sign x | x >  0 =  1
       | x == 0 =  0 
       | x <  0 = -1

--  ENUMERATION TYPES
--
-- From:https://www.fpcomplete.com/school/starting-with-haskell/introduction-to-haskell/2-algebraic-data-types 
--  
-- -*Main> :show Thing
-- syntax:  :show [ args | prog | prompt | editor | stop | modules | bindings
--                | breaks | context | packages | language ]
-- *Main> -
-- 
-- *Main> :t Shoe
-- Shoe :: Thing
-- *Main> :t Ship
-- Ship :: Thing
-- *Main> :t King
-- King :: Thing
-- *Main>  
-- -- 
--
--
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

-- functions on Things by pattern matching: 
-- 
-- *Main> isSmall Shoe
-- True
-- *Main> 
-- *Main> isSmall Cabbage
-- True
-- *Main> isSmall King
-- False
-- *Main> 
--
-- *Main> :t isSmall
-- isSmall :: Thing -> Bool
-- *Main>  
--
-- *Main> map isSmall [ Shoe, SealingWax, Ship, King, Cabbage, Shoe ]
-- [True,True,False,False,True,True]
--  
--
-- 
isSmall :: Thing -> Bool
isSmall Shoe       = True
isSmall Ship       = False
isSmall SealingWax = True
isSmall Cabbage    = True
isSmall King       = False

-- *Main> let a = Failure 
-- *Main> let b = OK 3.34
-- *Main> let main = print(a, b)
-- *Main> main
-- (Failure,OK 3.34)
-- *Main>  
-- *Main> :t OK
-- OK :: Double -> FailableDouble
-- *Main> 
-- *Main> :t Failure
-- Failure :: FailableDouble
-- *Main> 
-- *Main> 
--
-- *Main> let main = print (safeDiv 2 0, safeDiv 3 4)
-- *Main> main
-- (Failure,OK 0.75)
-- *Main> 
-- *Main> 
--
--
-- *Main> let f = safeDiv 10
-- *Main> 
-- *Main> map f [0 , 10, 40, 30]
-- [Failure,OK 1.0,OK 0.25,OK 0.3333333333333333]
-- *Main> 
-- 
--  Function Composition div10(x) = failureToZero(f)(x)
--
-- *Main> let div10 = failureToZero .  f
-- *Main> 
-- *Main> map div10 [0, 10, 40, 30]
-- [0.0,1.0,0.25,0.3333333333333333]
-- *Main> 
--  
-- 
--
data FailableDouble = Failure | OK Double
                    deriving Show

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

-- For some applications, we might consider mapping a failed computation to a value of zero: 
--
-- *Main> failureToZero (OK 3.34)
-- 3.34
-- *Main>  
-- 
--
failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d

 
isPrime n = testFactorsFrom 2 n
    where testFactorsFrom i n | i * i > n = True
                              | n `rem` i == 0 = False
                              | otherwise = testFactorsFrom (i + 1) n


n_primes n = filter isPrime [1..n]

 
-- *Main> let  n_primes n = filter isPrime [1..n]
-- *Main> n_primes 100
-- [1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97 
 
 
{- -----------------------------------
 - Tests 


*Main> (+10) 1
11
*Main> (+3) 100
103
*Main> (/3) 90
30.0
*Main> (*3) 30
90
*Main> 
 

> map (+3) [1..10]
[4,5,6,7,8,9,10,11,12,13]

> map (+(-3)) [1..10]
[-2,-1,0,1,2,3,4,5,6,7]

> map (/3) [0..3]
[0.0,0.3333333333333333,0.6666666666666666,1.0]



> map add10 [1..10]
[11,12,13,14,15,16,17,18,19,20]
 
*Main> add 10 20
30
*Main> add 1.23 (-33.3)
-32.07
*Main> 
*Main> :t add
add :: Num a => a -> a -> a
*Main> 

Declaration With Type Signature on GHCI

*Main> 
*Main> :{
*Main| let addTwo :: Int -> Int -> Int
*Main|     addTwo x y = x + y
*Main| :}
*Main> 
*Main> addTwo 3  6
9
*Main> addTwo 3  10
13
*Main> addTwo 3  3.44 -- Will an error message!!

-- Multiple Line Definition in one Line on GHCI
--
*Main> let addw :: Int -> Int -> Int ; addw x y = x + y
*Main> 
*Main> addw 10 23
33
*Main> addw 30 35
65
*Main> addw 20 3.33


*Main> :set +m
*Main> let addTwo :: Int -> Int -> Int
*Main|     addTwo x y = x + y
*Main| 
*Main> 
*Main> addTwo 10 23
33
*Main> addTwo 33 55
88
*Main> 

*Main> let  fact 0 = 1 ; fact n = n * fact (n-1)
*Main| 
*Main> fact 0
1
*Main> fact 1
1
*Main> fact 2
2
*Main> map fact [1..10]
[1,2,6,24,120,720,5040,40320,362880,3628800]
*Main> 

*Main> let absv n | n >= 0 = n | otherwise = -n

*Main> [-10..10]
[-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10]

*Main> 
*Main> map absv [-10..10]
[10,9,8,7,6,5,4,3,2,1,0,1,2,3,4,5,6,7,8,9,10]

 
- 
-}


