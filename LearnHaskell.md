<!-- # HASKELL BY EXAMPLE / PRACTICAL FUNCTIONAL PROGRAMMING -->

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Haskell Tool set](#haskell-tool-set)
  - [Toolset](#toolset)
  - [GHCI Reference](#ghci-reference)
- [Concepts](#concepts)
    - [Functional Programming](#functional-programming)
    - [Functional Programming Design Patterns](#functional-programming-design-patterns)
    - [Haskell Features](#haskell-features)
- [Basic Syntax](#basic-syntax)
  - [Operators](#operators)
    - [Logic Operators](#logic-operators)
    - [Powers](#powers)
    - [Application Operator - $](#application-operator---$)
    - [Pipelining Operator](#pipelining-operator)
  - [Defining Values and Types](#defining-values-and-types)
  - [Typeclasses and Types](#typeclasses-and-types)
    - [Basic Types](#basic-types)
    - [Typeclasses](#typeclasses)
    - [Useful notations for functions](#useful-notations-for-functions)
  - [Lists](#lists)
    - [Creating Lists](#creating-lists)
    - [List Operations](#list-operations)
    - [Chekings Lists](#chekings-lists)
- [Functions](#functions)
  - [Creating functions](#creating-functions)
  - [Anonymous Functions or Lambda Functions](#anonymous-functions-or-lambda-functions)
  - [Infix Operators](#infix-operators)
  - [Currying](#currying)
  - [Recursion](#recursion)
  - [Higher Order Functions](#higher-order-functions)
- [Pattern Matching](#pattern-matching)
- [](#)
- [List Comprehension](#list-comprehension)
  - [Simple List Comprehension](#simple-list-comprehension)
  - [Comprehensions with multiple generators](#comprehensions-with-multiple-generators)
  - [Function Inside List Comprehension](#function-inside-list-comprehension)
  - [Comprehension with Guards](#comprehension-with-guards)
- [Abstract Data Type](#abstract-data-type)
- [Error Handling with Maybe](#error-handling-with-maybe)
- [Applications](#applications)
  - [Mathematics](#mathematics)
  - [Numerical Methods](#numerical-methods)
    - [Polynomial](#polynomial)
    - [Numerical Derivate](#numerical-derivate)
    - [Equation Solving](#equation-solving)
- [     t -> Int -> (t -> t) -> (t -> t) -> t -> (t, t, Int)](#t---int---t---t---t---t---t---t-t-int)
  - [Statistics and Time Series](#statistics-and-time-series)
  - [Vector](#vector)
- [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

The purpose of this documentation is to ilustrate functional programming concepts in
Haskell Language by providing reusable and useful pieces of codes and it's its results.

Notes: 

* The codes with '>' symbol were run in the interactive haskell Shell ghci
and the line bellow without the symbol > are the output.

## Haskell Tool set

### Toolset

|                                    |                                                      |
|------------------------------------|------------------------------------------------------|
| ghc - the Glasgow Haskell Compiler | Transforms Haskell Source code .hs into native code. |
| ghci                               | Haskell Interactive Shell/ Interpreter               |
| runghc                             | Haskell Non Interactive Interpreter                  | 
| haddock                            | Documentation tool for annotated Haskell source code |
| cabal                              | GHC Haskell Cabal package manager                    |

### GHCI Reference

GHCI Interactive Shell

| Command                     |  Description                                |
|-----------------------------|---------------------------------------------|
| :help                       |  Show help                                  |
| :load [haskell-source.hs] |    Load Haskell Source Code                   |
| :reload                     |  Reload Code after it was edited            |
| :type [symbol]             |  Show the Type of a Symbol                  |
| :browser                    |  Gives the type signature of all functions  |
| :set +s                     |  Multiline Code                             |
| :{ [code here ] :}        |    Multiline Code                             |
| :set prompt ">"             |  Change the prompt to ">"                   |


## Concepts

#### Functional Programming

Functional Programming is all about programming with functions.

**Functional Programming Features**

* Pure Functions / Referencail Transparency / No side effect
* Function Composition
* Lambda Functions/ Anonymous Functions
* High Order Functions
* Currying/ Partial Function Application
* Clousure - Returning functions from functions

* Data Imutability
* Pattern Matching
* Lists are the fundamental data Structure

Non Essential Features:

* Static Typing
* Type Inferencing
* Algebraic Data Types

**Lazy Evaluation**

“Lazy evaluation” means that data structures are computed incrementally, as they 
are needed (so the trees never exist in memory all at once) parts that are never needed 
are never computed.

#### Functional Programming Design Patterns

* Curry/ Partial function application  - Creating new functions by holding a parameter constant
* Closure - Return functions from functions
* Function composition
* Composable functions
* High Order Functions
* MapReduce Algorithms - Split computation in multiple computers cores.
* Lazy Evaluation ( aka Delayed evaluation)
* Pattern Matching


#### Haskell Features

* Pure Functional programming language
* Static Typed Language with Type Inference (The haskell compiler deduce the types for you). 
* Lazy Evaluation ( Dealayed evaluation) by default
* Data Imutability/ Haskell has no variables
    * Values can be bound to a name and can only be assigned once.
    * Values can never change.
* Haskell has not for-loop, while statements.
* Algebraic Data types
* Pattern Matching
* Tail Recursions

## Basic Syntax

### Operators


#### Logic Operators

```
  True || False ⇒ True  
  True && False ⇒ False 
  True == False ⇒ False 
  True /= False ⇒ True  (/=) is the operator for different 
```

#### Powers

```
x^n     for n an integral (understand Int or Integer)
x**y    for y any kind of number (Float for example)
x^n     pour n un entier (comprenez Int ou Integer)
x**y    pour y tout type de nombre (Float par exemple)
```


#### Application Operator - $

Tje application operator '$' makes code more readable and cleaner since substitutes parenthesis.
It is also useful in higher-order situations, such as map ($ 0) xs, or zipWith ($) fs xs. 

```haskell
> f $ g $ h x = f (g (h x))
```

#### Pipelining Operator 

Haskell doesn't have a native Pipe operator like F# (F-Sharp) does, however
it can be defined by the user.

```haskell

> let (|>) x f = f x
> 
> let (|>>) x f = map f x

> let (?>>) x f = filter f x


> take 3 (reverse (filter even [1..10]))
[10,8,6]

> [1..10] |> filter even |> reverse |> take 3
[10,8,6]
> 


> [1..10] |>> (^2) |>> (/10) |>> (+100)
[100.1,100.4,100.9,101.6,102.5,103.6,104.9,106.4,108.1,110.0]

> 
> [1..10] ?>> even
[2,4,6,8,10]
> 
> [1..10] ?>> even |>> (+1)
[3,5,7,9,11]
> 
> 


```




### Defining Values and Types

```haskell

> let b = 100 :: Float
> let a  = 100 :: Int
> let c = 100 :: Double
> 
> b
100.0
> :t b 
b :: Float
> :t a 
a :: Int
> :t c
c :: Double
> 
> let x = 100.2323
> :t x
x :: Double
> 
> let y = [1..10]
> y
[1,2,3,4,5,6,7,8,9,10]
> 
> let z = [1, 2, 4, 5, 6] :: [Float]
> :t z
z :: [Float]

> let k = [1.2, 1.3, 1.4, 1.5 ]
> k
[1.2,1.3,1.4,1.5]
> 
> :t k
k :: [Double]
```

### Typeclasses and Types

- Typeclasses are sets of types.
- Types are sets of values.

```
Value -->  Type --> Typeclass
```

#### Basic Types

|            |                   |              |
|------------|-------------------|--------------|
| Char       |  'a' / 'b' / 'c'  |  Char Type   |
| [Char]     |  "String"         |  String      |
| Bool       |   True / False    |  Boolean     |
| Int        |   1, 2, 3, 4      |  Integers in a finite range.  -2^29 to (2^29 - 1) |          
| Integer    |   1, 2, 3, 4      |  Arbitrary Precision Integer |
| Float      | 1.0, 2.0, 3.0     |  32 bits float point |
| Double     | 1.0, 2.0, 3.0     |  64 bits float point |
| (Int, Char)|  (1, 'a')         | Tuples, unlike lists elements can have different types. |
| [a]        | [1, 2, 3, 4]      | List has the type [Int], [Char], [Double] |

#### Typeclasses

* Num : Integer, Int, Double and Float

#### Useful notations for functions 

Credits: http://yannesposito.com/Scratch/en/blog/Haskell-the-Hard-Way/

```
x :: Int            ⇔ x is of type Int
x :: a              ⇔ x can be of any type
x :: Num a => a     ⇔ x can be any type a
                      such that a belongs to Num type class 
f :: a -> b         ⇔ f is a function from a to b
f :: a -> b -> c    ⇔ f is a function from a to (b→c)
f :: (a -> b) -> c  ⇔ f is a function from (a→b) to c
```



### Lists

#### Creating Lists

```haskell

> [-4, 10, 20, 30.40]

> let x = [-23, 40, 60, 89, 100]
> x
[-23,40,60,89,100]


> [0..10]
[0,1,2,3,4,5,6,7,8,9,10]
> 
> [-4..10]
[-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10]
> 

```

#### List Operations

Picking the nth element of a list.

```haskell

> [1, 2, 3, 4, 5, 6] !! 2
3
> [1, 2, 3, 4, 5, 6] !! 3
4
> [1, 2, 3, 4, 5, 6] !! 0
1
```

```haskell
> let lst  = [-4..10]
> lst
[-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10]
```
First Element
```haskell
> head [1, 2, 3, 4, 5]
1
```

Last Element
```haskell
> last [1, 2, 3, 4, 5]
5
```

Maximum element
```haskell
> maximum lst
10
```

Minimum element
```haskell
> minimum lst
-4
```

Reversing a list
```haskell

> reverse [1, 2, 3, 4, 5]
[5,4,3,2,1]
```

Sum of all elements
```haskell
> sum lst
45
```

Product of all elements
```haskell
> product lst
0
```

Adding an element to the beggining of the list

```haskell
> 20 : lst
[20,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10]
```

Adding an element to end of the list

```haskell

> lst ++ [20]
[-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,20]
> 
```

Extract the elements after the head of a list, which must be non-empty. 
* tail :: [a] -> [a]    Source

```haskell
> tail [1, 2, 3, 4, 5]
[2,3,4,5]

```

Return all the elements of a list except the last one. The list must be non-empty.
* init :: [a] -> [a]    Source
```haskell
> init [1, 2, 3, 4, 5]
[1,2,3,4]
> 
```

Make a new list containing just the first N elements from an existing list. 
* take n xs
```haskell
> take 5 lst
[-4,-3,-2,-1,0]
```



Delete the first N elements from a list. 
* drop n xs

```haskell

> lst
[-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10]
> 
> drop 5 lst
[1,2,3,4,5,6,7,8,9,10]

```

Split a list into two smaller lists (at the Nth position). 

* splitAt n xs

```haskell

-- (Returns a tuple of two lists.) 

> splitAt 5 lst
([-4,-3,-2,-1,0],[1,2,3,4,5,6,7,8,9,10])
> 

```

TakeWhile, applied to a predicate p and a list xs, returns the longest 
prefix (possibly empty) of xs of elements that satisfy p:
* takeWhile :: (a -> Bool) -> [a] -> [a]

```haskell

> takeWhile (< 3) [1,2,3,4,1,2,3,4]
[1,2]
> takeWhile (< 9) [1,2,3]
[1,2,3]
>  takeWhile (< 0) [1,2,3]
[]

```

DropWhile p xs returns the suffix remaining after takeWhile p xs: 

* dropWhile :: (a -> Bool) -> [a] -> [a]    Source

```haskell

> takeWhile (< 3) [1,2,3,4,1,2,3,4]
[1,2]
> takeWhile (< 9) [1,2,3]
[1,2,3]
>  takeWhile (< 0) [1,2,3]
[]
> dropWhile (< 3) [1,2,3,4,5,1,2,3] 
[3,4,5,1,2,3]
>  dropWhile (< 9) [1,2,3]
[]
> dropWhile (< 0) [1,2,3] 
[1,2,3]
> 

```


#### Chekings Lists

Check if a list is empty. 
* null xs

```haskell

> null []
True
> null [1, 2, 3, 4, 5]
False

```

Find out whether any list element passes a given test. 
* any my_test xs

```haskell

> any (>3) [1, 2, 3, 4, 5]
True
> any (>10) [1, 2, 3, 4, 5]
False
> 
> any (==3) [1, 2, 3, 4, 5]
True
> 
> any (==10) [1, 2, 3, 4, 5]
False
> 
```

Check whether all list elements pass a given test. 
* all my_test xs

```haskell

> all (>3) [1, 2, 3, 4, 5]
False
> all (<10) [1, 2, 3, 4, 5]
True
> all (<10) [1, 2, 3, 4, 5, 20]
False
> 
```

Check if elements belongs to the list.

* elem :: Eq a => a -> [a] -> Bool

```haskell

> elem 1  [1,2,3] 
True
> elem 4 [1,2,3] 
False
>
```


## Functions

### Creating functions

In the GHCI Shell

```haskell

> let f x y = sqrt ( x^2 + y^2 )
> 
> f 80 60
100.0
> f 50 30
58.309518948453004
> 

> :t f
f :: Floating a => a -> a -> a

```

In a Haskell source file, *.hs

```haskell
f x y = sqrt ( x^2 + y^2 )
```

### Anonymous Functions or Lambda Functions



```haskell

> (\x -> x^2 - 2.5*x) 10
75.0

> let f = \x -> x^2 - 2.5*x

> map f [1, 2, 3, 4, 5]
[-1.5,-1.0,1.5,6.0,12.5]

> map (\x -> x^2 - 2.5*x) [1, 2, 3, 4, 5]
[-1.5,-1.0,1.5,6.0,12.5]

```

### Infix Operators

```haskell

> (+) 10 30.33
40.33

> (-) 100 30
70

> (/) 100 10
10.0

> (*) 40 30
1200

> (^) 2 6
64


> :t (+)
(+) :: Num a => a -> a -> a
> 
> :t (-)
(-) :: Num a => a -> a -> a
> 
> :t (/)
(/) :: Fractional a => a -> a -> a
> 
> :t (*)
(*) :: Num a => a -> a -> a
> 
> :t (^)
(^) :: (Integral b, Num a) => a -> b -> a
```

### Currying


Example 1:

```haskell

> let add a b = a + b
> let add10 = add 10
> 
> add 20 30
50
> add (-10) 30
20
> add10 20
30
> add10 30
40
> map add10 [-10, 20, 30, 40]
[0,30,40,50]
> 
```

Example 2: Derivate functions

### Recursion

Reverse A list

```haskell

reverse2 :: [a] -> [a]
reverse2 []     = []
reverse2 (x:xs) = reverse2 xs ++ [x]

*Main> reverse2 [1, 2, 3, 4, 5]
[5,4,3,2,1]
```

Product of a List

```haskell

prod :: [Int] -> Int
prod [] = 1
prod (x:xs) = x * prod xs


*Main> prod [1, 2, 3, 4, 5]
120
*Main> 
*Main> :t prod
prod :: [Int] -> Int
```
Factorial

```haskell

fact 0 = 1
fact n = n*fact(n-1)

> map fact [1..10]
[1,2,6,24,120,720,5040,40320,362880,3628800]
```

Fibbonacci Function

```haskell
fib 0 = 1
fib 1 = 1
fib n | n>= 2
    = fib(n-1) + fib(n-2)
```

### Higher Order Functions

**Map**

The map functional takes a function as its first argument, then applies it to every element of a list. 
[Programming in Haskell 3rd CCSC Northwest Conference • Fall 2001](http://www.willamette.edu/~fruehr/haskell/lectures/tutorial4.html#@sli@31)

```haskell

> map (^2) [1..10]
[1,4,9,16,25,36,49,64,81,100]

> map (`div` 3) [1..20]
[0,0,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6]

> map reverse ["hey", "there", "world"]
["yeh","ereht","dlrow"]

> reverse ["hey", "there", "world"]
["world","there","hey"]

```

**Higher-order predicates**

Predicates (boolean-valued functions) can be extended to lists via the higher-order predicates any and all. 
[Programming in Haskell 3rd CCSC Northwest Conference • Fall 2001](http://www.willamette.edu/~fruehr/haskell/lectures/tutorial4.html#@sli@31)]

```haskell

> map even [1..5]
[False,True,False,True,False]

> all even (map (2*) [1..5])
True

> any odd [ x^2 | x<-[1..5] ]
True
```

**Foldr**

The fold functions foldl and foldr combine elements of a list based on a binary function and an initial value. 

* Fold right

```haskell

-- Summation from 1 to 10
> foldr (+) 0 [1..10]
55

{- Product from 1 to 10 -}
> foldr (*) 1 [1..10]
3628800
> 

{- Maximum Number in a list -}

> foldr (\x y -> if x >= y then x else y ) 0 [ -10, 100, 1000, 20, 34.23, 10]
1000.0
> 

```


**Other Useful higher-order functions**

The standard Prelude defines scores of useful functions, many of which enjoy great generality due to the abstractional capabilities of polymorphic 
types and higher-order functions [[Programming in Haskell 3rd CCSC Northwest Conference • Fall 2001](http://www.willamette.edu/~fruehr/haskell/lectures/tutorial4.html#@sli@31)]


```haskell
> zipWith (*) [1..10] [1..10]
[1,4,9,16,25,36,49,64,81,100]

> :t replicate
replicate :: Int -> a -> [a]

> zipWith replicate [1..6] ['a'..'z']
["a","bb","ccc","dddd","eeeee","ffffff"]

> takeWhile (<100) [ 2^n | n<-[1..] ]
[2,4,8,16,32,64]

> :t takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
```


## Pattern Matching

Tuple Constructor

```haskell

> let norm3D (x, y, z) = sqrt(x^2 + y^2 + z^2)
> 
> norm3D (33, 11, 3)
34.91418050019218
> 
> norm3D (33, 1, 3)
33.15116890850155
> 
```

```haskell

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

> addVectors (8, 9)(-10, 12)
(-2,21)
> 
> addv1 = addVectors (1, 3)
> 
> let addv1 = addVectors (1, 3)
> 
> map addv1 [(12, 23), (45, 23), (6, 14)]
[(13,26),(46,26),(7,17)]
```

```haskell

add3Dvectors (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z
first  (x, _, _) = x
second (_, y, _) = y
third  (_, _, z) = z

> first (1, 2, 3)
1
> second  (1, 2, 3)
2
> third (1, 2, 3)
3   
> add3Dvectors (23, 12, 233) (10, 100, 30)
(33,112,263)
```

**Guarded Equations**

Absolute Value

```haskell
abs n | n >=0 = n
      | otherwise = -n
```

Signum/Sign Function

```haskell

-- Without Pattern Matching
sign n = if n < 0 then - 1 else if n == 0 then 0 else 1


sign x | x >  0 =  1
       | x == 0 =  0 
       | x <  0 = -1

----------- OR -----


sign n | n <  0    = -1
       | n == 0    = 0
       | otherwise = 1


--------------------

> let sign x | x > 0 = 1 | x == 0 = 0 | x < 0 = -1


> map sign [-4..4]
[-1,-1,-1,-1,0,1,1,1,1]

```

```haskell
f x y | y > z  = x^^2 - 10.5
      | y == z = x+10*y
      | y < z  = x/z + y
      where z = x^2 - 5*y

```

```haskell

units angle sym | sym == "deg" = angle*pi/180.0
                | sym == "rad" = angle

> :{
| let units angle sym | sym == "deg" = angle*pi/180.0
|                 | sym == "rad" = angle
| 
| :}
> 
> units 180 "deg"
3.141592653589793
> 
> units pi "rad"
3.141592653589793
> 
> units 90 "deg" == pi/2
True
> 
> sin(units 90 "deg")
1.0
> sin(units 1.57 "rad")
0.9999996829318346
> 
```

```haskell

password :: (Eq a, Num a) => a -> [Char]
password 3423 = "OK - Safe opened"
password x    = "Error: Wrong Password pal"

> password 10
"Error: Wrong Password pal"
> password 11
"Error: Wrong Password pal"
> password 3423
"OK - Safe opened"
> s
```

```haskell
sayMe :: (Integral a) => a -> [Char]
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5" 

> map sayMe [1..8]
["One!","Two!","Three!","Four!","Five!","Not between 1 and 5", "Not between 1 and 5","Not between 1 and 5"]


```

## List Comprehension

### Simple List Comprehension

```haskell 
> [x^2 | x <- [1..10]]
[1,4,9,16,25,36,49,64,81,100]

>  [ odd x | x <- [1..9]] 
[True,False,True,False,True,False,True,False,True]

```

### Comprehensions with multiple generators

Comprehensions with multiple generators, separated by commas.
The generators are x <- [1, 2, 4] and y <- [4,5].

```haskell
> [(x, y) | x <- [1, 2, 4], y <- [4,5]]
[(1,4),(1,5),(2,4),(2,5),(4,4),(4,5)]

> [(x-y, x+y) | x <- [1, 2, 4], y <- [4,5]]
[(-3,5),(-4,6),(-2,6),(-3,7),(0,8),(-1,9)]

> [(x/y, x*y) | x <- [1, 2, 4], y <- [4,5]]
[(0.25,4.0),(0.2,5.0),(0.5,8.0),(0.4,10.0),(1.0,16.0),(0.8,20.0)]
```

### Function Inside List Comprehension

```haskell

> let f x y = sqrt(x^2 + y^2)

> [ f x y | x <- [1, 2, 4], y <- [4,5]]
[4.123105625617661,5.0990195135927845,4.47213595499958,5.385164807134504,5.656854249492381,6.4031242374328485]

```

### Comprehension with Guards

Guards or filter is a boolean expression that removes elements that would 
otherwise have been included in the list comprehension. They restricts the values 
produced by earlier generators.

Even number sequence

```haskell

> [x | x <- [1..10], even x]
[2,4,6,8,10]

>  [x | x <- [1,5,12,3,23,11,7,2], x>10] 
[12,23,11]

> [(x,y) | x <- [1,3,5], y <- [2,4,6], x<y]
[(1,2),(1,4),(1,6),(3,4),(3,6),(5,6)]


```

Odd Number sequence

```haskell

> [x | x <- [1..10], odd x]
[1,3,5,7,9]
```


Number factors and Prime Numbers

```haskell

> let factors n = [ x | x <- [1..n], mod n x == 0]
> 
> factors 15
[1,3,5,15]
> 
> factors 10
[1,2,5,10]
> 
> factors 100
[1,2,4,5,10,20,25,50,100]
> 
> factors 17
[1,17]
> factors 19
[1,19]

> let prime n = factors n == [1, n]
> 
> prime 17
True
> prime 19
True
> prime 20
False
> 

{- Get all prime numbers until number n -}

> let primes_n n = [ x | x <- [1..n], prime x]
> 
> primes_n 10
[2,3,5,7]
> primes_n 100
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
> 

```




## Abstract Data Type

**Example: Days of Week**

Enumerated sets is type which can only have a limited number of values. 


```haskell

data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
  deriving (Eq, Ord, Enum)

fromDay :: Weekday -> Int
fromDay = fromEnum

toDay :: Int -> Weekday
toDay = toEnum   

> map (Monday<) [ Tuesday, Friday, Sunday]
[True,True,True]

> map (Thursday<) [Monday, Tuesday, Friday, Sunday]
[False,False,True,True]

> 
> Monday == Tuesday
False
> Tuesday == Tuesday
True
>  

> 
> fromDay Saturday 
5

> 1 + fromDay Monday 
1
> 1 + fromDay Saturday 
6
> Saturday 
Saturday
> 
> toDay 0
Monday
> toDay 6
Sunday
> 
> 


```

**Example: Colors**

```haskell

data Color
    = Red
    | Orange
    | Yellow
    | Green
    | Blue
    | Purple
    | White
    | Black
    | CustomColor Int Int Int -- R G B components
    deriving (Eq)

colorToRGB Red    = (255,0,0)
colorToRGB Orange = (255,128,0)
colorToRGB Yellow = (255,255,0)
colorToRGB Green  = (0,255,0)
colorToRGB Blue   = (0,0,255)
colorToRGB Purple = (255,0,255)
colorToRGB White = (255,255,255)
colorToRGB Black = (0,0,0)
colorToRGB (CustomColor r g b) = (r,g,b)   -- this one is new
 
    
> 
> Red == White
False
> 
> Red == Red
True
> 

> let b = CustomColor 120 240 100
> colorToRGB b
(120,240,100)

> map colorToRGB [ Blue, White, Yellow ]
[(0,0,255),(255,255,255),(255,255,0)]
> 


```

**Example: Shapes**

```haskell

data Shape = Circle  Float 
            | Rect   Float Float 

square   :: Float -> Shape
square n =  Rect n n 

area            :: Shape -> Float
area (Circle r)  = pi * r^2
area (Rect x y)  = x  * y

*Main> area $  Rect 20 30
600.0
*Main> area $ Circle 20
1256.6371
*Main> area $ square 20
400.0
*Main> 


```

**Example: Students GPA**

```haskell

data Student = USU String Float 
             deriving (Show)

get_gpa :: Student -> Float
get_gpa (USU _ grade) = grade

get_name :: Student -> String
get_name (USU name _ ) = name

class_gpa :: [Student] -> Float
class_gpa myclass = (sum c) / fromIntegral  (length c)
                  where 
                  c = map get_gpa myclass


*Main> let myke = USU "Mike" 4.0
*Main> 
*Main> get_name myke
"Mike"
*Main> get_gpa myke
4.0
*Main> 

*Main> let myclass = [USU "Mike" 3.7, USU "Steve" 3.9, USU "Fred" 2.9, USU "Joe" 1.5]
*Main> 

*Main> class_gpa myclass 
3.0
*Main

```

## Avoiding Null checking with Maybe

Using the Maybe type is possible to indicate that a function
might or not return value.

```
data Maybe x = Nothing | Just x
```

Examples without Maybe:

```haskell

λ :set prompt "λ > " 
λ > 
λ > 
λ >  head [1, 2, 3, 4]
1
λ > head []
*** Exception: Prelude.head: empty list
 

λ > tail [1, 2, 3, 4]
[2,3,4]
λ > 
λ > tail []
*** Exception: Prelude.tail: empty list

λ > div 10 2
5
λ > div 10 0
*** Exception: divide by zero
λ > 
```

Examples with Maybe monad:

```haskell

fromJust (Just x) = x

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (y:[]) = Just y
safeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (x:[]) = Just []
safeInit (x:xs) = Just (x : fromJust(safeInit xs))

safediv y x | x == 0    = Nothing
            | otherwise = Just(y/x)

λ > fromJust (Just 10)
10

λ > safeHead [1..5]
Just 1
λ > safeHead []
Nothing
λ > 

λ > safeTail  [1..5]
Just [2,3,4,5]
λ > safeTail  []
Nothing
λ > 

λ > let div10by = safediv 10
λ > let div100by = safediv 100


λ > safediv 10 2
Just 5.0
λ > safediv 10 0
Nothing
λ > 
λ > 

λ > div10by 2
Just 5.0

λ > div100by 20
Just 5.0
λ > div100by 0
Nothing
λ > 

λ > map div10by [-2..2]
[Just (-5.0),Just (-10.0),Nothing,Just 10.0,Just 5.0]
λ > 

{- Composition With May be with the >>= (Monad bind operator) -}

λ > div100by (div10by 2)

<interactive>:102:11:
    Couldn't match expected type `Double'
                with actual type `Maybe Double'
    In the return type of a call of `div10by'
    In the first argument of `div100by', namely `(div10by 2)'
    In the expression: div100by (div10by 2)
λ > 

λ > div10by 2 >>= div100by
Just 20.0

λ > div10by 2 >>= div10by >>= div100by 
Just 50.0
λ > 

λ > div10by 2 >>= safediv 0 >>= div100by 
Nothing
λ > 

λ > div10by 0 >>= safediv 1000 >>= div100by 
Nothing
λ > 

```

Reference:  http://www.fatvat.co.uk/2009/10/dealing-with-partial-functions.html 

## Applications


### Mathematics

**Pow Function**

* pow(base, exponent) = base ^ exponent

```haskell
let pow x y = exp $ y * log x

*Main> pow 2 3
7.999999999999998
*Main> 
*Main> pow 2 2
4.0
*Main> pow 2 6
63.99999999999998
*Main> 
*Main> pow 2 0.5
1.414213562373095

```

**Logarithm of Base N**

```haskell

logN n x = (log x)/(log n)

log10 = logN 10
log2  = logN 2

*Main> map log10 [1, 10, 100, 1000]
[0.0,1.0,2.0,2.9999999999999996]

*Main> map log2 [1, 2, 8, 16, 64]
[0.0,1.0,3.0,4.0,6.0]

```

**Trigonometric Degree Functions**

```haskell

deg2rad deg = deg*pi/180.0  -- convert degrees to radians
rad2deg rad = rad*180.0/pi  -- convert radians to degrees

sind = sin . deg2rad        
cosd = cos . deg2rad        
tand = tan . deg2rad
atand = rad2deg . atan
atan2d y x = rad2deg (atan2 y x )
```

### Numerical Methods 

#### Polynomial

Polynomial evaluation by the horner method.

```haskell
polyval :: Fractional a => [a] -> a -> a
polyval coeffs x = foldr (\b c -> b + x*c) 0 coeffs

polyderv :: Fractional a => [a] -> [a] 
polyderv coeffs = zipWith (*) (map fromIntegral [1..n]) (tail coeffs )
    where
    n = (length coeffs) - 1    

```

Example:

```
Reference: http://www.math10.com/en/algebra/horner.html

f(x) = a0 + a1x + a2x2 + a3x3 + a4x4 + a5x5
f(x0) = a0 + x0(a1 + x0(a2 + x0(a3 + x0(a4 + a5x0)))) 

Example: Evaluate the polynomial 
    f(x)  =  1x4 + 3x3 + 5x2 + 7x + 9 at x = 2 
    df(x) =  3x3 + 6x2 + 10x +  7
```

```haskell
    
> let coeffs  = [9.0, 7.0, 5.0, 3.0, 1.0] 
> let f  = polyval  coeffs

let df = polyval $  polyderv coeffs

> polyderv coeffs 
[7.0,10.0,9.0,4.0]

> f 2
83.0

> df 2
95.0

> (\x -> 7 + 10*x + 9*x^2 + 4*x^3) 2
95
```





#### Numerical Derivate

```haskell

derv dx f x = (f(x+dx) - f(x))/dx

f x = 2*x**2 - 2*x
df = derv 1e-5 f

*Main> map f [2, 3, 4, 5] 
[4.0,12.0,24.0,40.0]
*Main> 

*Main> let df = derv 1e-5 f
*Main> 
*Main> map df  [2, 3, 4, 5]
[6.000020000040961,10.000019999978349,14.000019999116374,18.000019998964945]
*Main> 

*Main> let dfx x = 4*x - 2
*Main> map dfx [2, 3, 4, 5]
[6,10,14,18]
```

#### Equation Solving


**Bissection Method**

```haskell

bissection_iterator :: (Floating a, Floating a1, Ord a1) => (a -> a1) -> [a] -> [a]
bissection_iterator f guesslist = newguess
    where
    a =  guesslist !! 0
    b =  guesslist !! 1
    c = (a+b)/2.0
    p = f(a)*f(c)
    newguess = (\p -> if p < 0.0 then [a, c] else [c, b] ) p


bissectionSolver eps itmax f x1 x2 = (root, error, iterations) 
    where  
    
    bissection_error xlist = abs(f $ xlist !! 1)
    check_error xlist = bissection_error xlist > eps

    iterator = bissection_iterator  f

    rootlist = [x1, x2] |> iterate iterator |> takeWhile check_error |> take itmax

    pair = last rootlist |> iterator
    root = last pair
    error = bissection_error pair

    iterations = length rootlist    

*Main> let f x  =  exp(-x) -3*log(x)
*Main> bissectionSolver 1e-5 100 f 0.05 3
(1.1154509544372555,8.86237816760671e-6,19)
*Main> 

```

**Newton Raphson Method**

```haskell
{-
Newton-Raphson Method Iterator, builds an iterator function
fromt the function to be solved and its derivate.

-}
newton_iterator f df x = x - f(x)/df(x)

{---------------------------------------------------------------------
    newtonSolver(eps, itmax, f, df, guess)

    Solve equation using the Newton-Raphson Method.
    
    params:
    
        eps   :  Tolerance of the solver
        itmax :  Maximum number of iterations
        f     :  Function which the root will be computed
        df    :  Derivate of the function
        guess :  Initial guess 

newtonSolver
  :: (Fractional t, Ord t) =>
     t -> Int -> (t -> t) -> (t -> t) -> t -> (t, t, Int)
-----------------------------------------------------------------------
-}
newtonSolver :: (Floating t, Ord t) => t -> Int -> (t -> t) -> (t -> t) -> t -> (t, t, Int)
newtonSolver eps itmax f df guess = (root, error, iterations)
    where
    check_root x = abs(f(x)) > eps                                  
    iterator = newton_iterator f df   -- Builds the Newton Iterator                              
    generator = iterate $ iterator    -- Infinite List that will that holds the roots (Lazy Evaluation)

    rootlist = take itmax $ takeWhile check_root $ generator guess                                  
    root = iterator $ last $ rootlist                                  
    error = abs(f(root))
    iterations = length rootlist


f :: Floating a => a -> a
f x = x^2 - 2.0


square_root a | a > 0       = newtonSolver 1e-6 50 (\x -> x^2 -a) (\x -> 2*x) a 
              | otherwise   = error ("The argument must be positive")

```

**Secant Method**

```haskell

(|>) x f = f x
(|>>) x f = map f x

secant_iterator :: Floating t => (t -> t) -> [t] -> [t]
secant_iterator f guesslist = [x, xnext]
    where
    x =  guesslist !! 0
    x_ = guesslist !! 1
    xnext = x - f(x)*(x-x_)/(f(x) - f(x_))

secantSolver eps itmax f x1 x2 = (root, error, iterations) 
    where  
    
    secant_error xlist = abs(f $ xlist !! 1)
    check_error xlist = secant_error xlist > eps

    iterator = secant_iterator  f

    rootlist = [x1, x2] |> iterate iterator |> takeWhile check_error |> take itmax

    pair = last rootlist |> iterator
    root = last pair
    error = secant_error pair

    iterations = length rootlist

*Main> let f x = x^2 - 2.0
*Main> secantSolver  1e-4 20 f 2 3
(1.4142394822006472,7.331301515467459e-5,6)
*Main> 
*Main> let f x = exp(x) - 3.0*x^2
*Main> secantSolver 1e-5 100 f (-2.0)  3.0
(-0.458964305393305,6.899607281729558e-6,24)
*Main> 

```

### Statistics and Time Series

Arithmetic Mean of a Sequence

```haskell
mean lst = sum lst / fromIntegral (length lst)
```

Geometric Mean of Squence 
```haskell

pow x y = exp $ y * log x
geomean lst = pow (product lst) $ 1/(fromIntegral (length lst))
```

Convert from decimal to percent
```haskell
to_pct   lst = map (100.0 *) lst {- Decimal to percent -}
from_pct lst = map (/100.0)  lsd {- from Percent to Decimal -}
```

Lagged Difference of a time serie
* lagddif [xi] = [x_i+1 - x_i]
```haskell
lagdiff lst = zipWith (-) (tail lst) lst
```

Growth of a Time Serie
* growth [xi] = [(x_i+1 - x_i)/xi]
```haskell
growth lst = zipWith (/) (lagdiff lst) lst
```

Percentual Growth
```haskell
growthp = to_pct . growth
```

Standard Deviation and Variance of a Sequence

```haskell
{- Standard Deviation-}
stdev values =  values   |>> (\x -> x -  mean values ) |>> (^2) |> mean |> sqrt

{- Standard Variance -}
stvar values = stdev values |> (^2)
```

**Example: Investment Return**

The annual prices of an Blue Chip company are given below,
find the percent growth rate at the end of each year and 
the [CAGR](http://www.investopedia.com/articles/analyst/041502.asp) Compound annual growth rate.

```
year    0    1     2     3     4     5
price  16.06 23.83 33.13 50.26 46.97 39.89
```

Solution:

```haskell

> let (|>) x f = f x
> let (|>>) x f = map f x
>
> let cagr prices = (growthp prices |>> (+100) |> geomean ) - 100
>
> let prices = [16.06, 23.83, 33.13, 50.26, 46.97, 39.89 ]
> 
> {- Percent Returns -}
> let returns = growthp prices
> 
> returns
[48.38107098381071,39.02643726395302,51.705402958044054,-6.545961002785513,-15.073451139024908]
> 

> let annual_cagr = cagr prices 
> annual_cagr 
19.956476057259906
> 

```


### Vector

**Dot Product of Two Vectors / Escalar Product**

* v1.v2 = (x1, y1, z1) . (x2, y2, z2) = x1.y1 + y1.y2 + z2.z1
* v1.v2 = Σai.bi

```haskell

> let dotp v1 v2 = sum ( zipWith (*) v1 v2 )   - With Parenthesis
> let dotp v1 v2 = sum $ zipWith (*) v1 v2     - Without Parenthesis with $ operator

> dotp [1.23, 33.44, 22.23, 40] [23, 10, 44, 12]
1820.81


```

**Norm of a Vector**

* norm = sqrt( Σxi^2)

```haskell
> let norm vector = (sqrt . sum) (map (\x -> x^2) vector)

> norm [1, 2, 3, 4, 5]
7.416198487095663

-- Vector norm in multiple line statements in GHCI interactive shell

> :{
| let {
|      norm2 vec =  sqrt(sum_squares)
|      where 
|      sum_squares = sum(map square vec)
|      square x = x*x
|      }
| :}
> 
> norm2 [1, 2, 3, 4, 5]
7.416198487095663
> 

```

**Linspace and Range Matlab Function**

```haskell

linspace d1 d2 n = [d1 + i*step | i <- [0..n-1] ]
    where 
    step = (d2 - d1)/(n-1)
        

range start stop step =  [start + i*step | i <- [0..n] ]
    where
    n = floor((stop - start)/step)

```

## References



* http://www.cis.upenn.edu/~matuszek/Concise%20Guides/Concise%20Haskell98.html
* http://www.cs.arizona.edu/~collberg/Teaching/372/2009/Handouts/Handout-11.pdf
* http://en.wikibooks.org/wiki/Yet_Another_Haskell_Tutorial/Language_basics
* https://courses.cs.washington.edu/courses/cse505/01au/functional/haskell-examples.txts


Toolset
* <http://en.wikibooks.org/wiki/Haskell/Using_GHCi_effectively>

List

* <https://wiki.haskell.org/How_to_work_on_lists>
* <https://hackage.haskell.org/package/base-4.2.0.1/docs/Data-List.html#v%3Atail>

List Comprehension

* <http://www.cs.nott.ac.uk/~gmh/chapter5.ppt>
* <http://www.cs.arizona.edu/~collberg/Teaching/372/2005/Html/Html-13/index.html>
* <http://zvon.org/other/haskell/Outputsyntax/listQcomprehension_reference.html>

Foreign Function Interface - FFI:
* <http://en.wikibooks.org/wiki/Haskell/FFI>

Misc:

* <https://www.fpcomplete.com/blog/2013/06/haskell-from-c>
* <https://wiki.haskell.org/Haskell_programming_tips>
* <http://bayleshanks.com/tutorials-haskell>

Lambda Calculus Concepts
* <https://wiki.haskell.org/Anonymous_function>
* <https://wiki.haskell.org/Closure>
* <https://wiki.haskell.org/Beta_reduction>

Data Types:
* <http://en.wikibooks.org/wiki/Haskell/More_on_datatypes>
* <https://www.fpcomplete.com/school/starting-with-haskell/introduction-to-haskell/2-algebraic-data-types>


Dollar Sign Operator: $
* <http://stackoverflow.com/questions/940382/haskell-difference-between-dot-and-dollar-sign>
* <http://snakelemma.blogspot.com.br/2009/12/dollar-operator-in-haskell.html>
* <http://lambda.jstolarek.com/2012/03/function-composition-and-dollar-operator-in-haskell/>

Pipelining:
* <http://stackoverflow.com/questions/1457140/haskell-composition-vs-fs-pipe-forward-operator>
* <http://stackoverflow.com/questions/4090168/is-there-an-inverse-of-the-haskell-operator>

Control:
* <http://hackage.haskell.org/package/base-4.7.0.2/docs/Control-Monad.html#v:forM>
* <http://en.wikibooks.org/wiki/Haskell/Control_structures>

Interesting:

* <http://research.microsoft.com/en-us/um/people/simonpj/Papers/financial-contracts/contracts-icfp.htm>

**Dr. Erik Meijier Serie: Functional Programming Fundamentals**

* [Function Definition    - Chapter 4 of 13](https://www.youtube.com/watch?v=fQU99SJdWGY)
* [List Comprehensions    - Chapter 5 of 13](https://www.youtube.com/watch?v=cdPyykm2-gg)
* [Recursive functions    - Chapter 6 of 13](https://www.youtube.com/watch?v=2ECvUT3nbqk)
* [Higher Order Functions - Chapter 7 of 13](https://www.youtube.com/watch?v=YRTQkBO2v-s)
* [Functional Parsers     - Chapter 8 of 13](https://www.youtube.com/watch?v=OrAVS4QbMqo)
