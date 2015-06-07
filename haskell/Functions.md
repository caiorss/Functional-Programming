<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Functions](#functions)
  - [Creating functions](#creating-functions)
  - [Anonymous Functions or Lambda Functions](#anonymous-functions-or-lambda-functions)
  - [Infix Functions](#infix-functions)
  - [Infix Operators as Functions](#infix-operators-as-functions)
  - [Currying](#currying)
  - [Function Composition / Composition Operator](#function-composition--composition-operator)
  - [The $ apply operator.](#the--apply-operator)
  - [Recursion](#recursion)
  - [Integer Arithmetic Functions](#integer-arithmetic-functions)
  - [Mathematical Functions](#mathematical-functions)
  - [Standard Functions](#standard-functions)
  - [Higher Order Functions](#higher-order-functions)
    - [Map](#map)
    - [Filter](#filter)
    - [Higher-order predicates](#higher-order-predicates)
    - [Fold](#fold)
    - [Scanl](#scanl)
    - [Curry and Uncurrying](#curry-and-uncurrying)
    - [Flip](#flip)
    - [Iterate](#iterate)
    - [takeWhile](#takewhile)
    - [dropWhile](#dropwhile)
    - [Zip and Unzip](#zip-and-unzip)
    - [ZipWith](#zipwith)
    - [Replicate](#replicate)
    - [Other Useful higher-order functions](#other-useful-higher-order-functions)
  - [Functions to Manipulate Characters and Strings](#functions-to-manipulate-characters-and-strings)
    - [Strings Features](#strings-features)
    - [Prelude String Functions](#prelude-string-functions)
    - [Data.Char String Functions](#datachar-string-functions)
    - [Data.List.Split String Functions](#datalistsplit-string-functions)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

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

> putStrLn "Hello World"
Hello World
> 


```

In a Haskell source file, functions.hs

```haskell

{- Triangle Area as function of sides -}
triangleArea a b c =  sqrt(p * (p - a) * ( p - b) * (p - c))
    where 
    p = (a + b + c) / 2.0

triangleArea2 a b c =  sqrt(p * pa * pb  *  pc)
    where 
    pa = p - a
    pb = p - b
    pc = p - c
    p = (a + b + c) / 2.0
    

triangleArea3 a b c =  let pa = p - a
                           pb = p - b
                           pc = p - c
                           p = (a + b + c) / 2.0
                           in sqrt(p * pa * pb  *  pc)

{- Returning more than one value -}    
areaAndPerimiter a b c =  (2.0 * p, sqrt(p * (p - a) * ( p - b) * (p - c)))
    where 
    p = (a + b + c) / 2.0

    
ft 0 = 10
ft 1 = 30
ft 2 = 50
ft 3 = 8
ft _ = error "Invalid Value"

{- Distance between two points in a plane -}
distance (x1, y1) (x2, y2)= sqrt ((x2 - x1)**2.0 + (y2 - y1)**2.0)
```

Loading functions.hs

```haskell
> :l functions.hs
[1 of 1] Compiling Main             ( /tmp/functions.hs, interpreted )
Ok, modules loaded: Main.
> 
> :t triangleArea 
triangleArea :: Floating a => a -> a -> a -> a

> triangleArea 24.0 30.0 18.0
216.0
> 
> triangleArea2 24.0 30.0 18.0
216.0
> 
> triangleArea3 24.0 30.0 18.0
216.0
> 

> areaAndPerimiter 24.0 30.0 18.0
(72.0,216.0)
> 

> :t distance 
distance :: Floating a => (a, a) -> (a, a) -> a
> 
{- Distance from (0, 0) to (3, 4) -}
> distance (0, 0) (3, 4)
5.0
> 
> distance (-3, -4) (3, 4)
10.0
> 


> ft 0
10
> ft 1
30
> map ft [0, 1, 2, 3]
[10,30,50,8]
> ft 10
*** Exception: Invalid Value
> ft 9
*** Exception: Invalid Value
> 

```

Sources:

* [Heron Formula](http://www.mathopenref.com/heronsformula.html)
 
### Anonymous Functions or Lambda Functions

<!--
@TODO: Add more anonymous functions examples.
-->

```haskell

> (\x -> x^2 - 2.5*x) 10
75.0

> let f = \x -> x^2 - 2.5*x

> map f [1, 2, 3, 4, 5]
[-1.5,-1.0,1.5,6.0,12.5]

> map (\x -> x^2 - 2.5*x) [1, 2, 3, 4, 5]
[-1.5,-1.0,1.5,6.0,12.5]

>  let f = (\x y -> x + y)
>  
>  f 20 30
50
>  let f20 = f 20
>  f20 10
30
>  (\x y -> x/y + x*y) 10 20
200.5
>  (\(x, y) -> x/y + x*y) (10, 20)
200.5
>  


```

### Infix Functions

Any function of two arguments can be used as an infix function or custom operator.

Example:

```haskell
> let addVectors (a1, a2, a3) (b1, b2, b3) = (a1+b1, a2+b2, a3+b3)
> let subVectors (a1, a2, a3) (b1, b2, b3) = (a1-b1, a2-b2, a3-b3)

> :t addVectors 
addVectors :: (Num t, Num t1, Num t2) =>  (t, t1, t2) -> (t, t1, t2) -> (t, t1, t2)

> :t subVectors 
subVectors   :: (Num t, Num t1, Num t2) => (t, t1, t2) -> (t, t1, t2) -> (t, t1, t2)


> addVectors (23, 10, 4) (3, 8, 9)
(26,18,13)


> (23, 10, 4) `addVectors` (3, 8, 9) -- Used as an infix function
(26,18,13)
> 

> subVectors (23, 10, 4) (3, 8, 9)
(20,2,-5)
> 

> (23, 10, 4) `subVectors` (3, 8, 9)
(20,2,-5)
> 

{- Using Custom Operators -}
> let (@+) (a1, a2, a3) (b1, b2, b3) = (a1+b1, a2+b2, a3+b3)
> let (@-) (a1, a2, a3) (b1, b2, b3) = (a1-b1, a2-b2, a3-b3)
> 
> 
> (23, 10, 4) @+ (3, 8, 9)
(26,18,13)
> (23, 10, 4) @-  (3, 8, 9)
(20,2,-5)
> 
>
> (@+) (23, 10, 4) (3, 8, 9)
(26,18,13)
> (@-) (23, 10, 4) (3, 8, 9)
(20,2,-5)
>

> let f x y = 10*x - y**2
> 
> f 10 3
91.0
> 10 `f` 3
91.0
> 

> let addJust (Just a) (Just b) = Just (a+b)
> addJust (Just 10) (Just 30)
Just 40
> 

> (Just 10) `addJust` (Just 30) -- Used as custom operator
Just 40
> 
```


### Infix Operators as Functions

<!--
@TODO: Add more examples with logical infix operators
-->

In Haskell the infix operators can be seen as a two-argument function.

```
x + y is equivalent to add x y or (+ x y)
```


**Arithmetic Operators**

| Shorthand  |  Equivalence         | Type Signature |
|------------|----------------------|----------------|
| (+4)       |  \x -> x + 4         |                |
| (*3)       |  \x -> x * 3         | |
| (/2)       |  \x -> x / 2         | |
| ((-)5)     |  \x -> 5 - x         | |
| (^2)       |  \x -> x ^ 2         | |
| (2^)       |  \x -> 2 ^ x         | |
| (+)        |  \x y -> x + y       | (+) :: Num a => a -> a -> a        |
| (-)        |  \x y -> x - y       | (-) :: Num a => a -> a -> a        |
| (/)        |  \x y -> x / y       | (/) :: Fractional a => a -> a -> a |
| (^)        |  \x y -> x ^ y       | (^) :: (Integral b, Num a) => a -> b -> a |
| (**)       |  \x y -> x ** y      | (**) :: Floating a => a -> a -> a |

**Comparison Operator**

| Shorthand  |  Equivalence         | Type Signature |
|------------|----------------------|----------------|
| (>)        |  \x y -> x > y       | (>) :: Ord a => a -> a -> Bool |
| (<)        |  \x y -> x < y       | (<) :: Ord a => a -> a -> Bool |
| (>=)       |  \x y -> x >= y      | (>=) :: Ord a => a -> a -> Bool |
| (<=)       |  \x y -> x <= y      | (<=) :: Ord a => a -> a -> Bool |
| (==)       |  \x y -> x == y      | (==) :: Eq a => a -> a -> Bool |
| (/=)       |  \x y -> x /= y      | (/=) :: Eq a => a -> a -> Bool |

**Boolean operators**

| Shorthand  |  Equivalence         | Type Signature | Name |
|------------|----------------------|----------------|------|
| (&&)       |  \x y -> x && y      | (&&) :: Bool -> Bool -> Bool   | And |
| (\|\|)     |  \x y -> x \|\| y    | (\|\|) :: Bool -> Bool -> Bool | Or |
| (not)      | not x                |  not :: Bool -> Bool           | Not |

**List and Tuples Operators**

| Shorthand  |  Equivalence         | Type Signature                   | Name            |
|------------|----------------------|----------------------------------|-----------------|
| (,)        |  \x y -> (x, y)      | (,) :: a -> b -> (a, b)          | Tuple of two elements |
| (,,)       |  \x y z -> (x, y, z) | (,,) :: a -> b -> c -> (a, b, c) | Tuple of three elements |
| (!!)       | alist !! i = alist[i] |  (!!) :: [a] -> Int -> a        | Nth element of a list |
| (:)        | \x xs -> x:xs       | (:) :: a -> [a] -> [a]           | Cons            |

**Examples**

```haskell
> (+) 10 30.33
40.33

> map ((+) 10) [1, 20, 43, 44]
[11,30,53,54]
> 

> (-) 100 30
70

> map ((-) 100) [10, 20, 80, -50]
[90,80,20,150]
> 
> map (flip (-)100) [10, 20, 80, -50]
[-90,-80,-20,-150]


> (/) 100 10
10.0

> (*) 40 30
1200

> map (*10) [1, 2, 3, 4]
[10,20,30,40]
> 

> (^) 2 6
64

> 4 ** 0.5
2.0

> 2 ** 0.5
1.4142135623730951

> (**) 2  3.5
11.313708498984761
>

> map ((**) 0.5) [1, 2, 3, 4]
[0.5,0.25,0.125,6.25e-2]

> map ((flip (**)) 0.5) [1, 2, 3, 4]
[1.0,1.4142135623730951,1.7320508075688772,2.0]
> 

> (,) 4 5
(4,5)

> map ((,)4) [1, 2, 3, 4]
[(4,1),(4,2),(4,3),(4,4)]

> map ((flip (,)) 4) [1, 2, 3, 4]
[(1,4),(2,4),(3,4),(4,4)]

> (,,) 4 5 7
(4,5,7)

> ((,,) 4) 5 6
(4,5,6)

> map (uncurry ((,,) 4)) [(5, 6), (1, 1), (3, 4)]
[(4,5,6),(4,1,1),(4,3,4)]

> map ((,,) 12 4) [1, 2, 3, 4]
[(12,4,1),(12,4,2),(12,4,3),(12,4,4)]
> 

> alist =  ['a', 'b', 'c', 'd', 'e'] 
> alist !! 0 
'a'
> alist !! 3
'd'
> 
> (!!3) alist
'd'
> (!!0) alist
'a'
> 
> (!!) alist 0
'a'
> (!!) alist 3
'd'

> map ((!!) alist) [0, 2, 3, 4]
"acde"
> 
> map (!!2) [['a', 'b', 'c'], ['y', 'w', 'x', 'z'], ['1', '2', '3', '4']]
"cx3"
> 


> (>) 30 10
True
> (<) 30 10
False
>
> map (>30) [60, 380, 23, 1, 100]
[True,True,False,False,True]
> 
> filter (>30) [60, 380, 23, 1, 100]
[60,380,100]
> 

> (==) 100 10
False
> (==) 10 10
True
> 
> 
> filter (==10) [100, 10, 20, 10, 30]
[10,10]
> 
> map (uncurry (==)) [(100, 100), (10, 23), (34, 44), (0, 0)]
[True,False,False,True]
> 
> filter (uncurry (==)) [(100, 100), (10, 23), (34, 44), (0, 0)]
[(100,100),(0,0)]
> 

> 10 /= 100
True
> 10 /= 10
False
> 
> 
> filter (/=10) [100, 10, 20, 10, 30]
[100,20,30]
> 
> filter (uncurry (/=)) [(100, 100), (10, 23), (34, 44), (0, 0)]
[(10,23),(34,44)]

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

{- Cons Operator -}
> :t (:)
(:) :: a -> [a] -> [a]
> 

> (1:) [9, 2, 3, 4]
[1,9,2,3,4]
> 

> (:) 1 []
[1]

> (:) 1 [0, 3, 5, 6]
[1,0,3,5,6]
> 


> map (-1:) [[1, 2, 3], [5, 6], [0]]
[[-1,1,2,3],[-1,5,6],[-1,0]]
> 

> map ((:) 89) [[1, 2, 3], [5, 6], [0]]
[[89,1,2,3],[89,5,6],[89,0]]
> 


> 1:[]
[1]
> 1:2:[]
[1,2]
> 1:2:3:[]
[1,2,3]
> 

> 1:[0]
[1,0]
> 2:1:[0]
[2,1,0]
> 

> (2:[1, 9, 8, 10])
[2,1,9,8,10]
> 

> (:[1, 2, 3, 4])  0
[0,1,2,3,4]
> 

> map (:[1, 2, 3, 4])  [89, 77, 55, 66]
[[89,1,2,3,4],[77,1,2,3,4],[55,1,2,3,4],[66,1,2,3,4]]
> 

> map (:["haskell "])  ["amazing", "awsome", "cool" ]
[["amazing","haskell "],["awsome","haskell "],["cool","haskell "]]
> 


```

### Currying

<!--
@TODO: Show the same example in other languages like: F#, Ocaml and Python
-->

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

### Function Composition / Composition Operator

In Haskell the operator (.) dot is used for composing functions. Pure functions can be composed like math functions.

See also: [Function composition (computer science)](http://en.wikipedia.org/wiki/Function_composition_%28computer_science%29)

```
(.) :: (b -> c) -> (a -> b) -> a -> c

Given:
    
    f :: b -> c
    g :: a -> b

(f . g ) x = f (g x)

    h = f . g
    h :: a -> c
```

Function Composition Block Diagram

```haskell
                f . g
        ................................
        . /------\        /------\     . 
a -->   . |  g   |  -->   |  f   | --> .---> c
        . \------/   b    \------/  c  . 
        ................................
           g :: a -> b   f :: b -> c
    
    (.) :: (b -> c) -> (a -> b) -> a -> c
```


Composition Law

```
id . f = f                  Left  identity law
f . id = f                  Right identity law
(f . g) . h = f . (g . h)   Associativity


Constant Function Composition
f       . const a = const (f a)
const a . f       = const a

dentity function            -->  id x = x 
const - Constant Function   --> const a b =  a   
```

Simplifying Code with function composition:

```
    h( f ( g( x)))  ==>  (h . f . g ) x   OR  h . f . g  $ x 
OR   
    h $ f $ g x     ==>   h . f . g $ x    

                                 Point Free Style
composed x = h . f . g $ x ==>   composed = h . f . g 
```

Function Composition with Map


``` 
    (map g (map f xs) == (map g . map f) xs = (map g . f) xs

OR
    map g . map f  == map (g . f)
        
Generalizing
    
    map f1 (map f2 (map f3 (map f4 xs))) 
    = (map f1)
    =  map (f1 . f2 . f3 . f4)  xs     
    =  f xs
    
Where f = map $ f1 . f2 . f3 . f4

Example:

    > map  (+3) [1, 2, 3, 4]
    [4,5,6,7]
    > map  (*2) [4, 5, 6, 7]
    [8,10,12,14]
    > 
    > map  (*2) (map (+3)  [1, 2, 3, 4])
    [8,10,12,14]
    > 
    > map  (*2) . map (+3) $  [1, 2, 3, 4]
    [8,10,12,14]
    > 

    > map ((*2) . (+3)) [1, 2, 3, 4]
    [8,10,12,14]

    > let f = map $ (*2) . (+3)
    > f [1, 2, 3, 4]
    [8,10,12,14]

```


```
h :: c -> [a]
f :: a -> b

map :: (a -> b) -> [a] -> [b]
filter :: (a -> Bool) -> [a] -> [a]


map     f (h c) = map    f . h $ c
filter  f (h c) = filter f . h $ c
```

Inverting Predicate Functions

```
inverted_predicate == not . predicate
```

```haskell
> not True
False
> not False
True
> 

> (>5) 10
True
> (>5) 3
False

> not . (>5) $ 10
False
> not . (>5) $ 3
True
> 

> let f = not . (>5)
> f 10
False
> f 5
True

> import Data.List
> 
> filter ( isPrefixOf "a" ) ["a","ab","cd","abcd","xyz"]
["a","ab","abcd"]
> 
> filter ( not . isPrefixOf "a" ) ["a","ab","cd","abcd","xyz"]
["cd","xyz"]
> 


```


Example:

```haskell
> let f = (+4)
> let g = (*3)
> 
> 
> f (g 6) -- (+4) ((*3) 6) = (+4) 18 = 22
22
> 
> (f . g) 6
22
> 
> (.) f g 6
22
> 
> let h = f . g
> 
> h 6
22
>  

> id 10
10
> id 3
3
> 
> id Nothing
Nothing
> id 'a'
'a'
> id (Just 10)
Just 10
> 


> (f . id) 10
14
> (id . f) 10
14
> 

> const 10 20
10
> const 10 3
10
> 

> (f . (const 10)) 4
14
> (f . (const 10)) 3
14
> const 10 . f $ 7
10
> const 10 . f $ 3
10
> 

{- Avoiding Parenthesis with composition -}
> let g x = x * 2
> let f x = x + 10
> let h x = x - 5
> 
> h (f (g 3))
11
> h $ f $ g 3
11
> 
> (h . f . g ) 3
11
> h . f . g $ 3
11
> 

{- Function Composition with curried functions -}

> let f1 x y = 10*x + 4*y
> let f2 a b c = 4*a -3*b + 2*c
> let f3 x = 3*x

> (f1 3 ( f3 5))
90
> 
> f1 3 $ f3 5
90
> 
> f1 3 . f3 $ 5
90
> 
> let f = f1 3 . f3 
> 
> f 5
90
> f 8
126
> 


> (f1 4 (f2 5 6 (f3 5)))
168
> 
> f1 4 $ f2 5 6 $ f3 5
168
> 
> f1 4 . f2 5 6 . f3 $ 5
168
> 
> let g = f1 4 . f2 5 6 . f3 {- You can also create new functions -}
> :t g
g :: Integer -> Integer
> g 5
168
> g 10
288
> 

{- Function Composition with Map and Filter -}

> import Data.Char

> :t ord
ord :: Char -> Int

> :t ordStr
ordStr :: [Char] -> [Int]
> 

> ordStr "curry"
[99,117,114,114,121]
> 
> let r x= x + 30
> 
> map r (ordStr "curry")
[129,147,144,144,151]
> 
> map r $ ordStr "curry"
[129,147,144,144,151]
> 
> map r . ordStr $ "curry"
[129,147,144,144,151]
> 
> sum . map r . ordStr $ "curry"
715
> 

> let s =  map r . ordStr
> s "curry"
[129,147,144,144,151]
> s "haskell"
[134,127,145,137,131,138,138]
> 

let sum_ord = sum . map r . ordStr 

> sum_s "curry"
715
> sum_s "haskell"
950
> 
> sum_ord "curry"
715
> sum_ord "haskell"
950
> 


> map ord (map toUpper "haskell")
[72,65,83,75,69,76,76]
> 
> map ord . map toUpper $ "haskell"
[72,65,83,75,69,76,76]
> 

> map (flip (-) 10) . map ord . map toUpper $ "haskell"
[62,55,73,65,59,66,66]
> 

> map chr . map (flip (-) 10) . map ord . map toUpper $ "haskell"
">7IA;BB"
> 

{- The function f is in point free style -}

> let f = map chr . map (flip (-) 10) . map ord . map toUpper
> 
> f "haskell"
">7IA;BB"
> 

```

<!--
-->


### The $ apply operator.

```haskell
f $ x = f x

> :t ($)
($) :: (a -> b) -> a -> b
```

Example: This operator is useful to apply an argument to a list of functions.

```haskell
> ($ 10) (*3)
30
> 
> let f x = x*8 - 4
> 
> ($ 10) f
76
> 

> map ($ 3) [(*3), (+4), (^3)]
[9,7,27]
> 

```

OR

```haskell
> let apply x f = f x
> 
> map (apply 3)  [(*3), (+4), (^3)]
[9,7,27]
> 

```

See also the Clojure function [juxt](https://clojuredocs.org/clojure.core/juxt)

Apply a set of functions to a single argument.

```haskell
> let juxt fs x = map ($ x) fs

> juxt [(*3), (+4), (/10)] 30
[90.0,34.0,3.0]
> 
> let fs = juxt [(*3), (+4), (/10)]
> 
> :t fs
fs :: Double -> [Double]
>
> fs 30
[90.0,34.0,3.0]
> fs 40
[120.0,44.0,4.0]
> 
> map fs [10, 20, 30]
[[30.0,14.0,1.0],[60.0,24.0,2.0],[90.0,34.0,3.0]]
> 
> 
```


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

Fibonacci Function

```haskell
fib 0 = 1
fib 1 = 1
fib n | n>= 2
    = fib(n-1) + fib(n-2)
```


### Integer Arithmetic Functions

| Function | Description |
|----------|-------------|
| even     | Test if number is even, multiple of 2 |
| odd      | Test if number is odd, non multiple of 2 |
| quot     | Quotient of two numbers | 
| rem      | Remainder from the quotient | 
| div      | Similar to "quot", but is rounded down towards minus infinity |
| mod | Returns the modulus of the two numbers | 
| divMod | Returns the quotient and the modulus tuple |
| gcd   | Greatest common divisor of two numbers |
| lcd   | Lowest Common Multiple of two numbers |
| compare | Compare two numbers |

Exaples:

```haskell

{- -------------------Interger Division----------------- -}

{- Division Quotient -}

> quot 70 8
8
> 
> quot (-80)  8
-10
> 
> 80 `quot` 8
10
> 

> div 100 8
12
> 
> div (-100) 8
-13
> quot (-100) 8
-12
> 
> 100 `div` 8
12
> 

{- Remainder-}

> rem 100 12
4
> rem 100 10
0
> 100 `rem` 12
4
> 100 `rem` 10
0
> 
> rem (-100) 12
-4
> rem (-100) 10
0
> (-100) `rem` 12
-4
> (-100) `rem` 10
0
> 


> mod 100 12
4
> mod 100 10
0
> 100 `mod` 12
4
> 100 `mod` 10
0
> 
> mod (-100) 12
8
> 
> (-100) `mod` 12
8
> 


{- DivMod Quotient and Modulus of Division -}

> divMod 100 12
(8,4)
> divMod 100 10
(10,0)
> 100 `divMod` 12
(8,4)
> 100 `divMod` 10
(10,0)
> 
> divMod (-100) 12
(-9,8)
> divMod (-100) 10
(-10,0)
> 


{- Odd / Even Test -}

> even 20
True
> even 31
False

> odd 20
False
> odd 31
True

> [1..10]
[1,2,3,4,5,6,7,8,9,10]
> filter odd [1..10]
[1,3,5,7,9]
> filter even [1..10]
[2,4,6,8,10]
> 

{- Greatest Common Divisor -}

> gcd 840 15
15
> 
> gcd 21 14
7
> 
> 

> foldl1 gcd [21, 14, 35, 700]
7
> 


{- Lowest Common Multiple -}

> lcm 9 36
36
> 
> lcm 15 35
105
> 
{- 
    Lowest Common multiple of a list of numbers
    15 = 3 x 5
    35 = 5 x 7
    20 = 4 x 5
    40 = 8 x 5
    
    lcm = 3 x 5 x 8 = 840
-}
> foldl1 lcm [15, 35, 20, 40]
840
> 
```

Reference:

* http://en.wikibooks.org/wiki/Haskell/A_Miscellany_of_Types


### Mathematical Functions

Negate, Sqrt, Log, Exp and Power 

```haskell

{- Negate -}

> negate 100
-100
> negate 100.324
-100.324
> 


{- Natural logarithm -}
> log 10
2.302585092994046
> 

{- Logarithm to any base -}

> let log10 = logBase 10
> let log2  = logBase 2
> 
> log10 100
2.0
> map log10 [1, 10, 20, 100, 1000]
[0.0,1.0,1.3010299956639813,2.0,3.0]
> 
> map log2 [1, 2, 4, 8, 16]
[0.0,1.0,2.0,3.0,4.0]
> 

{- Square Root -}
> sqrt 100
10.0
> sqrt 10
3.1622776601683795
> 

{- Exponential function -}
> exp 1
2.718281828459045
> exp 2
7.38905609893065
> 

{- Pow/ Power Function x^y-}

> sqrt 2
1.4142135623730951
> 2 ** 0.5
1.4142135623730951
> 

> 2** 3
8.0
> 
> (**) 2 3
8.0
> 

> map round [13.03123, 13.20, 13.50, 13.60, 13.992]
[13,13,14,14,14]
> 
```

Trigonometric Functions

```haskell

> pi
3.141592653589793
>

> sin pi
1.2246063538223773e-16
> 
> sin (pi/3)
0.8660254037844386
> 
> asin (0.8660254037844386) == pi/3
True
> 


> cos pi
-1.0
>
> acos (-1)
3.141592653589793
> acos (-1) == pi
True
> 



> atan 1
0.7853981633974483
> 

> atan2 1 (-1)
2.356194490192345
> 
```


Floor, Round, Ceil 

```haskell

> map round [13.03123, 13.20, 13.50, 13.60, 13.992]
[13,13,14,14,14]
> 

> map truncate  [13.03123, 13.20, 13.50, 13.60, 13.992]
[13,13,13,13,13]
> 

> map floor [13.03123, 13.20, 13.50, 13.60, 13.992]
[13,13,13,13,13]
> 

> map ceiling [13.03123, 13.20, 13.50, 13.60, 13.992]
[14,14,14,14,14]
> 
```


Convert Interger to Float Point

```haskell
> let inv x = 1/x

> :t inv
inv :: Fractional a => a -> a
> 


> let v = [1..10]
> v
[1,2,3,4,5,6,7,8,9,10]
> :t v
v :: [Integer]
> 

> inv 10
0.1
> inv 0.1
10.0
> 

> map inv v

<interactive>:20:5:
    No instance for (Fractional Integer) arising from a use of `inv'
    Possible fix: add an instance declaration for (Fractional Integer)
    In the first argument of `map', namely `inv'
    In the expression: map inv v
    In an equation for `it': it = map inv v

> map (inv . fromInteger) v
[1.0,0.5,0.3333333333333333,0.25,0.2,0.16666666666666666,0.14285714285714285,0.125,0.1111111111111111,0.1]
> 


```

### Standard Functions

**id Identity Function**

```haskell
> :t id
id :: a -> a

> 
> id 100
100
> id "Hello World"
"Hello World"
> 
```

**Constant Function**

```haskell
> :t const
const :: a -> b -> a
> 

> let f1 = const 10
> f1 20
10
> f1 0
10
> map f1 [1, 2, 3]
[10,10,10]

``` 


### Higher Order Functions

<!--
@TODO: Add more higher order functions: zip, zip3, zip4, unzip, unzip3 .., zipWith, zipWith3 ..
-->

Higher Order functions are functions that takes functions as arguments.

Why Higher Order Function?

* Common programming idioms, such as applying a function twice, can naturally be encapsulated as general purpose higher-order functions (Hutton);

* Special purpose languages can be defined within Haskell using higher-order functions, such as for list processing, interaction, or parsing (Hutton);

* Algebraic properties of higher-order functions can be used to reason about programs. (Hutton)

Reference:
* [Graham Hutton Lecture](http://www.cs.nott.ac.uk/~gmh/functional.ppt)
* [Graham Hutton - University of Nottingham](http://www.cs.nott.ac.uk/~gmh/)


#### Map

See also: [Map (higher-order function)](http://en.wikipedia.org/wiki/Map_(higher-order_function))

map :: (a -> b) -> [a] -> [b]

The map functional takes a function as its first argument, then applies it to every element of a list. 
[Programming in Haskell 3rd CCSC Northwest Conference • Fall 2001](http://www.willamette.edu/~fruehr/haskell/lectures/tutorial4.html#@sli@31)

```haskell

> map (^2) [1..10]
[1,4,9,16,25,36,49,64,81,100]

> map (`div` 3) [1..20]
[0,0,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6]

{- Map With Anonymous Functions -}
>  map (\x -> x*x - 10*x) [1..10]
[-9,-16,-21,-24,-25,-24,-21,-16,-9,0]


> map reverse ["hey", "there", "world"]
["yeh","ereht","dlrow"]

> reverse ["hey", "there", "world"]
["world","there","hey"]

```

**Example Estimating PI**

Pi number can be approximated by Gregory series. 

http://shuklan.com/haskell/lec06.html#/0/6


```
                n
              _____         k+1
              \         (-1)
            4  \      ___________
               /        2k - 1
              /____
                 1
```

```haskell

>  let f x = 4*(-1)^(x+1)/(2*k - 1) where k = fromIntegral x
>  let piGuess n = sum $ map f [1..n]
>  
>  map piGuess [1, 10, 20, 30, 50, 100]
[4.0,3.0418396189294032,3.09162380666784,3.108268566698947,3.121594652591011,3.1315929035585537]
>
>  {- Approximation Error -}
>  
>  map (pi -) $ map piGuess [1, 10, 20, 30, 50, 100]
[-0.8584073464102069,9.975303466038987e-2,4.996884692195325e-2,3.332408689084598e-2,1.999800099878213e-2,9.99975003123943e-3]


```

#### Filter

filter :: (a -> Bool) -> [a] -> [a]

Returns elements of a list that satisfy a predicate.
Predicate is boolean function which returns True or False.

```haskell

> filter even [1..10]
[2,4,6,8,10]
> 
> filter (>6) [1..20]
[7,8,9,10,11,12,13,14,15,16,17,18,19,20]
> 

```

**Example With custom types**

Credits: http://shuklan.com/haskell/lec06.html#/0/10

```haskell

> data Gender = Male | Female deriving(Show, Eq, Read)
> 
> let people = [(Male, "Tesla"), (Male, "Alber"), (Female, "Zoe"), (Male, "Tom"), (Female, "Olga"), (Female, "Mia"), (Male, "Abdulah")]

> filter (\(a, b) -> a==Female) people
[(Female,"Zoe"),(Female,"Olga"),(Female,"Mia")]
> 
> filter (\(a, b) -> a==Male) people
[(Male,"Tesla"),(Male,"Alber"),(Male,"Tom"),(Male,"Abdulah")]
> 
```


#### Higher-order predicates

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

#### Fold

The fold functions foldl and foldr combine elements of a list based on a binary function and an initial value. In some programming languages fold is known as reduce. The fold in some programming languages Python is called reduce.

"The higher-order library function foldr (“fold right”) encapsulates this simple pattern of recursion, with the function  and the value v as arguments" (Graham Hutton)

**Why Is Foldr Useful?** (Graham Hutton)

* Some recursive functions on lists, such as sum, are simpler to define using foldr;

* Properties of functions defined using foldr can be proved using algebraic properties of foldr, such as fusion and the banana split rule;

* Advanced program optimisations can be simpler if foldr is used in place of explicit recursion.

**Right Fold**

```
foldr f z [x]

    f is a function of two arguments:
    z is is the initial value of the accumulator
    [x] Is a list of values

foldr (+)  10  [1, 2, 3, 4]  =>  (+ 1 (+ 2 (+ 3 (+ 4 10)))) => 20

 
         \ f            (f 1 (f 2 (f 3 (f 4 10)))) => (+ 1 (+ 2 (+ 3 (+ 4 10))))
        / \
       1   \
           /\ f         (f 2 (f 3 (f 4 10)))
          /  \
         2    \
              /\ f      (f 3 (f 4 10))
             /  \
            3    \
                 /\ f   (f 4 10)
                /  \
               /    \
               4     \
                      z = 10
        
```

Foldr Definition:

```
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr []     = v
foldr (x:xs) = x (+) foldr xs
```    

**Left Fold**

```
foldl :: (a -> b -> a) -> a -> [b] -> a

foldl (+)  10  [1, 2, 3, 4]  =>  (+ 4 (+3 (+ 2 (+ 1 10)))) => 20

          \
          /\ f             (f 4 (f 3 (f 2 (f 1 10))))
         /  \
        /    \
       4      \ f          (f 3 (f 2 (f 1 10)))
             / \ 
            /   \
           3     \ f       (f 2 (f 1 10))
                 /\   
                /  \
               2    \ f    (f 1 10)
                   / \
                  /   \
                 1     \
                        z = 10
```


Common Haskell Functions can be defined using fold

```
sum     = foldr (+) 0
product = foldr (*) 1
and     = foldr (&&) True
```


Examples:

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


Reference:
* [Graham Hutton Lecture](http://www.cs.nott.ac.uk/~gmh/functional.ppt)
* [Graham Hutton - University of Nottingham](http://www.cs.nott.ac.uk/~gmh/)


#### Scanl

Shows the intermediate values of a fold.

```haskell

{- Cumulative Sum -}
> scanl (+) 0 [1..5]
[0,1,3,6,10,15]

{- Cumulative Product -}

> scanl (*) 1 [1..5]
[1,1,2,6,24,120]

```

#### Curry and Uncurrying


**Curry**

Converts a function ((a, b) -> c) that has a single argument: a tuple of two values (a, b) to a new function that has a two arguments a and b and returns c. For short: curry converts an uncurried function to a curried function.

```
curry :: ((a, b) -> c) -> a -> b -> c 
```


**Uncurry**

Converts a function (a -> b -> c) that accepts a sequence of arguments a, b and returns c to a function that accepts a tuple of two arguments (a, b) and returns c. For short: it converts a curried function to a function on pairs.

This function and its variants are useful to map a function of multiple arguments over a list of arguments.

```
uncurry :: (a -> b -> c) -> (a, b) -> c
```   

**Example: Uncurrying a function**

```haskell
> let f x y = 10*x - y
>
> :t f
f :: Num a => a -> a -> a
> 
> 
> f 2 4
```

The problem is: how to map f over a list of tuples ??

```haskell
> map f [(1, 2), (4, 5), (9, 10)]

<interactive>:122:5:
    No instance for (Num (t0, t1)) arising from a use of `f'
    Possible fix: add an instance declaration for (Num (t0, t1))
    In the first argument of `map', namely `f'
```

Solution: Uncurry the function f: 

```haskell

> let f' = uncurry f
>
> :t f'
f' :: (Integer, Integer) -> Integer
>
> 
> map f' [(1, 2), (4, 5), (9, 10)]
[8,35,80]
> 
> map (uncurry f) [(1, 2), (4, 5), (9, 10)]
[8,35,80]
> 
```

**Example: Currying a function**

```haskell
> let g (x, y) = 10*x - y
> 
> :t g
g :: Num a => (a, a) -> a
> 
> g (2, 4)
16
> 
> g 2 4
<interactive>:138:1:
    No instance for (Num (a0 -> t0)) arising from a use of `g'
    Possible fix: add an instance declaration for (Num (a0 -> t0))
    In the expression: g 2 4
    In an equation for `it': it = g 2 4
    ...
> 
> let g' = curry g
> :t g'
g' :: Integer -> Integer -> Integer
> 
> g' 2 4
16
> 
> (curry g) 2 4
16
> 
```

**Other Examples**

Map a function of 3 arguments and a function of 4 arguments of over a list of tuples:

```haskell

> let uncurry3 f (a, b, c) = f a b c
> let uncurry4 f (a, b, c, d) = f a b c d
> 
> :t uncurry3
uncurry3 :: (t1 -> t2 -> t3 -> t) -> (t1, t2, t3) -> t
> 
> :t uncurry4
uncurry4 :: (t1 -> t2 -> t3 -> t4 -> t) -> (t1, t2, t3, t4) -> t
> 
> 
> let f a b c = 10*a -2*(a+c) + 5*c
> 
> 
> map (uncurry3 f) [(2, 3, 5), (4, 9, 2), (3, 7, 9)]
[31,38,51]
> 
> 

> 
> let f x y z w = 2*x + 4*y + 10*z + w
> 
> map (uncurry4 f) [(2, 3, 5, 3), (4, 9, 2, 8), (3, 7, 9, 1)]
[69,72,125]
> 
```

#### Flip 

Converts a function of two arguments a, b to a new one with argument in inverse order of the old one.

```
flip :: (a -> b -> c) -> b -> a -> c
```

Example: 

```
> let f a b = 10*a + b
> 
> :t f
f :: Num a => a -> a -> a

> 
> f 5 6
56
>
> f 6 5
65
> 
> 
> (flip f) 5 6
65
> 
```

#### Iterate

This function is useful for recursive algorithms like, root finding, numerical serie approximation, differential equation solving and finite differences.


```
iterate f x = x : iterate f (f x)
```

It creates an infinite list of iterates.

```haskell
[x, f x, f (f x), f (f (f x)), ...]
```

Example: [source](http://www2.mae.ufl.edu/haftka/numerical/Lectures/Chapter6.1-2.pdf)

Find the square root of a number by Fixed-point iteration

```
Xi+1 = g(Xi)
```

The magnitude of the derivative of g must be smaller than 1 to the method work.

```
sqrt(a) --> f(x) = x^2 - a = 0 
x  = 1/2*(a/x+x)
x  = g(x) --> g(x) = 1/2*(a/x+x)
```


```haskell
> let f a x = 0.5*(a/x + x)

> let g = f 2 -- a = 2

> g 2
1.5
> g 1.5
1.4166666666666665
> g 1.41666
1.4142156747561165
> g 1.14142156
1.4468112805021982

{- OR -}

> let gen = iterate g 2
>
> take 5 gen
[2.0,1.5,1.4166666666666665,1.4142156862745097,1.4142135623746899]

{-- 
Finally the root algorithm  using the power of lazy evaluation
with the iterate function

--}

> let f a x = 0.5*(a/x + x)
> let root a =  last $ take 10 $ iterate (f a) a 
> 
> root 2
1.414213562373095
> root 2 - sqrt 2
-2.220446049250313e-16
> 
> root 10
3.162277660168379
> sqrt 10
3.1622776601683795
>
> root 10 - sqrt 10
-4.440892098500626e-16
> 
> 

```

#### takeWhile

Apply a predicate p to a list xs and returns the longest prefix, that can be empty of xs of elements that satisfy the predicate.

```haskell
> :t takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
> 

{- Constant Function that always returns True-}
> :t const True
const True :: b -> Bool
> 
> takeWhile (const True)  [10, 20, 8, 4, 5, 7, 9] 
[10,20,8,4,5,7,9]
> 

> takeWhile (const False)  [10, 20, 8, 4, 5, 7, 9] 
[]
> 

> takeWhile (>5) [10, 20, 8, 4, 5, 7, 9] 
[10,20,8]
> 

> takeWhile (<10) [1, 2, 3, 9, 10, 20, 30]
[1,2,3,9]
> 

> takeWhile (/='a') "testing a function"
"testing "
> 


```

#### dropWhile

The function dropWhile apply a predicate p to a list xs and returns the suffix remaining after takeWhile.

Example:

```haskell
> :t dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
> 

> dropWhile (const True) [10, 20, 8, 4, 5, 7, 9] 
[]

> dropWhile (const False) [10, 20, 8, 4, 5, 7, 9] 
[10,20,8,4,5,7,9]

> dropWhile (>5) [10, 20, 8, 4, 5, 7, 9]
[4,5,7,9]

> dropWhile (/='a') "testing a function"
"a function"
> 

```

#### Zip and Unzip

Zip takes two lists and returns a list of corresponding pairs. If one input list is short, excess elements of the longer list are discarded. 

```haskell
zip :: [a] -> [b] -> [(a, b)]
zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
```

Data.List

```haskell
zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
zip6  :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
```

Unzip transforms a list of pairs into a list of first components and a list of second components. It is the inverse of zip.

```haskell
unzip :: [(a, b)] -> ([a], [b])
unzip3 :: [(a, b, c)] -> ([a], [b], [c])
```

Data.List
```
unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip5 :: [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
unzip6 :: [(a, b, c, d, e, f)] -> ([a], [b], [c], [d], [e], [f])
```

Examples: Zip

```haskell
> zip [1, 3, 4, 5, 6] [10, 30, 40, 50, 60]
[(1,10),(3,30),(4,40),(5,50),(6,60)]
> 

> zip [1, 3, 4, 5, 6] [10, 30, 40]
[(1,10),(3,30),(4,40)]
> 

> zip [5, 6] [10, 30, 40, 50, 60]
[(5,10),(6,30)]
> 

> zip [1, 2, 3, 4, 5] ['a', 'b', 'c', 'd', 'e']
[(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e')]
> 

> zip ["haskell", "ocaml", "sml", "scala", "erlang"] ['a', 'b', 'c', 'd', 'e']
[("haskell",'a'),("ocaml",'b'),("sml",'c'),("scala",'d'),("erlang",'e')]
> 

> zip3 [1, 2, 3, 4, 5] ['a', 'b', 'c', 'd', 'e'] ["haskell", "ocaml", "sml", "scala", "erlang"]
[(1,'a',"haskell"),(2,'b',"ocaml"),(3,'c',"sml"),(4,'d',"scala"),(5,'e',"erlang")]
> 

> zip3 [1, 2, 3, 4, 5] ['a', 'b', 'c', 'd', 'e'] ["haskell", "ocaml", "sml", "scala"]
[(1,'a',"haskell"),(2,'b',"ocaml"),(3,'c',"sml"),(4,'d',"scala")]
> 

> zip3 [4, 5] ['a', 'b', 'c', 'd', 'e'] ["haskell", "ocaml", "sml", "scala"]
[(4,'a',"haskell"),(5,'b',"ocaml")]
> 
> 

> import Data.List
> 
> zip4 [1..5] [5..15] ['a'..'z'] (replicate 4 Nothing)
[(1,5,'a',Nothing),(2,6,'b',Nothing),(3,7,'c',Nothing),(4,8,'d',Nothing)]
> 
```

Example: Unzip

```haskell
> unzip [(1,10),(3,30),(4,40),(5,50),(6,60)]
([1,3,4,5,6],[10,30,40,50,60])
> 

> unzip [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e')]
([1,2,3,4,5],"abcde")
> 

> import Data.List

> unzip3 [(1,'a',"haskell"),(2,'b',"ocaml"),(3,'c',"sml"),(4,'d',"scala")]
([1,2,3,4],"abcd",["haskell","ocaml","sml","scala"])
> 

> unzip4 [(1,5,'a',Nothing),(2,6,'b',Nothing),(3,7,'c',Nothing),(4,8,'d',Nothing)]
([1,2,3,4],[5,6,7,8],"abcd",[Nothing,Nothing,Nothing,Nothing])
> 

> let (x, y) = unzip [(1,'a'),(2,'b'),(3,'c'),(4,'d'),(5,'e')]
> x
[1,2,3,4,5]
> y
"abcde"
> 

> let (a, b, c) = unzip3 [(1,'a',"haskell"),(2,'b',"ocaml"),(3,'c',"sml"),(4,'d',"scala")]
> a
[1,2,3,4]
> b
"abcd"
> c
["haskell","ocaml","sml","scala"]
> 

```


#### ZipWith 

ZipWith function applies a function of two arguments to each elements of two given lists returning a new list.

Defined in Prelude

```haskell
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
```

Defined in Data.List

```haskell
import Data.List
zipWith4   :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith5   :: (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zipWith7   :: (a -> b -> c -> d -> e -> f -> g -> h) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]
```

Examples:

```haskell

{- 

ZipWith takes a function of two arguments and two lists 
returning a new one

    ->  (a -> b -> c)   : Function of two arguments returning type c
    ->  [a]            :  List of type a
    ->  [b]            :  List of type b
    
    Returns 
    ->  [c]            :  List of type c
-}
> :t zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
> 

> :t (+)
(+) :: Num a => a -> a -> a
> 


{- Add each element of two lists -}
> zipWith (+) [1, 2, 3, 4] [9, 10, 3, 5]
[10,12,6,9]
> 
> 

{- Multiply each element of two lists-}
> zipWith (*) [1, 2, 3, 4] [9, 10, 3, 5]
[9,20,9,20]
> 

{- Subtract each element of two lists -}
> zipWith (-) [1, 2, 3, 4] [9, 10, 3, 5]
[-8,-8,0,-1]
> 

{- zipWith with anonymous functions -}

> zipWith (\x y -> 10*x + 4*y) [10, 20, 30] [3, 4, 5]
[112,216,320]
> 
> let f = zipWith (\x y -> 10*x + 4*y)
> 
> f [10, 20, 30] [3, 4, 5]
[112,216,320]
> 


{- New functions can be created by curring arguments -}

> let addVectors = zipWith (+)
> let subVectors = zipWith (-)
> let mulVectors = zipWith (*)
> 
> :t addVectors 
addVectors :: [Integer] -> [Integer] -> [Integer]
> :t subVectors 
subVectors :: [Integer] -> [Integer] -> [Integer]
> :t mulVectors 
mulVectors :: [Integer] -> [Integer] -> [Integer]
> 

> addVectors [1, 2, 3, 4] [9, 10, 3, 5]
[10,12,6,9]
> 
> subVectors [1, 2, 3, 4] [9, 10, 3, 5]
[-8,-8,0,-1]
> mulVectors [1, 2, 3, 4] [9, 10, 3, 5]
[9,20,9,20]
> 

{- Using Functions as operators -}

> [1, 2, 3, 4]  `addVectors` [9, 10, 3, 5]
[10,12,6,9]
> 
> 
> [1, 2, 3, 4]  `subVectors` [9, 10, 3, 5]
[-8,-8,0,-1]
> 
> [1, 2, 3, 4]  `mulVectors` [9, 10, 3, 5]
[9,20,9,20]
> 

> let f x y z = x*y*z 

> zipWith3 f [1, 3, 4, 8] [4, 5, 9] [8, 7, 3]
[32,105,108]
> 
> let zf = zipWith3 f
> 
> zf [1, 3, 4, 8] [4, 5, 9] [8, 7, 3]
[32,105,108]
> 

> import Data.List
> 
> zipWith4 g [12, 34, 1, 4] [8, 19, 33, 23] [5, 7, 8, 9] [33, 78, 17, 14]
[337,2371,-1277,-929]
> 

> let g1 = zipWith4 g [12, 34, 1, 4] 
> g1 [8, 19, 33, 23] [5, 7, 8, 9] [33, 78, 17, 14]
[337,2371,-1277,-929]

> let g2 = g1 [8, 19, 33, 23]
> g2 [5, 7, 8, 9] [33, 78, 17, 14]
[337,2371,-1277,-929]

> let g3 = g2 [5, 7, 8, 9]
> g3  [33, 78, 17, 14]
[337,2371,-1277,-929]

> let g4 = g3 [33, 78, 17, 14]
> g4
[337,2371,-1277,-929]
> 

``` 


#### Replicate

Replicate an element of type a n times.
```
replicate :: Int -> a -> [a]
```

Example:
```haskell
> :t replicate 
replicate :: Int -> a -> [a]
> 

> replicate 4 'a'
"aaaa"
> replicate 6 2.345
[2.345,2.345,2.345,2.345,2.345,2.345]
> 

{- You can also replicate functions -}
> let f = replicate 3 (+1)
> :t f
f :: [Integer -> Integer]
> 
> map ($ 5) f
[6,6,6]
> 

> replicate 3 (Just 5)
[Just 5,Just 5,Just 5]
> 
> replicate 3 Nothing
[Nothing,Nothing,Nothing]
> 

```

#### Other Useful higher-order functions

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


### Functions to Manipulate Characters and Strings

#### Strings Features

**Strings as List of Characters**

```
> 
> ['(', ')', '!', 'a', 'b', 'c', '0', '1']
"()!abc01"
> 
```


**Character Sequences**

```haskell
> ['a'..'z']
"abcdefghijklmnopqrstuvwxyz"

> ['A'..'Z']
"ABCDEFGHIJKLMNOPQRSTUVWXYZ"

> ['0'..'9']
"0123456789"
> 

> ['0'..'z']
"0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz"

```

**Add a character to a string**

```haskell

> "adding " ++ "three" ++ " strings"
"adding three strings"
> 

> "Hello world " ++ ['1']
"Hello world 1"
> 

> 'x' : "Hello World"
"xHello World"
> 
> 'y' : 'x' : "Hello World"
"yxHello World"
> 

```

#### Prelude String Functions

**lines**

Lines breaks a string up into a list of strings at newline characters. The resulting strings do not contain newlines.


```
lines :: String -> [String] 
```

```haskell
> let text = "Hello World\nHaskell\n Is very Cool"
> text
"Hello World\nHaskell\n Is very Cool"


> putStrLn text
Hello World
Haskell
 Is very Cool
> 
> 

> lines text
["Hello World","Haskell"," Is very Cool"]
> 
```

**unlines**

Unlines is an inverse operation to lines. It joins lines, after appending a terminating newline to each.

```
unlines :: [String] -> String 
```

```haskell
> unlines ["Hello World","Haskell"," Is very Cool"]
"Hello World\nHaskell\n Is very Cool\n"
> 
> putStrLn $ unlines ["Hello World","Haskell"," Is very Cool"]
Hello World
Haskell
 Is very Cool

>
```

**words**

Words breaks a string up into a list of words, which were delimited by white space.

```
words :: String -> [String] 
```

```haskell
> words "Hello world haskell 123 2312 --- "
["Hello","world","haskell","123","2312","---"]
> 
```

**unwords**

unwords is an inverse operation to words. It joins words with separating spaces.

```
unwords :: [String] -> String 
```

```haskell
> unwords ["Hello","world","haskell","123","2312","---"]
"Hello world haskell 123 2312 ---"
> 
```

**reverse**

Reverse a string. 

```haskell
> reverse "lleksaH dlroW olleH"
"Hello World Haskell"
> 
```

#### Data.Char String Functions

Documentation: https://hackage.haskell.org/package/base-4.2.0.1/docs/Data-Char.html

```haskell
import Data.Char

> ['0'..'z']
"0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz"
> 

> filter isDigit  ['0'..'z']
"0123456789"
> 

> filter isAlpha ['0'..'z']
"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
> 

> filter isLower ['0'..'z']
"abcdefghijklmnopqrstuvwxyz"
> 
> filter isUpper ['0'..'z']
"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
> 
> 

> filter isHexDigit ['0'..'z']
"0123456789ABCDEFabcdef"
> 

> filter isAlphaNum ['0'..'z']
"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
> 

> map toUpper ['a'..'z']
"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
>

> map toLower ['A'..'Z']
"abcdefghijklmnopqrstuvwxyz"
> 

{- Convert to ascii decimal number -}
> map ord ['a'..'f']
[97,98,99,100,101,102]
> 


{- Convert ascii to char -}
> map chr [97,98,99,100,101,102]
"abcdef"
> 
```


#### Data.List.Split String Functions

The Data.List.Split module contains a wide range of strategies for splitting lists with respect to some sort of delimiter, mostly implemented through a unified combinator interface. The goal is to be flexible yet simple.

Documentation: http://hackage.haskell.org/package/split-0.1.4.1/docs/Data-List-Split.html

```haskell
> import Data.List.Split
> 

> splitOn "," "1232,2323.232,323.434"
["1232","2323.232","323.434"]
> 
> 

> map (\s -> read s :: Double) $ splitOn "," "1232,2323.232,323.434"
[1232.0,2323.232,323.434]
> 
> 

> endBy "," "1232,2323.232,323.434,"
["1232","2323.232","323.434"]
> 
> 

>  splitOneOf ";.," "foo,bar;baz.glurk"
["foo","bar","baz","glurk"]
> 

> chunksOf  3 ['a'..'z']
["abc","def","ghi","jkl","mno","pqr","stu","vwx","yz"]
> 
> 

```



