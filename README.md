## Functional Programming in Haskell by Examples

The purpose of this documentation is to ilustrate functional programming concepts in
Haskell Language by providing reusable and useful pieces of codes and examples.

Notes: 

* The codes with '>' symbol were run in the interactive haskell Shell ghci
and the line bellow without the symbol > are the output.



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
    - [Concepts](#concepts-1)
      - [Pure Functions](#pure-functions)
      - [Lazy Evaluation](#lazy-evaluation)
- [Basic Syntax](#basic-syntax)
  - [Operators](#operators)
    - [Logic Operators](#logic-operators)
    - [Powers](#powers)
    - [Application Operator - $](#application-operator---$)
    - [Misc. Operators](#misc-operators)
    - [Pipelining Operator](#pipelining-operator)
  - [Defining Values and Types](#defining-values-and-types)
  - [Type System](#type-system)
    - [Basic Classes](#basic-classes)
    - [Basic Types](#basic-types)
    - [Numeric Types Conversion](#numeric-types-conversion)
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
    - [Map](#map)
    - [Filter](#filter)
    - [Higher-order predicates](#higher-order-predicates)
    - [Fold](#fold)
    - [Scanl](#scanl)
    - [Iterate](#iterate)
    - [Other Useful higher-order functions](#other-useful-higher-order-functions)
  - [Useful notations for functions](#useful-notations-for-functions)
- [Pattern Matching](#pattern-matching)
- [List Comprehension](#list-comprehension)
  - [Simple List Comprehension](#simple-list-comprehension)
  - [Comprehensions with multiple generators](#comprehensions-with-multiple-generators)
  - [Function Inside List Comprehension](#function-inside-list-comprehension)
  - [Comprehension with Guards](#comprehension-with-guards)
- [Abstract Data Type](#abstract-data-type)
- [Monads, Functors and Applicatives](#monads-functors-and-applicatives)
  - [IO Functions](#io-functions)
  - [Avoiding Null checking with Maybe](#avoiding-null-checking-with-maybe)
- [Applications](#applications)
  - [Mathematics](#mathematics)
  - [Numerical Methods](#numerical-methods)
    - [Polynomial](#polynomial)
    - [Numerical Derivate](#numerical-derivate)
    - [Equation Solving](#equation-solving)
    - [Differential Equations](#differential-equations)
  - [Statistics and Time Series](#statistics-and-time-series)
  - [Vector](#vector)
  - [Small DSL Domain Specific Language](#small-dsl-domain-specific-language)
- [Libraries](#libraries)
  - [Data.Time](#datatime)
    - [Get current year / month / day in Haskell](#get-current-year--month--day-in-haskell)
    - [Get Current Time](#get-current-time)
    - [Documentation By Examples - GHCI shell](#documentation-by-examples---ghci-shell)
      - [Date Time Manipulation](#date-time-manipulation)
      - [Difference between two dates](#difference-between-two-dates)
      - [Day in a Week/Month/Year or Week Number](#day-in-a-weekmonthyear-or-week-number)
      - [Parsing Dates and Times from Strings](#parsing-dates-and-times-from-strings)
      - [Printing a Date](#printing-a-date)
- [Documentation and Learning Materials](#documentation-and-learning-materials)
  - [Documentation](#documentation)
  - [Books](#books)
  - [Articles and Papers](#articles-and-papers)
  - [Community](#community)
- [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->



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
* Strong Static Typed Language 
* Type Inference (The haskell compiler deduce the types for you). 
* Lazy Evaluation ( Dealayed evaluation) by default
* Data Imutability/ Haskell has no variables
    * Values can be bound to a name and can only be assigned once.
    * Values can never change.
* Haskell has not for-loop, while statements.
* Algebraic Data types
* Pattern Matching
* Tail Recursions
* Compiles to native code.


#### Concepts

##### Pure Functions

Pure functions:

* Are functions without side effects, like mathematical functions. 
* For the same input the functions always returns the same output.
* Pure functions doens't rely on global variable, doesn't have internal states.
* Deterministic.

Why Pure Functions:

* Composability, one fuction can be connected to another.
* Can run in parallel, multi threading, multi core and GPU.
* Better debugging.

Example of Pure Functions:

```python
# A pure function
def min(x, y):
    if x < y:
        return x
    else:
        return y


# Impure function 
#   - Impure functions doesn't have always the same output for the same
#   imput. Examples functions that does IO or has Hidden State, Global Variables
exponent = 2

def powers(L):
    for i in range(len(L)):
        L[i] = L[i]**exponent
    return L
```
The function min is pure. It always produces the same result given 
the same inputs and it doesn’t affect any external variable.

The function powers is impure because it not always gives the same output
for the same input, it depends on the global variable exponent:

```python

>>> exponent = 2
>>> 
>>> def powers(L):
...     for i in range(len(L)):
...         L[i] = L[i]**exponent
...     return L
... 
>>> powers([1, 2, 3])
[1, 4, 9]
>>> exponent = 4 
>>> powers([1, 2, 3])  # (Impure since it doesn't give the same result )
[1, 16, 81]
>>> 
```

Another example, puryfing an impure Language:

```python

>>> lst = [1, 2, 3, 4]  # An pure function doesn't modify its arguments.
>>>                     # therefore lst.reverse is impure
>>> x = lst.reverse()
>>> x
>>> lst
[4, 3, 2, 1]

>>> lst.reverse()
>>> lst
[1, 2, 3, 4]
```

Reverse list function purified:

```python

>>> lst = [1, 2, 3, 4]
>>>
>>> def reverse(lst):
...     ls = lst.copy()
...     ls.reverse()
...     return ls
... 
>>> 
>>> reverse(lst)
[4, 3, 2, 1]
>>> lst
[1, 2, 3, 4]
>>> reverse(lst)
[4, 3, 2, 1]
>>> lst
[1, 2, 3, 4]

```


##### Lazy Evaluation

“Lazy evaluation” means that data structures are computed incrementally, as they are needed (so the trees never exist in memory all at once) parts that are never needed are never computed. Haskell uses lazy evaluation by default.

Example in Haskell: 

```haskell
λ> let lazylist = [2..1000000000]
λ> 
λ> let f x = x^6 
λ> 
λ> take 5 lazylist 
[2,3,4,5,6]
λ>
λ>
λ> {- Only the terms needed are computed. -}
λ> take 5 ( map f lazylist )
[64,729,4096,15625,46656]
λ> 
```

Example in Python:

* Python uses eager eavaluation by default. In order to get lazy evaluation in python the programmer must use iterators or generators. The example below uses generator.

```python

def lazy_list():
    """ Infinite list """
    x = 0 
    while True:
        x += 2
        yield x


>>> gen = lazy_list()
>>> next(gen)
2
>>> next(gen)
4
>>> next(gen)
6
>>> next(gen)
8
>>> next(gen)
10
>>> 

def take(n, iterable):
    return [next(iterable) for i in range(n)]

def mapi(func, iterable):   
    while True:
        yield func(next(iterable))
        
f = lambda x: x**5

>>> take(5, lazy_list())
[2, 4, 6, 8, 10]
>>> take(10, lazy_list())
[2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
>>> 

>>> take(5, mapi(f, lazy_list()))
[32, 1024, 7776, 32768, 100000]
>>> 
>>> take(6, mapi(f, lazy_list()))
[32, 1024, 7776, 32768, 100000, 248832]
>>> 

```



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

#### Misc. Operators

```haskell
>>=     bind
>>      then
*>      then
->      to                a -> b: a to b
<-      bind (drawn from) (as it desugars to >>=)
<$>     (f)map
<$      map-replace by    0 <$ f: "f map-replace by 0"
<*>     ap(ply)           (as it is the same as Control.Monad.ap)
$                         (none, just as " " [whitespace])
.       pipe to           a . b: "b pipe-to a"
!!      index
!       index / strict    a ! b: "a index b", foo !x: foo strict x
<|>     or / alternative  expr <|> term: "expr or term"
++      concat / plus / append
[]      empty list
:       cons
::      of type / as      f x :: Int: f x of type Int
\       lambda
@       as                go ll@(l:ls): go ll as l cons ls
~       lazy              go ~(a,b): go lazy pair a, b

_       Whatever          Used in Pattern Matching
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

### Type System

* A type is a collection of related values.

* Typeclasses are sets of types.

* A class is a collection of types that support certain operations, 
called the methods of the class.

* Each expressions must have a valid type, which is calculated before to evaluating the 
expression by the Haskell compiler, it is called type inference;

* Haskell programs are type safe, since type errors can never occur during run time;

* Type inference detects a very large class of programming errors, and is one of the most 
powerful and useful features of Haskell.


Reference: [Graham Hutton - University of Nottingham](http://www.agu.gov.br/page/download/index/id/11184731)

#### Basic Classes

|        |                  |
|--------|------------------|
| Eq     |  Equality Types  |
| Ord    |  Ordered Types   |
| Show   |  Showables Types |
| Read   |  Readable Types  |
| Num    |  Numeric Types   |
| Enum   |  Enum Types      |

Example Methods:

```haskell
(==) :: Eq a    a  a  Bool

(<)  :: Ord a   a  a  Bool

show :: Show a  a  String

read :: Read a  String  a

()  :: Num a   a  a  a
```


```
Value -->  Type --> Typeclass
```

Standard Typeclasses:

* Show: Representable as String
* Enum: Enumerable in a list
* Num:  Usable as a number
* Ord:  Used for thing with total order

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



**Selected Numeric Types**


| Type |  Description |
|------|--------------|
| Double |  Double-precision floating point. A common choice for floating-point data. |
| Float |  Single-precision floating point. Often used when interfacing with C. |
| Int |  Fixed-precision signed integer; minimum range [-2^29..2^29-1]. Commonly used. |
| Int8 |  8-bit signed integer |
| Int16 |  16-bit signed integer |
| Int32 |  32-bit signed integer |
| Int64 |  64-bit signed integer |
| Integer |  Arbitrary-precision signed integer; range limited only by machine resources. Commonly used. |
| Rational |  Arbitrary-precision rational numbers. Stored as a ratio of two Integers. |
| Word |  Fixed-precision unsigned integer; storage size same as Int |
| Word8 |  8-bit unsigned integer |
| Word16 |  16-bit unsigned integer |
| Word32 |  32-bit unsigned integer |
| Word64 |  64-bit unsigned integer |

References: 

* http://shuklan.com/haskell/lec03.html#/0/1
* http://shuklan.com/haskell/lec05.html
* http://book.realworldhaskell.org/read/using-typeclasses.html

| Class      |   Class Intance
|------------|------------------------------|
| Num        | Int, Integer, Nat, Float, Double, Complex  |
| Real       | Int, Integer, Nat. Float, Double, Complex  |
| Fractional | Float, Double, Rational, Complex  |
| Integral   | Int, Nat, Integer, Natural      |
| RealFrac   | Float, Double, Rational, Complex |
| Floating   | Float, Double, Complex    |
| RealFloat  | Float, Double, Complex |


![](images/classes.gif)

#### Numeric Types Conversion

```
fromInteger             :: (Num a) => Integer -> a
fromRational            :: (Fractional a) => Rational -> a
toInteger               :: (Integral a) => a -> Integer
toRational              :: (RealFrac a) => a -> Rational
fromIntegral            :: (Integral a, Num b) => a -> b
fromRealFrac            :: (RealFrac a, Fractional b) => a -> b

fromIntegral            =  fromInteger . toInteger
fromRealFrac            =  fromRational . toRational
```

https://www.haskell.org/tutorial/numbers.html


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

>  let f = (\x y -> x + y)
>  
>  f 20 30
50
>  let f20 = f 20
>  f20 10
30
>  x/y + x*y) 10 20
200.5
>  x/y + x*y) (10, 20)
200.5
>  


```

### Infix Operators


| Shorthand  |  Equivalence   |
|------------|----------------|
| (+4)       |  \x -> x 4     |
| (*3)       |  \x -> x*3     |
| (/2)       |  \x -> x/2     |
| ((-)5)     |  \x -> 5 - x   |
| (^2)       |  \x -> x^2     |
| (2^)       |  \x -> 2^x     |
| (+)        |  \x, y -> x+ y |
| (-)        |  \x, y -> x-y  |
| (/)        |  \x, y -> x/y  |
| (^)        |  \x, y -> x^y  |

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

Higher Order functions are functios that takes functions as 
arguments.

Why Higher Order Function?

* Common programming idioms, such as applying a function twice, can naturally be encapsulated as 
general purpose higher-order functions (Hutton);

* Special purpose languages can be defined within Haskell using higher-order functions, such as 
for list processing, interaction, or parsing (Hutton);

* Algebraic properties of higher-order functions can be used to reason about programs. (Hutton)


Reference:

* [Graham Hutton - University of Nottingham](http://www.agu.gov.br/page/download/index/id/11184731)

#### Map

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

Pi number can be aproximated by Gregory series. 

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

The fold functions foldl and foldr combine elements of a list based on a binary function and an initial value. In some programming languages fold is known as reduce.

"The higher-order library function foldr (“fold right”) encapsulates this simple pattern of recursion, with the function  and the value v as arguments" (Graham Hutton)

**Why Is Foldr Useful?** (Graham Hutton)

* Some recursive functions on lists, such as sum, are simpler to define using foldr;

* Properties of functions defined using foldr can be proved using algebraic properties of foldr, such as fusion and the banana split rule;

* Advanced program optimisations can be simpler if foldr is used in place of explicit recursion.

```
foldr :: (a -> b -> b) -> b -> [a] -> b

foldl :: (a -> b -> a) -> a -> [b] -> a

foldr []     = v
foldr (x:xs) = x (+) foldr xs


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
* [Graham Hutton - University of Nottingham](http://www.agu.gov.br/page/download/index/id/11184731)

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

The magnitude of the derivate of g must be smaller than 1 to the method work.

```haskell
{-
sqrt(a) --> f(x) = x^2 - a = 0 
x  = 1/2*(a/x+x)
x  = g(x) --> g(x) = 1/2*(a/x+x)
-}

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

### Useful notations for functions 

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

**Example: Typeclass with record Syntax**

```haskell

data Person = Person { firstName :: String, 
                       lastName :: String, 
                       age :: Int 
                     }
                     deriving (Eq, Show, Read)


people = [ Person { firstName = "Ayn",  lastName = "Rand",  age =50},
           Person { firstName = "John", lastName = "Galt",  age =28},
           Person { firstName = "Adam", lastName = "Smith", age =70}]


{- Get someone from the people database -}
getPerson n = people !! n

{- Show Person -}
showPerson :: Person -> String
showPerson person  = "Name: " ++ show(firstName person) ++ " - Last Name: " ++ show(lastName person) ++ " - Age " ++ show(age person) 

λ > people
[Person {firstName = "Ayn", lastName = "Rand", age = 50},Person {firstName = "John", lastName = "Galt", age = 28},Person {firstName = "Adam", lastName = "Smith", age = 70}]
λ > 

λ > map firstName people
["Ayn","John","Adam"]
λ > 

λ > map (\el ->  fst el ++ " " ++  snd el) $ zip (map firstName people) (map lastName people)
λ >  ["Ayn Rand","John Galt","Adam Smith"]


λ > people !! 1
Person {firstName = "John", lastName = "Galt", age = 28}
λ > people !! 2
Person {firstName = "Adam", lastName = "Smith", age = 70}
λ > 

λ > "person 0 is " ++ show (people !! 0)
"person 0 is Person {firstName = \"Ayn\", lastName = \"Rand\", age = 50}"
λ > 
λ > "person 1 is " ++ show (people !! 1)
"person 1 is Person {firstName = \"John\", lastName = \"Galt\", age = 28}"
λ >


λ > let person = read "Person {firstName =\"Elmo\", lastName =\"NA\", age = 0}" :: Person
λ > person
Person {firstName = "Elmo", lastName = "NA", age = 0}
λ > 
λ > firstName person 
"Elmo"
λ > last
last      lastName
λ > lastName person 
"NA"
λ > age person
0
λ > 

λ > let tesla = Person { firstName = "Nikola", lastName = "Tesla", age =30}
λ > tesla
Person {firstName = "Nikola", lastName = "Tesla", age = 30}
λ > 

λ > showPerson tesla
"Name: \"Nikola\" - Last Name: \"Tesla\" - Age 30"
λ > 

```

Reference: http://learnyouahaskell.com/making-our-own-types-and-typeclasses


## Monads, Functors and Applicatives

Monads in Haskell are used to perform IO, State, Pararelism, Exception Handling, parallellism, continuations and coroutines.

A monad is defined by three things:

* a type constructor M;
* a function return  ;
* an operator (>>=) which is pronounced "bind".

* Return:     return :: Monad m => a -> m a
* Bind:       (>>=)  :: (Monad m) => m a -> (a -> m b) -> m b

In Haskell, the Monad type class is used to implement monads. It is provided by the Control.Monad module and included in the Prelude. The class has the following methods:

```haskell
    class Monad m where
        return :: a -> m a
        (>>=)  :: m a -> (a -> m b) -> m b
 
        (>>)   :: m a -> m b -> m b
        fail   :: String -> m a
```

Under this interpretation, the functions behave as follows:

* fmap applies a given function to every element in a container
* return packages an element into a container,
* join takes a container of containers and flattens it into a single container.

```haskell

    fmap   :: (a -> b) -> M a -> M b  -- functor
    return :: a -> M a
    join   :: M (M a) -> M a
    
```

**Monad Laws**

In Haskell, all instances of the Monad type class (and thus all implementations of (>>=) and return) must obey the following three laws below:

```haskell
    m >>= return     =  m                        -- right unit
    return x >>= f   =  f x                      -- left unit

    (m >>= f) >>= g  =  m >>= (\x -> f x >>= g)  -- associativity
```



[Under Construction]

```

return :: Monad m => a -> m a

{- Bind Operator -}
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b

sequence  :: Monad m => [m a] -> m [a] 
sequence_ :: Monad m => [m a] -> m () 
mapM      :: Monad m => (a -> m b) -> [a] -> m [b]
mapM_     :: Monad m => (a -> m b) -> [a] -> m ()


{- monad composition operator -}
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = \x -> f x >>= g


data  Maybe a     =  Nothing | Just a  deriving (Eq, Ord, Read, Show)
data  Either a b  =  Left a | Right b  deriving (Eq, Ord, Read, Show)
data  Ordering    =  LT | EQ | GT deriving
                                  (Eq, Ord, Bounded, Enum, Read, Show)


```

### IO Functions

Haskell uses the data type IO (IO monad) for actions.

* > let n = v   Binds n to value v
* > n <- a      Executes action a and binds the anme n to the result
* > a           Executes action a
* do  notation  is syntactic sugar for (>>=) operations. 

**Read and Show**

```
show   :: (Show a) => a -> String
read   :: (Read a) => String -> a

{- lines 
    split string into substring at new line character \n \r
-}
lines :: String -> [String]
```

Example:

```haskell

λ > show(12.12 + 23.445)
"35.565"
λ > 

λ > read "1.245" :: Double
1.245
λ > 
λ > let x = read "1.245" :: Double
λ > :t x
x :: Double
λ > 
λ > read "[1, 2, 3, 4, 5]" :: [Int]
[1,2,3,4,5]
λ > 

```

**Output Functions**

```haskell
print :: Show a => a -> IO ()
putStrLn :: String -> IO ()

```

**Intput Functions**

```haskell
getChar             :: IO Char
getLine             :: IO String
getContents         :: IO String
interact            :: (String -> String) -> IO ()
readIO              :: Read a => String -> IO a
readLine            :: Read a => IO a
```

Examples:

```haskell

λ > :t getLine 
getLine :: IO String
λ > 
λ > 
λ > x <- getLine
hello
λ > x
"hello"
λ > :t x
x :: String
λ > 


```


**Sources**

* http://en.wikibooks.org/wiki/Haskell/Understanding_monads
* http://shuklan.com/haskell/lec09.html#/
* http://learnyouahaskell.com/functors-applicative-functors-and-monoids
* http://squing.blogspot.com.br/2008/01/unmonad-tutorial-io-in-haskell-for-non.html

### Avoiding Null checking with Maybe

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

Reference:  

* http://www.fatvat.co.uk/2009/10/dealing-with-partial-functions.html 
* http://en.wikibooks.org/wiki/Haskell/Understanding_monads

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

#### Differential Equations

**Euler Method**

The task is to implement a routine of Euler's method and then to use it to solve the given example of Newton's cooling law with it for three different step sizes of 2 s, 5 s and 10 s and to compare with the analytical solution. The initial temperature T0 shall be 100 °C, the room temperature TR 20 °C, and the cooling constant k 0.07. The time interval to calculate shall be from 0 s to 100 s

From: http://rosettacode.org/wiki/Euler_method

```
Solve differential equation by the Euler's Method.

    T(t)
    ---- =  -k(T(t) - Tr)
     dt
    
    T(t) = Tr + k(T0(t) - Tr).exp(-k*t)
```

```haskell


import Graphics.Gnuplot.Simple


eulerStep f step (x, y)= (xnew, ynew)
                    where
                    xnew = x + step
                    ynew = y + step * (f (x, y))

euler :: ((Double, Double) -> Double) -> Double -> Double -> Double -> Double -> [(Double, Double)]
euler f x0 xf y0 step = xypairs
                     where
                     iterator = iterate $ eulerStep f step
                     xypairs = takeWhile (\(x, y) -> x <= xf ) $ iterator (x0, y0)

λ> let dTemp k temp_r (t, temp) = -k*(temp - temp_r)

λ> euler (dTemp 0.07 20.0) 0.0 100.0 100.0 5.0
[(0.0,100.0),(5.0,72.0),(10.0,53.8),(15.0,41.97) \.\.\.
(100.0,20.01449963666907)]
λ> 

let t_temp = euler (dTemp 0.07 20.0) 0.0 100.0 100.0 5.0

plotList [] t_temp

```

![](images/euler_newton_cooling.png)

**Runge Kutta RK4**

See also: [Runge Kutta Methods](http://en.wikipedia.org/wiki/Runge%E2%80%93Kutta_methods)

```haskell

import Graphics.Gnuplot.Simple

rk4Step f h (x, y) = (xnext, ynext)
                      where
                      
                      k1 = f (x, y)
                      k2 = f (x+h/2, y+h/2*k1)
                      k3 = f (x+h/2, y+h/2*k2)
                      k4 = f (x+h,y+h*k3)
                      
                      xnext = x + h
                      ynext = y + h/6*(k1+2*k2+2*k3+k4)
                      

rk4 :: ((Double, Double) -> Double) -> Double -> Double -> Double -> Double -> [(Double, Double)]
rk4 f x0 xf y0 h = xypairs
                     where
                     iterator = iterate $ rk4Step f h
                     xypairs = takeWhile (\(x, y) -> x <= xf ) $ iterator (x0, y0)


λ> let t_temp = rk4 (dTemp 0.07 20.0) 0.0 100.0 100.0 5.0

plotList [] t_temp


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

### Small DSL Domain Specific Language

  
Simple DSL for describing cups of Starbucks coffee and computing prices (in dollars). 
Example taken from: http://www.fssnip.net/9w 


starbuck_dsl.hs

```haskell
data Size  = Tall | Grande | Venti
            deriving (Eq, Enum, Read, Show, Ord)
 
data Drink = Latte | Cappuccino | Mocha | Americano            
            deriving (Eq, Enum, Read, Show, Ord)

data Extra = Shot | Syrup
            deriving (Eq, Enum, Read, Show, Ord)

data Cup = Cup {
                cupDrink :: Drink,
                cupSize  :: Size,
                cupExtra :: [Extra]         
               }
               deriving(Eq, Show, Read)

{-
 -                  Table in the format:
 -                 -------------------
 -                  tall, grande, venti 
 -    Latte         p00   p01     p02
 -    Cappuccino    p10   p11     p12
 -    Mocha         p20   p21     p22
 -    Amaericano    p30   p31     p32
 -}

table = [
    [2.69, 3.19, 3.49],
    [2.69, 3.19, 3.49],
    [2.99, 3.49, 3.79],
    [1.89, 2.19, 2.59]
    ]    


extraPrice :: Extra -> Double
extraPrice Syrup = 0.59
extraPrice Shot  = 0.39

priceOfcup cup =  baseprice + extraprice
            where
            drinkrow = table !!  fromEnum  (cupDrink cup)
            baseprice   = drinkrow !!  fromEnum  (cupSize cup)
            extraprice = sum $ map extraPrice (cupExtra cup)
            


{- Constructor of Cup -}
cupOf drink size extra = Cup { 
                             cupSize = size, 
                             cupDrink = drink, 
                             cupExtra = extra}

drink_options = [ Latte, Cappuccino, Mocha, Americano]
size_options  = [ Tall, Grande, Venti]  
extra_options = [[], [Shot], [Syrup], [Shot, Syrup]]

cup_combinations =  
            [ cupOf drink size extra | drink <- drink_options, size <- size_options, extra <- extra_options]

```

Example:


```haskell
> :load starbucks_dsl.hs 
[1 of 1] Compiling Main             ( starbucks_dsl.hs, interpreted )
Ok, modules loaded: Main.
> 
> 

> let myCup = cupOf Latte Venti [Syrup]
> let price = priceOfcup myCup 
> myCup 
Cup {cupDrink = Latte, cupSize = Venti, cupExtra = [Syrup]}
> price
4.08
> 

> priceOfcup (cupOf Cappuccino Tall [Syrup, Shot])
3.67
> 

> let cups = [ cupOf Americano Venti extra |  extra <- extra_options]
> cups
[Cup {cupDrink = Americano, cupSize = Venti, cupExtra = []},
Cup {cupDrink = Americano, cupSize = Venti, cupExtra = [Shot]},
Cup {cupDrink = Americano, cupSize = Venti, cupExtra = [Syrup]},
Cup {cupDrink = Americano, cupSize = Venti, cupExtra = [Shot,Syrup]}]
> 

> let prices = map priceOfcup cups
> prices
[2.59,2.98,3.1799999999999997,3.57]
> 

> let cupPrices = zip cups prices
> cupPrices
[(Cup {cupDrink = Americano, cupSize = Venti, cupExtra = []},2.59),
(Cup {cupDrink = Americano, cupSize = Venti, cupExtra = [Shot]},2.98),
(Cup {cupDrink = Americano, cupSize = Venti, cupExtra = [Syrup]},3.1799999999999997),
(Cup {cupDrink = Americano, cupSize = Venti, cupExtra = [Shot,Syrup]},3.57)]
> 

```


## Libraries

### Data.Time

#### Get current year / month / day in Haskell

UTC time:

Note that the UTC time might differ from your local time depending on the timezone.

```haskell
import Data.Time.Clock
import Data.Time.Calendar

main = do
    now <- getCurrentTime
    let (year, month, day) = toGregorian $ utctDay now
    putStrLn $ "Year: " ++ show year
    putStrLn $ "Month: " ++ show month
    putStrLn $ "Day: " ++ show day
```

#### Get Current Time

Local time:

It is also possible to get your current local time using your system’s default timezone:

```haskell
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    let (year, month, day) = toGregorian $ localDay zoneNow
    putStrLn $ "Year: " ++ show year
    putStrLn $ "Month: " ++ show month
    putStrLn $ "Day: " ++ show day
```


```haskell
import Data.Time.Clock
import Data.Time.LocalTime

main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone now
    putStrLn $ "Hour: " ++ show hour
    putStrLn $ "Minute: " ++ show minute
    -- Note: Second is of type @Pico@: It contains a fractional part.
    -- Use @fromIntegral@ to convert it to a plain integer.
    putStrLn $ "Second: " ++ show second
```

#### Documentation By Examples - GHCI shell

##### Date Time Manipulation

```haskell
import Data.Time

λ > :t getCurrentTime
getCurrentTime :: IO UTCTime
λ > 


λ > t <- getCurrentTime
λ > t
2015-03-04 23:22:39.046752 UTC
λ > 

λ > :t t
t :: UTCTime


λ > today <- fmap utctDay getCurrentTime 
λ > today
2015-03-04
λ > :t today
today :: Day
λ > 
λ > 

λ >  let (year, _, _) = toGregorian today
λ > year
2015
λ > 

λ > :t fromGregorian 2015 0 0
fromGregorian 2015 0 0 :: Day

λ > fromGregorian 2015 0 0
2015-01-01
λ > 

λ > diffDays today (fromGregorian year 0 0)
62
λ > 

λ > import Text.Printf
λ >
λ > tm <- getCurrentTime
λ >  let (year, month, day) = toGregorian (utctDay tm)
λ > year
2015
λ > month
3
λ > day
4
λ > 

λ > printf "The current date is %04d %02d %02d\n" year month day
The current date is 2015 03 04


λ > import System.Locale
λ > 
λ > fmap (formatTime defaultTimeLocale "%Y-%m-%d") getCurrentTime
"2015-03-04"
λ > 
λ > 

```

##### Difference between two dates

```haskell

λ > import Data.Time
λ > import Data.Time.Clock.POSIX
λ > 

λ > let bree = UTCTime (fromGregorian 1981 6 16) (timeOfDayToTime $ TimeOfDay 4 35 25) -- 1981-06-16 04:35:25 UTC
λ > bree
1981-06-16 04:35:25 UTC
λ > 


λ > let nat  = UTCTime (fromGregorian 1973 1 18) (timeOfDayToTime $ TimeOfDay 3 45 50) -- 1973-01-18 03:45:50 UTC
λ > nat
1973-01-18 03:45:50 UTC
λ > 


λ > 
λ > let bree' = read "1981-06-16 04:35:25" :: UTCTime
λ > bree'
1981-06-16 04:35:25 UTC
λ > :t bree'
bree' :: UTCTime
λ > 
λ > let nat'  = read "1973-01-18 03:45:50" :: UTCTime
λ > 
λ > nat'
1973-01-18 03:45:50 UTC
λ > 


λ > difference = diffUTCTime bree nat / posixDayLength
λ > difference 
3071.03443287037s
λ > 

λ >  "There were " ++ (show $ round difference) ++ " days between Nat and Bree"
"There were 3071 days between Nat and Bree"
λ > 
```

##### Day in a Week/Month/Year or Week Number

```haskell

λ > import Data.Time
λ > import Data.Time.Calendar.MonthDay
λ > import Data.Time.Calendar.OrdinalDate
λ > import System.Locale

λ > :t fromGregorian
fromGregorian :: Integer -> Int -> Int -> Day

λ > let (year, month, day) = (1981, 6, 16) :: (Integer , Int , Int )
λ > 
λ > let date = (fromGregorian year month day)
λ > date
1981-06-16
λ > 
λ > let (week, week_day) = sundayStartWeek date
λ > week
24
λ > week_day
2
λ > 

λ > let (year_, year_day) = toOrdinalDate date
λ > year_
1981
λ > year_day
167
λ > 


λ > let (week_day_name, _) = wDays defaultTimeLocale !! week_day
λ > week_day_name
"Tuesday"
λ > 

λ > :t defaultTimeLocale 
defaultTimeLocale :: TimeLocale
λ > 
λ > defaultTimeLocale 
TimeLocale {wDays = [("Sunday","Sun"),("Monday","Mon"),("Tuesday","Tue"),("Wednesday","Wed"),("Thursday","Thu"),("Friday","Fri"),("Saturday","Sat")], months = [("January","Jan"),("February","Feb"),("March","Mar"),("April","Apr"),("May","May"),("June","Jun"),("July","Jul"),("August","Aug"),("September","Sep"),("October","Oct"),("November","Nov"),("December","Dec")], intervals = [("year","years"),("month","months"),("day","days"),("hour","hours"),("min","mins"),("sec","secs"),("usec","usecs")], amPm = ("AM","PM"), dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y", dateFmt = "%m/%d/%y", timeFmt = "%H:%M:%S", time12Fmt = "%I:%M:%S %p"}
λ > 
λ > 
```

##### Parsing Dates and Times from Strings

```haskell
λ > import Data.Time
λ > import Data.Time.Format
λ > import Data.Time.Clock.POSIX
λ > import System.Locale

λ > let day:: Day ; day = readTime defaultTimeLocale "%F" "1998-06-03"
λ > 
λ > day
1998-06-03
λ > 

```

##### Printing a Date

```haskell
λ > import Data.Time
λ > import Data.Time.Format
λ > import System.Locale
λ > 
λ > now <- getCurrentTime
λ > :t now
now :: UTCTime
λ > 
λ > formatTime defaultTimeLocale "The date is %A (%a) %d/%m/%Y" now
"The date is Wednesday (Wed) 04/03/2015"
λ > 
λ > 

λ > let t = do now <- getCurrentTime ; return $ formatTime defaultTimeLocale "The date is %A (%a) %d/%m/%Y" now
λ > t
"The date is Wednesday (Wed) 04/03/2015"
λ > 


```


Credits: 

* http://techoverflow.net/blog/2014/06/13/get-current-year-month-day-in-haskell/
* http://techoverflow.net/blog/2014/08/24/get-current-hour-minute-second-in-haskell/
* http://pleac.sourceforge.net/pleac_haskell/datesandtimes.html

## Documentation and Learning Materials


### Documentation

Haskell API search engine, which allows you to search many 
standard Haskell libraries by either function name, or by approximate type signature. 

![](https://www.haskell.org/hoogle/res/hoogle.png)

* [Hoogle](https://www.haskell.org/hoogle)

* https://www.haskell.org/documentation

Haskell Packages

* http://hackage.haskell.org/packages/archive/pkg-list.html

**Standard Library Prelude.hs**

Prelude.hs is the standard library loaded when Haskell starts.

* [Prelude - Standard defined functions documentation](http://hackage.haskell.org/package/base-4.7.0.2/docs/Prelude.html#v:getContents)
* [Prelude Source Code](https://www.haskell.org/onlinereport/standard-prelude.html)
* [Examples of How to use prelude functions](http://www.cse.unsw.edu.au/~en1000/haskell/inbuilt.html)

**Type Classe**

* http://www.haskell.org/haskellwiki/Typeclassopedia

**Monads**

* http://www.haskell.org/haskellwiki/All_About_Monads
* http://www.haskell.org/haskellwiki/IO_inside
* http://en.wikibooks.org/wiki/Haskell/Understanding_monads


**Install**

Binaries and Installation files: https://www.haskell.org/platform/

Install Haskell Libraries:

```
cabal update

cabal install <some package>
```

### Books

* [Real-World Haskell by Bryan O'Sullivan et al.](http://book.realworldhaskell.org)
* http://learnyouahaskell.com/
* http://en.wikibooks.org/wiki/Haskell

* [Haskell Data Analysis Cookbook](http://haskelldata.com/)

### Articles and Papers

* [Why the world needs Haskell](http://www.devalot.com/articles/2013/07/why-haskell.html)
* [Why functional programming still matters by John Hughes](http://www.cse.chalmers.se/~rjmh/Papers/whyfp.pdf)
* [Composing contracts: an adventure in financial engineering ](http://research.microsoft.com/en-us/um/people/simonpj/Papers/financial-contracts/contracts-icfp.htm) - Simon Peyton Jones et al.
* http://paulkoerbitz.de/posts/Why-I-love-Haskell.html

### Community

* http://reddit.com/r/haskell
* http://www.reddit.com/r/haskellquestions
* http://stackoverflow.com/questions/tagged?tagnames=haskell
* irc.freenode.net #haskell on Freenode

Haskell Wiki             

* http://www.haskell.org/haskellwiki/Category:Tutorials
* http://www.haskell.org/haskellwiki/Category:Mathematics
* http://www.haskell.org/haskellwiki/Category:FAQ
* http://www.haskell.org/haskellwiki/Category:Communitys



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


**Dr. Erik Meijier Serie: Functional Programming Fundamentals**

* [Function Definition    - Chapter 4 of 13](https://www.youtube.com/watch?v=fQU99SJdWGY)
* [List Comprehensions    - Chapter 5 of 13](https://www.youtube.com/watch?v=cdPyykm2-gg)
* [Recursive functions    - Chapter 6 of 13](https://www.youtube.com/watch?v=2ECvUT3nbqk)
* [Higher Order Functions - Chapter 7 of 13](https://www.youtube.com/watch?v=YRTQkBO2v-s)
* [Functional Parsers     - Chapter 8 of 13](https://www.youtube.com/watch?v=OrAVS4QbMqo)
