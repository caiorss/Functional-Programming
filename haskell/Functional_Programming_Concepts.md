<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Functional Programming Concepts](#functional-programming-concepts)
  - [Overview](#overview)
  - [Functional Programming Languages](#functional-programming-languages)
  - [Pure Functions](#pure-functions)
  - [Currying and Partial Application](#currying-and-partial-application)
  - [Lazy Evaluation](#lazy-evaluation)
  - [Function Composition](#function-composition)
    - [Function Composition In Haskell](#function-composition-in-haskell)
    - [Function Composition in Python](#function-composition-in-python)
    - [Selected Wikipedia Articles](#selected-wikipedia-articles)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Functional Programming Concepts

<!--
@TODO: Add concepts: Function composition, Pure language, Referential Transparency.
-->

### Overview

**Functional Programming**

Functional Programming is all about programming with functions.

**Functional Programming Features**

* Pure Functions / Referential Transparency / No side effect
* Function Composition
* Lambda Functions/ Anonymous Functions
* High Order Functions
* Currying/ Partial Function Application
* Closure - Returning functions from functions
* Data Immutability
* Pattern Matching
* Lists are the fundamental data Structure

Non Essential Features:

* Static Typing
* Type Inferencing
* Algebraic Data Types

**Functional Programming Design Patterns**

* Curry/ Partial function application  - Creating new functions by holding a parameter constant
* Closure - Return functions from functions
* Pure Functions: separate pure code from impure code.
* Function composition
* Composable functions
* High Order Functions
* MapReduce Algorithms - Split computation in multiple computers cores.
* Lazy Evaluation ( aka Delayed evaluation)
* Pattern Matching
* Monads

### Functional Programming Languages


Some Functional programing languages:

|  Language     | Purity  |Evaluation | Typing   | Type Discipline | Type Inference | OO  | AGDT | Platform | Family |  Curry |  Feature                |
|---------------|---------|-----------|----------|-----------------|----------------|-----|-----|----------|--------|------|---------------------------|  
| Haskell       | Pure    | Lazy    |   Static   |  Strong         | Yes            | No  | Yes | NAT      | ML/SML |  Yes |  Concurrency/Parallelism  |
| Ocaml         | Impure  | Strict  |   Static   |  Strong         | Yes            | Yes | Yes | NAT/BC   | ML/SML |  Yes |                            |
| F# (F sharp)  | Impure  | Strict  |   Static   |  Strong         | Yes            | Yes | Yes | .NET     | ML/SML |  Yes |  .NET Platform Integration |
| Scheme        | Impure  | Strict  |   Dynamic  |  Strong         | No             | No  | No  | -        | Lisp   |  No  |   Minimalistic Educational |
| Clojure       | Impure  | Strict  |   Dynamic  |  Strong         | No             | No  | No  | JVM      | Lisp   |  No  |   Java integration    | 
| Scala         | Impure  | Strict  |   Static   |  Strong         | Yes            | Yes | Yes | JVM      |        |      |   Java integration    |
| Erlang        | Impure  | Strict  |   Dynamic  |  Strong         | ?              | ?   |  ?  |          |  ?     |  ?   |   Telecommunications/ Servers |
| R             | Impure  | Strict  |   Dynamic  |  Strong         | No             | Yes |  -  | BC       | No     |  No  |   DSL - Statics  |
| Mathematica   | Impure  | Strict  |   Dynamic  |  ??             | Yes            | ?   |  ?  | ?        | No     |  No  |   DSL - Computer Algebraic System |


```
Notes:
* AGDT   - Algebraic Data Types
* JVM    - Java Virtual Machine / Java Platform
* .NET   - Dot Net Platform
* NAT    - Native Code
* BC     - Bytecode compilation
* OO     - Object Orientated
* Curry  - Curried functions like in Haskell
* DSL    - Domain Specific Language
```

More Information: [Comparison of Functional Programming Languages](http://en.wikipedia.org/wiki/Comparison_of_functional_programming_languages)

See also: [ML Dialects and Haskell: SML, OCaml, F#, Haskell](http://hyperpolyglot.org/ml)

### Pure Functions

Pure functions:

* Are functions without side effects, like mathematical functions. 
* For the same input the functions always returns the same output.
* The result of any function call is fully determined by its arguments. 
* Pure functions don't rely on global variable and don't have internal states.
* They don't do IO, i.e .:. don't print, don't write a file ...
* Pure functions are stateless
* Pure functions are deterministic

Why Pure Functions:

* Composability, one function can be connected to another.
* Can run in parallel, multi threading, multi core, GPU and distributed systems.
* Better debugging and testing.
* Predictability

**Example of pure functions**

```python
def min(x, y):
    if x < y:
        return x
    else:
        return y
```


**Example of impure function**

* Impure functions doesn't have always the same output for the same
* Impure functions does IO or has Hidden State or Global Variables

```python
exponent = 2

def powers(L):
    for i in range(len(L)):
        L[i] = L[i]**exponent
    return L
```
The function min is pure. It always produces the same result given 
the same inputs and it does not affect any external variable.

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
>>> powers([1, 2, 3])  # (It is impure since it doesn't give the same result )
[1, 16, 81]
>>> 
```

Another example, purifying an impure Language:

```python

>>> lst = [1, 2, 3, 4]  # An pure function doesn't modify its arguments.
>>>                     # therefore lst reverse is impure
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


### Currying and Partial Application


Currying is the decomposition of a function of multiples arguments in a chained sequence of functions of a single argument. The name currying comes from the mathematician [Haskell Curry](https://en.wikipedia.org/wiki/Haskell_Curry) who developed the concept of curried functions.

In Haskell, Standard ML, OCaml and F# all functions are curryfied by default:

```
    f (x, y) = 10*x - 3*y   
    
    f (4, 3)  = 10* 4 - 3*3 = 40 - 9 = 31
    f (4, 3)  = 31
    
In the curried form becomes:

     g(x) = (x -> y -> 10 * x - 3*y)
     
To evaluate f(4, 3): 

    h(y)  = (x -> y -> 10 * x - 3*y) 4 
          = ( y -> 10 * 4 -  3*y )
          =  y -> 40 - 3*y
          
    h(3)  = (y -> 40 - 3*y) 3
          = 40 - 3*3
          = 31
          
Or:
    (x -> y -> 10 * x - 3*y) 4 3 
      = (x -> (y -> 10 * x - 3*y)) 4 3 
      = ((x -> (y -> 10 * x - 3*y)) 4) 3 
      = (y -> 10 * 4 - 3 * y) 3
      = 10 * 4 - 3 * 3 
      = 31
```
          
The same function h(y) can be reused: applied to another arguments, used in mapping, filtering and another higher order functions.

```
Ex1
    h(y) = (y -> 40 - 3*y)
    
    h(10) = 40 - 3*10 = 40 - 30 = 10

Ex2    
    map(h, [2, 3, 4])
      = [h 2, h 3, h 4] 
      = [(y -> 40 - 3*y) 2, (y -> 40 - 3*y) 3, (y -> 40 - 3*y) 4]
      = [34, 31, 28]
```

**Example in Haskell GHCI**

```haskell
> let f x y = 10 * x - 3 * y
> :t f
f :: Num a => a -> a -> a
> 
> f 4 3 
31
> let h_y = f 4
> :t h_y
h_y :: Integer -> Integer
> 
> h_y 3
31
> map h_y [2, 3, 4]
[34,31,28]
> 

> -- It is evaluated as:

> ((f 4) 3)
31
> 

{-
   The function f can be also seen in this way
-}   

> let f' = \x -> \y -> 10 * x - 3 * y 
> 

> :t f'
f' :: Integer -> Integer -> Integer
> 

> f' 4 3
31
> 

> (f' 4 ) 3
31
> 

> let h__x_is_4_of_y = f' 4

> h__x_is_4_of_y 3
31
> 
{-
    (\x -> \y -> 10 * x - 3 * y) 4 3
    =  (\x -> (\y -> 10 * x - 3 * y) 4) 3
    =  (\y -> 10 * 4 - 3 * y) 3
    =  (10 * 4 - 3 * 3)
    =  40 - 9 
    =  31    
-}
> (\x -> \y -> 10 * x - 3 * y) 4 3
31
> 

> ((\x -> (\y -> 10 * x - 3 * y)) 4) 3
31
> 


{-
Curried functions are suitable for composition, pipelining 
(F#, OCaml with the |> operator),  mapping/ filtering operations,
and to create new function from previous defined increasing code reuse.

-}

> map (f 4) [2, 3, 4]
[34,31,28]
> 

> map ((\x -> \y -> 10 * x - 3 * y) 4) [2, 3, 4]
[34,31,28]
> 


> -- ----------------- 

> let f_of_x_y_z x y z = 10 * x + 3 * y + 4 * z
> 

> :t f_of_x_y_z 
f_of_x_y_z :: Num a => a -> a -> a -> a

> f_of_x_y_z 2 3 5
49
> 

> let g_of_y_z = f_of_x_y_z 2

> :t g_of_y_z 
g_of_y_z :: Integer -> Integer -> Integer
> 

> g_of_y_z 3 5
49
> 

> let h_of_z = g_of_y_z 3
> :t h_of_z 
h_of_z :: Integer -> Integer
> 

> h_of_z 5
49
> 

> -- So it is evaluated as 
> (((f_of_x_y_z 2) 3) 5)
49
> 
```

**Example in Python 3**

```python

 # In Python, the functions are not curried by default as in Haskell, 
 # Standard ML, OCaml and F#
 #
>>> def f(x, y): return 10 * x - 3*y

>>> f(4, 3)
    31

 # However the user can create the curried form of the function f:

>>> curried_f = lambda x: lambda y: 10*x - 3*y

>>> curried_f(4)
    <function __main__.<lambda>.<locals>.<lambda>>

>>> curried_f(4)(3)
    31

>>> h_y = curried_f(4) # x = 4 constant

>>> h_y(3)
    31

>>> h_y(5)
    25

>>> mapl = lambda f_x, xs: list(map(f_x, xs))

>>> mapl(h_y, [2, 3, 4])
    [34, 31, 28]

 # Or 

>>> mapl(curried_f(4), [2, 3, 4])
    [34, 31, 28]

 # Without currying the mapping would be:

>>> mapl(lambda y: f(4, y), [2, 3, 4])
    [34, 31, 28]

   ########################################

>> f_of_x_y_z = lambda x, y, z: 10 * x + 3 * y + 4 * z

 ## Curried form:
 
>>> curried_f_of_x_y_z = lambda x: lambda y: lambda z: 10 * x + 3 * y + 4 * z

>>> f_of_x_y_z (2, 3, 5)
    49

>>> curried_f_of_x_y_z (2)(3)(5)
    49

>>> g_of_y_z = curried_f_of_x_y_z(2)

>>> g_of_y_z
    <function __main__.<lambda>.<locals>.<lambda>>

>>> g_of_y_z (3)(5)
    49


>>> h_of_z = g_of_y_z(3)

>>> h_of_z
    <function __main__.<lambda>.<locals>.<lambda>.<locals>.<lambda>>

>>> h_of_z(5)
    49


```

**Example in Ocaml and F#**

```ocaml

    # let f x y = 10 * x - 3 * y ;;
    val f : int -> int -> int = <fun>

    # f 4 3 ;;
    - : int = 31

    # f 4 ;;
    - : int -> int = <fun>

    # (f 4) 3 ;;
    - : int = 31
    # 

    # let h_y = f 4 ;;
    val h_y : int -> int = <fun>

    # h_y 3 ;;
    - : int = 31
    # 

    # List.map h_y [2; 3; 4] ;;
    - : int list = [34; 31; 28]
    # 

    # List.map (f 4) [2; 3; 4] ;;
    - : int list = [34; 31; 28]

    # let f' = fun x -> fun y -> 10 * x - 3 * y ;;
    val f' : int -> int -> int = <fun>

    # (f' 4) 3 ;;
    - : int = 31

    # (fun x -> fun y -> 10 * x - 3 * y) 4 3 ;;
    - : int = 31
    # 

    # List.map ((fun x -> fun y -> 10 * x - 3 * y) 4) [2; 3; 4] ;;
    - : int list = [34; 31; 28]

```


<!--
Vocabulary:

* Arity: Number of arguments of a function
    *  Unary function: Takes only one argument .
    *  Binary function: Takes two arguments.
    *  Ternary function: Takes three arguments.
    *  Polyadic function: Takes more than one argument.
    *  Variadic function: Takes a variable number of arguments.
-->

### Lazy Evaluation

"Lazy evaluation" means that data structures are computed incrementally, as they are needed (so the trees never exist in memory all at once) parts that are never needed are never computed. Haskell uses lazy evaluation by default.

Example in Haskell: 

```haskell
> let lazylist = [2..1000000000]
> 
> let f x = x^6 
> 
> take 5 lazylist 
[2,3,4,5,6]
>
>
> {- Only the terms needed are computed. -}
> take 5 ( map f lazylist )
[64,729,4096,15625,46656]
> 
```

Example in Python:

* Python uses eager evaluation by default. In order to get lazy evaluation in python the programmer must use iterators or generators. The example below uses generator.

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


### Function Composition 

Function composition promotes shorter code, code reuse and higher modularity by creating new functions from previous defined ones. They also allow optimization of functional code when there is many maps. Only pure functions can be composed, function composition works like math functions, the output of one function is the input of another function.  Haskell, ML, Ocaml and F# has features that makes easier to use function composition, like a lightweight syntax, currying, partially applied functions, static typing and composition operators that are built in to the language.  In Haskell the operator (.) dot is used for composing functions. 

See also: [Function composition (computer science)](http://en.wikipedia.org/wiki/Function_composition_%28computer_science%29)


#### Function Composition In Haskell

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

#### Function Composition in Python

```python

def compose(funclist):   
    
    def _(x):
        y = x 
        
        for f in reversed(funclist):
            y = f(y)
        return y
    
    return _

>>> add10 = lambda x: x + 10

>>> mul3 = lambda x: x * 3

>>> x = 3
>>> a = add10(x)
>>> a
    13
>>> b = mul3(a)
>>> b
    39


>>> def f_without_composition (x):
 ...    a = add10(x)
 ...    b = mul3(a)
 ...    return b
 ...

>>> f_without_composition(3)
    39

>>> f_without_composition(4)
    42

 # It will create the function f = (mul3 ° add10)(x)
 # The flow is from right to left
 #
 #                   
 #     (mul3 . add10) 3 
 #   =  mul3 (add10 3) 
 #   =  mul3 13 
 #   =  39 
 #
>>> f = compose ([mul3, add10])  

>>> f(3)
    39

>>> f(4)
    42

>>> f
    <function __main__.compose.<locals>._>

>>> compose ([add10, mul3])(3)
    39

>>> compose ([add10, mul3])(4)
    42

 #
 # Composition is more intuitive when the flow is from
 # left to right, the functions in the left side are
 # executed first. 
 #
 #

 # Compose Forward
def composef (funclist):   
    
    def _(x):
        y = x         
        for f in funclist:
            y = f(y)
        return y
    
    return _

 #
 #   The symbol (>>) from F# will be used to mean forward composition
 #   here
 #
 #      (add10 >> mul3) 3 
 #    = mul3 (add10 3) 
 #    = mul3 13 
 #    = 39
 #                          add10 >> mul3
 #    Input  .................................................  Output
 #           .    |----------|           |---------|         .   39
 #   3  ---> .--> |  add10   | --------> |   mul3  | ------->.  ------->  
 #           .  3 |----------| 13 =(10+3)|---------|  39     .
 #           .                                39 = 3 * 13    .
 #           .................................................        
 #       
 #  The execution flow is from left to right, in the same order as the
 #  functions are written in the code.
 #
 
>>> g = composef ([add10, mul3])

>>> g(3)
    39

>>> g(4)
    42


>>> ### A more useful example: parse the following table:

text = """
 12.23,30.23,892.2323
 23.23,90.23,1000.23
 3563.23,100.23,45.23

"""



 # Unfortunately Python, don't have a favorable syntax to function 
 # composition like: composition operator, lightweight lambda and function
 # application without parenthesis.
 #

>>> mapl = lambda f: lambda xs: list(map(f, xs))
>>> filterl = lambda f: lambda xs: list(filter(f, xs))


>>> splitlines = lambda s: s.splitlines()
>>> reject_empty = lambda xs: list(filter(lambda x: x, xs))
>>> strip = lambda s: s.strip()
>>> split = lambda sep: lambda s: s.split(sep)


>>> composef([splitlines])(text)
    ['',
 ' 12.23,30.23,892.2323',
 ' 23.23,90.23,1000.23',
 ' 3563.23,100.23,45.23',
 '']
 
 
>>> composef([splitlines, reject_empty])(text)
    [' 12.23,30.23,892.2323', 
    ' 23.23,90.23,1000.23', 
    ' 3563.23,100.23,45.23']

    
>>> composef([splitlines, reject_empty, mapl(strip)])(text)
    ['12.23,30.23,892.2323', '23.23,90.23,1000.23', 
    '3563.23,100.23,45.23']


>>> composef([splitlines, reject_empty, mapl(strip), mapl(split(","))])(text)
    [['12.23', '30.23', '892.2323'],
 ['23.23', '90.23', '1000.23'],
 ['3563.23', '100.23', '45.23']]

>>> composef([splitlines, reject_empty, mapl(strip), mapl(split(",")), mapl(mapl(float))])(text)
    [[12.23000, 30.23000, 892.23230],
 [23.23000, 90.23000, 1000.23000],
 [3563.23000, 100.23000, 45.23000]]

parse_csvtable =  composef(
    [splitlines, 
    reject_empty, 
    mapl(strip), 
    mapl(split(",")), 
    mapl(mapl(float))]
    )


>>> parse_csvtable(text)
    [[12.23000, 30.23000, 892.23230],
 [23.23000, 90.23000, 1000.23000],
 [3563.23000, 100.23000, 45.23000]]

    #  Notice there is three maps together, so that it can be optimized 
    #  each map is like a for loop, by composing the functions in map1,  
    #  map2 and map3 the code can be more faster.
    #
    # parse_csvtable =  composef(
    # [splitlines, 
    # reject_empty, 
    # mapl(strip),          ---> map1
    # mapl(split(",")),     ---> map2
    # mapl(mapl(float))]    ---> map3
    # )


parse_csvtable_optmized =  composef(
    [splitlines, 
    reject_empty, 
    mapl(composef([strip, split(","), mapl(float)]))
    ])
    
>>> parse_csvtable_optmized(text)
    [[12.23000, 30.23000, 892.23230],
 [23.23000, 90.23000, 1000.23000],
 [3563.23000, 100.23000, 45.23000]]

    
```



#### Selected Wikipedia Articles

Selected Wikipedia Pages:

* [List of functional programming topics](http://en.wikipedia.org/wiki/List_of_functional_programming_topics)
* [Comparison of Functional Programming Languages](http://en.wikipedia.org/wiki/Comparison_of_functional_programming_languages)
* [Functional programming](http://en.wikipedia.org/wiki/Functional_programming)


* [Declarative programming](http://en.wikipedia.org/wiki/Declarative_programming)
* [Aspect-oriented programming](http://en.wikipedia.org/wiki/Aspect-oriented_programming)

**Pure Functions/ Lambda Calculus/ Closure/ Currying**

* [Lambda calculus](http://en.wikipedia.org/wiki/Lambda_calculus)
* [Higher-order function](http://en.wikipedia.org/wiki/Higher-order_function)
* [Referential transparency (computer science)](http://en.wikipedia.org/wiki/Referential_transparency_(computer_science))
* [Closure (computer programming)](http://en.wikipedia.org/wiki/Closure_(computer_programming))
* [Callback (computer programming)](http://en.wikipedia.org/wiki/Callback_(computer_programming))
* [Coroutine](http://en.wikipedia.org/wiki/Coroutine)

**Evaluation**

* [Eager Evaluation](http://en.wikipedia.org/wiki/Eager_evaluation)
* [Lazy Evaluation](http://en.wikipedia.org/wiki/Lazy_evaluation)
* [Short-circuit evaluation](http://en.wikipedia.org/wiki/Short-circuit_evaluation)

**Monads**

* [Monads Functional Programming](http://en.wikipedia.org/wiki/Monad_(functional_programming))
* [Haskell/Understanding monads](http://en.wikibooks.org/wiki/Haskell/Understanding_monads)
* [Monad transformer](http://en.wikipedia.org/wiki/Monad_transformer)

**Continuations**

* [Continuation](http://en.wikipedia.org/wiki/Continuation)
* [Continuation-passing style](http://en.wikipedia.org/wiki/Continuation-passing_style)

<!--
-->
