<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Functional Programming Concepts](#functional-programming-concepts)
  - [Overview](#overview)
    - [Functional Programming Languages](#functional-programming-languages)
    - [Notable People](#notable-people)
  - [Concepts](#concepts)
    - [First-Class Function](#first-class-function)
    - [Closure](#closure)
    - [Pure Functions](#pure-functions)
    - [Currying and Partial Application](#currying-and-partial-application)
      - [Currying](#currying)
      - [Partial Application](#partial-application)
    - [Lazy Evaluation](#lazy-evaluation)
    - [Fundamental Higher Order Functions](#fundamental-higher-order-functions)
      - [Map](#map)
      - [Filter](#filter)
      - [Reduce (Fold)](#reduce-fold)
    - [Function Composition](#function-composition)
      - [Function Composition In Haskell](#function-composition-in-haskell)
      - [Function Composition in Python](#function-composition-in-python)
  - [Miscellaneous](#miscellaneous)
      - [Selected Wikipedia Articles](#selected-wikipedia-articles)
      - [Selected Rosettacode Pages](#selected-rosettacode-pages)
    - [Concepts Examples](#concepts-examples)
    - [Languages](#languages)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Functional Programming Concepts

<!--
@TODO: Add concepts: Function composition, Pure language, Referential Transparency.
-->

## Overview

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


### Notable People

A selection of people who influenced functional programming:

* [Alonzo Church](https://en.wikipedia.org/wiki/Alonzo_Church), Mathematician -> Lambda Calculus

* [Haskell Curry](https://en.wikipedia.org/wiki/Haskell_Curry), Mathematician -> Concept of currying

* [Robin Milner](https://en.wikipedia.org/wiki/Robin_Milner), Computer Scientist -> Type inference, [Hindley–Milner type system](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system), [ML language](https://en.wikipedia.org/wiki/ML_(programming_language))

* [John McCarthy](https://en.wikipedia.org/wiki/John_McCarthy_(computer_scientist)),  Computer Scientist -> Creator of [Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)), Artificial intelligence

    * [Guy Steele Interviews John McCarthy, Father of Lisp](http://www.infoq.com/interviews/Steele-Interviews-John-McCarthy)

* [John Backus](https://en.wikipedia.org/wiki/John_Backus), Computer Scientist ->  Backus-Naur form (BNF), Fortran Language, 
    * [Can Programming Be Liberated from the von Neumann Style? A Functional Style and Its Algebra of Programs](https://web.stanford.edu/class/cs242/readings/backus.pdf)

* [Philip Wadler](https://en.wikipedia.org/wiki/Philip_Wadler),  Theory behind functional programming and the use of monads in functional programming, the design of the purely functional language Haskell.

    * [The essence of functional programing](http://www.eliza.ch/doc/wadler92essence_of_FP.pdf)
    * [Philip Wadler on Functional Programming - Interview](http://www.infoq.com/interviews/wadler-functional-programming)

* [Eugenio Moggi](https://en.wikipedia.org/wiki/Eugenio_Moggi), Professor of computer science at the University of Genoa, Italy. - He first described the general use of monads to structure programs.

    * [Notions of computation and monads - Eugenio Moggi](http://www.disi.unige.it/person/MoggiE/ftp/ic91.pdf)

* [Simon Peyton Jones](https://en.wikipedia.org/wiki/Simon_Peyton_Jones), Computer Scientist -> Major contributor to the design of the Haskell programming language.

* [John Hughes](https://en.wikipedia.org/wiki/John_Hughes_(computer_scientist)), Computer Scientist -> One of the most influentials papers in FP field: Why functional programing matters.


* [Gerald Jay Sussman](https://en.wikipedia.org/wiki/Gerald_Jay_Sussman), Mathematician and Computer Scientist
   * [Scheme Lisp](https://en.wikipedia.org/wiki/Scheme_(programming_language)) Language
   * Book: [Structure and Interpretation of Computer Programs](https://en.wikipedia.org/wiki/Structure_and_Interpretation_of_Computer_Programs)
   * Book: [Structure and Interpretation of Classical Mechanics](https://en.wikipedia.org/wiki/Structure_and_Interpretation_of_Classical_Mechanics)

   * [Lambda Papers](https://en.wikipedia.org/wiki/History_of_the_Scheme_programming_language#The_Lambda_Papers): A series of MIT AI Memos published between 1975 and 1980, developing the Scheme programming language and a number of influential concepts in programming language design and implementation.


## Concepts

### First-Class Function 

Functions can be passed as arguments to another functions, returned from functions, stored in variables and data structures and built at run time. The majority of languages supports firs-class functions like Scheme, Javascript, Python, Haskell, ML, OCaml and many others some exceptions are C, Java, Matlab and Forth.

Examples:

* Python:

The function f is passed is argument to the derivate function that returns a new function named _, that computes the derivate of f at x.

```python
def derivate (f, dx=1e-5):
    def _(x):
        return (f(x+dx) - f(x))/dx
    return _
    
 #  Algebraic derivate:
 #
 #  df(x) = 2*x - 3
 #    
>>> def f(x): return x**2 - 3*x + 4
... 

 # Numerical derivate of f
>>> df = derivate(f)
>>> 

  # Algebraic Derivate of f
>>> def dfa (x): return 2*x - 3
... 
>>> 

 ;; Functions can be stored in variables
>>> func = f
>>> func(5)
14
>>> 

>>> df = derivate(f)
>>> df(3)
3.000009999887254
>>> df(4)
5.000009999633903
>>> 

>>> dfa(3)
3
>>> dfa(4)
5
>>> 


>>> f(3)
4
>>> f(10)
74
>>> 
```

See also: 

Many examples of first class functions in several languages. 
* [First-class functions - Rosetta Code](http://rosettacode.org/wiki/First-class_functions#C)

* [First-class Functions in Scientific Programming](http://slidegur.com/doc/1814324/first-class-functions-in-scientific-programming)

* [Functional programming in R](http://adv-r.had.co.nz/Functional-programming.html)

### Closure

Closure is a function that remembers the environment at which it was created.

```python

>>> x = 10

 # The function adder remembers the environment at which it was created
 # it remembers the value of x
 #
def make_adder(x):
    def adder(y):
        return x + y
    return adder

>>> add5 = make_adder(5)
>>> add10 = make_adder(10)
>>> 
>>> add5(4)
9
>>> list(map(add5, [1, 2, 3, 4, 5]))
[6, 7, 8, 9, 10]

>>> x
10
>>> 

>>> list(map(add10, [1, 2, 3, 4, 5]))
[11, 12, 13, 14, 15]

 #
 
def make_printer(msg):
    def printer():
        print(msg)
    return printer

>>> p1 = make_printer ("Hello world")
>>> p2 = make_printer ("FP programming Rocks!!")
>>> 
>>> p1()
Hello world
>>> p2()
FP p

 # Mutable state with closure
 
idx = 100 
 
def make_counter():
    idx = -1    
    def _():
        nonlocal idx
        idx = idx + 1
        return idx    
    return _

>>> idx = 100
>>> counter1 = make_counter()
>>> counter1()
0
>>> counter1()
1
>>> counter1()
2
>>> counter1()
3

>>> idx
100
>>> counter2 = make_counter ()
>>> counter2()
0
>>> counter2()
1
>>> counter2()
2

>>> counter1()
5
>>> 

>>> del make_counter
>>> make_counter
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
NameError: name 'make_counter' is not defined
>>> 
>>> counter1()
6
>>> counter1()
7

```


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

#### Currying

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


#### Partial Application

A function of multiple arguments is converted into a new function that takes fewer arguments, some arguments are supplied and returns function with signature consisting of remaining arguments. **Partially applied** functions must not be confused with **currying**.

Example in Python:

```python
>>> from functools import partial

>>> def f(x, y, z): return 10 * x + 3 * y + 4 * z

>>> f(2, 3, 5)
    49

>>> f_yz = partial(f, 2) # x = 2
>>> f_yz(3, 5)
    49

>>> f_z = partial(f_yz, 3)

>>> f_z(5)
    49
    
>>> partial(f, 2, 3)(5)
    49
  
>>> list(map(partial(f, 2, 3), [2, 3, 5]))
    [37, 41, 49]
```

In languages like Haskell, Standard ML, OCaml and F# currying is similar to partial application.

Example in OCaml:

```ocaml

    # let f x y z = 10 * x + 3 *y + 4 * z ;;
    val f : int -> int -> int -> int = <fun>
    # 

    # (f 2 3) ;;
    - : int -> int = <fun>
    
    # let f_z = f 2 3 ;;
    val f_z : int -> int = <fun>

    # f_z 5 ;;
    - : int = 49
    #    
    
    (** Write (f 2 3) is the same as write (f 2)(3)  *)
    # List.map (f 2 3) [2; 3; 5] ;;
    - : int list = [37; 41; 49]
    # 
    
```


See also:

* [Java.next: Currying and partial application](http://www.ibm.com/developerworks/library/j-jn9/)
* [Partial application - Wikipedia](https://en.wikipedia.org/wiki/Partial_application)
* [What's Wrong with Java 8: Currying vs Closures](https://dzone.com/articles/whats-wrong-java-8-currying-vs)

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



### Fundamental Higher Order Functions 

The functions map, filter and reduce (fold left) are ubiquitous in many programing languages and also the most used higher order functions. 

They can be stricted evaluated like in Scheme and Javascript or lazy evaluated like in Python and Haskell. 

#### Map

```
    map :: ( a -> b) -> [a] -> [b]                
                |
                |
                |----> f :: a -> b
                
    
    
             f :: a -> b
     a   ------------------------>>>  b
    
    
           map f :: [a] -> [b]                    
    [a] ------------------------->>> [b]
    
    
``` 


*Haskell*

The function map is lazy evaluated.

```haskell
> let fun1 x = 3 * x + 1
> fun1 2
7
> map fun1 [1, 2, 3]
[4,7,10]
> 

  -- The sequence 1 to 1000000 is not evaluated at all, 
  --
> take 10 (map fun1 [1..1000000])
[4,7,10,13,16,19,22,25,28,31]

> take 10 (map fun1 [1..10000000000])
[4,7,10,13,16,19,22,25,28,31]
> 
> 



 -- 
 -- When applied to a function without a list, it creates 
 -- another function that operates over lists because all
 -- Haskell functions are curried by default.
 --
 --         f :: (a -> b)
 --  map    :: (a -> b) -> [a] -> [b]
 --
 -- It can be seen as:
 --
 --  When map is applied to f, it will create the function fs
 --  that take list of type a and returns list of type b.
 --
 --  map    :: (a -> b) -> ([a] -> [b])
 --                |            |
 --                |            |------ fs :: [a] -> [b] 
 --                |    
 --                -------------------- f  :: a -> b 
 --
> :t map
map :: (a -> b) -> [a] -> [b]
  
> let f x = 3 * x + 6
> :t f
f :: Num a => a -> a
> 


> map f [1, 2, 3]
[9,12,15]
> 

 -- Note: let is only needed in the REPL
 --
> let fs = map f

> :t fs
fs :: [Integer] -> [Integer]

> fs [1, 2, 3]
[9,12,15]
> 
```


*Python*

In Python 3 map and filter are lazy evaluated, they return a generator.

```python
>>> def fun1 (x):
    return 3*x + 6
... 
>>> g = map(fun1, [1, 2, 3])
>>> g
<map object at 0xb6b4a76c>
>>> next (g)
9
>>> next (g)
12
>>> next (g)
15
>>> next (g)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
StopIteration
>>> g
<map object at 0xb6b4a76c>
>>> 

 # Force the evaluation: 
 #
 >>> list(map(fun1, [1, 2, 3]))
 [9, 12, 15]
 

 # Strict Version of map
 # 
 # s_ stands for strict map.

def s_map (f, xs):
    return list(map(f, xs))
 
>>> s_map (fun1, [1, 2, 3])
[9, 12, 15]
>>> 

 # Due to python doesn't have tail call optimization
 # recusion must be avoided, a higher number of iterations
 # can lead to a stack overflow.
 
def strict_map (f, xs):
    return [f (x) for x in xs]
    
>>> strict_map (fun1, [1, 2, 3])
[9, 12, 15]
>>> strict_map (fun1, range(5))
[6, 9, 12, 15, 18]
>>> 

  # Lazy map implementation:
  # Note: the python native map is implemented in C, so
  # it is faster.
  #
  
def lazy_map (f, xs):
    for x in xs:
        yield x
        
>>> g = lazy_map (fun1, [1, 2, 3])
>>> next(g)
1
>>> next(g)
2
>>> next(g)
3
>>> next(g)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
StopIteration
>>> list(lazy_map (fun1, [1, 2, 3]))
[1, 2, 3]
>>>           

 #
 # To the map function work like in Haskell and ML 
 # it is need to be curried.   
 #

curry2 = lambda f: lambda x: lambda y: f(x, y)

 # The function curry2 currify a function of two arguments
 #
>>> strict_map_c = curry2(strict_map) 

>>> strict_map_c(fun1)
<function <lambda>.<locals>.<lambda>.<locals>.<lambda> at 0xb6afc0bc>

>>> strict_map_c(fun1)([1, 2, 3, 4])
[9, 12, 15, 18]
>>> 

>>> fun1_xs = strict_map_c(fun1)
>>> fun1_xs ([1, 2, 3, 4])
[9, 12, 15, 18]
>>>  
```

#### Filter

*Python*

```python

 ;;; Filter returns by default a 
>>> g = filter (lambda x: x > 10, [1, 20, 3, 40, 4, 14, 8])
>>> g
<filter object at 0xb6b4a58c>
>>> [x for x in g]
[20, 40, 14]
>>> [x for x in g]
[]
>>> list(filter (lambda x: x > 10, [1, 20, 3, 40, 4, 14, 8]))
[20, 40, 14]
>>> 

  # Stritct Version of filter function
  #
>>> _filter = lambda f, xs: list(filter(f, xs))
>>> 
>>> _filter (lambda x: x > 10,  [1, 20, 3, 40, 4, 14, 8])
[20, 40, 14]
>>> 

  # Filter implementation without recursion:
  #

def strict_filter (f, xs):
    result = []
    for x in xs:
        if f(x):
            result.append(x)
    return result

def lazy_filter (f, xs):
    for x in xs:
        if f(x):
            yield x

>>> strict_filter (lambda x: x > 10, [1, 20, 3, 40, 4, 14, 8])
[20, 40, 14]

>>> lazy_filter (lambda x: x > 10, [1, 20, 3, 40, 4, 14, 8])
<generator object lazy_filter at 0xb6b0f1bc>

>>> g = lazy_filter (lambda x: x > 10, [1, 20, 3, 40, 4, 14, 8])
>>> g
<generator object lazy_filter at 0xb6b0f194>
>>> next(g)
20
>>> next(g)
40
>>> next(g)
14
>>> next(g)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
StopIteration
>>> 

>>> list(lazy_filter (lambda x: x > 10, [1, 20, 3, 40, 4, 14, 8]))
[20, 40, 14]
>>> 
  
```

#### Reduce (Fold)

See also: 
  * [Fold (higher-order function) - Wikipedia, the free encyclopedia](https://en.wikipedia.org/wiki/Fold_(higher-order_function))
  * [A tutorial on the universality and expressiveness of fold. GRAHAM HUTTON](http://www.cs.nott.ac.uk/~pszgmh/fold.pdf)
  * [Haskell unit 6: The higher-order fold functions | Antoni Diller](http://www.cantab.net/users/antoni.diller/haskell/units/unit06.html)

*Haskell*

Fold Left:

```
 foldl :: (acc -> x -> acc) -> acc -> [x] -> acc
 
                  |             |      |       | 
                  |             |      |       |---> Returns the accumulated 
                  |             |      |             value
                  |             |      |----- xs 
                  |             |                  
                  |             |     Inital Value of accumulator
                  |             |---  acc0
                  |
                  |-----------------  f :: acc -> x -> acc
                                                  |
                                                  |--- Element of list 

 foldl :: (b -> a -> b) -> b -> [a] -> b
 foldl f z []     = z
 foldl f z (x:xs) = foldl f (f z x) xs
```


```haskell

> :t foldl
foldl :: (a -> b -> a) -> a -> [b] -> a
> 
> foldl (\acc x -> 10 * acc + x) 0 [1, 2, 3, 4, 5] 
12345
> 

```

It is equivalent to:

```haskell
> let f acc x = 10 * acc + x
> 
> (f 0 1)
1
> (f (f 0 1) 2)
12
> (f (f (f 0 1) 2) 3)
123
> 
> (f (f (f (f 0 1) 2) 3) 4)
1234
> (f (f (f (f (f 0 1) 2) 3) 4) 5)
12345
> 
```

Fold right:

```
 foldr :: (x -> acc -> acc) -> acc -> [x] -> acc

 foldr :: (a -> b -> b) -> b -> [a] -> b
 foldr f z []     = z
 foldr f z (x:xs) = f x (foldr f z xs)
```

```haskell
> foldr (\x acc -> 10 * acc + x) 0 [1, 2, 3, 4, 5] 
54321

> (f 0 5)
5
> (f (f 0 5) 4)
54
> (f (f (f 0 5) 4) 3)
543
> (f (f (f (f 0 5) 4) 3) 2)
5432
> (f (f (f (f (f 0 5) 4) 3) 2) 1)
54321
> 

 --
 -- Derive fold_right from foldl (fold left)
 -- 

> let fold_right f acc xs = foldl (\x acc -> f acc x) acc (reverse xs)
> 
> :t fold_right
fold_right :: (b -> a -> a) -> a -> [b] -> a
> 
> 
> fold_right (\x acc -> 10 * acc + x) 0 [1, 2, 3, 4, 5]
54321
> 


```


*Python*

In Python 3 the function reduce is not default anymore, however it can be found in the native library functools, that has a lot of builtin functions for functional programing. The function reduce is equivalent to Haskell function foldl (fold left) which is tail recursive. 

```
reduce(function, sequence[, initial]) -> value

reduce :: (acc -> x -> acc) -> [x] ?acc0  -> acc
```

```python
>>> from functools import reduce
>>> 

>>> reduce (lambda acc, x: 10 *  acc + x , [1, 2, 3, 4, 5], 0)
12345
>>> 

>>> f = lambda acc, x: 10 *  acc + x
>>> 
>>> f(0, 1)
1
>>> f( f(0, 1), 2)
12
>>> f( f( f(0, 1), 2), 3)
123
>>> f( f( f( f(0, 1), 2), 3), 4)
1234
>>> f( f( f( f( f(0, 1), 2), 3), 4), 5)
12345
>>> 

def my_reduce (f, xs, acc0=None):
    "Non recursive implementation of reduce (fold_left)
     with optional initial accumulator value.
    "

    if acc0 is None:
        acc = xs[0]   
        xss = xs[1:]
    else:
        acc = acc0
        xss = xs
        
    for x in xss:
        acc = f (acc, x)
        
    return acc


>>> 
>>> my_reduce(lambda acc, x: 10 * acc + x, [1, 2, 3, 4, 5], 0)
12345
>>> my_reduce(lambda acc, x: 10 * acc + x, [1, 2, 3, 4, 5])
12345
>>> my_reduce(lambda acc, x:  acc + x, [1, 2, 3, 4, 5], 0)
15
>>> my_reduce(lambda acc, x:  acc * x, [1, 2, 3, 4, 5], 1)
120
>>> 
 
 #
 # Implementation without recursion.
 #

def fold_left (f_acc_x_to_acc, acc0, xs):
    "Haskell-like fold left function
    
    fold_left :: (acc -> x -> acc) -> acc -> [x]
    "
    acc = acc0
    
    for x in xs:
        acc = f_acc_x_to_acc (acc, x)
        
    return acc
      
>>> fold_left (lambda acc, x: 10 * acc + x, 0, [1, 2, 3, 4, 5])
12345
>>>       


def fold_right (f, acc0, xs):
    return fold_left ((lambda acc, x: f(x, acc)), acc0, reversed(xs))

>>> fold_right (lambda x, acc: 10 * acc + x, 0, [1, 2, 3, 4, 5])
54321
>>>

def fold_right2 (f, acc0, xs):
    acc = acc0
    
    for x in reversed(xs):
        acc = f(x, acc)
        
    return acc

>>> fold_right2 (lambda x, acc: 10 * acc + x, 0, [1, 2, 3, 4, 5])
54321
>>>     

```

*Usefulness of Fold*

Many functions and recursive algorithms can be implemented using the fold function, including map, filter, sum, product and others.

It is based in the paper:  
   - [A tutorial on the universality and expressiveness of fold. GRAHAM HUTTON](http://www.cs.nott.ac.uk/~pszgmh/fold.pdf)

In the paper was usef fold right, here was used fold left. 

```python 

def fold_left (f_acc_x_to_acc, acc0, xs):
    "Haskell-like fold left function
    
    fold_left :: (acc -> x -> acc) -> acc -> [x]
    "
    acc = acc0
    
    for x in xs:
        acc = f_acc_x_to_acc (acc, x)
        
    return acc
    
    
    ;;; Function fold in curried form 
    
curry3 = lambda f: lambda x: lambda y: lambda z: f(x, y, z)

fold = curry3(fold_left)

>>> summation = fold(lambda acc, x: acc + x)(0)
>>> 
>>> summation([1, 2, 3, 4, 5, 6])
21
>>> 

>>> product = fold(lambda acc, x: acc * x)(1)
>>> product([1, 2, 3, 4, 5])
120
>>> 

>>> f_or = fold(lambda acc, x: acc or x)(False)
>>> f_or([False, False, False])
False
>>> 
>>> f_or([False, False, True])
True
>>> 

>>> f_and = fold(lambda acc, x: acc and x)(True)
>>> 
>>> f_and([False, True, True])
False
>>> f_and([True, True, True])
True
>>> 

>>> length = fold(lambda acc, x: acc + 1)(0)
>>> length ([1, 2, 3, 4, 5])
5

>>> _map = lambda f, xs: fold(lambda acc, x: acc + [f(x)] )([])(xs)
>>> _map (lambda x: x * 3, [1, 2, 3, 4])
[3, 6, 9, 12]
>>> 

>>> _filter = lambda p, xs: fold(lambda acc, x: (acc + [x]) if p(x) else  acc )([])(xs)
>>> 
>>> _filter(lambda x: x > 10, [10, 3, 8, 2, 20, 30])
[20, 30]
>>> 


 #
 # Function composition
 # 
 #  (f3 (f2 (f1 (f0 x))))
 #
 #  (f3 . f2 . f1 . f0) x
 #
 #  or using, forward composition:
 # 
 #  (f0 >> f2 >> f1 >> f0) x
 #
 
>>> f1 = lambda x: 3 * x
>>> f2 = lambda x: 5 + x
>>> f3 = lambda x: 2 ** x


>>> _fcomp = lambda functions: lambda x: fold(lambda acc, f: f(acc)) (x) (functions)

>>> _fcomp([f1, f2, f3])(3)
16384

>>> (f3 (f2 (f1 (3))))
16384
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



## Miscellaneous

#### Selected Wikipedia Articles

Selected Wikipedia Pages:

* [List of functional programming topics](http://en.wikipedia.org/wiki/List_of_functional_programming_topics)

* [Comparison of Functional Programming Languages](http://en.wikipedia.org/wiki/Comparison_of_functional_programming_languages)
* [Functional programming](http://en.wikipedia.org/wiki/Functional_programming)

* [Declarative programming](http://en.wikipedia.org/wiki/Declarative_programming)
* [Aspect-oriented programming](http://en.wikipedia.org/wiki/Aspect-oriented_programming)

**Functions**

First Class Functions

* [First-class function](https://en.wikipedia.org/wiki/First-class_function)
* [Pure function](https://en.wikipedia.org/wiki/Pure_function)
* [Side effect (computer science)](https://en.wikipedia.org/wiki/Side_effect_%28computer_science%29)
* [Purely functional](https://en.wikipedia.org/wiki/Purely_functional)

* [Referential transparency (computer science)](https://en.wikipedia.org/wiki/Referential_transparency_%28computer_science%29)
* [Function type](https://en.wikipedia.org/wiki/Function_type)

* [Arity](https://en.wikipedia.org/wiki/Arity)
* [Variadic function](https://en.wikipedia.org/wiki/Variadic_function)

Composition

* [Function composition (computer science)](https://en.wikipedia.org/wiki/Function_composition_%28computer_science%29)
* [Function composition - Mathematics](https://en.wikipedia.org/wiki/Function_composition)
* [Composability](https://en.wikipedia.org/wiki/Composability)

* [Functional decomposition](https://en.wikipedia.org/wiki/Functional_decomposition)

Scope

* [Scope (computer science)](https://en.wikipedia.org/wiki/Scope_%28computer_science%29)

Currying and Partial Evaluation

* [Currying](https://en.wikipedia.org/wiki/Currying)
* [Partial evaluation](https://en.wikipedia.org/wiki/Partial_evaluation)

Higher Order Functions, Closures, Anonymous Functions

* [Anonymous function](https://en.wikipedia.org/wiki/Anonymous_function)
* [Closure (computer programming)](https://en.wikipedia.org/wiki/Closure_%28computer_programming%29)
* [Higher-order function](https://en.wikipedia.org/wiki/Higher-order_function)
* [Fixed-point combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator)
* [Defunctionalization](https://en.wikipedia.org/wiki/Defunctionalization)

* [Closure (computer programming)](http://en.wikipedia.org/wiki/Closure_(computer_programming))
* [Callback (computer programming)](http://en.wikipedia.org/wiki/Callback_(computer_programming))
* [Coroutine](http://en.wikipedia.org/wiki/Coroutine)


Recursion

* [Recursion (computer science)](https://en.wikipedia.org/wiki/Recursion_%28computer_science%29)
* [Tail call](https://en.wikipedia.org/wiki/Tail_call)
* [Double recursion](https://en.wikipedia.org/wiki/Double_recursion)
* [Primitive recursive function](https://en.wikipedia.org/wiki/Primitive_recursive_function)


* [Ackermann function](https://en.wikipedia.org/wiki/Ackermann_function)
* [Tak (function)](https://en.wikipedia.org/wiki/Tak_%28function%29)


Lambda Calculus and Process Calculus

* [Typed lambda calculus](https://en.wikipedia.org/wiki/Typed_lambda_calculus)
* [Lambda calculus](http://en.wikipedia.org/wiki/Lambda_calculus)
* [Process calculus](https://en.wikipedia.org/wiki/Process_calculus)


* [Futures and promises](https://en.wikipedia.org/wiki/Futures_and_promises)
* [Combinatory logic](https://en.wikipedia.org/wiki/Combinatory_logic)


**Evaluation**

* [Evaluation strategy](https://en.wikipedia.org/wiki/Evaluation_strategy)

* [Eager Evaluation](http://en.wikipedia.org/wiki/Eager_evaluation)
* [Short-circuit evaluation](http://en.wikipedia.org/wiki/Short-circuit_evaluation)

Related to Lazy Evaluation

* [Lazy Evaluation](http://en.wikipedia.org/wiki/Lazy_evaluation)
* [Thunk](https://en.wikipedia.org/wiki/Thunk)

**Monads**

* [Monads Functional Programming](http://en.wikipedia.org/wiki/Monad_(functional_programming))
* [Haskell/Understanding monads](http://en.wikibooks.org/wiki/Haskell/Understanding_monads)
* [Monad transformer](http://en.wikipedia.org/wiki/Monad_transformer)

**Continuations**

* [Continuation](http://en.wikipedia.org/wiki/Continuation)
* [Continuation-passing style](http://en.wikipedia.org/wiki/Continuation-passing_style)

**Fundamental Data Structure**

* [List (abstract data type)](https://en.wikipedia.org/wiki/List_%28abstract_data_type%29)
* [Array data structure](https://en.wikipedia.org/wiki/Array_data_structure)
* [Array data type](https://en.wikipedia.org/wiki/Array_data_type)


**Types**

* [Category theory](https://en.wikipedia.org/wiki/Category_theory)
* [Type Theory](https://en.wikipedia.org/wiki/Type_theory)
* [Type System](https://en.wikipedia.org/wiki/Type_system)

* [Algebraic data type](https://en.wikipedia.org/wiki/Algebraic_data_type)

* [Type signature](https://en.wikipedia.org/wiki/Type_signature)
* [Enumerated type](https://en.wikipedia.org/wiki/Enumerated_type)
* [Product type](https://en.wikipedia.org/wiki/Product_type)
* [Tagged union](https://en.wikipedia.org/wiki/Tagged_union)
* [Dependent type](https://en.wikipedia.org/wiki/Dependent_type)


* [Recursive data type](https://en.wikipedia.org/wiki/Recursive_data_type)

* [Generalized algebraic data type](https://en.wikipedia.org/wiki/Generalized_algebraic_data_type)

* [Disjoint union](https://en.wikipedia.org/wiki/Disjoint_union)

**Miscellaneous**

* [Call stack](https://en.wikipedia.org/wiki/Call_stack)
* [Call graph](https://en.wikipedia.org/wiki/Call_graph)

* [Reflection (computer programming)](https://en.wikipedia.org/wiki/Reflection_%28computer_programming%29)

* [Function object](https://en.wikipedia.org/wiki/Function_object)

* [Memoization](https://en.wikipedia.org/wiki/Memoization)

* [Garbage collection (computer science)](https://en.wikipedia.org/wiki/Garbage_collection_%28computer_science%29)

**Functional Languages**

* [Lisp (programming language)](https://en.wikipedia.org/wiki/Lisp_%28programming_language%29)
* [Scheme Lisp](https://en.wikipedia.org/wiki/Scheme_%28programming_language%29)

* [Haskell](https://en.wikipedia.org/wiki/Haskell)

* [ML (programming language)](https://en.wikipedia.org/wiki/ML_%28programming_language%29)
* [Standard ML](https://en.wikipedia.org/wiki/Standard_ML)
* [OCaml](https://en.wikipedia.org/wiki/OCaml)
* [F# - Fsharp](https://en.wikipedia.org/wiki/F_Sharp_%28programming_language%29)


<!--
-->

#### Selected Rosettacode Pages

### Concepts Examples

* [Call a function](http://rosettacode.org/wiki/Call_a_function)

* [Higher-order functions](http://rosettacode.org/wiki/Higher-order_functions)

* [Closures/Value capture](http://rosettacode.org/wiki/Closures/Value_capture)

* [Function composition](http://rosettacode.org/wiki/Function_composition)

* [Partial function application](http://rosettacode.org/wiki/Partial_function_application)

* [Currying](http://rosettacode.org/wiki/Currying)

* [Catamorphism - Fold/Reduce](http://rosettacode.org/wiki/Catamorphism)

* [Null object](http://rosettacode.org/wiki/Null_object)

* [Y combinator](http://rosettacode.org/wiki/Y_combinator)

Recursion:

* [Anonymous recursion](http://rosettacode.org/wiki/Anonymous_recursion)

* [Ackermann function](http://rosettacode.org/wiki/Ackermann_function)

### Languages

* [Haskell](http://rosettacode.org/wiki/Haskell)

* [Ocaml](http://rosettacode.org/wiki/OCaml)

* [F# - Fsharp](http://rosettacode.org/wiki/Fsharp)

* [Scheme](http://rosettacode.org/wiki/scheme)

* [Racket](http://rosettacode.org/wiki/Racket)

* [Clojure](http://rosettacode.org/wiki/Clojure)

* [Scala](http://rosettacode.org/wiki/Scala)

* [JavaScript / ECMAScript](http://rosettacode.org/wiki/Category:JavaScript)
