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
* Function composition
* Composable functions
* High Order Functions
* MapReduce Algorithms - Split computation in multiple computers cores.
* Lazy Evaluation ( aka Delayed evaluation)
* Pattern Matching


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

