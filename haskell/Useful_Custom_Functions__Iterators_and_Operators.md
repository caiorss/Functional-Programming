- [Useful Custom Functions/ Iterators and Operators](#useful-custom-functions/-iterators-and-operators)
  - [Pipelining Operators](#pipelining-operators)
    - [Iterators](#iterators)
    - [Pair Iterator](#pair-iterator)
    - [Triples Iterator](#triples-iterator)
    - [Sliding Window Iterator](#sliding-window-iterator)
    - [Enumerate Iterator](#enumerate-iterator)
  - [Applying Multiples Functions](#applying-multiples-functions)
    - [Applying a list of functions to the same argument.](#applying-a-list-of-functions-to-the-same-argument.)
    - [Applying a tuple of functions to a same argument.](#applying-a-tuple-of-functions-to-a-same-argument.)
    - [Control Flow Functions](#control-flow-functions)

# Useful Custom Functions/ Iterators and Operators<a id="sec-1" name="sec-1"></a>

This is a collection of useful short code snippets. 

## Pipelining Operators<a id="sec-1-1" name="sec-1-1"></a>

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

### Iterators<a id="sec-1-1-1" name="sec-1-1-1"></a>

### Pair Iterator<a id="sec-1-1-2" name="sec-1-1-2"></a>

Definition:

```haskell
pairs alist = zip alist (tail alist)
```

The pairs iterator converts a list of elements to a new list of consecutive elements tuple. 

Pseudo code:

```
pairs [e0, e1, e2, e3, e4 ...] ==> [(e0, e1), (e1, e2), (e3, e4) ...]
```

Let f be a function of two arguments:

```
f :: a -> a -> b
```

The function f can be applied to to the pairs sequence using the higher order function uncurry.

Pseudo code:

```
> g = uncurry(f) :: (a, a) -> b
> g (x, y) = f x y

> map uncurry(f) $ pairs [e0, e1, e2, e3, e4 ...]
>   [g (e0, e1), g (e1, e2), g (e2, e3), g (e3, e4) ...]
```

It can be useful to calculate the distance between two points, lagged difference, growth of a time series, draw a line between each two consecutive points or apply any function to two consecutive elements.

Example: Grouping Consecutive numbers

```haskell
> let pairs alist = zip alist (tail alist)

> pairs [1..5]
[(1,2),(2,3),(3,4),(4,5)]
```

Example: Lagged Difference

Pseudo code:

```
lagdiff [e0, e1, e2, e3, e4 ...] ==> [(e1 - e0), (e2 - e1), (e3 - e2) ... ]
```

Development:

```haskell
> let pairs alist = zip alist (tail alist)

> :t pairs
pairs :: [b] -> [(b, b)]

> :t (-)
(-) :: Num a => a -> a -> a

> :t uncurry(-)
uncurry(-) :: Num c => (c, c) -> c
> 

> (-) 20 10
10
> 

> uncurry(-) (20, 10)
10
> 

> uncurry(-) (10, 20)
-10
> 

> uncurry(flip (-)) (10, 20)
10
> 

> pairs [10.3, 20.5, 5.6, 8.23, 40.3]
[(10.3,20.5),(20.5,5.6),(5.6,8.23),(8.23,40.3)]
> 

> map (uncurry ( flip (-))) $ pairs [10.3, 20.5, 5.6, 8.23, 40.3]
[10.2,-14.9,2.630000000000001,32.06999999999999]
> 

> let lagdiff series = map (uncurry ( flip (-))) $ pairs series
> :t lagdiff 
lagdiff :: Num b => [b] -> [b]
> 

> lagdiff [10.3, 20.5, 5.6, 8.23, 40.3]
[10.2,-14.9,2.630000000000001,32.06999999999999]
> 

> lagdiff [10, 30, 5, 8, 100]
[20,-25,3,92]
>
```

Example: Distance between points on the plane.

```haskell
> let pairs alist = zip alist (tail alist)

{- [(X, Y)]  coordinates of points in a plane -}
> let points = [(1.0, 2.0), (3.0, 4.0), (-1.0, 5.0), (6.0, 6.0)]
> let distance (x1, y1) (x2, y2) = sqrt( (x2-x1)^2 + (y2-y1)^2 )

> let lines = pairs points 
> lines
[((1.0,2.0),(3.0,4.0)),((3.0,4.0),(-1.0,5.0)),((-1.0,5.0),(6.0,6.0))]
> 

> distance (1.0, 2.0) (3.0, 4.0)
2.8284271247461903
> 

{- Calculate the length of each line segment -}

> map (uncurry(distance)) lines
[2.8284271247461903,4.123105625617661,7.0710678118654755]
> 

> sum $ map (uncurry(distance)) lines
14.022600562229327
> 

> let totalLength points  =  sum $  map (uncurry(distance)) $ pairs (points)
> 
> totalLength points 
14.022600562229327
>
```

### Triples Iterator<a id="sec-1-1-3" name="sec-1-1-3"></a>

Definition:

```haskell
triples alist = zip3 alist (tail alist) (tail $ tail alist)
```

Example:

```haskell
> triples [1..10]
[(1,2,3),(2,3,4),(3,4,5),(4,5,6),(5,6,7),(6,7,8),(7,8,9),(8,9,10)]
> 

> :t triples 
triples :: [c] -> [(c, c, c)]
> 
>
```

### Sliding Window Iterator<a id="sec-1-1-4" name="sec-1-1-4"></a>

This iterator is used in Scala and it is a generalized pairs iterator.

Definition:

```haskell
sliding n alist = map (take n) (take (length(alist) -n + 1 ) $ iterate tail alist)
```

Example:

```haskell
>  sliding 3 [1..10]
[[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7],[6,7,8],[7,8,9],[8,9,10]]

>  sliding 4 [1..10]
[[1,2,3,4],[2,3,4,5],[3,4,5,6],[4,5,6,7],[5,6,7,8],[6,7,8,9],[7,8,9,10]]

> sliding 5 [1..10]
[[1,2,3,4,5],[2,3,4,5,6],[3,4,5,6,7],[4,5,6,7,8],[5,6,7,8,9],[6,7,8,9,10]]

> sliding 6 [1..10]
[[1,2,3,4,5,6],[2,3,4,5,6,7],[3,4,5,6,7,8],[4,5,6,7,8,9],[5,6,7,8,9,10]]

> sliding 9 [1..10]
[[1,2,3,4,5,6,7,8,9],[2,3,4,5,6,7,8,9,10]]
>
```

Scala Equivalent

```
scala> (1 to 5).iterator.sliding(3).toList
res2: List[Seq[Int]] = List(List(1, 2, 3), List(2, 3, 4), List(3, 4, 5))
```

### Enumerate Iterator<a id="sec-1-1-5" name="sec-1-1-5"></a>

Equivalent to python enumerate.

Definition:

```haskell
enumerate :: [b] -> [(Int, b)]
enumerate alist = zip [0..(length(alist)-1)] alist
```

Example:

```haskell
> enumerate ['a', 'b', 'c', 'd', 'e', 'f']
[(0,'a'),(1,'b'),(2,'c'),(3,'d'),(4,'e'),(5,'f')]

> take 8  (enumerate ['a'..'z'])
[(0,'a'),(1,'b'),(2,'c'),(3,'d'),(4,'e'),(5,'f'),(6,'g'),(7,'h')]
>
```

**Group by length**

Definition:

```haskell
groupByLen n alist = filter (\a -> length(a) == n  ) ( map f indexes )
    where
    len = length(alist)
    indexes = map (\i -> n*i) [0..(div len n)]
    f idx = take n (drop idx alist)
```

Example:

```haskell
> groupByLen 3 [1..15]
[[1,2,3],[4,5,6],[7,8,9],[10,11,12],[13,14,15]]
> 
> groupByLen 5 [1..15]
[[1,2,3,4,5],[6,7,8,9,10],[11,12,13,14,15]]
> 
> groupByLen 6 [0..20]
[[0,1,2,3,4,5],[6,7,8,9,10,11],[12,13,14,15,16,17]]
> 
> groupByLen 3 ['a'..'z']
["abc","def","ghi","jkl","mno","pqr","stu","vwx"]
>
```

## Applying Multiples Functions<a id="sec-1-2" name="sec-1-2"></a>

### Applying a list of functions to the same argument.<a id="sec-1-2-1" name="sec-1-2-1"></a>

**juxt** is a function that allows apply a list of functions of same type signature to a single argument. This is useful for numerical analysis, statistics and engineering. This function was taken from the Clojure library.

```haskell
juxt fs x = map ($ x) fs
```

Example:

```
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
```

### Applying a tuple of functions to a same argument.<a id="sec-1-2-2" name="sec-1-2-2"></a>

The family of functions juxt2, juxt3, juxt4 allow apply tuples of functions to a single argument. This is necessary when the functions don't have the same type signature. 

```haskell
juxt2 (f1, f2) x = (f1 x, f2 x)
juxt3 (f1, f2, f3) x = (f1 x, f2 x, f3 x)
juxt4 (f1, f2, f3, f4) x = (f1 x, f2 x, f3 x, f4 x)
juxt5 (f1, f2, f3, f4, f5) x = (f1 x, f2 x, f3 x, f4 x, f5 x)
```

Example:

```haskell
{- 

This function fails in static typed language when 
the functions don't have the same type signature 

-}
> juxt [(>100), (+100)]  30

<interactive>:36:9:
    No instance for (Num Bool) arising from the literal `100'
    Possible fix: add an instance declaration for (Num Bool)
    In the second argument of `(>)', namely `100'
    In the expression: (> 100)
    In the first argument of `juxt', namely `[(> 100), (+ 100)]'


> juxt2 ((>100), (+100))  30
(False,130)
> 
> 
> let f = juxt2 ((>100), (+100))
> :t f
f :: Integer -> (Bool, Integer)
> 
> f 30
(False,130)
>
> map f [10, 20, 25, 30, 100, 150]
[(False,110),(False,120),(False,125),(False,130),(False,200),(True,250)]
> 


> 
> juxt3 (length, maximum, minimum)  [100.23, 23.23, 12.33, -123.23, -1000.23, 4000.5]
(6,4000.5,-1000.23)
> 


{- 

    The type system fails to resolve the types in someone
    cases. So when it happens the developer must make the
    function types explicit.

-}
> let analytics = juxt3 (length, maximum, minimum)
> 
> :t analytics 
analytics :: [()] -> (Int, (), ())
> 
> analytics [100.23, 23.23, 12.33, -123.23, -1000.23, 4000.5]

<interactive>:79:12:
    No instance for (Fractional ()) arising from the literal `100.23'
    Possible fix: add an instance declaration for (Fractional ())
    In the expression: 100.23
    In the first argument of `analytics', namely
      `[100.23, 23.23, 12.33, - 123.23, ....]'
    In the expression: analytics [100.23, 23.23, 12.33, - 123.23, ....]

> let analytics :: [Double] -> (Int, Double, Double)  ; analytics = juxt3 (length, maximum, minimum)
> 
> :t analytics 
analytics :: [Double] -> (Int, Double, Double)
> 

> analytics [100.23, 23.23, 12.33, -123.23, -1000.23, 4000.5]
(6,4000.5,-1000.23)
> 

> import Data.Char
> 
> let f = juxt3 (succ, pred, ord)
> :t f
f :: Char -> (Char, Char, Int)
> 
> let a = map f "haskell is fun"
> a
[('i','g',104),('b','`',97),('t','r',115),('l','j',107),('f','d',101),('m','k',108),('m','k',108),('!','\US',32),('j','h',105),('t','r',115),('!','\US',32),('g','e',102),('v','t',117),('o','m',110)]
> 
> unzip3 a
("ibtlfmm!jt!gvo","g`rjdkk\UShr\USetm",[104,97,115,107,101,108,108,32,105,115,32,102,117,110])
> 
>
```

### Control Flow Functions<a id="sec-1-2-3" name="sec-1-2-3"></a>

**between**

```haskell
between a b x = a <= x && x <= b
```

```haskell
> filter (between 10 20) [23, 5, 8, 17, 24, 13, 12]
[17,13,12]
> 

> filter (not . between 10 20) [23, 5, 8, 17, 24, 13, 12]
[23,5,8,24]
```

**ifelseDo**

Pseudo Code:

```
f(x) = if pred(x) == True then: fa(x) else fb(x)
```

```haskell
ifelseDo pred fa fb  x  =  if pred x then fa x else fb x
```

Example: Apply the list the function (if x >10 then 10\*x else x+5)

```haskell
> let  ifelseDo pred fa fb  x  =  if pred x then fa x else fb x

> map (ifelseDo (>10) (*4) (+5)) [-1, 2, 7, 8, 9, 10, 20, 30, 50]
[4,7,12,13,14,15,80,120,200]

> let f = ifelseDo (>10) (*4) (+5)

> f 1
6
> f 3
8
> f 5
10
> 

> f 20
80
> f 30
120

λ> map f [-1, 2, 7, 8, 9, 10, 20, 30, 50]
[4,7,12,13,14,15,80,120,200]
```

**ifelse\***

Pseudo Code:

```
f(x) = if pred(x) == True then: a else b
```

```haskell
ifelse   pred a  b   x  =  if pred x then a   else b
```

Example: 

If x<0 set x to -1 else set to 1

```haskell
> let f  = ifelse (<0) (-1) 1
> 
> f 2
1
> f 40
1
> f (-1)
-1
> f (-10)
-1

> map f [-1, -2, 3, 4, -5, -6, 0, 2]
[-1,-1,1,1,-1,-1,1,1]
>
```

**ifelseEq**

Pseudo Code:

```
f(x) = if pred(x) == True then: a else x
```

```haskell
ifelseEq pred a      x  =  if pred x then a   else x
```

Example: if(x) > 10 then 30 else x

```haskell
> let ifelseEq pred a x  =  if pred x then a   else x

> let f = ifelseEq (>10) 30

> map f [1, 2, 20, 10, 9, 8, 100]
[1,2,30,10,9,8,30]
>
```
