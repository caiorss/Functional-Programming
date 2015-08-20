<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Functional Programming in Scheme](#functional-programming-in-scheme)
  - [Why Lisp](#why-lisp)
  - [Usefulness Of Scheme](#usefulness-of-scheme)
  - [Scheme Implementations](#scheme-implementations)
  - [Basic Syntax](#basic-syntax)
    - [Data Types](#data-types)
    - [Arithmetic](#arithmetic)
    - [Operators](#operators)
    - [Math Functions](#math-functions)
    - [Type Testing](#type-testing)
    - [Defining Functions](#defining-functions)
    - [List Operations](#list-operations)
  - [Books](#books)
  - [Articles](#articles)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Functional Programming in Scheme

## Why Lisp

* Lisp is used as embedded script language in many products
    * Autocad -> Autolisp
    * GNUCash -> GNU Guile Scheme 
    * GIMP Editor -> GNU Guile Scheme 

Products that use lisp:
* Emacs -> Emacs Lisp
* GNU Maxima -> Common Lisp

## Usefulness Of Scheme

* Learn the principles of programming languages
* Meta programming
* Lisp dialect that endorses Functional Programming
* Lightweight extension language or embedded language
* Scheme is being used as extension language of softwares like GIMP and GnuCash
* Scheme can be used to test JVM and .NET API in the REPL.
    * Google App Engine Uses Kawa scheme which is a implementations for the JVM.

## Scheme Implementations

| Implementation | Feature                                  |
|----------------|------------------------------------------|
| [MIT/Scheme](http://www.gnu.org/software/mit-scheme/)   | Classical Scheme Implementation  used by SCIP  |
| [Kawa](http://www.gnu.org/software/kawa/)           | Scheme for the JVM - Can use Java API and compile to the JVM |
| [Iron Scheme](https://ironscheme.codeplex.com/)    | Scheme for .NET platform - Can Use .NET API                |
| [GNU Guile](http://www.gnu.org/software/guile/docs/docs.html) | Used as embedded extension language for many apps  |
| [Chicken](http://www.call-cc.org/)  | Compile to Native Code, produces C code  | 
| [Chibi Scheme](https://github.com/ashinn/chibi-scheme) | Minimal Scheme Implementation for use as an Extension Language |
| [Racket](http://racket-lang.org/)   | IDE and Debugger. Superset of scheme, not fully compatible.  | 


See also: [An opinionated guide to scheme implementations](https://wingolog.org/archives/2013/01/07/an-opinionated-guide-to-scheme-implementations)

## Basic Syntax

### Data Types

```scheme

;;; Boolean
;;
;;---------------------------------
1 ]=> #f

;Value: #f

1 ]=> #t

;Value: #t

        
;;
;; Number
;;
;;---------------------------------
1 ]=> 12323

;Value: 12323

1 ]=> 123.232

;Value: 123.232

;; String
;;
;;---------------------------------
1 ]=> "hello world scheme"

;Value 15: "hello world scheme"

;; Symbol 
;;
;;---------------------------------
1 ]=> 'mysymbol

;Value: mysymbol

1 ]=> (quote mysymbol)

;Value: mysymbol



;; List
;; 
;;---------------------------------
1 ]=> '(23.23 1000 40 50 102)

;Value 16: (23.23 1000 40 50 102


;; List of Strings
;;
;;---------------------------------
1 ]=> '( "hello" "world" "scheme")

;Value 17: ("hello" "world" "scheme")

;; List of Symbols
;;
;;---------------------------------
1 ]=> '(hello world scheme)

;Value 18: (hello world scheme)

1 ]=> (quote (hello worl symbols))

;Value 31: (hello worl symbols)

;; S-expression
;;---------------------------------

1 ]=> '(+ 10 2)

;Value 19: (+ 10 2)

1 ]=> (quote (+ (sin 0.4) (cos 0.01)))

;Value 32: (+ (sin .4) (cos .01))

1 ]=> (quote (1 2 3 4 5 6))

;Value 33: (1 2 3 4 5 6)


```

### Arithmetic 

```scheme
$ rlwrap scheme
MIT/GNU Scheme running under GNU/Linux
Type `^C' (control-C) followed by `H' to obtain information about interrupts.

Copyright (C) 2011 Massachusetts Institute of Technology
This is free software; see the source for copying conditions. There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Image saved on Tuesday October 22, 2013 at 12:31:09 PM
  Release 9.1.1 || Microcode 15.3 || Runtime 15.7 || SF 4.41 || LIAR/i386 4.118
  Edwin 3.116

]=> 

]=> (+ 10 20 )

;Value: 30

]=> (+ 1 2 3 4 5 6)

;Value: 21

]=> (* 3 5)

;Value: 15

]=> (* 1 2 3 4 5 6)

;Value: 720

]=> (/ 10 20)

;Value: 1/2

]=> (/ 10.0 20.0)

;Value: .5

1 ]=> (/ 29 3)

;Value: 29/3

1 ]=> (/ 29 3 7)

;Value: 29/21

1 ]=> (/ 29 3 7 2)

;Value: 29/42

1 ]=> (exact->inexact (/ 29 3 7 2))

;Value: .6904761904761905


;;; 10 - 20
]=> (- 10 20)

;Value: -10

]=> 

;;; 3 * 4 + 8 / 4 + (-3) * 5
;;
1 ]=> (+ (* 3 4) (/ 8 4) (* -3 5))

;Value: -1



```

### Operators

```scheme

;;; Comparison Operators

]=> (> 10 200)

;Value: #f

]=> (= 10 10)

;Value: #t

]=> (< 10 200)

;Value: #t

]=> 

]=> (<= 10 20)

;Value: #t

]=> (>= 10 20)

;Value: #f

]=> (>= 10 10)

;Value: #t

]=> 

;;; Logical Operators

1 ]=> (not #t)

;Value: #f

1 ]=> (not #f)

;Value: #t

1 ]=>     

1 ]=> (and #f #t)

;Value: #f

1 ]=> (and #t #t)

;Value: #t

1 ]=> (or #t #t)

;Value: #t

1 ]=> (or #t #f)

;Value: #t


    
```

### Math Functions

```scheme

1 ]=> (sqrt 10)

;Value: 3.1622776601683795

1 ]=> (sqrt 100)

;Value: 10


1 ]=> (exp 1.0)

;Value: 2.718281828459045

1 ]=> (map exp '(-2 -1 0 1 2 3))

;Value 20: (.1353352832366127 .36787944117144233 1 2.718281828459045 7.38905609893065 20.08553692318767)

1 ]=> (log 10)

;Value: 2.302585092994046

1 ]=> (log (exp 1))

;Value: 1.


1 ]=> (define (logbase base) (lambda (x) (/ (log x) (log base))))

;Value: logbase

1 ]=> (logbase 10)

;Value 21: #[compound-procedure 21]

1 ]=> ((logbase 10) 10)

;Value: 1.

1 ]=> ((logbase 10) 100)

;Value: 2.

1 ]=> 

1 ]=> (define log10 (logbase 10))

;Value: log10

1 ]=> (log10 1000)

;Value: 2.9999999999999996

1 ]=> (map log10 '(0.01 1.0 10.0 100.0))

;Value 22: (-1.9999999999999996 0. 1. 2.)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Trigonometric Functions

;; Create PI constant
1 ]=> (define pi (* 4 (atan 1.0)))

;Value: pi

1 ]=> pi

;Value: 3.141592653589793


1 ]=> (define (rad2deg rad) (* (/ rad pi) 180))

;Value: rad2deg

1 ]=> (rad2deg pi)

;Value: 180.

1 ]=> 

1 ]=> (define (deg2rad deg) (* (/ deg 180) pi))

;Value: deg2rad

1 ]=> (deg2rad 180)

;Value: 3.141592653589793


;;
;;  Transforms a function that accepts angle in radians to a function
;;  that accepts a angle in degrees

1 ]=> (define (make-deg-func func) (lambda (deg) (func (deg2rad deg))))

;Value: make-deg-func

1 ]=> (define sind (make-deg-func sin))

;Value: sind

1 ]=> (sind 60)

;Value: .8660254037844386

1 ]=> (sind 30)

;Value: .49999999999999994

1 ]=> (sind 90)

;Value: 1.

1 ]=> (define cosd (make-deg-func cos))

;Value: cosd

1 ]=> (map cosd '(0 30 60 90 180))

;Value 23: (1 .8660254037844387 .5000000000000001 6.123233995736766e-17 -1.)



;;; Inverse Trigonometric Functions

1 ]=> (asin 0.5)

;Value: .523598775598298

1 ]=> (atan 1)

;Value: .7853981633974483

1 ]=> (define (make-inv-deg-func func) (lambda (x) (rad2deg (func x))))

;Value: make-inv-deg-func

1 ]=> ((make-inv-deg-func atan) 1)

;Value: 45.

1 ]=> (define atand (make-inv-deg-func atan))

;Value: atand

1 ]=> (atand 1.0)

;Value: 45.

1 ]=> (map atand '(0.0 0.5 1.0 2.0 1e10))

;Value 26: (0. 26.565051177077986 45. 63.43494882292201 89.99999999427042)


```

### Type Testing 


```scheme
(boolean? #f)

;Value: #t

1 ]=> (boolean? 100)

;Value: #f


1 ]=> (symbol? "x")

;Value: #f

1 ]=> (symbol? 'x)

;Value: #t


1 ]=> (integer? 100)

;Value: #t

1 ]=> (integer? 3.232)

;Value: #f


1 ]=> (real? 3232)

;Value: #t

1 ]=> (real? 3232.232)

;Value: #t


1 ]=> (string? "hello world Scheme Lisp")

;Value: #t

1 ]=> (string? 100232)

;Value: #f


1 ]=> (list? '(1 2 3 5 6))

;Value: #t

1 ]=> (list? 2323)

;Value: #f



1 ]=> (procedure? sin)

;Value: #t

1 ]=> (procedure? 2323)

;Value: #f


```

### Defining Functions

```scheme
]=> (define (f x) (* x 10))

;Value: f

]=> f

;Value 11: #[compound-procedure 11 f]

]=> (f 10)

;Value: 100

;; Map a function over a list
;;;; 
]=> (map f '(1 2 3 4 5 6))

;Value 12: (10 20 30 40 50 60)

;; Define a function of multiple variables
;;;;;

]=> (define (fxy x y) (+ (* 4 x) (* 3 y)))

;Value: fxy

]=> (fxy 3 5)

;Value: 27

;; Map a fucntion multiple variables over alist
;;;;
]=> (map (lambda (y) (fxy 3 y)) '(1 2 3 4 5 6))

;Value 14: (15 18 21 24 27 30)

;; Apply a list as function argument
;;;;;
1 ]=> (apply fxy '( 5 6))

;Value: 38

;; Transforms a function f into a new function that accepts
;; a list of arguments
;;
;;;;;;;
1 ]=> (define (currify f) (lambda (x) (apply f x)))

;Value: currify

1 ]=> (define fxy_c (currify fxy))

;Value: fxy_c

1 ]=> (fxy_c '( 3 4))

;Value: 24

1 ]=> (fxy_c '( 5 6))

;Value: 38

1 ]=> 

1 ]=> (map fxy_c (list '(5 6) '(3 7) '(8 9) '( 1 5)))

;Value 29: (38 33 59 19)

  ;;; OR
  
1 ]=> (map (currify fxy)  (list '(5 6) '(3 7) '(8 9) '( 1 5)))

;Value 30: (38 33 59 19)
```

Anonymous Functions/ Lambda Functions

```scheme
1 ]=> (lambda (x) (+ (* x 4) 10))

;Value 31: #[compound-procedure 31]

1 ]=> ((lambda (x) (+ (* x 4) 10))  10)

;Value: 50

1 ]=> (map (lambda (x) (+ (* x 4) 10))  '(10 20 30 40 50))

;Value 32: (50 90 130 170 210)

1 ]=> (define f (lambda (x) (+ (* x 4) 10)))

;Value: f

1 ]=> f

;Value 33: #[compound-procedure 33 f]

1 ]=> (map f '(10 20 30 40 50))

;Value 34: (50 90 130 170 210)

;; Scheme is a Functional Programming Language,
;;  so it can return functions from fucntions that
;;  can be used to define curried functions
;;
1 ]=> (define (addxy x y) (lambda (x) (lambda (y) (+ x y))))

;Value: addxy

1 ]=> ((addxy 10) 20)

;Value: 30

1 ]=> (define add10 (addxy 10))

;Value: add10

1 ]=> (add10 20)

;Value: 30

1 ]=> 


1 ]=> (map (addxy 10) '(10 20 30 40 50 60))

;Value 37: (20 30 40 50 60 70)

```

### List Operations

**List Functions**

```scheme

1 ]=> (length '(1 2 3 4 5 6))

;Value: 6

1 ]=> (reverse '(1 2 3 4 5 6))

;Value 27: (6 5 4 3 2 1)

1 ]=> (append '(1 2 3 4) '(5 6 7) '(8 9 10)) 

;Value 28: (1 2 3 4 5 6 7 8 9 10)

;; Test if list is empty 
;;
1 ]=> (null? '(1 2 3 4 5 6))

;Value: #f

1 ]=> (null? '())

;Value: #t

```

**Car, Cdr, Cons**

```scheme

1 ]=> (define Nil '())

;Value: nil

1 ]=> nil

;Value: ()

;;; Cons - List Constructor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

1 ]=> (cons 4 nil)

;Value 11: (4)

1 ]=> (cons 4 (cons 10 nil))

;Value 12: (4 10

1 ]=> '(1 2 3 4 5 6)

;Value 38: (1 2 3 4 5 6)

1 ]=> (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 '() ))))))

;Value 39: (1 2 3 4 5 6)

1 ]=> (cons 10 '( 1 2 3))

;Value 15: (10 1 2 3)

1 ]=> (cons 4 (cons 10 '( 1 2 3)))

;Value 16: (4 10 1 2 3)

;; Car - List Head
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

1 ]=> (car '(10 2 3 4 5 6))

;Value: 10


;; Cdr  - List tail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

1 ]=> (cdr '(10 2 3 4 5 6))

;Value 13: (2 3 4 5 6)

```

**Recursive Functions Over Lists**

```scheme

(define (count-list alist)
    (if (null? alist)
        0
        (+ 1 (count-list (cdr alist)))))

;Value: count-list

1 ]=> (count-list '())

;Value: 0

1 ]=> (count-list '(8 91 293 23))

;Value: 4

1 ]=> (count-list '(20 2932 1923 129 12 535 22))

;Value: 7

(define (sum-list alist)
    (if (null? alist)
        0
        (+ (car alist) (sum-list (cdr alist)))))

1 ]=> (sum-list '(1 2 3 4 5 6 ))

;Value: 21


(define (find predicate alist)
    (if (null? alist)
        (error "Error: Predicate not found")
        (if (predicate (car alist))
            (car alist)
            (find predicate (cdr alist)))))

1 ]=> (find (lambda (x) (< x 10)) '( 20 40 8 9 100 50 7))

;Value: 8

1 ]=> (find (lambda (x) (> x 50))  '( 20 40 8 9 100 50 7))

;Value: 100


1 ]=> (find (lambda (x) (> x 500))  '( 20 40 8 9 100 50 7))

;Error: Predicate not found
;To continue, call RESTART with an option number:
; (RESTART 1) => Return to read-eval-print level 1.

2 error> 


(define (take n alist)
    (if (or (null? alist) (= n 0))
        '()
        (cons (car alist) (take (- n 1) (cdr alist)))))

1 ]=> (take 10 '())

;Value: ()

1 ]=> (take 3 '(1 2 3 4 5 6 ))

;Value 17: (1 2 3)

1 ]=> (take 4 '(1 2 3 4 5 6 ))

;Value 18: (1 2 3 4)

1 ]=> (take 100 '(1 2 3 4 5 6 ))

;Value 19: (1 2 3 4 5 6)

1 ]=> 

(define (drop n alist)
    (if (or (null? alist) (= n 0))
        alist
        (drop (- n 1) (cdr alist))))

1 ]=> (drop 3 '(1 2 3 4 5 6 ))

;Value 20: (4 5 6)

1 ]=> (drop 5 '(1 2 3 4 5 6 ))

;Value 21: (6)

1 ]=> (drop 15 '(1 2 3 4 5 6 ))

;Value: ()

1 ]=> (drop 15 '())

;Value: ()

1 ]=> 


(define (take_while predicate alist)
    (if (or (null? alist) (not (predicate (car alist))))
        '()
        (cons (car alist) (take_while predicate (cdr alist)))))
        
1 ]=> (take_while (lambda (x) (< x 10)) '(1 8 7 9 10 20 5 62 2 3))

;Value 29: (1 8 7 9)

1 ]=> (take_while (lambda (x) (> x 10)) '(1 8 7 9 10 20 5 62 2 3))

;Value: ()

1 ]=> (take_while (lambda (x) (< x 10)) '())

;Value: ()


(define (drop_while predicate alist)
    (if (or (null? alist) (not (predicate (car alist))))
        alist
        (drop_while predicate (cdr alist))))


1 ]=> (drop_while (lambda (x) (< x 10)) '(1 8 7 9 10 20 5 62 2 3))

;Value 30: (10 20 5 62 2 3)

1 ]=> (drop_while (lambda (x) (< x 100)) '())

;Value: ()


(define (select predicate alist)
    (if (null? alist)
    '()
    (if (predicate (car alist))        
        (cons (car alist) (select predicate (cdr alist)))        
        (select predicate (cdr alist)))))
        

1 ]=> (define (is_even x) (= 0 (modulo x 2)))

1 ]=> (select is_even '( 1 2 3 4 5 6 7 8 9 1))

;Value 34: (2 4 6 8)

;; Higher Order function to invert Predicate
1 ]=> (define (neg predicate) (lambda (x) (not (predicate x))))

;Value: neg

1 ]=> (select (neg is_even) '( 1 2 3 4 5 6 7 8 9 1))

;Value 35: (1 3 5 7 9 1)

1 ]=> (define is_odd (neg is_even))

;Value: is_odd


1 ]=> (select is_odd  '( 1 2 3 4 5 6 7 8 9 1))

;Value 36: (1 3 5 7 9 1)

(define (reject predicate alist)
    (if (null? alist)
    '()
    (if (not (predicate (car alist)))        
        (cons (car alist) (reject predicate (cdr alist)))        
        (reject predicate (cdr alist)))))

1 ]=> (reject is_even '( 1 2 3 4 5 6 7 8 9 1))

;Value 11: (1 3 5 7 9 1)

1 ]=>  (reject (neg is_even) '( 1 2 3 4 5 6 7 8 9 1))

;Value 13: (2 4 6 8)

;; Fold Right - https://en.wikipedia.org/wiki/Fold_(higher-order_function)
;;
;; foldr :: (a -> b -> b) -> b -> [a] -> b
;; foldr f z []     = z
;; foldr f z (x:xs) = f x (foldr f z xs)

(define (foldr f_el_acc acc alist)
    (if (null? alist)
        acc
        (f_el_acc (car alist) (foldr f_el_acc acc (cdr alist)))))
        
1 ]=> (foldr + 0 '(1 2 3 4 5 6))

;Value: 21

1 ]=> (foldr (lambda (el acc) (+ el (* 10 acc))) 0 '(1 2 3 4 5 6))

;Value: 654321

 
;; Fold Left - https://en.wikipedia.org/wiki/Fold_(higher-order_function)
;;
;; foldl :: (b -> a -> b) -> b -> [a] -> b
;; foldl f z []     = z
;; foldl f z (x:xs) = foldl f (f z x) xs       

(define (foldl f_el_acc acc alist)
    (if (null? alist)
        acc
        (foldl f_el_acc (f_el_acc acc (car alist)) (cdr alist))))

1 ]=> (foldl + 0 '(1 2 3 4 5 6))

;Value: 21

1 ]=> (foldl (lambda (el acc) (+ (* 10 el)  acc)) 0 '(1 2 3 4 5 6))

;Value: 123456

```

### Java API With Kawa Scheme 

* [Kawa Scheme](http://www.gnu.org/software/kawa/Getting-Kawa.html)
* https://en.wikipedia.org/wiki/Kawa_(Scheme_implementation)


Product built with Kawa Scheme: [App Inventor for Android](https://en.wikipedia.org/wiki/App_Inventor_for_Android)

#### Install and Run

```
    $ curl -O ftp://ftp.gnu.org/pub/gnu/kawa/kawa-2.0.jar

    $ java -jar kawa-2.0.jar 
    #|kawa:1|# 
    #|kawa:2|# 

    $ java -jar kawa-2.0.jar 
    #|kawa:1|# 

    #|kawa:2|# (+ 1 2 3 4 5 6)
    21

    #|kawa:3|# (define (foldl f_el_acc acc alist)
        (if (null? alist)
            acc
            (foldl f_el_acc (f_el_acc acc (car alist)) (cdr alist))))
    #|kawa:7|# (foldl (lambda (el acc) (+ (* 10 el)  acc)) 0 '(1 2 3 4 5 6))
    123456
    #|kawa:8|# 

```


#### Calling Java Methods in Kawa Scheme

Apis Used:

* [java.lang.Math](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
* [java.util.Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)

```scheme 

    $ rlwrap java -jar kawa-2.0.jar 
    
    #|kawa:1|# 
    #|kawa:2|# (java.lang.Math:sqrt 9.0)
    3.0
    #|kawa:3|# (java.lang.Math:log10 100.0)
    2.0
    #|kawa:4|# (java.lang.Math:log10 1000.0)
    3.0

    #|kawa:9|# (define (log10 x ) (java.lang.Math:log10 x))
    #|kawa:10|# 
    #|kawa:11|# (log10 100)
    2.0
    #|kawa:12|# (log10 1000)
    3.0
    #|kawa:13|# 

    #|kawa:13|# (map log10 '(10 100 1000 1000))
    (1.0 2.0 3.0 3.0)
    #|kawa:14|# 

    ;;;  java.util.Date 
    ;;;----------------------------

    #|kawa:28|# (java.util.Date )
    Thu Aug 20 09:44:57 BRT 2015
    #|kawa:29|# 
    #|kawa:30|# 
    
    #|kawa:30|# (define today (java.util.Date))
    #|kawa:31|# today
    Thu Aug 20 09:45:37 BRT 2015
    #|kawa:32|# 

    #|kawa:38|# (today:getTime)
    /dev/stdin:38:2: warning - no known slot 'getTime' in java.lang.Object
    1440074737071
    #|kawa:39|# 

    #|kawa:40|# (today:getMonth)
    /dev/stdin:40:2: warning - no known slot 'getMonth' in java.lang.Object
    7
    #|kawa:41|# 
    
    #|kawa:41|# (today:toString)
    Thu Aug 20 09:45:37 BRT 2015
    #|kawa:42|#

    ;;; Java.util.Calendar
    ;;;--------------------------------------

    #|kawa:2|# (define Calendar java.util.Calendar)
    #|kawa:3|# 

    
    #|kawa:5|# (Calendar:getInstance)
    java.util.GregorianCalendar[time=1440075518905,areFieldsSet=true,areAllFieldsSet=true,lenient=true,zone=sun.util.calendar.ZoneInfo[id="America/Recife",offset=-10800000,dstSavings=0,useDaylight=false,transitions=41,lastRule=null],firstDayOfWeek=1,minimalDaysInFirstWeek=1,ERA=1,YEAR=2015,MONTH=7,WEEK_OF_YEAR=34,WEEK_OF_MONTH=4,DAY_OF_MONTH=20,DAY_OF_YEAR=232,DAY_OF_WEEK=5,DAY_OF_WEEK_IN_MONTH=3,AM_PM=0,HOUR=9,HOUR_OF_DAY=9,MINUTE=58,SECOND=38,MILLISECOND=905,ZONE_OFFSET=-10800000,DST_OFFSET=0]
    #|kawa:6|# 
   

    #|kawa:44|# (define cal (Calendar:getInstance))
    #|kawa:45|# 

    #|kawa:9|# cal
    java.util.GregorianCalendar[time=1440075574190,areFieldsSet=true,areAllFieldsSet=true,lenient=true,zone=sun.util.calendar.ZoneInfo[id="America/Recife",offset=-10800000,dstSavings=0,useDaylight=false,transitions=41,lastRule=null],firstDayOfWeek=1,minimalDaysInFirstWeek=1,ERA=1,YEAR=2015,MONTH=7,WEEK_OF_YEAR=34,WEEK_OF_MONTH=4,DAY_OF_MONTH=20,DAY_OF_YEAR=232,DAY_OF_WEEK=5,DAY_OF_WEEK_IN_MONTH=3,AM_PM=0,HOUR=9,HOUR_OF_DAY=9,MINUTE=59,SECOND=34,MILLISECOND=190,ZONE_OFFSET=-10800000,DST_OFFSET=0]
    #|kawa:10|# 


    #|kawa:16|# (cal:get Calendar:ERA)
    /dev/stdin:16:2: warning - no known slot 'get' in java.lang.Object
    /dev/stdin:16:10: warning - no known slot 'ERA' in java.lang.Object
    1
    #|kawa:17|# 

    
    #|kawa:51|# (cal:get Calendar:DATE)
    /dev/stdin:51:2: warning - no known slot 'get' in java.lang.Object
    20
    #|kawa:52|# 
    
    #|kawa:14|# (cal:getTime)
    Thu Aug 20 09:59:34 BRT 2015
    #|kawa:15|# 

    #|kawa:17|# (cal:toString)
    java.util.GregorianCalendar[time=1440075574190,areFieldsSet=true,areAllFieldsSet=true,lenient=true,zone=sun.util.calendar.ZoneInfo[id="America/Recife",offset=-10800000,dstSavings=0,useDaylight=false,transitions=41,lastRule=null],firstDayOfWeek=1,minimalDaysInFirstWeek=1,ERA=1,YEAR=2015,MONTH=7,WEEK_OF_YEAR=34,WEEK_OF_MONTH=4,DAY_OF_MONTH=20,DAY_OF_YEAR=232,DAY_OF_WEEK=5,DAY_OF_WEEK_IN_MONTH=3,AM_PM=0,HOUR=9,HOUR_OF_DAY=9,MINUTE=59,SECOND=34,MILLISECOND=190,ZONE_OFFSET=-10800000,DST_OFFSET=0]
    #|kawa:18|# 

    #|kawa:18|# (cal:getTimeZone)
    /dev/stdin:18:2: warning - no known slot 'getTimeZone' in java.lang.Object
    sun.util.calendar.ZoneInfo[id="America/Recife",offset=-10800000,dstSavings=0,useDaylight=false,transitions=41,lastRule=null]
    #|kawa:19|#     

```

### Create a GUI

**Create a GUI in the REPL**

![](images/java_swing_window1.png)

Original Java Code: [Java Swing Tutorial](http://www.wideskills.com/java-tutorial/java-swing-tutorial)

Int the REPL:

```scheme
    $ rlwrap java -jar kawa-2.0.jar 
    #|kawa:1|# 

    #|kawa:4|# (define JFrame javax.swing.JFrame)
    #|kawa:5|# (define JLabel javax.swing.JLabel)
    #|kawa:6|# 

    #|kawa:10|# (define jlbHelloWorld (JLabel))

    #|kawa:7|# (define HelloWorldFrame (JFrame))
    #|kawa:8|# HelloWorldFrame
    javax.swing.JFrame[frame0,0,0,0x0,invalid,hidden,layout=java.awt.BorderLayout,title=,resizable,normal,defaultCloseOperation=HIDE_ON_CLOSE,rootPane=javax.swing.JRootPane[,0,0,0x0,invalid,layout=javax.swing.JRootPane$RootLayout,alignmentX=0.0,alignmentY=0.0,border=,flags=16777673,maximumSize=,minimumSize=,preferredSize=],rootPaneCheckingEnabled=true]
    #|kawa:9|# 


    #|kawa:13|# jlbHelloWorld
    javax.swing.JLabel[,0,0,0x0,invalid,alignmentX=0.0,alignmentY=0.0,border=,flags=8388608,maximumSize=,minimumSize=,preferredSize=,defaultIcon=,disabledIcon=,horizontalAlignment=LEADING,horizontalTextPosition=TRAILING,iconTextGap=4,labelFor=,text=,verticalAlignment=CENTER,verticalTextPosition=CENTER]
    #|kawa:14|# 

    #|kawa:18|# (HelloWorldFrame:add jlbHelloWorld)
    /dev/stdin:18:2: warning - no known slot 'add' in java.lang.Object
    javax.swing.JLabel[,0,0,0x0,invalid,alignmentX=0.0,alignmentY=0.0,border=,flags=8388608,maximumSize=,minimumSize=,preferredSize=,defaultIcon=,disabledIcon=,horizontalAlignment=LEADING,horizontalTextPosition=TRAILING,iconTextGap=4,labelFor=,text=Hello World Kawa,verticalAlignment=CENTER,verticalTextPosition=CENTER]
    #|kawa:19|# 

    #|kawa:22|# (HelloWorldFrame:setSize 100 100)
    /dev/stdin:22:2: warning - no known slot 'setSize' in java.lang.Object
    #!null

    #|kawa:23|# (HelloWorldFrame:setVisible #t)
    /dev/stdin:23:2: warning - no known slot 'setVisible' in java.lang.Object
    #!null
    #|kawa:24|# 
```

In the file: [java_swing_gui1.scm](src/java_swing_gui1.scm)
```scheme
;; Creates Java GUI
;;
;; Similar to: http://www.wideskills.com/java-tutorial/java-swing-tutorial 
;; 
(define JFrame javax.swing.JFrame)
(define JLabel javax.swing.JLabel)

(define jlbHelloWorld (JLabel "Hello World Kawa"))

(define HelloWorldFrame (JFrame))

(HelloWorldFrame:add jlbHelloWorld)
(HelloWorldFrame:setSize 100 100)
(HelloWorldFrame:setVisible #t)
```

Run the file:

```
$ java -jar kawa-2.0.jar java_swing_gui1.scm
```

## Books

* [Structure and Interpretation of Computer Programs - SCIP / Abelson, Sussman, and Sussman.](https://mitpress.mit.edu/sicp/)
* [Structure and Interpretation of Computer Programs - Video Lectures by Hal Abelson and Gerald Jay Sussman](http://groups.csail.mit.edu/mac/classes/6.001/abelson-sussman-lectures/)

* [SCIP Solutions](http://community.schemewiki.org/?SICP-Solutions)


* [Structure and Interpretation of Classical Mechanics - Geral Jay Sussman and Jack Wisdom](https://mitpress.mit.edu/books/structure-and-interpretation-classical-mechanics)


* [Wikibok - Programming with Scheme](https://en.wikibooks.org/wiki/Scheme_Programming)
* [The Scheme Programming Language Fourth Edition - R. Kent Dybvig - Illustrations by Jean-Pierre Hébert](http://www.scheme.com/tspl4/)

* [MIT/GNU Scheme Reference Manual](http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref.pdf)


## Articles

* [Beating the Average - The Secret Weapon - By Paul Graham](http://www.paulgraham.com/avg.html)

* [7 lines of code, 3 minutes: Implement a programming language from scratch](http://matt.might.net/articles/implementing-a-programming-language/)


* [Canonical S-expression](https://en.wikipedia.org/wiki/Canonical_S-expressions)
* [SEXP---(S-expressions)](http://people.csail.mit.edu/rivest/sexp.html)
* [S-Expressions](http://people.csail.mit.edu/rivest/Sexp.txt)
