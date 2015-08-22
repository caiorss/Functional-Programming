<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Functional Programming in Scheme](#functional-programming-in-scheme)
  - [Why Lisp](#why-lisp)
  - [Usefulness Of Scheme](#usefulness-of-scheme)
  - [Scheme Implementations](#scheme-implementations)
  - [Basic Syntax](#basic-syntax)
    - [Data Types](#data-types)
    - [Defining Functions and Variables](#defining-functions-and-variables)
      - [Global Variable](#global-variable)
      - [Local Variable](#local-variable)
      - [Functions](#functions)
    - [Arithmetic](#arithmetic)
    - [Operators](#operators)
    - [Math Functions](#math-functions)
    - [Type Testing](#type-testing)
    - [String Functions](#string-functions)
    - [List Operations](#list-operations)
      - [Define a List](#define-a-list)
      - [Primitive List Operations](#primitive-list-operations)
      - [List Functions](#list-functions)
  - [Higher Order Functions](#higher-order-functions)
    - [Special Functions](#special-functions)
    - [Functions Composition](#functions-composition)
    - [Applying Multiple Functions to a Single Argument](#applying-multiple-functions-to-a-single-argument)
    - [Miscellaneous](#miscellaneous)
  - [Kawa Scheme - Access Java API from Scheme](#kawa-scheme---access-java-api-from-scheme)
      - [Install and Run](#install-and-run)
      - [Calling Java Methods in Kawa Scheme](#calling-java-methods-in-kawa-scheme)
    - [Create a GUI](#create-a-gui)
    - [See also](#see-also)
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
| [MIT - Scheme](http://www.gnu.org/software/mit-scheme/)   | Classical Scheme Implementation  used by [SCIP](https://en.wikipedia.org/wiki/Structure_and_Interpretation_of_Computer_Programs)  |
| [Kawa](http://www.gnu.org/software/kawa/)           | Scheme for the JVM - Java API access. Compiles to the JVM |
| [Iron Scheme](https://ironscheme.codeplex.com/)    | Scheme for .NET platform - .NET API Acess       |
| [GNU Guile](http://www.gnu.org/software/guile/docs/docs.html) | Used as embedded extension language for many apps like GIMP, GNUCash, GEDA |
| [Chicken](http://www.call-cc.org/)  | Compiles to Native Code, produces C code  | 
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

### Defining Functions and Variables

#### Global Variable

```scheme
1 ]=> (define g 9.81)

;Value: g

1 ]=> g

;Value: 9.81

1 ]=> (define (speed v0 t) (+ (* g t) v0))

;Value: speed

1 ]=> (speed 3 2.4)

;Value: 26.544

```

#### Local Variable

```scheme 
(let 
    (
     (x 10)
     (y 20)
     (f (lambda (a) (* a 10)))
    )
    (f (+ x y))
)

;Value: 300

1 ]=> x
;Unbound variable: x

1 ]=> y
;Unbound variable: y

1 ]=> f
;Unbound variable: f
```

#### Functions

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

### String Functions

**Predicates**

```scheme

;;;; Test if is String

1 ]=> (string? "scheme")

;Value: #t

1 ]=> (string? 1000)

;Value: #f

;;; Test is String is null, empty

1 ]=> (string-null? "")

;Value: #t

1 ]=> (string-null? "scheme")

;Value: #f

;;;; Test if String starts with prefix

1 ]=> (string-prefix?  "example" "example-11232.x")

;Value: #t

1 ]=> (string-prefix?  "example" "11232.x")

;Value: #f


;;;; Test if String ends with suffix

1 ]=> (string-suffix? "class" "machine.class")

;Value: #t

1 ]=> (string-suffix? "class" "machine.dex")

;Value: #f


```

**Misc**

```scheme

;;;; String Construction

1 ]=> (make-string 10 #\x)

;Value 21: "xxxxxxxxxx"

1 ]=> (make-string 20 #\a)

;Value 22: "aaaaaaaaaaaaaaaaaaaa

1 ]=> (string #\s #\c #\h #\e #\m #\e)

;Value 23: "scheme"

;;;; Length of a String

1 ]=> (string-length "1234567890")

;Value: 10

;;;; Trim String

1 ]=> (string-trim " \n\nlisp\n scheme    \n\n")

;Value 18: "lisp\n scheme"

1 ]=> (string-trim-left " \n\nlisp\n scheme    \n\n")

;Value 19: "lisp\n scheme    \n\n"

1 ]=> (string-trim-right " \n\nlisp\n scheme    \n\n")

;Value 20: " \n\nlisp\n scheme"

1 ]=> 

;;;; Append Strings

1 ]=> (string-append "hello " "world" "  scheme" " lisp ")

;Value 25: "hello world  scheme lisp "


;;; Split String 
;;
;; Not defined in MIT-Scheme, but defined in 
;; Chicken Scheme (csi Repl), GNU Guile and others.
;;
> (string-split "hello world")
("hello" "world")

> (string-split "hello:world"  ":")
("hello" "world")
 


```

**To String**

```scheme
1 ]=> (number->string 12.323)

;Value 11: "12.323"

1 ]=> (symbol->string 'asymbol)

;Value 12: "asymbol"

1 ]=> 

1 ]=> (list->string '(#\h #\e #\l #\l #\o #\space #\w #\o #\r #\l #\d))

;Value 16: "hello world"
```

**From String**

```scheme
1 ]=> (string->number "222.23")

;Value: 222.23

1 ]=> (string->symbol  "asymbol")

;Value: asymbol


;;; To list of characters
;;
1 ]=> (string->list "hello world")

;Value 15: (#\h #\e #\l #\l #\o #\space #\w #\o #\r #\l #\d)


```

### List Operations

#### Define a List

```scheme
1 ]=> '(1 2 3 4 5 6 7 8)

;Value 11: (1 2 3 4 5 6 7 8)

1 ]=> (list 1 2 3 4 5 -100)

;Value 15: (1 2 3 4 5 -100)

;;--------------------------------------;;

1 ]=> '("hello" "world" "scheme" "lisp")

;Value 12: ("hello" "world" "scheme" "lisp")


1 ]=> (list "hello" "world" "scheme" "lisp")

;Value 16: ("hello" "world" "scheme" "lisp")

;;--------------------------------------;;

1 ]=> '(list of symbols)

;Value 13: (list of symbols)

1 ]=> '((+ 10 20) (* 30 50) (+ 10 (* 3 4)))

;Value 14: ((+ 10 20) (* 30 50) (+ 10 (* 3 4)))

;;--------------------------------------;;

```

#### Primitive List Operations

* **Nil** - Empty List '()

* **Cons** - List constructor, Construct a list cell 

```scheme 
1 ]=> (define Nil '())

;Value: nil

1 ]=> Nil

;Value: ()

1 ]=> (Cons 5 Nil)

;Value 19: (5)

1 ]=> (Cons 5 (Cons 6 Nil))

;Value 20: (5 6)

1 ]=> (Cons 4 (Cons 5 (Cons 6 Nil)))

;Value 21: (4 5 6)
```

* **car** - Select the first element, "head" of a list cell

```scheme
1 ]=> (car (list 1 2 3 4))

;Value: 1

1 ]=> (car '(2 3 4))

;Value: 2

1 ]=> (car '(x y z))

;Value: x

```

* **cdr** - Select the "tail" of a list, removes the first element

```scheme 
1 ]=> (cdr (list 1 2 3 4))

;Value 17: (2 3 4)

1 ]=> (cdr '(x y z w))

;Value 18: (y z w)
```

#### List Functions

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



## Higher Order Functions

All the functions defined are in the file: [hof_functions.scm](src/hof_functions.scm) that can be loaded in scheme by typing:

```scheme
scheme@(guile-user) [1]>  (load "hof_functions.scm")
```

### Special Functions

```scheme
;;; Constant function - Will return a,
;;  regardless the value of x
;;
(define (constant a)
  (lambda (x) a))

(define (id x) x)

1 ]=> (id 10)

;Value: 10

1 ]=> (map id '(1 2 3 4 6))

;Value 11: (1 2 3 4 6)

1 ]=> ((constant 10) 20)

;Value: 10

1 ]=> ((constant 10) 2000)

;Value: 10

1 ]=> (define f (constant 10))

;Value: f

1 ]=> f

;Value 12: #[compound-procedure 12]

1 ]=> (f 45)

;Value: 10

1 ]=> (f 100)

;Value: 10

1 ]=> (map (constant 5) '(1 2 3 4 5 6))

;Value 13: (5 5 5 5 5 5)

```

### Functions Composition

**Basic Functiona Composition**


```scheme
1 ]=> (define (compose f g) (lambda (x) (f (g x))))

;Value: compose

;; Foward Composition
1 ]=> (define (fcompose f g) (lambda (x) (g (f x))))

;Value: fcompose


;;;;;;;;;;;;;;;;;;;;;;;;;;

1 ]=> (define pi (* 4 (atan 1.0)))

;Value: pi

1 ]=> pi

;Value: 3.141592653589793

1 ]=> (define (deg2rad deg) (* (/ deg 180) pi))

;Value: deg2rad

1 ]=> (map sind '(0 45 60 90 180 270))

;Value 13: (0 .7071067811865475 .8660254037844386 1. 1.2246467991473532e-16 -1.)

;;---------------

1 ]=> (define sind (fcompose deg2rad sin))

;Value: sind

1 ]=> (map sind '(0 45 60 90 180 270))

;Value 15: (0 .7071067811865475 .8660254037844386 1. 1.2246467991473532e-16 -1.)

```

**Composition of a list of Functions**

```scheme 

(define (__compose-funcs list-of-functions x)
  (if (null? list-of-functions)
      x
      (__compose-funcs (cdr list-of-functions) ((car list-of-functions) x))
  ))      

(define (compose-funcs . list-of-functions)
  (lambda (x)(__compose-funcs list-of-functions x)))

;;;------------------------------;;;

1 ]=> ((compose-funcs log exp sin asin) 0.60)

;Value: .6000000000000001

1 ]=> 
(define sind (compose-funcs deg2rad sin))

;Value: sind

1 ]=> (sind 30)

;Value: .49999999999999994

1 ]=> (sind 90)

;Value: 1.


```

### Applying Multiple Functions to a Single Argument


Returns a functions that takes a list of functions and applies it to a single value.

```scheme
(define (juxt . fxs)
  (lambda (x)
    (map (lambda (f) (f x)) fxs)))

1 ]=> ((juxt sqrt exp log) 3.0)

;Value 20: (1.7320508075688772 20.08553692318767 1.0986122886681098)

1 ]=> (define f (juxt sqrt exp log))

;Value: f

1 ]=> (f 3)

;Value 21: (1.7320508075688772 20.08553692318767 1.0986122886681098)


(map f '(1 2 3))

;Value 23: ((1 2.718281828459045 0) (1.4142135623730951 7.38905609893065 .6931471805599453) (1.7320508075688772 20.08553692318767 1.0986122886681098))


```

### Miscellaneous

**Replicate N times a value**

```scheme

(define (replicate n x)
    (if (zero? n)
        ;; Then
        '()
        ;; Else
        (cons x (replicate (- n 1) x))
    )
);; End of replicate


1 ]=> (replicate 4 0)
$12 = (0 0 0 0)

1 ]=> (replicate 8 "abc")
$13 = ("abc" "abc" "abc" "abc" "abc" "abc" "abc" "abc")

1 ]=> (replicate 3 'symbol)
$15 = (symbol symbol symbol)

```

**Cycle**

```scheme

(define (cycle_aux n xs acc)
    (if  (zero? n)     ;;(or (zero? n) (null? xs))
        ;; Then
        '()
        ;; Else
        (if (null? xs)
            (cycle_aux (- n 1) acc acc)
            (cons (car xs) (cycle_aux (- n 1) (cdr xs) acc))
        )
    );; End if
);; End of cycle_aux
        


1 ]=> (cycle_aux 10 '(1 2 3) '(1 2 3))
$16 = (1 2 3 1 2 3 1 2)

1 ]=> (cycle_aux 20 '(1 2 3) '(1 2 3))
$17 = (1 2 3 1 2 3 1 2 3 1 2 3 1 2 3)

(define (cycle n xs)
    (cycle_aux n xs xs))
    
1 ]=> (cycle 10 '(a b c))
$18 = (a b c a b c a b)

1 ]=> (cycle 20 '(0 1))
$19 = (0 1 0 1 0 1 0 1 0 1 0 1 0 1)
    
```

**Count Number of Elements**

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

```

**Sum of list elements**

```scheme
(define (sum-list alist)
    (if (null? alist)
        0
        (+ (car alist) (sum-list (cdr alist)))))

1 ]=> (sum-list '(1 2 3 4 5 6 ))

;Value: 21

```

**Find Element in a List**

```scheme
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

```

**Drop / Take n elements**

```scheme

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
```

**Take while and Drop While**

```scheme
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

```

**Select (Filter) / Reject list elements**

```scheme
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

```




**Folds: Fold Right/ Fold Left**

```scheme

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

**Zip Lists**

```scheme

(define (zip2 list1 list2)
    
    (if (or (null? list1) (null? list2))
        ;; Then
        '()        
        ;; Else
        (cons (list (car list1) (car list2)) 
            (zip2 (cdr list1) (cdr list2)))
    )
)

1 ]=> (zip2 '() '())

;Value: ()

1 ]=> (zip2 '() '(1 2 3 4))

;Value: ()

1 ]=> (zip2 '(1 2 3 4 5) '())

;Value: ()

1 ]=> (zip2 '(1 2 3 4 5) '(a b c d e f g i j l m n))

;Value 11: ((1 a) (2 b) (3 c) (4 d) (5 e))

1 ]=> 

;;
;; The function zip is defined in MIT-Scheme, but not in other Schemes
;; like GNU-Guile.
;;

;;; Returns true if any element of a list satisfies the predicate 
;;  function
;;
(define (any predicate alist)
    (if (null? alist)
        ;; Then
        #f
        ;; Else
        (if (predicate (car alist))
            #t
            (any predicate (cdr alist))
        )
    ) ;; End if
);; End of any


1 ]=> (any (lambda (x) (> x 10))  '( -3 4 5 8 9))
$1 = #f

1 ]=> (any (lambda (x) (> x 10))  '( -3 10 4 5 8 20 9))
$2 = #t

1 ]=> (any (lambda (x) (> x 10))  '())
$3 = #f


1 ]=> (any null? (list '(1 2) '(3 5)))
$5 = #f

1 ]=> (any null? (list '(1 2) '(3 5) '()))
$6 = #t


(define (zip_aux list-of-lists)
    (if (any null? list-of-lists)
        ;; Then
        '()
        ;; Else
        (cons (map car list-of-lists) (zip_aux (map cdr list-of-lists)))
    )
)

(define (zip . lists) (zip_aux lists))

1 ]=> (zip_aux (list '( 1 2 3 4 5) '(a b c d e f g h) '("hello" "schme" "lisp" "fp")))
$7 = ((1 a "hello") (2 b "schme") (3 c "lisp") (4 d "fp"))

1 ]=> (zip '( 1 2 3 4 5) '(a b c d e f g h) '("hello" "schme" "lisp" "fp"))
$7 = ((1 a "hello") (2 b "schme") (3 c "lisp") (4 d "fp"))



```

**Unzip Lists**

```scheme

(define (unzip2 list-of-pairs)
    (if (null? list-of-pairs)
        ;; Then
        '()
        ;; Else        
        (list (map car list-of-pairs)  (map (lambda (xy) (car (cdr xy)))  list-of-pairs))
    )
)
    
1 ]=> (define xys '((1 a) (2 b) (3 c) (4 d)))

1 ]=> xys
$9 = ((1 a) (2 b) (3 c) (4 d))

1 ]=> (unzip2 xys)
$11 = ((1 2 3 4) (a b c d))

;;  Unzip with foldr
;;
;; Haskell:
;; 
;; unzip :: [(a, b)] -> ([a], [b])
;; unzip = foldr f ([],[])
;;  where f (x,y) ~(xs,ys) = (x:xs,y:ys)
;;

(define (foldr f_el_acc acc alist)
    (if (null? alist)
        acc
        (f_el_acc (car alist) (foldr f_el_acc acc (cdr alist)))))

(define fst car)
(define (snd xs) (car (cdr xs)))
        
(define (unzip2f list-of-pairs)
    (if (null? list-of-pairs)
        '()
        (foldr 
            (lambda (xy xys)  
                (list 
                    (cons (fst xy) (fst xys))
                    (cons (snd xy) (snd xys))
                )
            )
            (list '() '()) ;; ([],[])
            list-of-pairs
        )
    )
)

1 ]=> (unzip2f '((1 a) (2 b) (3 c) (4 d)))
$24 = ((1 2 3 4) (a b c d))
            
1 ]=> (unzip2f '())
$25 = ()
    

;;;;; Unzip for list of multiple lists
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Constant function - Will return a,
;;  regardless the value of x
;;
(define (constant a)
  (lambda (x) a))

(define (unzip-aux alist)
    (map (lambda (x) (list x)) alist)
    )

(define (unzip list-of-list)
    (if (null? list-of-list)
        ;; Then
        '()        
        ;; Else
        (foldr 
            (lambda (t ts)
              
               (map
                (lambda (x) (cons (car x) (car (cdr x)))) 
                (zip t ts)
               )             
              );; End lambda

            (map (constant '()) list-of-list)
            list-of-list
                         
        );; End foldr

    );; End If
)

;; Variadic version of unzip
;;
(define (unzip-v . lists) (unzip lists))

1 ]=> (unzip '( (1 2 3) (11 10 20) (30 40 50)))
$4 = ((1 11 30) (2 10 40) (3 20 50)

1 ]=> (unzip '( (1 a "c") (230 b "xs") (1000 sym "ccw") (434 con "xyzw")))
$5 = ((1 230 1000 434) (a b sym con) ("c" "xs" "ccw" "xyzw"))

1 ]=> (unzip-v '(1 2 3) '(11 10 20) '(30 40 50))
$6 = ((1 11 30) (2 10 40) (3 20 50))

1 ]=> (unzip-v '(1 a "c") '(230 b "xs") '(1000 sym "ccw") '(434 con "xyzw"))
$7 = ((1 230 1000 434) (a b sym con) ("c" "xs" "ccw" "xyzw"))

```

**Zip With**

```scheme

(define (zip_aux list-of-lists)
    (if (any null? list-of-lists)
        ;; Then
        '()
        ;; Else
        (cons (map car list-of-lists) (zip_aux (map cdr list-of-lists)))
    )
)

(define (zip_with f list-of-lists)
    (map (lambda (xs) (apply f xs)) (zip_aux list-of-lists))
)

1 ]=> (define (f x y z) (+ (* 3 x) (* 4 y) (* -5 z)))

;Value: f

;;
;;  -22 = f 1 5 9 = (+ (* 3 1) (* 4 5) (* -5 9))) = (+ 3 20 -45) = -22
;;   -5 = f 2 6 7 = (+ (* 3 2) (* 4 6) (* -5 7))) = (+ 6 24 -35) = -5
;;   22 = f 3 7 3 = (+ (* 3 3) (* 4 7) (* -5 3))) = (+ 9 28 -15) =  22
;;
1 ]=> (zip_with f '((1 2 3) (5 6 7) (9 7 3)))
;Value 15: (-22 -5 22)

```

## Kawa Scheme - Access Java API from Scheme

[Kawa Scheme](http://www.gnu.org/software/kawa) is a implementation of the language in Java that can compile to Java bytecodes and has access to the Java API.

Features:

* Compiles to JVM
* Can be embedded as  scripting language for Java applications.
* Can call Java API in the REPL, provides interactive Java development.
* With some tricks can compile to Android "Java", Dalvik VM.
* Product built with Kawa Scheme: [App Inventor for Android](https://en.wikipedia.org/wiki/App_Inventor_for_Android)

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

**Temperature Conversion GUI**

![](images/temperature_conversion_gui.png)

File: [FahrenheitGUI.scm](src/FahrenheitGUI.scm)
```scheme
;; Must be used with Kawa
;; 
;; Based on: http://www.cs.dartmouth.edu/~cs5/lectures/0509/FahrenheitGUI.java 
;; 
;;
;;


;; import java.awt.*;
;; import java.awt.event.*;
;;import javax.swing.*;
;;

(define JFrame javax.swing.JFrame)
(define JLabel javax.swing.JLabel)
(define JPanel javax.swing.JPanel)
(define JTextField javax.swing.JTextField)

(define WIDTH 300)
(define HEIGHT 75)

(define (fahrenheit2celcius f) (* (- f 32.0) (/ 5.0 9.0)))

;; frame = new JFrame("Temperature Conversion");
;;
(define frame (JFrame "Temperature Conversion"))

;;frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
;;
(frame:setDefaultCloseOperation JFrame:EXIT_ON_CLOSE)

(define inputLabel (JLabel "Enter Fahrenheit temperature (Hit Return to Compute the temperature):"))

;;JLabel outputLabel = new JLabel("Temperature in Celsius: ") 
(define outputLabel (JLabel "Temperature in Celsius: ")) 

(define resultLabel (JLabel "---"))  
 
;; fahrenheit = new JTextField(5); 
;; 
(define fahrenheit (JTextField 5))  
  
;; panel = new JPanel(); 
;; 
(define panel (JPanel))

;; panel.setPreferredSize(new Dimension(WIDTH, HEIGHT));
(panel:setPreferredSize (java.awt.Dimension WIDTH HEIGHT))
(panel:setBackground java.awt.Color:yellow)

;;(panel:setPreferredSize (new Dimension(WIDTH, HEIGHT))

(panel:add inputLabel)
(panel:add fahrenheit)
(panel:add outputLabel)
(panel:add resultLabel)
(frame:add panel)
(frame:pack)

(define show_frame (lambda () (frame:setVisible #t)))


(define (update-temperature)   
   (resultLabel:setText 
     (number->string (fahrenheit2celcius (string->number (fahrenheit:getText))))))

(define-namespace ActionListener <java.awt.event.ActionListener>)
(define-namespace ActionEvent <java.awt.event.ActionEvent>)


; ; fahrenheit.addActionListener(new TempListener());    
;;(frame:addActionListener (lambda (evt) (update-temperature)))
;;
(define action-listener
  (object (ActionListener)
    ((action-performed e :: ActionEvent) :: <void>
      (update-temperature))))

(fahrenheit:addActionListener action-listener)

(show_frame)

```

### See also


* https://en.wikipedia.org/wiki/Kawa_(Scheme_implementation)

* [Kawa scheme and event handlers](http://www.redmountainsw.com/wordpress/1395/kawa-scheme-and-event-handlers/)

* [Exploring LISP on the JVM](http://www.infoq.com/articles/lisp-for-jvm)
* [EXPLORING LISP ON THE JVM](http://pjacobsson.com/articles/lisp-on-the-jvm.html)

* [Kawa: Compiling Scheme to Java](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.24.5572&rep=rep1&type=pdf)
* [Kawa: Compiling Scheme to Java](http://www.delorie.com/gnu/docs/kawa/kawa-tour_2.html)



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

* [Why Structure and Interpretation of Computer Programs matters](https://www.cs.berkeley.edu/~bh/sicp.html)

* [7 lines of code, 3 minutes: Implement a programming language from scratch](http://matt.might.net/articles/implementing-a-programming-language/)


* [Canonical S-expression](https://en.wikipedia.org/wiki/Canonical_S-expressions)
* [SEXP---(S-expressions)](http://people.csail.mit.edu/rivest/sexp.html)
* [S-Expressions](http://people.csail.mit.edu/rivest/Sexp.txt)
