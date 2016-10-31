;; SCIP page 83
;;
;; Data Abstraction:
;;
;;   * Compounded data
;;
;;   * Data abstraction: the general technique to isolate
;;     parts of parts of a program that deal of how data are represented.
;;
;;     Metodology that enables that enable us to isolate how a compound 
;;     data object is used from the details of how its constructed from 
;;     a more primitive data object.
;;
;;     --> Selectors
;;     --> Constructors
;;
;;     The program should data in a way that makes no assumption the data that
;;     are not strictely necessary to perform the task at hand.
;;
;;    * Concrete representation: Is defined independet of the program that uses ;;     the data.
;;
;;   * Conventional Interfaces: Compound data object can serve
;;     as conventional interfaces for combining program modules
;;
;;   * Exploit closures
;;
;;   * Generic operations: Deal with data that may be represent
;;     parts of a program
;;
;;
;;    * Data-directed programming: Technique that allows individual
;;     data representation to be designed in  isolation and then combined
;;    addictively without modification.
;;
;;------------------------------------------------------------------------


;; Set of procedures to manipulate racional numbers
;;
;;
;;  (make-rat <n> <d>)  --> returns (n d) / Constructor
;;  (numer <x>)         --> returns n
;;  (denom <x>)         --> returns d

(define (make-rat n d)
  (let ((g (gcd n d)))
  (cons (/ n g) (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

;;
;;  n1/d1 + n2/d2 = (n1.d2 + n2.d1)/(d1.d2)
;;
(define (add-rat x y)
  (define n1 (numer x))
  (define n2 (numer y))
  (define d1 (denom x))
  (define d2 (denom y))
  (make-rat (+ (* n1 d2) (* n2 d1))
            (* d1 d2)))
;;
;;
;; n2/d2 - n1/d1 = (n2.d1 - n1.d2)/d1.d2
;;
(define (sub-rat x y)
  (define n1 (numer x))
  (define n2 (numer y))
  (define d1 (denom x))
  (define d2 (denom y))
  (make-rat (- (* n2 d1) (* n1 d2))
            (* d1 d2)))

;;
;; (n1/d1)/(n2/d2) = (n1.d2)/(d1.n2)
;;
(define (div-rat x y)
  (make-rat (* (numer x) (denom y)
               (denom x) (numer y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
;;
;; n1/d1 == n2/d2 ==> n1.d2 = n2.d1
;;
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))

(define-syntax letrat5
  (syntax-rules ()
    ((letrat expr ...)    
     (let
         (
          (+ add-rat)
          (- sub-rat)
          (* mul-rat)
          (/ div-rat)
          )
       expr ...))))

(define-syntax letrat
  (λ (x)
    (syntax-case x ()
      ((letrat expr ...)
       (with-syntax ((+ (datum->syntax x '+))
                     (- (datum->syntax x '-))
                     (* (datum->syntax x '*))
                     (/ (datum->syntax x '/)))
         #'(let ((+ add-rat)
                 (- sub-rat)
                 (* mul-rat)
                 (/ div-rat))
             expr ...))))))

(define-syntax for
  (syntax-rules ()
    ((for element in list body ...)
     (map (lambda (element)
            body ...)
          list))))

(define-syntax swap
  (syntax-rules ()
    ((swap a b)

     (let ((tmp a))
       (set! a b)       ;; a <- b
       (set! b tmp))))) ;; b <- tmp

;; (define-syntax-rule (swap x y)
;;   (let ((tmp x))
;;     (set! x y)
;;     (set! y tmp)))

(define-syntax-rule (null! x)
  (set! x '()))

(define-syntax-rule (delayc expr)
  (lambda () expr))

(define-syntax-rule (forcec expr)
  (expr))
