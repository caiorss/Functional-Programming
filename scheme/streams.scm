;; 
;; Lazy Evaluation / Stream - SCIP
;;
;;
;;
;;

(define-syntax
  lazy
  (syntax-rules ()
    ((lazy expression)
     (lambda () expression))))



(define-syntax
  cons-stream
  (syntax-rules ()
    ((cons-stream head tail)       
     (cons head (lambda () tail)))))

(define-syntax
  force-thunk
  (syntax-rules ()
    ((force-thunk stream)
     (stream))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force-thunk (cdr stream)))

(define empty-stream '())

(define (range start stop step)
  (if (> start stop)
      empty-stream
      (cons-stream start (range (+ start step) stop step))))

(define stream-null? null?)

(define (start-from n)
  (cons-stream n (start-from (+ n 1))))

(define (take n stream)
  (if (or (zero? n) (stream-null? stream))
      empty-stream
      (cons (stream-car stream) (take (- n 1) (stream-cdr stream)))))

;; Tail recursive version
;;
(define (take-tr n stream)
  (define (take-acc n stream acc)
    (if (or (zero? n) (stream-null? stream))
        acc
        (take-acc (- n 1) (stream-cdr stream)
                  (cons (stream-car stream) acc))))

  (reverse (take-acc n stream '())))

(define (show x)
  (display x))

(define (to-list stream)
  (define (to-list-acc stream acc)
    (if (stream-null? stream)
        acc
        (to-list-acc (stream-cdr stream)
                     (cons (stream-car stream) acc))))
  (reverse (to-list-acc stream '())))

(define (of-list list)
  (if (null? list)
      empty-stream
      (cons-stream (car list) (of-list (cdr list)))))

(define (stream-map proc .  argstreams)
  (if (stream-null? (car argstreams))
      empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;; (define (stream-map func stream)
;;   (if (stream-null? stream)
;;       empty-stream
;;       (cons-stream (func (stream-car stream)) (stream-map func (stream-cdr stream)))))


  


(define (stream-filter predic stream)
  (if (stream-null? stream)
      empty-stream
      (if (predic (stream-car stream))
          (cons-stream (stream-car stream) (stream-filter predic (stream-cdr stream)))
          (stream-filter predic (stream-cdr stream)))))
          
       
(define (stream-ref stream n)
  (if (stream-null? stream)
      (error "Element not found")
      (if (zero? n)
          (stream-car stream)
          (stream-ref (stream-cdr stream) (- n 1)))))
  
(define (stream-iterate func x)
  (cons-stream x (stream-iterate func (func x))))

;;;;;;; Fibonnaci Sequence ;;;;;;;;

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

;;;;;;; Primes - Sieve of Erastones ;;;;;;;;;;;;;

(define (divisible? x) (lambda (y) (= (remainder x y) 0)))
(define (not-divisble? x y) (not (= (remainder x y) 0)))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
             (lambda (x) (not-divisble? x (stream-car stream)))
             (stream-cdr stream)))))

(define primes (sieve (start-from 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (scale-stream x stream)
  (cons-stream  (* x (stream-car stream)) (scale-stream x (stream-cdr stream))))

(define (add-stream x stream)
  (cons-stream  (+ x (stream-car stream)) (add-stream x (stream-cdr stream))))
  

;;; Exercise 3.55 (Partial Sum)
;;;
(define (partial-sums stream)
  (define (partial-sums-acc stream acc)
    (if (stream-null? stream)
        0
        (let
            ((sum (+ acc (stream-car stream))))
          (cons-stream sum (partial-sums-acc (stream-cdr stream) sum)))))

  (partial-sums-acc stream 0))



;;;;;;;;;; Approximation of PI
;;;;;;;;;;
;;
;;
;;   pi/4 = 1 -1/3 + 1/5 - 1/7 = ...
;;


(define (pi-summands n sgn)
  (cons-stream (* (/ 1.0 (+ 1 (* n 2))) sgn) (pi-summands (+ n 1) (* -1 sgn))))


(define pi-stream (scale-stream 4 (partial-sums (pi-summands 0 1))))

;;
;;
;;  Euler Transform - Serie accelerator
;;
;;  Sn+2 = Sn+1 - (Sn+1 - Sn)^2 / (Sn-1 -2*Sn + Sn+1)
;;
;;


(define (euler-transform s)
  (define (sqr x) (* x x))
  (let (
        (s0 (stream-ref s 0)) ;; Sn-1
        (s1 (stream-ref s 1)) ;; Sn
        (s2 (stream-ref s 2)) ;; Sn+1
       )
    (cons-stream (- s2 (/ (sqr (- s2 s1)) (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))
        

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))


(define (stream-limit stream tolerance)
  (let
      (
       (s0 (stream-ref stream 0))
       (s1 (stream-ref stream 1))
      )
    (if (< (abs (- s0 s1)) tolerance)
        s1
        (stream-limit (stream-cdr stream) tolerance))))

(define (stream-sequence state-function state0)
  (let
      ( 
       (next-state   (cdr (state-function state0)))
       (next-output  (car (state-function state0)))
      )
    (cons-stream next-output (stream-sequence state-function next-state))))
  
