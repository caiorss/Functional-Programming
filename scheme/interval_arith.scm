;;
;;
;; 2.1.4 Extended Exercise: Interval Arithmetic
;; 
;;
;;  interval [l1 , u1] --> l1 <= x <= u1
;;  interval [l2 , u2] --> l2 <= x <= u2
;;
;;
;;

(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (upper-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

(define (inv-interval x)
  (make-interval (/ 1.0 (upper-bound x))
                 (/ 1.0 (lower-bound x))))

(define (div-interval x y)
  (mul-interval x (inv-interval y)))

(define (max-upper-bound x y)
  (max (upper-bound x) (upper-bound y)))

(define (min-lower-bound x y)
  (min (lower-bound y) (lower-bound y)))

(define (width-upper-bound x y)
  (- (upper-bound x) (upper-bound y)))

(define (width-lower-bound x y)
  (- (lower-bound x) (lower-bound y)))

(define (join-interval x y)
  (- (max-upper-bound x y) (min-lower-bound x y)))

;;
;;  min x -- max x
;;  min y -- max y
;;
;;  y - x ==> 
;;
(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y)
                   (- (upper-bound x) (lower-bound y)))))
                 
(define (make-center-percent c p)
  (let
      ((a (* (/ p 100.0) c)))
      (make-interval (- c a) (+ c a))))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))


(define (center x)
  (/ (+  (upper-bound x) (lower-bound x)) 2.0))

;;
;;  Xmin = x - tol.x  ==>  x   = (Xmax + Xmin)/ 2 = center (interval)
;;  Xmax = x + tol.x  ==>  tol = (Xmax - Xmin)/(2x) = width(inteval) / 2 * center(interval)
;;  
(define (tolerance-percent x)
  (/ (* 100 (width x)) (* 2 (center x))))

(define (center-tol x)
  (cons (center x) (tolerance-percent x)))

;;
;; Parallel resistance
;;
(define (para1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (para2 r1 r2)
  (inv-interval (add-interval (inv-interval r1)
                              (inv-interval r2))))
