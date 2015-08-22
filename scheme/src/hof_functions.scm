;; 
;; Author: Caior Rodrigues
;; Description: Higher Order Functions for Scheme Lisp
;;
;; This file implements Haskell Higher Order functions
;; in Scheme
;;                                                          ;;
;;----------------------------------------------------------;;


;;; Constant function - Will return a,
;;  regardless the value of x
;;
(define (constant a)
  (lambda (x) a))

(define (id x) x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (compose f g) (lambda (x) (f (g x))))

(define (fcompose f g) (lambda (x) (g (f x))))

(define (__compose-funcs list-of-functions x)
  (if (null? list-of-functions)
      x
      (__compose-funcs (cdr list-of-functions) ((car list-of-functions) x))
  ))      

(define (compose-funcs . list-of-functions)
  (lambda (x)(__compose-funcs list-of-functions x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 
;; Returns a functions that takes a list of functions and applies it
;; to a single value
;;
(define (juxt . fxs)
  (lambda (x)
    (map (lambda (f) (f x)) fxs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (replicate n x)
    (if (zero? n)
        ;; Then
        '()
        ;; Else
        (cons x (replicate (- n 1) x))
    )
);; End of replicate


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

(define (cycle n xs)
  (cycle_aux n xs xs))

(define (count-list alist)
    (if (null? alist)
        0
        (+ 1 (count-list (cdr alist)))))

(define (sum-list alist)
    (if (null? alist)
        0
        (+ (car alist) (sum-list (cdr alist)))))

(define (find predicate alist)
    (if (null? alist)
        (error "Error: Predicate not found")
        (if (predicate (car alist))
            (car alist)
            (find predicate (cdr alist)))))

;;; T A K E / D R O P 

(define (take n alist)
    (if (or (null? alist) (= n 0))
        '()
        (cons (car alist) (take (- n 1) (cdr alist)))))

(define (drop n alist)
    (if (or (null? alist) (= n 0))
        alist
        (drop (- n 1) (cdr alist))))

;; T A K E   W H I L E -- D R O P   W H I L E

(define (take_while predicate alist)
    (if (or (null? alist) (not (predicate (car alist))))
        '()
        (cons (car alist) (take_while predicate (cdr alist)))))

(define (drop_while predicate alist)
    (if (or (null? alist) (not (predicate (car alist))))
        alist
        (drop_while predicate (cdr alist))))

;;; S E L E C T  /  R E J E C T

(define (select predicate alist)
    (if (null? alist)
    '()
    (if (predicate (car alist))        
        (cons (car alist) (select predicate (cdr alist)))        
        (select predicate (cdr alist)))))

(define (reject predicate alist)
    (if (null? alist)
    '()
    (if (not (predicate (car alist)))        
        (cons (car alist) (reject predicate (cdr alist)))        
        (reject predicate (cdr alist)))))

;;; F O L D

(define (foldr f_el_acc acc alist)
    (if (null? alist)
        acc
        (f_el_acc (car alist) (foldr f_el_acc acc (cdr alist)))))

(define (foldl f_el_acc acc alist)
    (if (null? alist)
        acc
        (foldl f_el_acc (f_el_acc acc (car alist)) (cdr alist))))

; Z I P

(define (zip2 list1 list2)

    (if (or (null? list1) (null? list2))
        ;; Then
        '()        
        ;; Else
        (cons (list (car list1) (car list2)) 
            (zip2 (cdr list1) (cdr list2)))
    )
    )



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

(define (zip_aux list-of-lists)
    (if (any null? list-of-lists)
        ;; Then
        '()
        ;; Else
        (cons (map car list-of-lists) (zip_aux (map cdr list-of-lists)))
    )
)

(define (zip . lists) (zip_aux lists))


;;; U N Z I P

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

;;;;; Unzip for list of multiple lists
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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


(define (zip_with f list-of-lists)
    (map (lambda (xs) (apply f xs)) (zip_aux list-of-lists))
)
