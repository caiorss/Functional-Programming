;;
;; Monads in Scheme
;;
;; * http://wiki.call-cc.org/eggref/4/monad
;; * 
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;---------------------------------

(define true #t)
(define false #f)

(define (some? opt)
  (if (eq? (car opt) 'Some)
      true
      (if (eq? (car opt) 'None)
          false
          (error "Not option type"))))

(define (none? opt)
  (not (some? opt)))

(define (option-value opt)
  (if (some? opt)
      (car opt)
      (error "None value")))
      

(define (option-bind opt optfunc)
  (if (none? opt)
      'None
      (optfunc (option-value opt))))
      
  
