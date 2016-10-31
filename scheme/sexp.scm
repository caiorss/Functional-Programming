;;
;;
;; 1 ]=> (map (juxt-map get-key-c '(:id :name :deadline )) tasklist)
;;
;; ;Value 37: ((0 "Study the rules of the game" (date 2014 7 1)) (1 "Deliver the presentation" (from (date 2014 7 1) (date 2014 7 12))))
;;
;; 1 ]=> 


;;(load "data.txt")

(define (load-sexp-file filename)
  (define fd (open-input-file filename))
  (let
        (( sexp (read fd)))    
        (begin
         (close-input-port fd)
         sexp         
        )
    ))

(define (juxt . fxs)
  (lambda (x)
    (map (lambda (f) (f x)) fxs)))

(define (juxt-map func params)
  (apply juxt (map func params)))

(define (get-key key asslist)
  (cadr (assq key (cdr asslist))))

(define (get-key-c key)
  (lambda (asslist) (cadr (assq key (cdr asslist)))))

(define tasklist (load-sexp-file "data.txt"))

(define data
  (map (juxt-map get-key-c '(:id :name :location :deadline)) tasklist))
