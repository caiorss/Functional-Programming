
(define function-table
  (list
   (cons 'cons cons)
   (cons  'car  car)
   (cons  'cdr  cdr)
   (cons 'list list)
   (cons '+ +)
   (cons '- -)
   (cons '* *)
   (cons '/ *)
   (cons '** expt)
   (cons 'expt expt)
   (cons 'sin sin)
   (cons 'cos cos)
   (cons 'tan tan)
   (cons 'exp exp)
   (cons 'sqrt sqrt)
   (cons 'sqr (lambda (x) (* x x)))
   (cons 'log log)))

(define (get-key  key assoclist)
      (cdr (assoc key assoclist)))

(define (has-key? key assoclist)
  (member key (map car assoclist)))

(define (func? sym) (has-key? sym function-table))
(define (func-dispatch sym) (get-key sym function-table))
  

(define (operator-dispatch sym)
  (case sym
      ((+) +)
      ((-) -)
      ((/) /)
      ((*) *)
      ((**) expt)
    ))
;;
;; Evaluate s-expressions in prefix notation
;;
(define (eval-prefix expr)
  (if (or (number? expr) (string? expr) (null? expr))
      expr
      (let  ((p (car expr)))
        
          (cond
           ((list? p)  (eval-prefix p))
           ((func? p)(apply (func-dispatch p)
                       (map eval-prefix (cdr expr)))))                          
          
         );; End let
 ));; End of prefix-eval


(define expr '(+ (* 3 4) (- 5 8) (/ 10 2) 20))


