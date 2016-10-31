;;
;; Forth Interpreter in Scheme
;;
;; Evaluates Expressions in Postfix notations
;;
;;  60 100 + 2 * ==> (60 + 100) * 2 ==> 160*2 ==> 320
;;

;; 
;; Returns true if symbol is an operator, false otherwise
(define (operator? op)
  (member op '(= + / *)))

(define (get-operator op)
  (cond
   ((eq? '+ op) +)
   ((eq? '- op) -)
   ((eq? '* op) *)
   ((eq? '/ op) /)
   ))

(define stack '())

(define (push elem)
  (set! stack (cons elem stack)))
  
(define (pop)
  (let
      (
       (out (car stack)))
      (begin
        (set! stack (cdr stack))
        out
      )))
  
  

(define (eval-postfix sexp)
  (if (null? sexp)
      (pop)
      
      (let
          (
           (sym   (car sexp))
           (tail  (cdr sexp))
           )
        (
         (cond
          ((number? sym) (push sym))
          ((operator? sym) (push ((get-operator sym) (pop) (pop))))
          
          )
         (if (not (null? tail))
            (eval-postfix tail))
         )
        );; End let
      ));; End eval-postfix
  
;;   ((number? (car sexp))  (push (car sexp)))
;;   ((operator? (car sexp)) ((car sexp) (push) (push)))))
  
