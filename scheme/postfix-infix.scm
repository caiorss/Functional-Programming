;;
;;  Evaluation of Prefix and Postfix expressions using stack 
;;  * https://www.youtube.com/watch?v=MeRb_1bddWg
;;
;;  Infix, Prefix and Postfix
;;  * https://www.youtube.com/watch?v=jos1Flt21is
;;
;;  * http://cs-study.blogspot.com.br/2012/11/infix-to-postfix-conversion.html
;;  * http://interactivepython.org/runestone/static/pythonds/BasicDS/InfixPrefixandPostfixExpressions.html
;;  * http://scriptasylum.com/tutorials/infix_postfix/algorithms/infix-postfix/
;;
;; * ftp://ftp.cs.indiana.edu/pub/scheme-repository/doc/pubs/swob.txt
;; * http://okmij.org/ftp/Scheme/oop-in-fp.txt
;;
;;


(define (make-stack)   
  (define stack '())
  (define (show) stack)
  (define (top) (car stack))
  (define (empty?)
    (null? stack))

  (define (reset)
    (set! stack '()))

  (define (push x)
    (set! stack (cons x stack)))

  (define (pop)
    (let
        ((p (car stack)))
      (begin
        (set! stack (cdr stack))
        p)))
  (define (pop-all)
    (let
        ((p stack))
      (begin
        (set! stack '())
        p)))
  (lambda (selector . args)
    (case selector
      ((show)    (apply show args))
      ((reset)   (apply reset args))
      ((push)    (apply push args))
      ((pop)     (apply pop args))
      ((top)    (apply top args))
      ((pop-all) (apply pop-all args)))))

(define (operator? sym)
  (member sym '(+ - * / ^)))

(define operators
  (list (cons '+ +) (cons '- -) (cons '* *) (cons '/ /)))

(define (operator-func sym)
  (cdr (assoc sym operators)))

(define functions-list
  (list (cons 'sin sin)
        (cons 'cos cos)
        (cons 'exp exp)
        (cons 'expt expt)
        (cons 'log log)
        (cons 'sqrt sqrt)
        (cons 'sqr (lambda (x) (* x x)))
        ))
        

(define (function? sym)
  (assoc sym functions-list))

(define (find-function sym)
  (cdr (assoc sym functions-list)))
  



(define (evaluate-postfix expr)

  (define s (make-stack))
  (define (evaluate-function expr)
    (s 'push ((find-function expr) (s 'pop))))

  (define (evaluate-operator op)
    (let
        ((a (s 'pop))
         (b (s 'pop))
         )
      (s 'push ((operator-func op) b a))))

  (define (evaluator expr)
    (if (null? expr)
        (s 'pop)
        (begin 
          (cond
           ((number? (car expr))    (s 'push (car expr)))
           ((operator? (car expr))  (evaluate-operator (car expr)))
           ((function? (car expr))  (evaluate-function (car expr))))
          
          (evaluator (cdr expr)))))

  (evaluator expr))


;;  while( not end of input ) {
;; c = next input character;
;; if( c is an operand )
;; add c to postfix string;
;; else {
;; while( !s.empty() && prcd(s.top(),c) ){
;; op = s.pop();
;; add op to the postfix string;
;; }
;; s.push( c );
;; }
;; while( !s.empty() ) {
;; op = s.pop();
;; add op to postfix string;
;; } 

;; (define (infix->postfix expr)
;;   (define s (make-stack))

;;   (define (aux expr)

;;   ))
  
  
  
  

;;
;;  100 20 + 3 * 20 -
;;  (100 + 20) 3 * 20 -
;;  120 3 * 20 - 
;;  (120 * 3) 20 -
;;  360 20 -
;;  (360 - 20)
;;  340
;; 
(define expr1 '( 100 20 + 3 * 20 - ))
