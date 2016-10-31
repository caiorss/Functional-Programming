
;;; 1.2.1 Recursion X Iteration
;;;
;;;

(define (sum-ints n)
  (if (= n 0)
      0
      (+ n (sum-ints (- n 1)))))

;; Factorial with recursion
;;
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; Factorial with Iteration
;;
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (factorial2 n)
  (fact-iter 1 1 n))

(define (fib-aux n a b)
  (if (or (= n 0) (= n 1))
      b
      (fib-aux (- n 1) b (+ a b))))

(define (fib-fast n) (fib-aux n 1 1))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))




(define (divisor? n) (lambda (i) (= 0 (modulo n i))))

(define (range a b step)
  (if (> a b)
      '()
      (cons a  (range (+ a step) b step))))
  
(define (prime? n)
  (null? (filter (divisor? n) (range 2 (- n 1) 1))))


(define state '())

(for-each (lambda (i)
            [call/cc (lambda (h)
                       (set! state h)
                       (display i))]
            )

          '(1 2 3 4 5))
            
            
(define-macro (defun name args body)
  `(define (,name ,@args) ,body))

(define-macro (defvar name value)
  `(define ,name ,value))

(define (make-symbol base sym)
  (string->symbol (string-append base (symbol->string sym))))

(define-macro (make-recored name fields)
       `(define ( ,(make-symbol "make-" name) ,@fields) ,fields))


(define (template value body)
  (map (lambda (row) `((,(car row) ,value) ,(cadr row))) body))


(define-macro (case-pred value . body)  
  `(cond 
      ,@(template value body)))

(define body '((negative? "Neg") (positive? "Pos") (zero? "Zero")))



(case-pred x
   (negative? "Neg")
   (positive? "Pos")
   (zero?     "Zero"))
       
(case-pred 10
   (negative? "Neg")
   (positive? "Pos")
   (zero?     "Zero"))

(cond 
 ((negative? x) "Neg")
 ((positive? x) "Pos")
 ((zero? x)    "Zero"))


(define-syntax case-pred
    (syntax-rules () 
     ((case-pred value (predicate result) ...)
      (cond ((predicate value) result) ...))))


(define plist '(
               (
                name: "Netherlands"
                lang: (Dutch)
                capital: "Amsterdam"
                domain: ".nl"
                ISO3166code: "NL"
                currency: EUR
                )
               (
                name: "Australia"
                lang: (English)
                capital: "Camberra"
                domain: ".au"
                ISO3166code: "AU"
                currency: AUD
                )
               (
                name: "Canada"
                lang: (English French)
                domain: ".ca"
                ISO3166code: "CA"
                currency: CAD
                )))
                
(define data '((name: . "Canada")  (lang: . (English French)) (domain: ".ca") (ISO3166code: "CA")))          

(define (get-keys assocl)
  (map car assocl))

(define (get-values assocl)
  (map cdr assocl))
                
(define (get-key key assocl)
  (cdr (assoc key assocl )))
  
(define (get-key/c key)
  (lambda (assocl) (cdr (assoc key assocl))))
                
(define (make-record-constructor fields)
  (lambda (values)
    (map (lambda (f v) (cons f v)) fields values)))


(define (plist->assoc plist)
  (if (null? plist)
      '()      
      (cons
       (cons (car plist) (cadr plist))
       (plist->assoc (cddr plist)))))

(define (assoc->plist assocl)
(if (null? assocl)
      '()
      (let
          ((hd (car assocl))
           (tl (cdr assocl)))
        (cons (car hd)
           (cons (cdr hd)
                 (assoc->plist tl))))))
        

(define (sexp->string sexp)
  (call-with-output-string
   (lambda (out)
     (write sexp out))))

(define (string->sexp str)
  (with-input-from-string str
    (lambda () (read))))

(define (sexp->file filename sexp)
  (define out (open-output-file filename))
  (begin
    (write sexp out)
    (close-output-port out)))

(define (file->sexp filename)
  (define in (open-input-file filename))
  (define sexp (read in))
  (begin
    (close-input-port in)
    sexp))
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; https://en.wikibooks.org/wiki/Scheme_Programming/Abstractions_with_Data
;;

(define typed-variable
  (lambda (type value)
    (cons 'Typed (list type value))
  )
)
(define typed?
  (lambda (var)
    (and (list? var) (= 'Typed (car var)))
  )
)
(define type-of
  (lambda (var)
    (if (typed? var)
      (car (cdr var)
    )
  )
)

;; Clojure like defn in scheme
(define-macro (defn name args body)
  `(define (,name ,@args) ,body))

;; Define variable, value
(define-macro (def name value)
  `(define ,name ,value))

;; Lambda function statement
;;
(define-macro (fn args body)
  `(lambda ,args ,body))

