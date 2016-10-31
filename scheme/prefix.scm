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


(define (operand? x)
  (or (number? x) (string? x)))

(define (operator-list op)
  (list
   (cons '+ +)
   (cons '- -)
   (cons '/ /)
   (cons '* *)
   ))

(define (precendence op)
  (list
   (cons '+ 1)
   (cons '- 1)
   (cons '* 3)
   (cons '/ 3)
   ))

(define (operator? op)
  (member op (map car operator-list))

(define operand-stack (make-stack))
(define operator-stack (make-stack))

(define (append-el alist elem) (append alist (cons elem '())))

(define (infix->postfix expr)

  (define postfix '())
  
  (define (hasHigherPrec_top op)
    (> (precendence (operator-stack 'top)) (precendence op)))

  (define (aux expr)

    (if (?null expr)
        (operator-stack 'pop-all)

        (let
            (
             (hd (car expr))
             (tl (cdr expr))
            )

          (if (operand? hd)
              (set! postfix (cons hd postfix))
          (if (
              

          

           )                
   ))


  );  

  

          

      
