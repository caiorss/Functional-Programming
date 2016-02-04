#! /usr/bin/scsh  -s 
!#

;; #!/usr/bin/scsh -o srfi-1 -o srfi-2 -o srfi-9 -o srfi-13 -o srfi-14 -o srfi-16 -o srfi-37 -e main -s
;; !#


(define (tree)
  (run/string (tree)))

(define (ls)
  (run/string (ls)))


(define (find-files directory pattern)
    (port->list read-line
              (run/port
               (find ,directory -name ,pattern))))

;; Move file from origin to destine  
;;
(define (mv orig dest)
   (run (mv ,orig ,dest)))

(define (mv-backup filename)
   (mv filename (string-append filename ".back" )))

;; Converts org-mode to html file 
;;
(define (org->html filename)
  (& (emacs ,filename --batch  -f org-html-export-to-html --kill)))

;; Converts org-mode to github markdown file *.md
;;
(define (org->gfm filename)
  (& (emacs -l ~/.emacs.d/init.el --batch --visit ,filename -funcall org-gfm-export-to-markdown --kill)))


(display "Running ... ")

(define (update)

   ;; Export all .org mode files to github markdown .md    
  (for-each (lambda (f)

              (display (string-append "Updating " f))
              (newline)
              
              (org->gfm  f)
              (org->html f))

          (find-files "." "*.org")))


(define (main . args)

  (update)
  (display "Update Finished.")
  ) ;; 

(display "Running")

(main)


